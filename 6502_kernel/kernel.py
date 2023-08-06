import os
import subprocess
import tempfile
import contextlib
import math
import textwrap

from ipykernel.kernelbase import Kernel


def build_binary(source):
    cmd = [
        "/usr/bin/64tass",
        "-",
        "-o",
        "/dev/stdout",
    ]
    binary = subprocess.check_output(cmd, input=source.encode(), stderr=subprocess.PIPE)
    return binary


@contextlib.contextmanager
def temp_binary(binary):
    with tempfile.NamedTemporaryFile(delete=False) as f:
        f.write(binary)
    path = f.name
    os.chmod(path, 0o755)
    try:
        yield path
    finally:
        os.unlink(path)


@contextlib.contextmanager
def run_binary(binary, *args, gdb_script=None, **kwargs):
    for name in ["stdin", "stdout", "stderr"]:
        kwargs.setdefault(name, subprocess.PIPE)

    cmd = []
    cmd += [
        "/usr/bin/6502_kernel",
    ]

    with temp_binary(binary) as binary_path:
        cmd += [binary_path, *args]
        process = subprocess.Popen(cmd, **kwargs)
        try:
            yield process
        finally:
            process.kill()


def html(element):
    def html_recursive(element):
        if isinstance(element, dict):
            assert len(element) >= 1
            node, *attrs = list(element.items())
            name, value = node
            attrs = " ".join(f'{attr}="{value}"' for attr, value in attrs)
            yield f"<{name} {attrs}>" + "".join(html_recursive(value)) + f"</{name}>"
        elif isinstance(element, (list, tuple)):
            for value in element:
                yield from html_recursive(value)
        else:
            yield str(element)

    return "".join(html_recursive(element))


class ASMRepl:

    def __init__(self):
        self.code_blocks = []
        self.gdb = None
        if self.__class__ == ASMRepl:
            raise NotImplementedError()

    @property
    def code(self):
        code = "".join(f"block\n" for block in self.code_blocks)
        code = code.strip()
        print(code)
        return code

    @property
    def gdb_script(self):
        return None

    def read(self, code):
        raise NotImplementedError()

    def evaluate(self, gdb_script=None):
        gdb_script = gdb_script or self.gdb_script
        binary = build_binary(self.code)
        print(repr(gdb_script), flush=True)
        with run_binary(binary, gdb_script=gdb_script) as process:
            stdout, stderr = process.communicate()
            returncode = process.returncode
        return dict(stdout=stdout, stderr=stderr, returncode=returncode)

    def reset(self):
        self.code_blocks.clear()


class LinearASMRepl(ASMRepl):

    @property
    def read(self, code):
        block_id = len(self.code_blocks)
        code = f".block.{block_id}:\n" + code
        self.code_blocks.append(code)
        return True

    def evaluate(self, *args, **kwargs):
        result = super().evaluate(*args, **kwargs)

        gdb_output = result["stdout"].decode()
        return self.print_result(gdb_output)

    def print_result(self, gdb_data):
        def register_values(block_id):
            return {
                k: (v[0] if k != "eflags" else v[1])
                for k, v in gdb_data[block_id].items()
                if k not in ["cs", "ss", "ds", "es", "fs", "gs"]
            }

        registers = register_values(".end")
        last_block_id = len(self.code_blocks) - 1
        changed = {
            k
            for k, v in register_values(f".block.{last_block_id}").items()
            if v != registers[k]
        }

        def register_style(register):
            if register in changed:
                return "background-color: rgba(255, 0, 255, 0.05)"
            return ""

        registers = [
            {
                "div": [{"b": k}, {"div": v}],
                "style": f"text-align: center; {register_style(k)}",
            }
            for k, v in registers.items()
        ]

        columns = 3
        rows = math.ceil(len(registers) / columns)

        def register_element(row, column):
            return registers[len(registers) // columns * column + row]

        result = {}
        result["registers"] = html(
            {
                "table": {
                    "tbody": [
                        {
                            "tr": [
                                {"td": register_element(row, column)}
                                for column in range(columns)
                            ]
                        }
                        for row in range(rows)
                    ]
                },
                "class": "table table-bordered",
                "style": "font-family: monospace",
            }
        )
        return result


class ASM6502Kernel(Kernel):
    implementation = "ASM"
    implementation_version = "0.1"
    language_info = {
        "name": "6502asm",
        "architecture": "6502",
        "mimetype": "text/x-gas",
        "file_extension": ".s",
    }
    banner = "6502 kernel"

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        # self.asm_repl = EditableASMRepl()
        self.asm_repl = LinearASMRepl()

    def do_execute(
        self, code, silent, store_history=True, user_expressions=None, allow_stdin=False
    ):
        print(code)
        if not silent:
            try:
                results = dict()
                results = self.asm_repl.evaluate()

                output = ""
                for key, value in results.items():
                    output += html({"h3": key})
                    if isinstance(value, str):
                        output += value
                    else:
                        if isinstance(value, bytes):
                            value = value.decode("latin")
                        output += html({"code": value})

                display_data = {
                    "metadata": {},
                    "data": {"text/html": output},
                }
                self.send_response(self.iopub_socket, "display_data", display_data)

            except subprocess.CalledProcessError as e:
                stderr = e.stderr.decode("latin")
                stream = {"name": "stderr", "text": stderr}
                self.send_response(self.iopub_socket, "stream", stream)

        return {
            "status": "ok",
            "execution_count": self.execution_count,
            "payload": [],
            "user_expressions": {},
        }

