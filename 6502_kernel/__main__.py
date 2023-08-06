from ipykernel.kernelapp import IPKernelApp
from . import ASM6502Kernel

IPKernelApp.launch_instance(kernel_class=ASM6502Kernel)
