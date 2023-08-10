# 6502 ASM - Jupyter Kernel

This is a simple 6502 assembly Jupyter kernel. It enables you to write and run 6502 assembly
in a Jupyter Notebook. Very useful for experimenting with 6502 routines.

## Installation

To install the 6502 emulator `little_6502_emu` which the kernel uses to run your 6502 assembly code:

```sh
sudo make install
```
To install the kernel itself:

```sh
pip install .
python -m 6502_kernel.install
```

## Using the 6502 ASM kernel in Jupyter Notebooks

Notebook: The New menu in the notebook should show an option for a 6502 notebook.

Try out the [`6502-Examples.ipynb`](6502-Examples.ipynb) notebook to see examples of usage.
