# tractorbeam-2600
A game for the Atari 2600

Still work in progress. More levels to be added, and hopefully more stuff that moves.

![Screenshopt](tb-screenshot-jpg.jpg)

To compile, you need the dasm assembler. The file `compile.sh` assumed that it is located one directory up from the current directory, in a directory called `dasm`.

You may also need to edit the path to the include files at the start of `game.asm`.

To play, compile and then load either `tractor_beam_ntsc.bin` or `tractor_beam_pal.bin` into an emulator such as Stella. To run on real hardware, burn the 4K `.bin` file onto an EEPROM.

The level editor outputs code to paste into `game.asm`. It is meant to run in Unix/Linux shells, and probably won't work on Windows. To run, use `run.sh`, or run using Python: `python main.py`. It's written to run on Python 2.7.


