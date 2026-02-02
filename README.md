# super‑tank

**super‑tank** is a top‑down shooter game written in x86 assembly.  It aims to recreate the classic “Tank” arcade game (also known as *Battle City* or *Tank 1990*).  The game runs in a DOS environment and uses bitmap images to draw the map, player and enemies.  It demonstrates low‑level graphics programming, collision detection and game logic using assembly language.

## Gameplay

* The player controls a tank that can move in four directions and shoot bullets.  Enemy tanks spawn at predefined positions and move around the map.
* The game draws the map and objects by reading `.bmp` files (e.g. `map1.bmp`, `tank.bmp`, `Etank.bmp`, `Lives.bmp`) into memory and copying the pixel data to video memory.
* A main game loop (`mainloop`) handles key presses, moves the player and enemy bullets, checks for collisions and updates the score.
* Variables are defined for bullet direction, collision flags, enemy positions and score counters.  The loop calls subroutines to move bullets, tanks and enemy bullets and checks for the exit key to end the game.
* When the player’s bullet hits an enemy tank, the enemy is destroyed and the player’s score increases.  If the player is hit, a “red mode” is activated and lives are reduced.  After clearing a level the map changes and new enemy tanks appear.

## Files

* **`game.asm`** – the assembly source code.  It includes macros to show/hide the mouse cursor, loads bitmaps, defines global variables and implements the main game loop.  The code uses BIOS interrupts and direct memory access to display graphics and handle input.  Comments and constants at the top of the file define the sizes and spawn points of tanks, bullet speed, score strings and file names.
* **Bitmap images** – a set of `.bmp` files used to draw the player tank, enemy tank, map, score and other UI elements.  These images must be present in the same directory when running the game.

## Building and Running

1. Assemble and link `game.asm` using an x86 assembler such as **TASM** or **MASM**:

   ```bash
   tasm game.asm
   tlink game.obj
   ```

2. Ensure that the `.bmp` files (e.g. `tank.bmp`, `map1.bmp`, `Etank.bmp`, `Lives.bmp`, `Start.bmp`, etc.) are in the same directory as the executable.

3. Run the game in a DOS environment or emulator (such as DOSBox):

   ```bash
   dosbox game.exe
   ```

   Use the arrow keys to move your tank and the spacebar to shoot.  Destroy enemy tanks and avoid being hit.

## Notes

* This project is written entirely in 16‑bit real‑mode assembly.  It requires a DOS environment to run.  The game was created as a learning exercise in assembly programming and graphics manipulation; it does not include modern conveniences like sound or configurable controls.
* The bitmaps provided are 16‑colour images encoded in a simple BMP format.  You can edit or replace them to change the game’s appearance.
