# Chip-8
Chip-8 and Superchip-8 interpreter for Atari 8-bit

Progammed in 2005-2006, compiled with omc65.
Updated 2024 - rewritten for mads and SpartaDosX / BW-DOS.


##2024-02-xx: v2.3
* program does not kill resident SDX utils like TD (thanks @Drac030)

##2024-02-08: v2.2
* much faster config reading thanks to @Drac030
* small bugfixes, incl. no dangling ESC after quitting to DOS
* new SuperChip8 games: ALIENHUN(t), BINDING( of Cosmac), B(lack)RAINBOW CHIPCROS(s), CODEGRID, DVN8, FIELD

##2024-02-01: v2.1
* much better error messages
* does not hang when CHIP8.CFG not found
* incorrect file error handling fixed
* 4 new SuperChip8 games with config - ROCKTO, TURNOVER, SOKOBAN, CTETRIS

##2023-01-27: v2.0
Command line interface, first attempt.


Would be nice to add new games and joystick driven game selection.

My most important contribution to Chip-8 scene is the configuration file CHIP8.CFG - it contains mapping of Chip-8 keys to a joystick.
