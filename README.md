# Da Huuuuuudge - a Dactyl ManuForm Keyboard fork with a nasty blob thing for a thumb cluster

This is a fork of [Carbonfet's Dactyl Manuform](http://github.com/carbonfet/dactyl-manuform), which is a fork of the [Dactyl ManuForm Mini](https://github.com/l4u/dactyl-manuform-mini-keyboard), which is a fork of the [Dactyl-ManuForm](https://github.com/tshort/dactyl-keyboard). The Dactyl-Manuform is a fork of the [Dactyl](https://github.com/adereth/dactyl-keyboard) with the thumb cluster from [ManuForm](https://github.com/jeffgran/ManuForm). 

## Features
- 7 thumb keys 
- IMHO the thumb keys are more comfortable and reachable than any fork I know of. 4 are in an arc similar to Keyboardio Model 01, and the rest are strategically-placed to be easily-reachable from the home position.
- Removed the two-key row at the bottom to allow pulling thumbs inward.
- Claws
- Horns
- Tusks
- Tentacles
- Proboscis

![photo of keyboard](./hudge1.PNG)
![photo of keyboard](./hudge2.PNG)
![photo of keyboard](./hudge3.PNG)
![photo of keyboard](./hudge4.PNG)
![photo of keyboard](./hudge5.PNG)

prototypes and research of existing forks
![photo of keyboard](./prototypes.PNG)

Additional research:

https://www.youtube.com/watch?v=dmzk5y_mrlg

## Generate OpenSCAD and STL models

* Run `lein generate` or `lein auto generate`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to export STL files



## License

Copyright © 2015-2018 Matthew Adereth, Tom Short and Leo Lou

The source code for generating the models is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE).

The generated models are distributed under the [Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)](LICENSE-models).
