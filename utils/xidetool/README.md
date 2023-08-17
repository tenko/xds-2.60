# xidetool XDS Oberon-2/Modula-2 IDE integration

This code is from the excelent Obide (Oberon IDE) plugin for Notepad++
developed by Alexander Iljin. 

Information on the plugin found (Russian): [oberoncore.ru](https://forum.oberoncore.ru/viewtopic.php?f=30&t=2027)


## Build instructions

In the utils/xidetool directory:

```bash
xc =p src/xidetool
```

## Test

In the utils/xidetool directory:

```bash
./xidetool.exe src/Project.ob2
```

Should output valid JSON data for VSCode plugin consumption.

## TODO

 * Support reading data from imported modules
 * Support item information from record member and local items in procedures.
