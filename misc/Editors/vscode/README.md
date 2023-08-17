# Oberon-2 vscode extension

This is the vscode extension for the Oberon-2/Modula-2 language for the XDS dialect.

This extension rely on executing the `xidetool` in order to parse files
and read syntax information in the form of JSON back.

There are no pre-packaged versions of this extension, as changes to frequently.

## Build instructions

In the editors/vscode directory:

```bash
npm install
```

To create the package

```bash
npx vsce package
```

Then install vscode by switching to the Extensions section on the left, then the `...` at the top middle and choose "Install from VSIX..." and choose the package you just created.

In the settings for jakt under Preferences: Open Settings (UI) from the command palate (cmd/ctrl + shift + p), set the XIdeTool: Executable Path to your configured `/path/to/bin/xidetool`. This must be an absolute path.

Alternatively, modify the `.vscode/settings.json` file to have an entry similar to the following:

```
"oberon2.executablePath": "/path/to/bin/xidetool"
```
