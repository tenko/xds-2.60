// Oberon-2/Modula-2 extension modified from the Jakt language server
import * as vscode from 'vscode';

import util = require("node:util");
import { TextEncoder } from "node:util";

// eslint-disable-next-line @typescript-eslint/no-var-requires
const {execSync} = require('child_process');

//const config = vscode.workspace.getConfiguration("", { languageId: "oberon2" });

// Settings
interface Settings {
	maxRunTime: number;
    executablePath: string;
}

const defaultSettings: Settings = {
    maxRunTime: 500,
    executablePath: "C:\\msys64\\home\\rute\\.local\\xds\\bin\\xidetool.exe"
};

//let globalSettings: Settings = defaultSettings;

// Cache the settings of all open documents
//const documentSettings: Map<string, Thenable<Settings>> = new Map();

// Symbol types
interface SymbolInterface {
    name: string;
    kind: "namespace" | "interface" | "constant" | "variable" | "function";
    start: number;
	end: number;
	children?: SymbolInterface[];
}

class SymbolClass {
	readonly kind: string;
	readonly start: vscode.Position;
	readonly end: vscode.Position;
	constructor(kind: string, start: vscode.Position, end: vscode.Position) {
		this.kind = kind;
		this.start = start;
		this.end = end;
	}
}

// Cache of parsed symbols
const Symbols = new Map<string, Map<string, SymbolClass>>();

vscode.workspace.onDidCloseTextDocument((document) => {
	Symbols.delete(document.uri.fsPath);
});

vscode.workspace.onDidSaveTextDocument((document) => {
	Symbols.delete(document.uri.fsPath);
});

function runTool(filename : string) : string {
	try {
		const maxRunTime = vscode.workspace.getConfiguration('oberon2').maxRunTime;
		const executablePath = vscode.workspace.getConfiguration('oberon2').executablePath;
		const output = execSync(
			executablePath + " " + filename,
			{
				timeout: Number(maxRunTime),
				encoding: 'utf8'
			}
		);
		//vscode.window.showInformationMessage(`Parsed ${output}`);
		return output;
	} catch (e) {
		console.error(e);
	}
	return "";
}

function getSymbolsForDocument(document: vscode.TextDocument): Map<string, SymbolClass> | undefined {
	const filename = document.uri.fsPath;
	let ret = Symbols.get(filename);
	if (!ret || ret.size == 0) {
		try {
			const text = document.getText();
			const lineBreaks = findLineBreaks(text ?? "");
			const stdout = runTool(filename);
			const toDefinition = (symbol: SymbolInterface): [string, SymbolClass] => {
				return [symbol.name,
						new SymbolClass(
							symbol.kind,
							convertSpan(symbol.start, lineBreaks),
							convertSpan(symbol.end, lineBreaks),
						)
				];
			};
			const result = (JSON.parse(stdout) as SymbolInterface[]).map(symbol => toDefinition(symbol));
			ret = new Map<string, SymbolClass>();
			for (let i = 0; i < result.length; i++) {
				ret.set(result[i][0], result[i][1]);
			}
			Symbols.set(filename, ret);
		} catch (e) {
			console.error(e);
		}
	}
	return ret;
}

export function activate(context: vscode.ExtensionContext) {
	const documentSymbolProvider = vscode.languages.registerDocumentSymbolProvider('oberon2', {
		provideDocumentSymbols( document: vscode.TextDocument, token: vscode.CancellationToken) {
			const ret: vscode.DocumentSymbol[] = [];
			const importSymbols: vscode.DocumentSymbol[] = [];
			const typeSymbols: vscode.DocumentSymbol[] = [];
			const constSymbols: vscode.DocumentSymbol[] = [];
			const varSymbols: vscode.DocumentSymbol[] = [];
			const procSymbols: vscode.DocumentSymbol[] = [];

			function createSymbol(name : string, kind : vscode.SymbolKind, symbol : SymbolClass) : vscode.DocumentSymbol {
				const range = new vscode.Range(symbol.start, symbol.end);
				const sym = new vscode.DocumentSymbol(name, "", kind, range, range);
				return sym;
			}

			const symbols = getSymbolsForDocument(document);
			if (symbols) {
				for (const item of symbols) {
					switch (item[1].kind) {
						case "namespace":
							importSymbols.push(createSymbol(item[0], vscode.SymbolKind.Module, item[1]));
							break;
						case "interface":
							typeSymbols.push(createSymbol(item[0], vscode.SymbolKind.TypeParameter, item[1]));
							break;
						case "constant":
							constSymbols.push(createSymbol(item[0], vscode.SymbolKind.Constant, item[1]));
							break;
						case "variable":
							varSymbols.push(createSymbol(item[0], vscode.SymbolKind.Variable, item[1]));
							break;
						case "function":
							procSymbols.push(createSymbol(item[0], vscode.SymbolKind.Function, item[1]));
							break;
					}
				}
			}
			const pos = new vscode.Position(0,0);
			// IMPORT
			const importSymbol = new SymbolClass("namespace", pos, pos);
			const importDocSymbol = createSymbol("IMPORT", vscode.SymbolKind.Namespace, importSymbol);
			importDocSymbol.children = importSymbols;
			ret.push(importDocSymbol);
			// TYPE
			const typeSymbol = new SymbolClass("namespace", pos, pos);
			const typeDocSymbol = createSymbol("TYPE", vscode.SymbolKind.Namespace, typeSymbol);
			typeDocSymbol.children = typeSymbols;
			ret.push(typeDocSymbol);
			// CONST
			const constSymbol = new SymbolClass("namespace", pos, pos);
			const constDocSymbol = createSymbol("CONST", vscode.SymbolKind.Namespace, constSymbol);
			constDocSymbol.children = constSymbols;
			ret.push(constDocSymbol);
			// VAR
			const varSymbol = new SymbolClass("namespace", pos, pos);
			const varDocSymbol = createSymbol("VAR", vscode.SymbolKind.Namespace, varSymbol);
			varDocSymbol.children = varSymbols;
			ret.push(varDocSymbol);
			// PROCEDURE
			const procSymbol = new SymbolClass("namespace", pos, pos);
			const procDocSymbol = createSymbol("PROCEDURE", vscode.SymbolKind.Namespace, procSymbol);
			procDocSymbol.children = procSymbols;
			ret.push(procDocSymbol);
			return ret;
		}
	});

	const completionItemProvider = vscode.languages.registerCompletionItemProvider('oberon2', {

		provideCompletionItems(document: vscode.TextDocument, position: vscode.Position,
                               token: vscode.CancellationToken, context: vscode.CompletionContext) {
			const filename = document.uri.fsPath;
			const str = "ABS|AND|ARRAY|ASH|ASSERT|BEGIN|BOOLEAN|BY|CAP|CARDINAL|CASE|CHAR|CHR|CONST|COPY|DEC|DEFINITION|DISPOSE|DIV|DO|ELSE|ELSIF|END|ENTIER|EXCL|EXIT|EXPORT|FALSE|FOR|HALT|IF|IMPLEMENTATION|IMPORT|IN|INC|INCL|INTEGER|IS|LEN|LENGTH|LONG|LONGCARD|LONGINT|LONGLONGINT|LONGREAL|LOOP|MAX|MIN|MOD|MODULE|NEW|NIL|NOT|ODD|OF|OR|ORD|POINTER|PROCEDURE|QUALIFIED|REAL|RECORD|REPEAT|RETURN|SET|SHORT|SHORTCARD|SHORTINT|SHORTREAL|SIZE|THEN|TO|TRUE|TYPE|UNTIL|VAR|WHILE|WITH";
			const ret: vscode.CompletionItem[] = [];
			const keywords = str.split('|');
			for (let i = 0; i < keywords.length; i++) {
				const completion = new vscode.CompletionItem(keywords[i]);
				completion.kind = vscode.CompletionItemKind.Keyword;
				ret.push(completion);
			}
			const procSnippet = new vscode.CompletionItem('PROCEDURETPL');
			procSnippet.kind = vscode.CompletionItemKind.Keyword;
			procSnippet.insertText = new vscode.SnippetString('PROCEDURE ${1:name}(${2})${3};\nBEGIN\n\t${0}\nEND ${1};');
			ret.push(procSnippet);

			const modSnippet = new vscode.CompletionItem('MODULETPL');
			modSnippet.kind = vscode.CompletionItemKind.Keyword;
			modSnippet.insertText = new vscode.SnippetString('MODULE ${1:name};\n\n${0}\n\nEND ${1}.');
			ret.push(modSnippet);

			const symbols = getSymbolsForDocument(document);
			if (symbols) {
				for (const item of symbols) {
					const completion = new vscode.CompletionItem(item[0]);
					switch (item[1].kind) {
						case "namespace":
							completion.kind = vscode.CompletionItemKind.Module;
							break;
						case "interface":
							completion.kind = vscode.CompletionItemKind.Interface;
							break;
						case "constant":
							completion.kind = vscode.CompletionItemKind.Constant;
							break;
						case "variable":
							completion.kind = vscode.CompletionItemKind.Variable;
							break;
						case "function":
							completion.kind = vscode.CompletionItemKind.Function;
							break;
					}
					ret.push(completion);
				}
			}
			//vscode.window.showInformationMessage(`OK ${filename}`);
			return ret;
		}
	});
	context.subscriptions.push(completionItemProvider);
	context.subscriptions.push(documentSymbolProvider);
}

function findLineBreaks(utf16_text: string): Array<number> {
    const utf8_text = new TextEncoder().encode(utf16_text);
    const lineBreaks: Array<number> = [];
    for (let i = 0; i < utf8_text.length; ++i) {
        if (utf8_text[i] == 0x0a) {
            lineBreaks.push(i);
        }
    }
    return lineBreaks;
}

function lowerBoundBinarySearch(arr: number[], num: number): number {
    let low = 0;
    let mid = 0;
    let high = arr.length - 1;
    if (num >= arr[high]) return high;
    while (low < high) {
        // Bitshift to avoid floating point division
        mid = (low + high) >> 1;
        if (arr[mid] < num) {
            low = mid + 1;
        } else {
            high = mid;
        }
    }
    return low - 1;
}

function convertSpan(utf8_offset: number, lineBreaks: Array<number>): vscode.Position {
    const lineBreakIndex = lowerBoundBinarySearch(lineBreaks, utf8_offset);
    const start_of_line_offset = lineBreakIndex == -1 ? 0 : lineBreaks[lineBreakIndex] + 1;
    const character = Math.max(0, utf8_offset - start_of_line_offset);
	return new vscode.Position(lineBreakIndex + 1, character);
}