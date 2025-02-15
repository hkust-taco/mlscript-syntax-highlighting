import { DocumentSymbolProvider, LogOutputChannel } from "vscode";

declare function MLscriptSymbolProvider(
  extensionLogOutput: LogOutputChannel,
  providerLogOutput: LogOutputChannel,
): DocumentSymbolProvider;
