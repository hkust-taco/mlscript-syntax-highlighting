import { MLscriptSymbolProvider } from "ported";
import { ExtensionContext, languages, window } from "vscode";

export const extensionLogOutput = window.createOutputChannel(
  "MLscript Extension",
  { log: true }
);
export const providerLogOutput = window.createOutputChannel(
  "MLscript Symbol Provider",
  { log: true }
);

export function activate(_context: ExtensionContext) {
  extensionLogOutput.info("Activating extension");
  languages.registerDocumentSymbolProvider(
    "mlscript",
    MLscriptSymbolProvider(extensionLogOutput, providerLogOutput)
  );
  extensionLogOutput.info("Registered document symbol provider");
}
