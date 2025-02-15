import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import fs from "node:fs";
import { defineConfig } from "vite";

const __dirname = dirname(fileURLToPath(import.meta.url));

const pkgJson: { dependencies?: Record<string, string> } = JSON.parse(
  fs.readFileSync(resolve(__dirname, "package.json"), "utf-8")
);
const vscodeDeps = Object.keys(pkgJson.dependencies ?? {})
  .filter((dep) => dep.includes("vscode"))
  .map((dep) => new RegExp(dep));

export default defineConfig({
  build: {
    target: "node16",
    lib: {
      entry: resolve(__dirname, "index.ts"),
      fileName: (format) => `extension.${format}.js`,
      formats: ["cjs"],
    },
    outDir: resolve(__dirname, "..", "..", "dist"),
    sourcemap: true,
    rollupOptions: {
      output: {
        entryFileNames: "extension.js",
        format: "cjs",
      },
      external: ["vscode", "path", "fs", ...vscodeDeps],
    },
  },
  plugins: [],
});
