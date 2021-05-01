# amazonka-gen

For pull requests which affect generated output, please _do not include_ the
actual regenerated service code, only commit the updates to the generator,
config and annex specifications.

This ensures the Continuous Integration process is the single source of truth
for generated code changes, and keeps pull requests readable and focused on
actual generator code/logic changes.

Generate all models:

    $ nix-build generate.nix
