# gen + gen-configs

This Bazel package contains code generators for synthesising Haskell packages from the botocore service definitions.

For pull requests which affect generated output, please _do not include_ the actual regenerated service code, only commit the updates to the generator, config and annex specifications. This ensures the Continuous Integration process is the single source of truth for generated code changes, and keeps pull requests readable and focused on actual generator code/logic changes.

To run the generator on all services configured under `./config/services`:

```
> ./scripts/generate
```

Or, you can selectively run the generator on one or more services:

```
> ./scripts/generate ec2 s3 iam
```

To generate missing service configurations:

```
> ./scripts/generate-configs
```

Service configurations generated in this way are intended as examples only and the resulting `configs/services/<name>.json:libraryName` (Haskell package name) and `configs/annexes/<name>.json:serviceAbbreviation` (Haskell package namespace) should be manually verified and curated as necessary.