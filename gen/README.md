# amazonka-gen

This Bazel package contains the code generator tooling for synthesising Haskell packages from the botocore service definitions.

For pull requests which affect generated output, please _do not include_ the actual regenerated service code, only commit the updates to the generator, config and annex specifications.

This ensures the Continuous Integration process is the single source of truth for generated code changes, and keeps pull requests readable and focused on actual generator code/logic changes.

To run the generator on all services configured under `./config/services`:

```
> ./scripts/generate
```

Or, you can selectively run the generator on one or more services:

```
> ./scripts/generate ec2 s3 iam
```
