# Amazonka

* [Description](#description)
* [Contribute](#contribute)
* [Licence](#licence)

## Description

Client library containing request and response logic to communicate
with Amazon Web Service compatible APIs. Intended to be used alongside
the types supplied by the various `amazonka-*` service libraries.


## Migrating from 1.6.1 to 2.0

`CHANGELOG.md` is extremely thorough, but these notes should get you started:

* Modules have been moved from `Network.AWS.*` to `Amazonka.*`. Perform a find/replace on your import statements, e.g.:

  ```sh
  perl -pi -e 's/Network\.AWS/Amazonka/g' `find . -type f -name '*.hs'`
  ```

* The `AWST` transformer from `Control.Monad.Trans.AWS` has been removed. Functions such as `send` now take an `Env` as their first argument. You can provide an `Env` directly, or use whatever transformer or effect system you prefer.

* The `Credentials` data type no longer exists. Credential discovery methods are now represented as functions of type `EnvNoAuth -> m Env`, and common ones are exported from `Amazonka.Auth`. In most cases you can downcase the first character of a former `Credentials` constructor and it will do the right thing:

   ```haskell
   -- 1.6.1
   env <- newEnv Discover

   -- 2.0
   env <- newEnv discover
   ```

   A full list of new credential providers and their 1.6.1 equivalents, if any, are listed under the "Major Changes" heading of the 2.0 RC 2 section of `CHANGELOG.md`.

* On Windows, the {credential,config} files are read from `%USERPROFILE%\.aws\{credentials,config}` to [match the AWS SDK](https://docs.aws.amazon.com/sdk-for-net/v3/developer-guide/creds-file.html#creds-file-general-info).

* Many Amazonka functions now require `Typeable` instances on request/response types. If you write code which is polymorphic across requests and responses, you may need to add `Typeable a` and `Typeable (AWSResponse a)` constraints alongside each `AWSRequest a` constraint.

* Request/response data types have been simplified:

  - Data type smart constructors are prefixed with `new`. Example: `Network.AWS.S3.getObject` -> `Amazonka.S3.newGetObject`.
  - All generated types export their constructors, which are always the "primed" name of the base type. Example: `data GetObject = GetObject' { ... }`.
  - Records also export all their fields, which no longer have any prefix.
  - The recommended way to fill in additional record fields is to use a library such as [`generic-lens`](https://hackage.haskell.org/package/generic-lens) or [`optics`](https://hackage.haskell.org/package/optics), possibly with the `OverloadedLabels` extension:

    ```haskell
    -- 1.6.1
    import Network.AWS.S3
    let req = getObject "my-bucket" "/foo/bar.txt" & goVersionId ?~ "some-version"

    -- 2.0
    {-# LANGUAGE OverloadedLabels #-}
    import Amazonka.S3
    import Control.Lens ((?~))
    import Data.Generics.Labels ()
    let req = newGetObject "my-bucket" "/foo/bar.txt" & #versionId ?~ "some-version"
    ```
  - Generated lenses are still available, but no longer use heuristic abbreviations. Example: `Network.AWS.S3.goVersionId` is now `Amazonka.S3.Lens.getObject_versionId`
  - Enums (sum types) are now `newtype` wrappers over `Text`. "Constructors" for these enums are provided as ["bundled" pattern synonyms](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pattern_synonyms.html#import-and-export-of-pattern-synonyms), but other values are permitted. This is especially useful for new EC2 instance types or new AWS region names. As with generated lens names, the type name is baked into the pattern synonym. Example: `InstanceType_R4_8xlarge`.

* All hand-written records in `amazonka-core` and `amazonka` now follow the conventions set by generated records: no leading underscores and no inconsistent prefixing of fields. As part of this, some functions were renamed or removed:

  - `Amazonka.Env.configure` -> `Amazonka.Env.configureService` (and its re-export from `Amazonka`)
  - `Amazonka.Env.override` -> `Amazonka.Env.overrideService` (and its re-export from `Amazonka`)
  - `Amazonka.Env.timeout` -> `Amazonka.Env.globalTimeout` (and its re-export from `Amazonka`)
  - `Amazonka.Env.within`: removed; with `AWST` gone, it is just a record update

  The removal of `AWST` means that `Network.AWS.Env` functions which used to operate on an `Env` inside a `MonadReader` now operate on the `Env` directly.

* Serialisation classes like `ToText` and `ToByteString`, and their associated helper functions, are no longer directly exported from module `Amazonka`. If you need these, you may need to import `Amazonka.Data` directly.

* The interface to the EC2 Instance Metadata Service (IMDS) is no longer exported from the root `Amazonka` module. If you used this, you should should import `Amazonka.EC2.Metadata` directly.

  - The functions `Amazonka.dynamic`, `Amazonka.metadata` and `Amazonka.userdata` have been removed in favour of their equivalents in `Amazonka.EC2.Metadata` which only require a HTTP `Manager`, not an entire `Env`.
  - If you were using them, read the `manager :: Manager` field directly from your `Env`.


## Contribute

For any problems, comments, or feedback please create an issue [here on GitHub](https://github.com/brendanhay/amazonka/issues).


## Licence

`amazonka` is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/).
