{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.CreateSecret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new secret. A secret in Secrets Manager consists of both the
-- protected secret data and the important information needed to manage the
-- secret.
--
-- Secrets Manager stores the encrypted secret data in one of a collection
-- of \"versions\" associated with the secret. Each version contains a copy
-- of the encrypted secret data. Each version is associated with one or
-- more \"staging labels\" that identify where the version is in the
-- rotation cycle. The @SecretVersionsToStages@ field of the secret
-- contains the mapping of staging labels to the active versions of the
-- secret. Versions without a staging label are considered deprecated and
-- not included in the list.
--
-- You provide the secret data to be encrypted by putting text in either
-- the @SecretString@ parameter or binary data in the @SecretBinary@
-- parameter, but not both. If you include @SecretString@ or @SecretBinary@
-- then Secrets Manager also creates an initial secret version and
-- automatically attaches the staging label @AWSCURRENT@ to the new
-- version.
--
-- -   If you call an operation to encrypt or decrypt the @SecretString@ or
--     @SecretBinary@ for a secret in the same account as the calling user
--     and that secret doesn\'t specify a Amazon Web Services KMS
--     encryption key, Secrets Manager uses the account\'s default Amazon
--     Web Services managed customer master key (CMK) with the alias
--     @aws\/secretsmanager@. If this key doesn\'t already exist in your
--     account then Secrets Manager creates it for you automatically. All
--     users and roles in the same Amazon Web Services account
--     automatically have access to use the default CMK. Note that if an
--     Secrets Manager API call results in Amazon Web Services creating the
--     account\'s Amazon Web Services-managed CMK, it can result in a
--     one-time significant delay in returning the result.
--
-- -   If the secret resides in a different Amazon Web Services account
--     from the credentials calling an API that requires encryption or
--     decryption of the secret value then you must create and use a custom
--     Amazon Web Services KMS CMK because you can\'t access the default
--     CMK for the account using credentials from a different Amazon Web
--     Services account. Store the ARN of the CMK in the secret when you
--     create the secret or when you update it by including it in the
--     @KMSKeyId@. If you call an API that must encrypt or decrypt
--     @SecretString@ or @SecretBinary@ using credentials from a different
--     account then the Amazon Web Services KMS key policy must grant
--     cross-account access to that other account\'s user or role for both
--     the kms:GenerateDataKey and kms:Decrypt operations.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:CreateSecret
--
-- -   kms:GenerateDataKey - needed only if you use a customer-managed
--     Amazon Web Services KMS key to encrypt the secret. You do not need
--     this permission to use the account default Amazon Web Services
--     managed CMK for Secrets Manager.
--
-- -   kms:Decrypt - needed only if you use a customer-managed Amazon Web
--     Services KMS key to encrypt the secret. You do not need this
--     permission to use the account default Amazon Web Services managed
--     CMK for Secrets Manager.
--
-- -   secretsmanager:TagResource - needed only if you include the @Tags@
--     parameter.
--
-- __Related operations__
--
-- -   To delete a secret, use DeleteSecret.
--
-- -   To modify an existing secret, use UpdateSecret.
--
-- -   To create a new version of a secret, use PutSecretValue.
--
-- -   To retrieve the encrypted secure string and secure binary values,
--     use GetSecretValue.
--
-- -   To retrieve all other details for a secret, use DescribeSecret. This
--     does not include the encrypted secure string and secure binary
--     values.
--
-- -   To retrieve the list of secret versions associated with the current
--     secret, use DescribeSecret and examine the @SecretVersionsToStages@
--     response value.
module Network.AWS.SecretsManager.CreateSecret
  ( -- * Creating a Request
    CreateSecret (..),
    newCreateSecret,

    -- * Request Lenses
    createSecret_addReplicaRegions,
    createSecret_secretBinary,
    createSecret_kmsKeyId,
    createSecret_forceOverwriteReplicaSecret,
    createSecret_secretString,
    createSecret_clientRequestToken,
    createSecret_description,
    createSecret_tags,
    createSecret_name,

    -- * Destructuring the Response
    CreateSecretResponse (..),
    newCreateSecretResponse,

    -- * Response Lenses
    createSecretResponse_versionId,
    createSecretResponse_arn,
    createSecretResponse_name,
    createSecretResponse_replicationStatus,
    createSecretResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newCreateSecret' smart constructor.
data CreateSecret = CreateSecret'
  { -- | (Optional) Add a list of regions to replicate secrets. Secrets Manager
    -- replicates the KMSKeyID objects to the list of regions specified in the
    -- parameter.
    addReplicaRegions :: Prelude.Maybe (Prelude.NonEmpty ReplicaRegionType),
    -- | (Optional) Specifies binary data that you want to encrypt and store in
    -- the new version of the secret. To use this parameter in the command-line
    -- tools, we recommend that you store your binary data in a file and then
    -- use the appropriate technique for your tool to pass the contents of the
    -- file as a parameter.
    --
    -- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
    -- They cannot both be empty.
    --
    -- This parameter is not available using the Secrets Manager console. It
    -- can be accessed only by using the Amazon Web Services CLI or one of the
    -- Amazon Web Services SDKs.
    secretBinary :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | (Optional) Specifies the ARN, Key ID, or alias of the Amazon Web
    -- Services KMS customer master key (CMK) to be used to encrypt the
    -- @SecretString@ or @SecretBinary@ values in the versions stored in this
    -- secret.
    --
    -- You can specify any of the supported ways to identify a Amazon Web
    -- Services KMS key ID. If you need to reference a CMK in a different
    -- account, you can use only the key ARN or the alias ARN.
    --
    -- If you don\'t specify this value, then Secrets Manager defaults to using
    -- the Amazon Web Services account\'s default CMK (the one named
    -- @aws\/secretsmanager@). If a Amazon Web Services KMS CMK with that name
    -- doesn\'t yet exist, then Secrets Manager creates it for you
    -- automatically the first time it needs to encrypt a version\'s
    -- @SecretString@ or @SecretBinary@ fields.
    --
    -- You can use the account default CMK to encrypt and decrypt only if you
    -- call this operation using credentials from the same account that owns
    -- the secret. If the secret resides in a different account, then you must
    -- create a custom CMK and specify the ARN in this field.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) If set, the replication overwrites a secret with the same
    -- name in the destination region.
    forceOverwriteReplicaSecret :: Prelude.Maybe Prelude.Bool,
    -- | (Optional) Specifies text data that you want to encrypt and store in
    -- this new version of the secret.
    --
    -- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
    -- They cannot both be empty.
    --
    -- If you create a secret by using the Secrets Manager console then Secrets
    -- Manager puts the protected secret text in only the @SecretString@
    -- parameter. The Secrets Manager console stores the information as a JSON
    -- structure of key\/value pairs that the Lambda rotation function knows
    -- how to parse.
    --
    -- For storing multiple values, we recommend that you use a JSON text
    -- string argument and specify key\/value pairs. For more information, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
    -- in the Amazon Web Services CLI User Guide.
    secretString :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | (Optional) If you include @SecretString@ or @SecretBinary@, then an
    -- initial version is created as part of the secret, and this parameter
    -- specifies a unique identifier for the new version.
    --
    -- If you use the Amazon Web Services CLI or one of the Amazon Web Services
    -- SDK to call this operation, then you can leave this parameter empty. The
    -- CLI or SDK generates a random UUID for you and includes it as the value
    -- for this parameter in the request. If you don\'t use the SDK and instead
    -- generate a raw HTTP request to the Secrets Manager service endpoint,
    -- then you must generate a @ClientRequestToken@ yourself for the new
    -- version and include the value in the request.
    --
    -- This value helps ensure idempotency. Secrets Manager uses this value to
    -- prevent the accidental creation of duplicate versions if there are
    -- failures and retries during a rotation. We recommend that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value to ensure uniqueness of your versions within the specified secret.
    --
    -- -   If the @ClientRequestToken@ value isn\'t already associated with a
    --     version of the secret then a new version of the secret is created.
    --
    -- -   If a version with this value already exists and the version
    --     @SecretString@ and @SecretBinary@ values are the same as those in
    --     the request, then the request is ignored.
    --
    -- -   If a version with this value already exists and that version\'s
    --     @SecretString@ and @SecretBinary@ values are different from those in
    --     the request, then the request fails because you cannot modify an
    --     existing version. Instead, use PutSecretValue to create a new
    --     version.
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies a user-provided description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies a list of user-defined tags that are attached to
    -- the secret. Each tag is a \"Key\" and \"Value\" pair of strings. This
    -- operation only appends tags to the existing list of tags. To remove
    -- tags, you must use UntagResource.
    --
    -- -   Secrets Manager tag key names are case sensitive. A tag with the key
    --     \"ABC\" is a different tag from one with key \"abc\".
    --
    -- -   If you check tags in IAM policy @Condition@ elements as part of your
    --     security strategy, then adding or removing a tag can change
    --     permissions. If the successful completion of this operation would
    --     result in you losing your permissions for this secret, then this
    --     operation is blocked and returns an @Access Denied@ error.
    --
    -- This parameter requires a JSON text string argument. For information on
    -- how to format a JSON parameter for the various command line tool
    -- environments, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
    -- in the /CLI User Guide/. For example:
    --
    -- @[{\"Key\":\"CostCenter\",\"Value\":\"12345\"},{\"Key\":\"environment\",\"Value\":\"production\"}]@
    --
    -- If your command-line tool or SDK requires quotation marks around the
    -- parameter, you should use single quotes to avoid confusion with the
    -- double quotes required in the JSON text.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per secret—50
    --
    -- -   Maximum key length—127 Unicode characters in UTF-8
    --
    -- -   Maximum value length—255 Unicode characters in UTF-8
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Do not use the @aws:@ prefix in your tag names or values because
    --     Amazon Web Services reserves it for Amazon Web Services use. You
    --     can\'t edit or delete tag names or values with this prefix. Tags
    --     with this prefix do not count against your tags per secret limit.
    --
    -- -   If you use your tagging schema across multiple services and
    --     resources, remember other services might have restrictions on
    --     allowed characters. Generally allowed characters: letters, spaces,
    --     and numbers representable in UTF-8, plus the following special
    --     characters: + - = . _ : \/ \@.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the friendly name of the new secret.
    --
    -- The secret name must be ASCII letters, digits, or the following
    -- characters : \/_+=.\@-
    --
    -- Do not end your secret name with a hyphen followed by six characters. If
    -- you do so, you risk confusion and unexpected results when searching for
    -- a secret by partial ARN. Secrets Manager automatically adds a hyphen and
    -- six random characters at the end of the ARN.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addReplicaRegions', 'createSecret_addReplicaRegions' - (Optional) Add a list of regions to replicate secrets. Secrets Manager
-- replicates the KMSKeyID objects to the list of regions specified in the
-- parameter.
--
-- 'secretBinary', 'createSecret_secretBinary' - (Optional) Specifies binary data that you want to encrypt and store in
-- the new version of the secret. To use this parameter in the command-line
-- tools, we recommend that you store your binary data in a file and then
-- use the appropriate technique for your tool to pass the contents of the
-- file as a parameter.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
-- They cannot both be empty.
--
-- This parameter is not available using the Secrets Manager console. It
-- can be accessed only by using the Amazon Web Services CLI or one of the
-- Amazon Web Services SDKs.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'kmsKeyId', 'createSecret_kmsKeyId' - (Optional) Specifies the ARN, Key ID, or alias of the Amazon Web
-- Services KMS customer master key (CMK) to be used to encrypt the
-- @SecretString@ or @SecretBinary@ values in the versions stored in this
-- secret.
--
-- You can specify any of the supported ways to identify a Amazon Web
-- Services KMS key ID. If you need to reference a CMK in a different
-- account, you can use only the key ARN or the alias ARN.
--
-- If you don\'t specify this value, then Secrets Manager defaults to using
-- the Amazon Web Services account\'s default CMK (the one named
-- @aws\/secretsmanager@). If a Amazon Web Services KMS CMK with that name
-- doesn\'t yet exist, then Secrets Manager creates it for you
-- automatically the first time it needs to encrypt a version\'s
-- @SecretString@ or @SecretBinary@ fields.
--
-- You can use the account default CMK to encrypt and decrypt only if you
-- call this operation using credentials from the same account that owns
-- the secret. If the secret resides in a different account, then you must
-- create a custom CMK and specify the ARN in this field.
--
-- 'forceOverwriteReplicaSecret', 'createSecret_forceOverwriteReplicaSecret' - (Optional) If set, the replication overwrites a secret with the same
-- name in the destination region.
--
-- 'secretString', 'createSecret_secretString' - (Optional) Specifies text data that you want to encrypt and store in
-- this new version of the secret.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
-- They cannot both be empty.
--
-- If you create a secret by using the Secrets Manager console then Secrets
-- Manager puts the protected secret text in only the @SecretString@
-- parameter. The Secrets Manager console stores the information as a JSON
-- structure of key\/value pairs that the Lambda rotation function knows
-- how to parse.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
--
-- 'clientRequestToken', 'createSecret_clientRequestToken' - (Optional) If you include @SecretString@ or @SecretBinary@, then an
-- initial version is created as part of the secret, and this parameter
-- specifies a unique identifier for the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes it as the value
-- for this parameter in the request. If you don\'t use the SDK and instead
-- generate a raw HTTP request to the Secrets Manager service endpoint,
-- then you must generate a @ClientRequestToken@ yourself for the new
-- version and include the value in the request.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to
-- prevent the accidental creation of duplicate versions if there are
-- failures and retries during a rotation. We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness of your versions within the specified secret.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and the version
--     @SecretString@ and @SecretBinary@ values are the same as those in
--     the request, then the request is ignored.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ and @SecretBinary@ values are different from those in
--     the request, then the request fails because you cannot modify an
--     existing version. Instead, use PutSecretValue to create a new
--     version.
--
-- This value becomes the @VersionId@ of the new version.
--
-- 'description', 'createSecret_description' - (Optional) Specifies a user-provided description of the secret.
--
-- 'tags', 'createSecret_tags' - (Optional) Specifies a list of user-defined tags that are attached to
-- the secret. Each tag is a \"Key\" and \"Value\" pair of strings. This
-- operation only appends tags to the existing list of tags. To remove
-- tags, you must use UntagResource.
--
-- -   Secrets Manager tag key names are case sensitive. A tag with the key
--     \"ABC\" is a different tag from one with key \"abc\".
--
-- -   If you check tags in IAM policy @Condition@ elements as part of your
--     security strategy, then adding or removing a tag can change
--     permissions. If the successful completion of this operation would
--     result in you losing your permissions for this secret, then this
--     operation is blocked and returns an @Access Denied@ error.
--
-- This parameter requires a JSON text string argument. For information on
-- how to format a JSON parameter for the various command line tool
-- environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /CLI User Guide/. For example:
--
-- @[{\"Key\":\"CostCenter\",\"Value\":\"12345\"},{\"Key\":\"environment\",\"Value\":\"production\"}]@
--
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per secret—50
--
-- -   Maximum key length—127 Unicode characters in UTF-8
--
-- -   Maximum value length—255 Unicode characters in UTF-8
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use the @aws:@ prefix in your tag names or values because
--     Amazon Web Services reserves it for Amazon Web Services use. You
--     can\'t edit or delete tag names or values with this prefix. Tags
--     with this prefix do not count against your tags per secret limit.
--
-- -   If you use your tagging schema across multiple services and
--     resources, remember other services might have restrictions on
--     allowed characters. Generally allowed characters: letters, spaces,
--     and numbers representable in UTF-8, plus the following special
--     characters: + - = . _ : \/ \@.
--
-- 'name', 'createSecret_name' - Specifies the friendly name of the new secret.
--
-- The secret name must be ASCII letters, digits, or the following
-- characters : \/_+=.\@-
--
-- Do not end your secret name with a hyphen followed by six characters. If
-- you do so, you risk confusion and unexpected results when searching for
-- a secret by partial ARN. Secrets Manager automatically adds a hyphen and
-- six random characters at the end of the ARN.
newCreateSecret ::
  -- | 'name'
  Prelude.Text ->
  CreateSecret
newCreateSecret pName_ =
  CreateSecret'
    { addReplicaRegions = Prelude.Nothing,
      secretBinary = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      forceOverwriteReplicaSecret = Prelude.Nothing,
      secretString = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | (Optional) Add a list of regions to replicate secrets. Secrets Manager
-- replicates the KMSKeyID objects to the list of regions specified in the
-- parameter.
createSecret_addReplicaRegions :: Lens.Lens' CreateSecret (Prelude.Maybe (Prelude.NonEmpty ReplicaRegionType))
createSecret_addReplicaRegions = Lens.lens (\CreateSecret' {addReplicaRegions} -> addReplicaRegions) (\s@CreateSecret' {} a -> s {addReplicaRegions = a} :: CreateSecret) Prelude.. Lens.mapping Lens.coerced

-- | (Optional) Specifies binary data that you want to encrypt and store in
-- the new version of the secret. To use this parameter in the command-line
-- tools, we recommend that you store your binary data in a file and then
-- use the appropriate technique for your tool to pass the contents of the
-- file as a parameter.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
-- They cannot both be empty.
--
-- This parameter is not available using the Secrets Manager console. It
-- can be accessed only by using the Amazon Web Services CLI or one of the
-- Amazon Web Services SDKs.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createSecret_secretBinary :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.ByteString)
createSecret_secretBinary = Lens.lens (\CreateSecret' {secretBinary} -> secretBinary) (\s@CreateSecret' {} a -> s {secretBinary = a} :: CreateSecret) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | (Optional) Specifies the ARN, Key ID, or alias of the Amazon Web
-- Services KMS customer master key (CMK) to be used to encrypt the
-- @SecretString@ or @SecretBinary@ values in the versions stored in this
-- secret.
--
-- You can specify any of the supported ways to identify a Amazon Web
-- Services KMS key ID. If you need to reference a CMK in a different
-- account, you can use only the key ARN or the alias ARN.
--
-- If you don\'t specify this value, then Secrets Manager defaults to using
-- the Amazon Web Services account\'s default CMK (the one named
-- @aws\/secretsmanager@). If a Amazon Web Services KMS CMK with that name
-- doesn\'t yet exist, then Secrets Manager creates it for you
-- automatically the first time it needs to encrypt a version\'s
-- @SecretString@ or @SecretBinary@ fields.
--
-- You can use the account default CMK to encrypt and decrypt only if you
-- call this operation using credentials from the same account that owns
-- the secret. If the secret resides in a different account, then you must
-- create a custom CMK and specify the ARN in this field.
createSecret_kmsKeyId :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Text)
createSecret_kmsKeyId = Lens.lens (\CreateSecret' {kmsKeyId} -> kmsKeyId) (\s@CreateSecret' {} a -> s {kmsKeyId = a} :: CreateSecret)

-- | (Optional) If set, the replication overwrites a secret with the same
-- name in the destination region.
createSecret_forceOverwriteReplicaSecret :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Bool)
createSecret_forceOverwriteReplicaSecret = Lens.lens (\CreateSecret' {forceOverwriteReplicaSecret} -> forceOverwriteReplicaSecret) (\s@CreateSecret' {} a -> s {forceOverwriteReplicaSecret = a} :: CreateSecret)

-- | (Optional) Specifies text data that you want to encrypt and store in
-- this new version of the secret.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
-- They cannot both be empty.
--
-- If you create a secret by using the Secrets Manager console then Secrets
-- Manager puts the protected secret text in only the @SecretString@
-- parameter. The Secrets Manager console stores the information as a JSON
-- structure of key\/value pairs that the Lambda rotation function knows
-- how to parse.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
createSecret_secretString :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Text)
createSecret_secretString = Lens.lens (\CreateSecret' {secretString} -> secretString) (\s@CreateSecret' {} a -> s {secretString = a} :: CreateSecret) Prelude.. Lens.mapping Core._Sensitive

-- | (Optional) If you include @SecretString@ or @SecretBinary@, then an
-- initial version is created as part of the secret, and this parameter
-- specifies a unique identifier for the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes it as the value
-- for this parameter in the request. If you don\'t use the SDK and instead
-- generate a raw HTTP request to the Secrets Manager service endpoint,
-- then you must generate a @ClientRequestToken@ yourself for the new
-- version and include the value in the request.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to
-- prevent the accidental creation of duplicate versions if there are
-- failures and retries during a rotation. We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness of your versions within the specified secret.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and the version
--     @SecretString@ and @SecretBinary@ values are the same as those in
--     the request, then the request is ignored.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ and @SecretBinary@ values are different from those in
--     the request, then the request fails because you cannot modify an
--     existing version. Instead, use PutSecretValue to create a new
--     version.
--
-- This value becomes the @VersionId@ of the new version.
createSecret_clientRequestToken :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Text)
createSecret_clientRequestToken = Lens.lens (\CreateSecret' {clientRequestToken} -> clientRequestToken) (\s@CreateSecret' {} a -> s {clientRequestToken = a} :: CreateSecret)

-- | (Optional) Specifies a user-provided description of the secret.
createSecret_description :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Text)
createSecret_description = Lens.lens (\CreateSecret' {description} -> description) (\s@CreateSecret' {} a -> s {description = a} :: CreateSecret)

-- | (Optional) Specifies a list of user-defined tags that are attached to
-- the secret. Each tag is a \"Key\" and \"Value\" pair of strings. This
-- operation only appends tags to the existing list of tags. To remove
-- tags, you must use UntagResource.
--
-- -   Secrets Manager tag key names are case sensitive. A tag with the key
--     \"ABC\" is a different tag from one with key \"abc\".
--
-- -   If you check tags in IAM policy @Condition@ elements as part of your
--     security strategy, then adding or removing a tag can change
--     permissions. If the successful completion of this operation would
--     result in you losing your permissions for this secret, then this
--     operation is blocked and returns an @Access Denied@ error.
--
-- This parameter requires a JSON text string argument. For information on
-- how to format a JSON parameter for the various command line tool
-- environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /CLI User Guide/. For example:
--
-- @[{\"Key\":\"CostCenter\",\"Value\":\"12345\"},{\"Key\":\"environment\",\"Value\":\"production\"}]@
--
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per secret—50
--
-- -   Maximum key length—127 Unicode characters in UTF-8
--
-- -   Maximum value length—255 Unicode characters in UTF-8
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use the @aws:@ prefix in your tag names or values because
--     Amazon Web Services reserves it for Amazon Web Services use. You
--     can\'t edit or delete tag names or values with this prefix. Tags
--     with this prefix do not count against your tags per secret limit.
--
-- -   If you use your tagging schema across multiple services and
--     resources, remember other services might have restrictions on
--     allowed characters. Generally allowed characters: letters, spaces,
--     and numbers representable in UTF-8, plus the following special
--     characters: + - = . _ : \/ \@.
createSecret_tags :: Lens.Lens' CreateSecret (Prelude.Maybe [Tag])
createSecret_tags = Lens.lens (\CreateSecret' {tags} -> tags) (\s@CreateSecret' {} a -> s {tags = a} :: CreateSecret) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the friendly name of the new secret.
--
-- The secret name must be ASCII letters, digits, or the following
-- characters : \/_+=.\@-
--
-- Do not end your secret name with a hyphen followed by six characters. If
-- you do so, you risk confusion and unexpected results when searching for
-- a secret by partial ARN. Secrets Manager automatically adds a hyphen and
-- six random characters at the end of the ARN.
createSecret_name :: Lens.Lens' CreateSecret Prelude.Text
createSecret_name = Lens.lens (\CreateSecret' {name} -> name) (\s@CreateSecret' {} a -> s {name = a} :: CreateSecret)

instance Core.AWSRequest CreateSecret where
  type AWSResponse CreateSecret = CreateSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecretResponse'
            Prelude.<$> (x Core..?> "VersionId")
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> ( x Core..?> "ReplicationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSecret

instance Prelude.NFData CreateSecret

instance Core.ToHeaders CreateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.CreateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSecret where
  toJSON CreateSecret' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AddReplicaRegions" Core..=)
              Prelude.<$> addReplicaRegions,
            ("SecretBinary" Core..=) Prelude.<$> secretBinary,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("ForceOverwriteReplicaSecret" Core..=)
              Prelude.<$> forceOverwriteReplicaSecret,
            ("SecretString" Core..=) Prelude.<$> secretString,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateSecret where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecretResponse' smart constructor.
data CreateSecretResponse = CreateSecretResponse'
  { -- | The unique identifier associated with the version of the secret you just
    -- created.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the secret that you just created.
    --
    -- Secrets Manager automatically adds several random characters to the name
    -- at the end of the ARN when you initially create a secret. This affects
    -- only the ARN and not the actual friendly name. This ensures that if you
    -- create a new secret with the same name as an old secret that you
    -- previously deleted, then users with access to the old secret /don\'t/
    -- automatically get access to the new secret because the ARNs are
    -- different.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret that you just created.
    name :: Prelude.Maybe Prelude.Text,
    -- | Describes a list of replication status objects as @InProgress@, @Failed@
    -- or @InSync@.
    replicationStatus :: Prelude.Maybe [ReplicationStatusType],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'createSecretResponse_versionId' - The unique identifier associated with the version of the secret you just
-- created.
--
-- 'arn', 'createSecretResponse_arn' - The Amazon Resource Name (ARN) of the secret that you just created.
--
-- Secrets Manager automatically adds several random characters to the name
-- at the end of the ARN when you initially create a secret. This affects
-- only the ARN and not the actual friendly name. This ensures that if you
-- create a new secret with the same name as an old secret that you
-- previously deleted, then users with access to the old secret /don\'t/
-- automatically get access to the new secret because the ARNs are
-- different.
--
-- 'name', 'createSecretResponse_name' - The friendly name of the secret that you just created.
--
-- 'replicationStatus', 'createSecretResponse_replicationStatus' - Describes a list of replication status objects as @InProgress@, @Failed@
-- or @InSync@.
--
-- 'httpStatus', 'createSecretResponse_httpStatus' - The response's http status code.
newCreateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSecretResponse
newCreateSecretResponse pHttpStatus_ =
  CreateSecretResponse'
    { versionId = Prelude.Nothing,
      arn = Prelude.Nothing,
      name = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier associated with the version of the secret you just
-- created.
createSecretResponse_versionId :: Lens.Lens' CreateSecretResponse (Prelude.Maybe Prelude.Text)
createSecretResponse_versionId = Lens.lens (\CreateSecretResponse' {versionId} -> versionId) (\s@CreateSecretResponse' {} a -> s {versionId = a} :: CreateSecretResponse)

-- | The Amazon Resource Name (ARN) of the secret that you just created.
--
-- Secrets Manager automatically adds several random characters to the name
-- at the end of the ARN when you initially create a secret. This affects
-- only the ARN and not the actual friendly name. This ensures that if you
-- create a new secret with the same name as an old secret that you
-- previously deleted, then users with access to the old secret /don\'t/
-- automatically get access to the new secret because the ARNs are
-- different.
createSecretResponse_arn :: Lens.Lens' CreateSecretResponse (Prelude.Maybe Prelude.Text)
createSecretResponse_arn = Lens.lens (\CreateSecretResponse' {arn} -> arn) (\s@CreateSecretResponse' {} a -> s {arn = a} :: CreateSecretResponse)

-- | The friendly name of the secret that you just created.
createSecretResponse_name :: Lens.Lens' CreateSecretResponse (Prelude.Maybe Prelude.Text)
createSecretResponse_name = Lens.lens (\CreateSecretResponse' {name} -> name) (\s@CreateSecretResponse' {} a -> s {name = a} :: CreateSecretResponse)

-- | Describes a list of replication status objects as @InProgress@, @Failed@
-- or @InSync@.
createSecretResponse_replicationStatus :: Lens.Lens' CreateSecretResponse (Prelude.Maybe [ReplicationStatusType])
createSecretResponse_replicationStatus = Lens.lens (\CreateSecretResponse' {replicationStatus} -> replicationStatus) (\s@CreateSecretResponse' {} a -> s {replicationStatus = a} :: CreateSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createSecretResponse_httpStatus :: Lens.Lens' CreateSecretResponse Prelude.Int
createSecretResponse_httpStatus = Lens.lens (\CreateSecretResponse' {httpStatus} -> httpStatus) (\s@CreateSecretResponse' {} a -> s {httpStatus = a} :: CreateSecretResponse)

instance Prelude.NFData CreateSecretResponse
