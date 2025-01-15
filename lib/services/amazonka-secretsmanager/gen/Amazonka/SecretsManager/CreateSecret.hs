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
-- Module      : Amazonka.SecretsManager.CreateSecret
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new secret. A /secret/ can be a password, a set of credentials
-- such as a user name and password, an OAuth token, or other secret
-- information that you store in an encrypted form in Secrets Manager. The
-- secret also includes the connection information to access a database or
-- other service, which Secrets Manager doesn\'t encrypt. A secret in
-- Secrets Manager consists of both the protected secret data and the
-- important information needed to manage the secret.
--
-- For information about creating a secret in the console, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/manage_create-basic-secret.html Create a secret>.
--
-- To create a secret, you can provide the secret value to be encrypted in
-- either the @SecretString@ parameter or the @SecretBinary@ parameter, but
-- not both. If you include @SecretString@ or @SecretBinary@ then Secrets
-- Manager creates an initial secret version and automatically attaches the
-- staging label @AWSCURRENT@ to it.
--
-- For database credentials you want to rotate, for Secrets Manager to be
-- able to rotate the secret, you must make sure the JSON you store in the
-- @SecretString@ matches the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_secret_json_structure.html JSON structure of a database secret>.
--
-- If you don\'t specify an KMS encryption key, Secrets Manager uses the
-- Amazon Web Services managed key @aws\/secretsmanager@. If this key
-- doesn\'t already exist in your account, then Secrets Manager creates it
-- for you automatically. All users and roles in the Amazon Web Services
-- account automatically have access to use @aws\/secretsmanager@. Creating
-- @aws\/secretsmanager@ can result in a one-time significant delay in
-- returning the result.
--
-- If the secret is in a different Amazon Web Services account from the
-- credentials calling the API, then you can\'t use @aws\/secretsmanager@
-- to encrypt the secret, and you must create and use a customer managed
-- KMS key.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- except @SecretBinary@ or @SecretString@ because it might be logged. For
-- more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:CreateSecret@. If you include
-- tags in the secret, you also need @secretsmanager:TagResource@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
--
-- To encrypt the secret with a KMS key other than @aws\/secretsmanager@,
-- you need @kms:GenerateDataKey@ and @kms:Decrypt@ permission to the key.
module Amazonka.SecretsManager.CreateSecret
  ( -- * Creating a Request
    CreateSecret (..),
    newCreateSecret,

    -- * Request Lenses
    createSecret_addReplicaRegions,
    createSecret_clientRequestToken,
    createSecret_description,
    createSecret_forceOverwriteReplicaSecret,
    createSecret_kmsKeyId,
    createSecret_secretBinary,
    createSecret_secretString,
    createSecret_tags,
    createSecret_name,

    -- * Destructuring the Response
    CreateSecretResponse (..),
    newCreateSecretResponse,

    -- * Response Lenses
    createSecretResponse_arn,
    createSecretResponse_name,
    createSecretResponse_replicationStatus,
    createSecretResponse_versionId,
    createSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newCreateSecret' smart constructor.
data CreateSecret = CreateSecret'
  { -- | A list of Regions and KMS keys to replicate secrets.
    addReplicaRegions :: Prelude.Maybe (Prelude.NonEmpty ReplicaRegionType),
    -- | If you include @SecretString@ or @SecretBinary@, then Secrets Manager
    -- creates an initial version for the secret, and this parameter specifies
    -- the unique identifier for the new version.
    --
    -- If you use the Amazon Web Services CLI or one of the Amazon Web Services
    -- SDKs to call this operation, then you can leave this parameter empty.
    -- The CLI or SDK generates a random UUID for you and includes it as the
    -- value for this parameter in the request. If you don\'t use the SDK and
    -- instead generate a raw HTTP request to the Secrets Manager service
    -- endpoint, then you must generate a @ClientRequestToken@ yourself for the
    -- new version and include the value in the request.
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
    -- | The description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to overwrite a secret with the same name in the
    -- destination Region.
    forceOverwriteReplicaSecret :: Prelude.Maybe Prelude.Bool,
    -- | The ARN, key ID, or alias of the KMS key that Secrets Manager uses to
    -- encrypt the secret value in the secret. An alias is always prefixed by
    -- @alias\/@, for example @alias\/aws\/secretsmanager@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/alias-about.html About aliases>.
    --
    -- To use a KMS key in a different account, use the key ARN or the alias
    -- ARN.
    --
    -- If you don\'t specify this value, then Secrets Manager uses the key
    -- @aws\/secretsmanager@. If that key doesn\'t yet exist, then Secrets
    -- Manager creates it for you automatically the first time it encrypts the
    -- secret value.
    --
    -- If the secret is in a different Amazon Web Services account from the
    -- credentials calling the API, then you can\'t use @aws\/secretsmanager@
    -- to encrypt the secret, and you must create and use a customer managed
    -- KMS key.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The binary data to encrypt and store in the new version of the secret.
    -- We recommend that you store your binary data in a file and then pass the
    -- contents of the file as a parameter.
    --
    -- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
    --
    -- This parameter is not available in the Secrets Manager console.
    secretBinary :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | The text data to encrypt and store in this new version of the secret. We
    -- recommend you use a JSON structure of key\/value pairs for your secret
    -- value.
    --
    -- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
    --
    -- If you create a secret by using the Secrets Manager console then Secrets
    -- Manager puts the protected secret text in only the @SecretString@
    -- parameter. The Secrets Manager console stores the information as a JSON
    -- structure of key\/value pairs that a Lambda rotation function can parse.
    secretString :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A list of tags to attach to the secret. Each tag is a key and value pair
    -- of strings in a JSON text string, for example:
    --
    -- @[{\"Key\":\"CostCenter\",\"Value\":\"12345\"},{\"Key\":\"environment\",\"Value\":\"production\"}]@
    --
    -- Secrets Manager tag key names are case sensitive. A tag with the key
    -- \"ABC\" is a different tag from one with key \"abc\".
    --
    -- If you check tags in permissions policies as part of your security
    -- strategy, then adding or removing a tag can change permissions. If the
    -- completion of this operation would result in you losing your permissions
    -- for this secret, then Secrets Manager blocks the operation and returns
    -- an @Access Denied@ error. For more information, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html#tag-secrets-abac Control access to secrets using tags>
    -- and
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html#auth-and-access_tags2 Limit access to identities with tags that match secrets\' tags>.
    --
    -- For information about how to format a JSON parameter for the various
    -- command line tool environments, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>.
    -- If your command-line tool or SDK requires quotation marks around the
    -- parameter, you should use single quotes to avoid confusion with the
    -- double quotes required in the JSON text.
    --
    -- The following restrictions apply to tags:
    --
    -- -   Maximum number of tags per secret: 50
    --
    -- -   Maximum key length: 127 Unicode characters in UTF-8
    --
    -- -   Maximum value length: 255 Unicode characters in UTF-8
    --
    -- -   Tag keys and values are case sensitive.
    --
    -- -   Do not use the @aws:@ prefix in your tag names or values because
    --     Amazon Web Services reserves it for Amazon Web Services use. You
    --     can\'t edit or delete tag names or values with this prefix. Tags
    --     with this prefix do not count against your tags per secret limit.
    --
    -- -   If you use your tagging schema across multiple services and
    --     resources, other services might have restrictions on allowed
    --     characters. Generally allowed characters: letters, spaces, and
    --     numbers representable in UTF-8, plus the following special
    --     characters: + - = . _ : \/ \@.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the new secret.
    --
    -- The secret name can contain ASCII letters, numbers, and the following
    -- characters: \/_+=.\@-
    --
    -- Do not end your secret name with a hyphen followed by six characters. If
    -- you do so, you risk confusion and unexpected results when searching for
    -- a secret by partial ARN. Secrets Manager automatically adds a hyphen and
    -- six random characters after the secret name at the end of the ARN.
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
-- 'addReplicaRegions', 'createSecret_addReplicaRegions' - A list of Regions and KMS keys to replicate secrets.
--
-- 'clientRequestToken', 'createSecret_clientRequestToken' - If you include @SecretString@ or @SecretBinary@, then Secrets Manager
-- creates an initial version for the secret, and this parameter specifies
-- the unique identifier for the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDKs to call this operation, then you can leave this parameter empty.
-- The CLI or SDK generates a random UUID for you and includes it as the
-- value for this parameter in the request. If you don\'t use the SDK and
-- instead generate a raw HTTP request to the Secrets Manager service
-- endpoint, then you must generate a @ClientRequestToken@ yourself for the
-- new version and include the value in the request.
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
-- 'description', 'createSecret_description' - The description of the secret.
--
-- 'forceOverwriteReplicaSecret', 'createSecret_forceOverwriteReplicaSecret' - Specifies whether to overwrite a secret with the same name in the
-- destination Region.
--
-- 'kmsKeyId', 'createSecret_kmsKeyId' - The ARN, key ID, or alias of the KMS key that Secrets Manager uses to
-- encrypt the secret value in the secret. An alias is always prefixed by
-- @alias\/@, for example @alias\/aws\/secretsmanager@. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/alias-about.html About aliases>.
--
-- To use a KMS key in a different account, use the key ARN or the alias
-- ARN.
--
-- If you don\'t specify this value, then Secrets Manager uses the key
-- @aws\/secretsmanager@. If that key doesn\'t yet exist, then Secrets
-- Manager creates it for you automatically the first time it encrypts the
-- secret value.
--
-- If the secret is in a different Amazon Web Services account from the
-- credentials calling the API, then you can\'t use @aws\/secretsmanager@
-- to encrypt the secret, and you must create and use a customer managed
-- KMS key.
--
-- 'secretBinary', 'createSecret_secretBinary' - The binary data to encrypt and store in the new version of the secret.
-- We recommend that you store your binary data in a file and then pass the
-- contents of the file as a parameter.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
--
-- This parameter is not available in the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'secretString', 'createSecret_secretString' - The text data to encrypt and store in this new version of the secret. We
-- recommend you use a JSON structure of key\/value pairs for your secret
-- value.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
--
-- If you create a secret by using the Secrets Manager console then Secrets
-- Manager puts the protected secret text in only the @SecretString@
-- parameter. The Secrets Manager console stores the information as a JSON
-- structure of key\/value pairs that a Lambda rotation function can parse.
--
-- 'tags', 'createSecret_tags' - A list of tags to attach to the secret. Each tag is a key and value pair
-- of strings in a JSON text string, for example:
--
-- @[{\"Key\":\"CostCenter\",\"Value\":\"12345\"},{\"Key\":\"environment\",\"Value\":\"production\"}]@
--
-- Secrets Manager tag key names are case sensitive. A tag with the key
-- \"ABC\" is a different tag from one with key \"abc\".
--
-- If you check tags in permissions policies as part of your security
-- strategy, then adding or removing a tag can change permissions. If the
-- completion of this operation would result in you losing your permissions
-- for this secret, then Secrets Manager blocks the operation and returns
-- an @Access Denied@ error. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html#tag-secrets-abac Control access to secrets using tags>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html#auth-and-access_tags2 Limit access to identities with tags that match secrets\' tags>.
--
-- For information about how to format a JSON parameter for the various
-- command line tool environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>.
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of tags per secret: 50
--
-- -   Maximum key length: 127 Unicode characters in UTF-8
--
-- -   Maximum value length: 255 Unicode characters in UTF-8
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use the @aws:@ prefix in your tag names or values because
--     Amazon Web Services reserves it for Amazon Web Services use. You
--     can\'t edit or delete tag names or values with this prefix. Tags
--     with this prefix do not count against your tags per secret limit.
--
-- -   If you use your tagging schema across multiple services and
--     resources, other services might have restrictions on allowed
--     characters. Generally allowed characters: letters, spaces, and
--     numbers representable in UTF-8, plus the following special
--     characters: + - = . _ : \/ \@.
--
-- 'name', 'createSecret_name' - The name of the new secret.
--
-- The secret name can contain ASCII letters, numbers, and the following
-- characters: \/_+=.\@-
--
-- Do not end your secret name with a hyphen followed by six characters. If
-- you do so, you risk confusion and unexpected results when searching for
-- a secret by partial ARN. Secrets Manager automatically adds a hyphen and
-- six random characters after the secret name at the end of the ARN.
newCreateSecret ::
  -- | 'name'
  Prelude.Text ->
  CreateSecret
newCreateSecret pName_ =
  CreateSecret'
    { addReplicaRegions = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      description = Prelude.Nothing,
      forceOverwriteReplicaSecret = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      secretBinary = Prelude.Nothing,
      secretString = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A list of Regions and KMS keys to replicate secrets.
createSecret_addReplicaRegions :: Lens.Lens' CreateSecret (Prelude.Maybe (Prelude.NonEmpty ReplicaRegionType))
createSecret_addReplicaRegions = Lens.lens (\CreateSecret' {addReplicaRegions} -> addReplicaRegions) (\s@CreateSecret' {} a -> s {addReplicaRegions = a} :: CreateSecret) Prelude.. Lens.mapping Lens.coerced

-- | If you include @SecretString@ or @SecretBinary@, then Secrets Manager
-- creates an initial version for the secret, and this parameter specifies
-- the unique identifier for the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDKs to call this operation, then you can leave this parameter empty.
-- The CLI or SDK generates a random UUID for you and includes it as the
-- value for this parameter in the request. If you don\'t use the SDK and
-- instead generate a raw HTTP request to the Secrets Manager service
-- endpoint, then you must generate a @ClientRequestToken@ yourself for the
-- new version and include the value in the request.
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

-- | The description of the secret.
createSecret_description :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Text)
createSecret_description = Lens.lens (\CreateSecret' {description} -> description) (\s@CreateSecret' {} a -> s {description = a} :: CreateSecret)

-- | Specifies whether to overwrite a secret with the same name in the
-- destination Region.
createSecret_forceOverwriteReplicaSecret :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Bool)
createSecret_forceOverwriteReplicaSecret = Lens.lens (\CreateSecret' {forceOverwriteReplicaSecret} -> forceOverwriteReplicaSecret) (\s@CreateSecret' {} a -> s {forceOverwriteReplicaSecret = a} :: CreateSecret)

-- | The ARN, key ID, or alias of the KMS key that Secrets Manager uses to
-- encrypt the secret value in the secret. An alias is always prefixed by
-- @alias\/@, for example @alias\/aws\/secretsmanager@. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/alias-about.html About aliases>.
--
-- To use a KMS key in a different account, use the key ARN or the alias
-- ARN.
--
-- If you don\'t specify this value, then Secrets Manager uses the key
-- @aws\/secretsmanager@. If that key doesn\'t yet exist, then Secrets
-- Manager creates it for you automatically the first time it encrypts the
-- secret value.
--
-- If the secret is in a different Amazon Web Services account from the
-- credentials calling the API, then you can\'t use @aws\/secretsmanager@
-- to encrypt the secret, and you must create and use a customer managed
-- KMS key.
createSecret_kmsKeyId :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Text)
createSecret_kmsKeyId = Lens.lens (\CreateSecret' {kmsKeyId} -> kmsKeyId) (\s@CreateSecret' {} a -> s {kmsKeyId = a} :: CreateSecret)

-- | The binary data to encrypt and store in the new version of the secret.
-- We recommend that you store your binary data in a file and then pass the
-- contents of the file as a parameter.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
--
-- This parameter is not available in the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createSecret_secretBinary :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.ByteString)
createSecret_secretBinary = Lens.lens (\CreateSecret' {secretBinary} -> secretBinary) (\s@CreateSecret' {} a -> s {secretBinary = a} :: CreateSecret) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The text data to encrypt and store in this new version of the secret. We
-- recommend you use a JSON structure of key\/value pairs for your secret
-- value.
--
-- Either @SecretString@ or @SecretBinary@ must have a value, but not both.
--
-- If you create a secret by using the Secrets Manager console then Secrets
-- Manager puts the protected secret text in only the @SecretString@
-- parameter. The Secrets Manager console stores the information as a JSON
-- structure of key\/value pairs that a Lambda rotation function can parse.
createSecret_secretString :: Lens.Lens' CreateSecret (Prelude.Maybe Prelude.Text)
createSecret_secretString = Lens.lens (\CreateSecret' {secretString} -> secretString) (\s@CreateSecret' {} a -> s {secretString = a} :: CreateSecret) Prelude.. Lens.mapping Data._Sensitive

-- | A list of tags to attach to the secret. Each tag is a key and value pair
-- of strings in a JSON text string, for example:
--
-- @[{\"Key\":\"CostCenter\",\"Value\":\"12345\"},{\"Key\":\"environment\",\"Value\":\"production\"}]@
--
-- Secrets Manager tag key names are case sensitive. A tag with the key
-- \"ABC\" is a different tag from one with key \"abc\".
--
-- If you check tags in permissions policies as part of your security
-- strategy, then adding or removing a tag can change permissions. If the
-- completion of this operation would result in you losing your permissions
-- for this secret, then Secrets Manager blocks the operation and returns
-- an @Access Denied@ error. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html#tag-secrets-abac Control access to secrets using tags>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access_examples.html#auth-and-access_tags2 Limit access to identities with tags that match secrets\' tags>.
--
-- For information about how to format a JSON parameter for the various
-- command line tool environments, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>.
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of tags per secret: 50
--
-- -   Maximum key length: 127 Unicode characters in UTF-8
--
-- -   Maximum value length: 255 Unicode characters in UTF-8
--
-- -   Tag keys and values are case sensitive.
--
-- -   Do not use the @aws:@ prefix in your tag names or values because
--     Amazon Web Services reserves it for Amazon Web Services use. You
--     can\'t edit or delete tag names or values with this prefix. Tags
--     with this prefix do not count against your tags per secret limit.
--
-- -   If you use your tagging schema across multiple services and
--     resources, other services might have restrictions on allowed
--     characters. Generally allowed characters: letters, spaces, and
--     numbers representable in UTF-8, plus the following special
--     characters: + - = . _ : \/ \@.
createSecret_tags :: Lens.Lens' CreateSecret (Prelude.Maybe [Tag])
createSecret_tags = Lens.lens (\CreateSecret' {tags} -> tags) (\s@CreateSecret' {} a -> s {tags = a} :: CreateSecret) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new secret.
--
-- The secret name can contain ASCII letters, numbers, and the following
-- characters: \/_+=.\@-
--
-- Do not end your secret name with a hyphen followed by six characters. If
-- you do so, you risk confusion and unexpected results when searching for
-- a secret by partial ARN. Secrets Manager automatically adds a hyphen and
-- six random characters after the secret name at the end of the ARN.
createSecret_name :: Lens.Lens' CreateSecret Prelude.Text
createSecret_name = Lens.lens (\CreateSecret' {name} -> name) (\s@CreateSecret' {} a -> s {name = a} :: CreateSecret)

instance Core.AWSRequest CreateSecret where
  type AWSResponse CreateSecret = CreateSecretResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSecretResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> ( x
                            Data..?> "ReplicationStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSecret where
  hashWithSalt _salt CreateSecret' {..} =
    _salt
      `Prelude.hashWithSalt` addReplicaRegions
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` forceOverwriteReplicaSecret
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` secretBinary
      `Prelude.hashWithSalt` secretString
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSecret where
  rnf CreateSecret' {..} =
    Prelude.rnf addReplicaRegions `Prelude.seq`
      Prelude.rnf clientRequestToken `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf forceOverwriteReplicaSecret `Prelude.seq`
            Prelude.rnf kmsKeyId `Prelude.seq`
              Prelude.rnf secretBinary `Prelude.seq`
                Prelude.rnf secretString `Prelude.seq`
                  Prelude.rnf tags `Prelude.seq`
                    Prelude.rnf name

instance Data.ToHeaders CreateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.CreateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSecret where
  toJSON CreateSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddReplicaRegions" Data..=)
              Prelude.<$> addReplicaRegions,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("ForceOverwriteReplicaSecret" Data..=)
              Prelude.<$> forceOverwriteReplicaSecret,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("SecretBinary" Data..=) Prelude.<$> secretBinary,
            ("SecretString" Data..=) Prelude.<$> secretString,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateSecret where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSecretResponse' smart constructor.
data CreateSecretResponse = CreateSecretResponse'
  { -- | The ARN of the new secret. The ARN includes the name of the secret
    -- followed by six random characters. This ensures that if you create a new
    -- secret with the same name as a deleted secret, then users with access to
    -- the old secret don\'t get access to the new secret because the ARNs are
    -- different.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the new secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of the replicas of this secret and their status:
    --
    -- -   @Failed@, which indicates that the replica was not created.
    --
    -- -   @InProgress@, which indicates that Secrets Manager is in the process
    --     of creating the replica.
    --
    -- -   @InSync@, which indicates that the replica was created.
    replicationStatus :: Prelude.Maybe [ReplicationStatusType],
    -- | The unique identifier associated with the version of the new secret.
    versionId :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'createSecretResponse_arn' - The ARN of the new secret. The ARN includes the name of the secret
-- followed by six random characters. This ensures that if you create a new
-- secret with the same name as a deleted secret, then users with access to
-- the old secret don\'t get access to the new secret because the ARNs are
-- different.
--
-- 'name', 'createSecretResponse_name' - The name of the new secret.
--
-- 'replicationStatus', 'createSecretResponse_replicationStatus' - A list of the replicas of this secret and their status:
--
-- -   @Failed@, which indicates that the replica was not created.
--
-- -   @InProgress@, which indicates that Secrets Manager is in the process
--     of creating the replica.
--
-- -   @InSync@, which indicates that the replica was created.
--
-- 'versionId', 'createSecretResponse_versionId' - The unique identifier associated with the version of the new secret.
--
-- 'httpStatus', 'createSecretResponse_httpStatus' - The response's http status code.
newCreateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSecretResponse
newCreateSecretResponse pHttpStatus_ =
  CreateSecretResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      replicationStatus = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the new secret. The ARN includes the name of the secret
-- followed by six random characters. This ensures that if you create a new
-- secret with the same name as a deleted secret, then users with access to
-- the old secret don\'t get access to the new secret because the ARNs are
-- different.
createSecretResponse_arn :: Lens.Lens' CreateSecretResponse (Prelude.Maybe Prelude.Text)
createSecretResponse_arn = Lens.lens (\CreateSecretResponse' {arn} -> arn) (\s@CreateSecretResponse' {} a -> s {arn = a} :: CreateSecretResponse)

-- | The name of the new secret.
createSecretResponse_name :: Lens.Lens' CreateSecretResponse (Prelude.Maybe Prelude.Text)
createSecretResponse_name = Lens.lens (\CreateSecretResponse' {name} -> name) (\s@CreateSecretResponse' {} a -> s {name = a} :: CreateSecretResponse)

-- | A list of the replicas of this secret and their status:
--
-- -   @Failed@, which indicates that the replica was not created.
--
-- -   @InProgress@, which indicates that Secrets Manager is in the process
--     of creating the replica.
--
-- -   @InSync@, which indicates that the replica was created.
createSecretResponse_replicationStatus :: Lens.Lens' CreateSecretResponse (Prelude.Maybe [ReplicationStatusType])
createSecretResponse_replicationStatus = Lens.lens (\CreateSecretResponse' {replicationStatus} -> replicationStatus) (\s@CreateSecretResponse' {} a -> s {replicationStatus = a} :: CreateSecretResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier associated with the version of the new secret.
createSecretResponse_versionId :: Lens.Lens' CreateSecretResponse (Prelude.Maybe Prelude.Text)
createSecretResponse_versionId = Lens.lens (\CreateSecretResponse' {versionId} -> versionId) (\s@CreateSecretResponse' {} a -> s {versionId = a} :: CreateSecretResponse)

-- | The response's http status code.
createSecretResponse_httpStatus :: Lens.Lens' CreateSecretResponse Prelude.Int
createSecretResponse_httpStatus = Lens.lens (\CreateSecretResponse' {httpStatus} -> httpStatus) (\s@CreateSecretResponse' {} a -> s {httpStatus = a} :: CreateSecretResponse)

instance Prelude.NFData CreateSecretResponse where
  rnf CreateSecretResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf replicationStatus `Prelude.seq`
          Prelude.rnf versionId `Prelude.seq`
            Prelude.rnf httpStatus
