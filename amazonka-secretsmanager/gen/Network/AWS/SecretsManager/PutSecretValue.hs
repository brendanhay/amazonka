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
-- Module      : Network.AWS.SecretsManager.PutSecretValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a new encrypted secret value in the specified secret. To do this,
-- the operation creates a new version and attaches it to the secret. The
-- version can contain a new @SecretString@ value or a new @SecretBinary@
-- value. You can also specify the staging labels that are initially
-- attached to the new version.
--
-- The Secrets Manager console uses only the @SecretString@ field. To add
-- binary data to a secret with the @SecretBinary@ field you must use the
-- AWS CLI or one of the AWS SDKs.
--
-- -   If this operation creates the first version for the secret then
--     Secrets Manager automatically attaches the staging label
--     @AWSCURRENT@ to the new version.
--
-- -   If you do not specify a value for VersionStages then Secrets Manager
--     automatically moves the staging label @AWSCURRENT@ to this new
--     version.
--
-- -   If this operation moves the staging label @AWSCURRENT@ from another
--     version to this version, then Secrets Manager also automatically
--     moves the staging label @AWSPREVIOUS@ to the version that
--     @AWSCURRENT@ was removed from.
--
-- -   This operation is idempotent. If a version with a @VersionId@ with
--     the same value as the @ClientRequestToken@ parameter already exists
--     and you specify the same secret data, the operation succeeds but
--     does nothing. However, if the secret data is different, then the
--     operation fails because you cannot modify an existing version; you
--     can only create new ones.
--
-- -   If you call an operation to encrypt or decrypt the @SecretString@ or
--     @SecretBinary@ for a secret in the same account as the calling user
--     and that secret doesn\'t specify a AWS KMS encryption key, Secrets
--     Manager uses the account\'s default AWS managed customer master key
--     (CMK) with the alias @aws\/secretsmanager@. If this key doesn\'t
--     already exist in your account then Secrets Manager creates it for
--     you automatically. All users and roles in the same AWS account
--     automatically have access to use the default CMK. Note that if an
--     Secrets Manager API call results in AWS creating the account\'s
--     AWS-managed CMK, it can result in a one-time significant delay in
--     returning the result.
--
-- -   If the secret resides in a different AWS account from the
--     credentials calling an API that requires encryption or decryption of
--     the secret value then you must create and use a custom AWS KMS CMK
--     because you can\'t access the default CMK for the account using
--     credentials from a different AWS account. Store the ARN of the CMK
--     in the secret when you create the secret or when you update it by
--     including it in the @KMSKeyId@. If you call an API that must encrypt
--     or decrypt @SecretString@ or @SecretBinary@ using credentials from a
--     different account then the AWS KMS key policy must grant
--     cross-account access to that other account\'s user or role for both
--     the kms:GenerateDataKey and kms:Decrypt operations.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:PutSecretValue
--
-- -   kms:GenerateDataKey - needed only if you use a customer-managed AWS
--     KMS key to encrypt the secret. You do not need this permission to
--     use the account\'s default AWS managed CMK for Secrets Manager.
--
-- __Related operations__
--
-- -   To retrieve the encrypted value you store in the version of a
--     secret, use GetSecretValue.
--
-- -   To create a secret, use CreateSecret.
--
-- -   To get the details for a secret, use DescribeSecret.
--
-- -   To list the versions attached to a secret, use ListSecretVersionIds.
module Network.AWS.SecretsManager.PutSecretValue
  ( -- * Creating a Request
    PutSecretValue (..),
    newPutSecretValue,

    -- * Request Lenses
    putSecretValue_secretBinary,
    putSecretValue_versionStages,
    putSecretValue_secretString,
    putSecretValue_clientRequestToken,
    putSecretValue_secretId,

    -- * Destructuring the Response
    PutSecretValueResponse (..),
    newPutSecretValueResponse,

    -- * Response Lenses
    putSecretValueResponse_versionStages,
    putSecretValueResponse_arn,
    putSecretValueResponse_versionId,
    putSecretValueResponse_name,
    putSecretValueResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newPutSecretValue' smart constructor.
data PutSecretValue = PutSecretValue'
  { -- | (Optional) Specifies binary data that you want to encrypt and store in
    -- the new version of the secret. To use this parameter in the command-line
    -- tools, we recommend that you store your binary data in a file and then
    -- use the appropriate technique for your tool to pass the contents of the
    -- file as a parameter. Either @SecretBinary@ or @SecretString@ must have a
    -- value, but not both. They cannot both be empty.
    --
    -- This parameter is not accessible if the secret using the Secrets Manager
    -- console.
    secretBinary :: Core.Maybe (Core.Sensitive Core.Base64),
    -- | (Optional) Specifies a list of staging labels that are attached to this
    -- version of the secret. These staging labels are used to track the
    -- versions through the rotation process by the Lambda rotation function.
    --
    -- A staging label must be unique to a single version of the secret. If you
    -- specify a staging label that\'s already associated with a different
    -- version of the same secret then that staging label is automatically
    -- removed from the other version and attached to this version.
    --
    -- If you do not specify a value for @VersionStages@ then Secrets Manager
    -- automatically moves the staging label @AWSCURRENT@ to this new version.
    versionStages :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | (Optional) Specifies text data that you want to encrypt and store in
    -- this new version of the secret. Either @SecretString@ or @SecretBinary@
    -- must have a value, but not both. They cannot both be empty.
    --
    -- If you create this secret by using the Secrets Manager console then
    -- Secrets Manager puts the protected secret text in only the
    -- @SecretString@ parameter. The Secrets Manager console stores the
    -- information as a JSON structure of key\/value pairs that the default
    -- Lambda rotation function knows how to parse.
    --
    -- For storing multiple values, we recommend that you use a JSON text
    -- string argument and specify key\/value pairs. For information on how to
    -- format a JSON parameter for the various command line tool environments,
    -- see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
    -- in the /AWS CLI User Guide/.
    --
    -- For example:
    --
    -- @[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]@
    --
    -- If your command-line tool or SDK requires quotation marks around the
    -- parameter, you should use single quotes to avoid confusion with the
    -- double quotes required in the JSON text.
    secretString :: Core.Maybe (Core.Sensitive Core.Text),
    -- | (Optional) Specifies a unique identifier for the new version of the
    -- secret.
    --
    -- If you use the AWS CLI or one of the AWS SDK to call this operation,
    -- then you can leave this parameter empty. The CLI or SDK generates a
    -- random UUID for you and includes that in the request. If you don\'t use
    -- the SDK and instead generate a raw HTTP request to the Secrets Manager
    -- service endpoint, then you must generate a @ClientRequestToken@ yourself
    -- for new versions and include that value in the request.
    --
    -- This value helps ensure idempotency. Secrets Manager uses this value to
    -- prevent the accidental creation of duplicate versions if there are
    -- failures and retries during the Lambda rotation function\'s processing.
    -- We recommend that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value to ensure uniqueness within the specified secret.
    --
    -- -   If the @ClientRequestToken@ value isn\'t already associated with a
    --     version of the secret then a new version of the secret is created.
    --
    -- -   If a version with this value already exists and that version\'s
    --     @SecretString@ or @SecretBinary@ values are the same as those in the
    --     request then the request is ignored (the operation is idempotent).
    --
    -- -   If a version with this value already exists and the version of the
    --     @SecretString@ and @SecretBinary@ values are different from those in
    --     the request then the request fails because you cannot modify an
    --     existing secret version. You can only create new versions to store
    --     new secret values.
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | Specifies the secret to which you want to add a new version. You can
    -- specify either the Amazon Resource Name (ARN) or the friendly name of
    -- the secret. The secret must already exist.
    --
    -- If you specify an ARN, we generally recommend that you specify a
    -- complete ARN. You can specify a partial ARN too—for example, if you
    -- don’t include the final hyphen and six random characters that Secrets
    -- Manager adds at the end of the ARN when you created the secret. A
    -- partial ARN match can work as long as it uniquely matches only one
    -- secret. However, if your secret has a name that ends in a hyphen
    -- followed by six characters (before Secrets Manager adds the hyphen and
    -- six characters to the ARN) and you try to use that as a partial ARN,
    -- then those characters cause Secrets Manager to assume that you’re
    -- specifying a complete ARN. This confusion can cause unexpected results.
    -- To avoid this situation, we recommend that you don’t create secret names
    -- ending with a hyphen followed by six characters.
    --
    -- If you specify an incomplete ARN without the random suffix, and instead
    -- provide the \'friendly name\', you /must/ not include the random suffix.
    -- If you do include the random suffix added by Secrets Manager, you
    -- receive either a /ResourceNotFoundException/ or an
    -- /AccessDeniedException/ error, depending on your permissions.
    secretId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutSecretValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretBinary', 'putSecretValue_secretBinary' - (Optional) Specifies binary data that you want to encrypt and store in
-- the new version of the secret. To use this parameter in the command-line
-- tools, we recommend that you store your binary data in a file and then
-- use the appropriate technique for your tool to pass the contents of the
-- file as a parameter. Either @SecretBinary@ or @SecretString@ must have a
-- value, but not both. They cannot both be empty.
--
-- This parameter is not accessible if the secret using the Secrets Manager
-- console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'versionStages', 'putSecretValue_versionStages' - (Optional) Specifies a list of staging labels that are attached to this
-- version of the secret. These staging labels are used to track the
-- versions through the rotation process by the Lambda rotation function.
--
-- A staging label must be unique to a single version of the secret. If you
-- specify a staging label that\'s already associated with a different
-- version of the same secret then that staging label is automatically
-- removed from the other version and attached to this version.
--
-- If you do not specify a value for @VersionStages@ then Secrets Manager
-- automatically moves the staging label @AWSCURRENT@ to this new version.
--
-- 'secretString', 'putSecretValue_secretString' - (Optional) Specifies text data that you want to encrypt and store in
-- this new version of the secret. Either @SecretString@ or @SecretBinary@
-- must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then
-- Secrets Manager puts the protected secret text in only the
-- @SecretString@ parameter. The Secrets Manager console stores the
-- information as a JSON structure of key\/value pairs that the default
-- Lambda rotation function knows how to parse.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For information on how to
-- format a JSON parameter for the various command line tool environments,
-- see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/.
--
-- For example:
--
-- @[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]@
--
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text.
--
-- 'clientRequestToken', 'putSecretValue_clientRequestToken' - (Optional) Specifies a unique identifier for the new version of the
-- secret.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation,
-- then you can leave this parameter empty. The CLI or SDK generates a
-- random UUID for you and includes that in the request. If you don\'t use
-- the SDK and instead generate a raw HTTP request to the Secrets Manager
-- service endpoint, then you must generate a @ClientRequestToken@ yourself
-- for new versions and include that value in the request.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to
-- prevent the accidental creation of duplicate versions if there are
-- failures and retries during the Lambda rotation function\'s processing.
-- We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ or @SecretBinary@ values are the same as those in the
--     request then the request is ignored (the operation is idempotent).
--
-- -   If a version with this value already exists and the version of the
--     @SecretString@ and @SecretBinary@ values are different from those in
--     the request then the request fails because you cannot modify an
--     existing secret version. You can only create new versions to store
--     new secret values.
--
-- This value becomes the @VersionId@ of the new version.
--
-- 'secretId', 'putSecretValue_secretId' - Specifies the secret to which you want to add a new version. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret. The secret must already exist.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
newPutSecretValue ::
  -- | 'secretId'
  Core.Text ->
  PutSecretValue
newPutSecretValue pSecretId_ =
  PutSecretValue'
    { secretBinary = Core.Nothing,
      versionStages = Core.Nothing,
      secretString = Core.Nothing,
      clientRequestToken = Core.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Specifies binary data that you want to encrypt and store in
-- the new version of the secret. To use this parameter in the command-line
-- tools, we recommend that you store your binary data in a file and then
-- use the appropriate technique for your tool to pass the contents of the
-- file as a parameter. Either @SecretBinary@ or @SecretString@ must have a
-- value, but not both. They cannot both be empty.
--
-- This parameter is not accessible if the secret using the Secrets Manager
-- console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putSecretValue_secretBinary :: Lens.Lens' PutSecretValue (Core.Maybe Core.ByteString)
putSecretValue_secretBinary = Lens.lens (\PutSecretValue' {secretBinary} -> secretBinary) (\s@PutSecretValue' {} a -> s {secretBinary = a} :: PutSecretValue) Core.. Lens.mapping (Core._Sensitive Core.. Core._Base64)

-- | (Optional) Specifies a list of staging labels that are attached to this
-- version of the secret. These staging labels are used to track the
-- versions through the rotation process by the Lambda rotation function.
--
-- A staging label must be unique to a single version of the secret. If you
-- specify a staging label that\'s already associated with a different
-- version of the same secret then that staging label is automatically
-- removed from the other version and attached to this version.
--
-- If you do not specify a value for @VersionStages@ then Secrets Manager
-- automatically moves the staging label @AWSCURRENT@ to this new version.
putSecretValue_versionStages :: Lens.Lens' PutSecretValue (Core.Maybe (Core.NonEmpty Core.Text))
putSecretValue_versionStages = Lens.lens (\PutSecretValue' {versionStages} -> versionStages) (\s@PutSecretValue' {} a -> s {versionStages = a} :: PutSecretValue) Core.. Lens.mapping Lens._Coerce

-- | (Optional) Specifies text data that you want to encrypt and store in
-- this new version of the secret. Either @SecretString@ or @SecretBinary@
-- must have a value, but not both. They cannot both be empty.
--
-- If you create this secret by using the Secrets Manager console then
-- Secrets Manager puts the protected secret text in only the
-- @SecretString@ parameter. The Secrets Manager console stores the
-- information as a JSON structure of key\/value pairs that the default
-- Lambda rotation function knows how to parse.
--
-- For storing multiple values, we recommend that you use a JSON text
-- string argument and specify key\/value pairs. For information on how to
-- format a JSON parameter for the various command line tool environments,
-- see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters>
-- in the /AWS CLI User Guide/.
--
-- For example:
--
-- @[{\"username\":\"bob\"},{\"password\":\"abc123xyz456\"}]@
--
-- If your command-line tool or SDK requires quotation marks around the
-- parameter, you should use single quotes to avoid confusion with the
-- double quotes required in the JSON text.
putSecretValue_secretString :: Lens.Lens' PutSecretValue (Core.Maybe Core.Text)
putSecretValue_secretString = Lens.lens (\PutSecretValue' {secretString} -> secretString) (\s@PutSecretValue' {} a -> s {secretString = a} :: PutSecretValue) Core.. Lens.mapping Core._Sensitive

-- | (Optional) Specifies a unique identifier for the new version of the
-- secret.
--
-- If you use the AWS CLI or one of the AWS SDK to call this operation,
-- then you can leave this parameter empty. The CLI or SDK generates a
-- random UUID for you and includes that in the request. If you don\'t use
-- the SDK and instead generate a raw HTTP request to the Secrets Manager
-- service endpoint, then you must generate a @ClientRequestToken@ yourself
-- for new versions and include that value in the request.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to
-- prevent the accidental creation of duplicate versions if there are
-- failures and retries during the Lambda rotation function\'s processing.
-- We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ or @SecretBinary@ values are the same as those in the
--     request then the request is ignored (the operation is idempotent).
--
-- -   If a version with this value already exists and the version of the
--     @SecretString@ and @SecretBinary@ values are different from those in
--     the request then the request fails because you cannot modify an
--     existing secret version. You can only create new versions to store
--     new secret values.
--
-- This value becomes the @VersionId@ of the new version.
putSecretValue_clientRequestToken :: Lens.Lens' PutSecretValue (Core.Maybe Core.Text)
putSecretValue_clientRequestToken = Lens.lens (\PutSecretValue' {clientRequestToken} -> clientRequestToken) (\s@PutSecretValue' {} a -> s {clientRequestToken = a} :: PutSecretValue)

-- | Specifies the secret to which you want to add a new version. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret. The secret must already exist.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
putSecretValue_secretId :: Lens.Lens' PutSecretValue Core.Text
putSecretValue_secretId = Lens.lens (\PutSecretValue' {secretId} -> secretId) (\s@PutSecretValue' {} a -> s {secretId = a} :: PutSecretValue)

instance Core.AWSRequest PutSecretValue where
  type
    AWSResponse PutSecretValue =
      PutSecretValueResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSecretValueResponse'
            Core.<$> (x Core..?> "VersionStages")
            Core.<*> (x Core..?> "ARN")
            Core.<*> (x Core..?> "VersionId")
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutSecretValue

instance Core.NFData PutSecretValue

instance Core.ToHeaders PutSecretValue where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("secretsmanager.PutSecretValue" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutSecretValue where
  toJSON PutSecretValue' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecretBinary" Core..=) Core.<$> secretBinary,
            ("VersionStages" Core..=) Core.<$> versionStages,
            ("SecretString" Core..=) Core.<$> secretString,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath PutSecretValue where
  toPath = Core.const "/"

instance Core.ToQuery PutSecretValue where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutSecretValueResponse' smart constructor.
data PutSecretValueResponse = PutSecretValueResponse'
  { -- | The list of staging labels that are currently attached to this version
    -- of the secret. Staging labels are used to track a version as it
    -- progresses through the secret rotation process.
    versionStages :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The Amazon Resource Name (ARN) for the secret for which you just created
    -- a version.
    arn :: Core.Maybe Core.Text,
    -- | The unique identifier of the version of the secret you just created or
    -- updated.
    versionId :: Core.Maybe Core.Text,
    -- | The friendly name of the secret for which you just created or updated a
    -- version.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutSecretValueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionStages', 'putSecretValueResponse_versionStages' - The list of staging labels that are currently attached to this version
-- of the secret. Staging labels are used to track a version as it
-- progresses through the secret rotation process.
--
-- 'arn', 'putSecretValueResponse_arn' - The Amazon Resource Name (ARN) for the secret for which you just created
-- a version.
--
-- 'versionId', 'putSecretValueResponse_versionId' - The unique identifier of the version of the secret you just created or
-- updated.
--
-- 'name', 'putSecretValueResponse_name' - The friendly name of the secret for which you just created or updated a
-- version.
--
-- 'httpStatus', 'putSecretValueResponse_httpStatus' - The response's http status code.
newPutSecretValueResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutSecretValueResponse
newPutSecretValueResponse pHttpStatus_ =
  PutSecretValueResponse'
    { versionStages =
        Core.Nothing,
      arn = Core.Nothing,
      versionId = Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of staging labels that are currently attached to this version
-- of the secret. Staging labels are used to track a version as it
-- progresses through the secret rotation process.
putSecretValueResponse_versionStages :: Lens.Lens' PutSecretValueResponse (Core.Maybe (Core.NonEmpty Core.Text))
putSecretValueResponse_versionStages = Lens.lens (\PutSecretValueResponse' {versionStages} -> versionStages) (\s@PutSecretValueResponse' {} a -> s {versionStages = a} :: PutSecretValueResponse) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) for the secret for which you just created
-- a version.
putSecretValueResponse_arn :: Lens.Lens' PutSecretValueResponse (Core.Maybe Core.Text)
putSecretValueResponse_arn = Lens.lens (\PutSecretValueResponse' {arn} -> arn) (\s@PutSecretValueResponse' {} a -> s {arn = a} :: PutSecretValueResponse)

-- | The unique identifier of the version of the secret you just created or
-- updated.
putSecretValueResponse_versionId :: Lens.Lens' PutSecretValueResponse (Core.Maybe Core.Text)
putSecretValueResponse_versionId = Lens.lens (\PutSecretValueResponse' {versionId} -> versionId) (\s@PutSecretValueResponse' {} a -> s {versionId = a} :: PutSecretValueResponse)

-- | The friendly name of the secret for which you just created or updated a
-- version.
putSecretValueResponse_name :: Lens.Lens' PutSecretValueResponse (Core.Maybe Core.Text)
putSecretValueResponse_name = Lens.lens (\PutSecretValueResponse' {name} -> name) (\s@PutSecretValueResponse' {} a -> s {name = a} :: PutSecretValueResponse)

-- | The response's http status code.
putSecretValueResponse_httpStatus :: Lens.Lens' PutSecretValueResponse Core.Int
putSecretValueResponse_httpStatus = Lens.lens (\PutSecretValueResponse' {httpStatus} -> httpStatus) (\s@PutSecretValueResponse' {} a -> s {httpStatus = a} :: PutSecretValueResponse)

instance Core.NFData PutSecretValueResponse
