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
-- We recommend you avoid calling @PutSecretValue@ at a sustained rate of
-- more than once every 10 minutes. When you update the secret value,
-- Secrets Manager creates a new version of the secret. Secrets Manager
-- removes outdated versions when there are more than 100, but it does not
-- remove versions created less than 24 hours ago. If you call
-- @PutSecretValue@ more than once every 10 minutes, you create more
-- versions than Secrets Manager removes, and you will reach the quota for
-- secret versions.
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
-- -   secretsmanager:PutSecretValue
--
-- -   kms:GenerateDataKey - needed only if you use a customer-managed
--     Amazon Web Services KMS key to encrypt the secret. You do not need
--     this permission to use the account\'s default Amazon Web Services
--     managed CMK for Secrets Manager.
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
    putSecretValue_versionStages,
    putSecretValue_secretBinary,
    putSecretValue_secretString,
    putSecretValue_clientRequestToken,
    putSecretValue_secretId,

    -- * Destructuring the Response
    PutSecretValueResponse (..),
    newPutSecretValueResponse,

    -- * Response Lenses
    putSecretValueResponse_versionId,
    putSecretValueResponse_arn,
    putSecretValueResponse_versionStages,
    putSecretValueResponse_name,
    putSecretValueResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newPutSecretValue' smart constructor.
data PutSecretValue = PutSecretValue'
  { -- | (Optional) Specifies a list of staging labels that are attached to this
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
    versionStages :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Optional) Specifies binary data that you want to encrypt and store in
    -- the new version of the secret. To use this parameter in the command-line
    -- tools, we recommend that you store your binary data in a file and then
    -- use the appropriate technique for your tool to pass the contents of the
    -- file as a parameter. Either @SecretBinary@ or @SecretString@ must have a
    -- value, but not both. They cannot both be empty.
    --
    -- This parameter is not accessible if the secret using the Secrets Manager
    -- console.
    secretBinary :: Prelude.Maybe (Core.Sensitive Core.Base64),
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
    -- string argument and specify key\/value pairs. For more information, see
    -- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
    -- in the Amazon Web Services CLI User Guide.
    secretString :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | (Optional) Specifies a unique identifier for the new version of the
    -- secret.
    --
    -- If you use the Amazon Web Services CLI or one of the Amazon Web Services
    -- SDK to call this operation, then you can leave this parameter empty. The
    -- CLI or SDK generates a random UUID for you and includes that in the
    -- request. If you don\'t use the SDK and instead generate a raw HTTP
    -- request to the Secrets Manager service endpoint, then you must generate
    -- a @ClientRequestToken@ yourself for new versions and include that value
    -- in the request.
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
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the secret to which you want to add a new version. You can
    -- specify either the Amazon Resource Name (ARN) or the friendly name of
    -- the secret. The secret must already exist.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSecretValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
--
-- 'clientRequestToken', 'putSecretValue_clientRequestToken' - (Optional) Specifies a unique identifier for the new version of the
-- secret.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes that in the
-- request. If you don\'t use the SDK and instead generate a raw HTTP
-- request to the Secrets Manager service endpoint, then you must generate
-- a @ClientRequestToken@ yourself for new versions and include that value
-- in the request.
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
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
newPutSecretValue ::
  -- | 'secretId'
  Prelude.Text ->
  PutSecretValue
newPutSecretValue pSecretId_ =
  PutSecretValue'
    { versionStages = Prelude.Nothing,
      secretBinary = Prelude.Nothing,
      secretString = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      secretId = pSecretId_
    }

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
putSecretValue_versionStages :: Lens.Lens' PutSecretValue (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
putSecretValue_versionStages = Lens.lens (\PutSecretValue' {versionStages} -> versionStages) (\s@PutSecretValue' {} a -> s {versionStages = a} :: PutSecretValue) Prelude.. Lens.mapping Lens.coerced

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
putSecretValue_secretBinary :: Lens.Lens' PutSecretValue (Prelude.Maybe Prelude.ByteString)
putSecretValue_secretBinary = Lens.lens (\PutSecretValue' {secretBinary} -> secretBinary) (\s@PutSecretValue' {} a -> s {secretBinary = a} :: PutSecretValue) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

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
-- string argument and specify key\/value pairs. For more information, see
-- <https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters.html Specifying parameter values for the Amazon Web Services CLI>
-- in the Amazon Web Services CLI User Guide.
putSecretValue_secretString :: Lens.Lens' PutSecretValue (Prelude.Maybe Prelude.Text)
putSecretValue_secretString = Lens.lens (\PutSecretValue' {secretString} -> secretString) (\s@PutSecretValue' {} a -> s {secretString = a} :: PutSecretValue) Prelude.. Lens.mapping Core._Sensitive

-- | (Optional) Specifies a unique identifier for the new version of the
-- secret.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes that in the
-- request. If you don\'t use the SDK and instead generate a raw HTTP
-- request to the Secrets Manager service endpoint, then you must generate
-- a @ClientRequestToken@ yourself for new versions and include that value
-- in the request.
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
putSecretValue_clientRequestToken :: Lens.Lens' PutSecretValue (Prelude.Maybe Prelude.Text)
putSecretValue_clientRequestToken = Lens.lens (\PutSecretValue' {clientRequestToken} -> clientRequestToken) (\s@PutSecretValue' {} a -> s {clientRequestToken = a} :: PutSecretValue)

-- | Specifies the secret to which you want to add a new version. You can
-- specify either the Amazon Resource Name (ARN) or the friendly name of
-- the secret. The secret must already exist.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
putSecretValue_secretId :: Lens.Lens' PutSecretValue Prelude.Text
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
            Prelude.<$> (x Core..?> "VersionId")
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "VersionStages")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSecretValue

instance Prelude.NFData PutSecretValue

instance Core.ToHeaders PutSecretValue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.PutSecretValue" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutSecretValue where
  toJSON PutSecretValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VersionStages" Core..=) Prelude.<$> versionStages,
            ("SecretBinary" Core..=) Prelude.<$> secretBinary,
            ("SecretString" Core..=) Prelude.<$> secretString,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath PutSecretValue where
  toPath = Prelude.const "/"

instance Core.ToQuery PutSecretValue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSecretValueResponse' smart constructor.
data PutSecretValueResponse = PutSecretValueResponse'
  { -- | The unique identifier of the version of the secret you just created or
    -- updated.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the secret for which you just created
    -- a version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The list of staging labels that are currently attached to this version
    -- of the secret. Staging labels are used to track a version as it
    -- progresses through the secret rotation process.
    versionStages :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The friendly name of the secret for which you just created or updated a
    -- version.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSecretValueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'putSecretValueResponse_versionId' - The unique identifier of the version of the secret you just created or
-- updated.
--
-- 'arn', 'putSecretValueResponse_arn' - The Amazon Resource Name (ARN) for the secret for which you just created
-- a version.
--
-- 'versionStages', 'putSecretValueResponse_versionStages' - The list of staging labels that are currently attached to this version
-- of the secret. Staging labels are used to track a version as it
-- progresses through the secret rotation process.
--
-- 'name', 'putSecretValueResponse_name' - The friendly name of the secret for which you just created or updated a
-- version.
--
-- 'httpStatus', 'putSecretValueResponse_httpStatus' - The response's http status code.
newPutSecretValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSecretValueResponse
newPutSecretValueResponse pHttpStatus_ =
  PutSecretValueResponse'
    { versionId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      versionStages = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the version of the secret you just created or
-- updated.
putSecretValueResponse_versionId :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe Prelude.Text)
putSecretValueResponse_versionId = Lens.lens (\PutSecretValueResponse' {versionId} -> versionId) (\s@PutSecretValueResponse' {} a -> s {versionId = a} :: PutSecretValueResponse)

-- | The Amazon Resource Name (ARN) for the secret for which you just created
-- a version.
putSecretValueResponse_arn :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe Prelude.Text)
putSecretValueResponse_arn = Lens.lens (\PutSecretValueResponse' {arn} -> arn) (\s@PutSecretValueResponse' {} a -> s {arn = a} :: PutSecretValueResponse)

-- | The list of staging labels that are currently attached to this version
-- of the secret. Staging labels are used to track a version as it
-- progresses through the secret rotation process.
putSecretValueResponse_versionStages :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
putSecretValueResponse_versionStages = Lens.lens (\PutSecretValueResponse' {versionStages} -> versionStages) (\s@PutSecretValueResponse' {} a -> s {versionStages = a} :: PutSecretValueResponse) Prelude.. Lens.mapping Lens.coerced

-- | The friendly name of the secret for which you just created or updated a
-- version.
putSecretValueResponse_name :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe Prelude.Text)
putSecretValueResponse_name = Lens.lens (\PutSecretValueResponse' {name} -> name) (\s@PutSecretValueResponse' {} a -> s {name = a} :: PutSecretValueResponse)

-- | The response's http status code.
putSecretValueResponse_httpStatus :: Lens.Lens' PutSecretValueResponse Prelude.Int
putSecretValueResponse_httpStatus = Lens.lens (\PutSecretValueResponse' {httpStatus} -> httpStatus) (\s@PutSecretValueResponse' {} a -> s {httpStatus = a} :: PutSecretValueResponse)

instance Prelude.NFData PutSecretValueResponse
