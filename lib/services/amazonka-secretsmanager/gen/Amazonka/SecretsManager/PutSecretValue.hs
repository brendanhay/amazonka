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
-- Module      : Amazonka.SecretsManager.PutSecretValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version with a new encrypted secret value and attaches it
-- to the secret. The version can contain a new @SecretString@ value or a
-- new @SecretBinary@ value.
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
-- You can specify the staging labels to attach to the new version in
-- @VersionStages@. If you don\'t include @VersionStages@, then Secrets
-- Manager automatically moves the staging label @AWSCURRENT@ to this
-- version. If this operation creates the first version for the secret,
-- then Secrets Manager automatically attaches the staging label
-- @AWSCURRENT@ to it. If this operation moves the staging label
-- @AWSCURRENT@ from another version to this version, then Secrets Manager
-- also automatically moves the staging label @AWSPREVIOUS@ to the version
-- that @AWSCURRENT@ was removed from.
--
-- This operation is idempotent. If you call this operation with a
-- @ClientRequestToken@ that matches an existing version\'s VersionId, and
-- you specify the same secret data, the operation succeeds but does
-- nothing. However, if the secret data is different, then the operation
-- fails because you can\'t modify an existing version; you can only create
-- new ones.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- except @SecretBinary@ or @SecretString@ because it might be logged. For
-- more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:PutSecretValue@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.PutSecretValue
  ( -- * Creating a Request
    PutSecretValue (..),
    newPutSecretValue,

    -- * Request Lenses
    putSecretValue_versionStages,
    putSecretValue_clientRequestToken,
    putSecretValue_secretBinary,
    putSecretValue_secretString,
    putSecretValue_secretId,

    -- * Destructuring the Response
    PutSecretValueResponse (..),
    newPutSecretValueResponse,

    -- * Response Lenses
    putSecretValueResponse_versionStages,
    putSecretValueResponse_name,
    putSecretValueResponse_arn,
    putSecretValueResponse_versionId,
    putSecretValueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newPutSecretValue' smart constructor.
data PutSecretValue = PutSecretValue'
  { -- | A list of staging labels to attach to this version of the secret.
    -- Secrets Manager uses staging labels to track versions of a secret
    -- through the rotation process.
    --
    -- If you specify a staging label that\'s already associated with a
    -- different version of the same secret, then Secrets Manager removes the
    -- label from the other version and attaches it to this version. If you
    -- specify @AWSCURRENT@, and it is already attached to another version,
    -- then Secrets Manager also moves the staging label @AWSPREVIOUS@ to the
    -- version that @AWSCURRENT@ was removed from.
    --
    -- If you don\'t include @VersionStages@, then Secrets Manager
    -- automatically moves the staging label @AWSCURRENT@ to this version.
    versionStages :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A unique identifier for the new version of the secret.
    --
    -- If you use the Amazon Web Services CLI or one of the Amazon Web Services
    -- SDKs to call this operation, then you can leave this parameter empty
    -- because they generate a random UUID for you. If you don\'t use the SDK
    -- and instead generate a raw HTTP request to the Secrets Manager service
    -- endpoint, then you must generate a @ClientRequestToken@ yourself for new
    -- versions and include that value in the request.
    --
    -- This value helps ensure idempotency. Secrets Manager uses this value to
    -- prevent the accidental creation of duplicate versions if there are
    -- failures and retries during the Lambda rotation function processing. We
    -- recommend that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value to ensure uniqueness within the specified secret.
    --
    -- -   If the @ClientRequestToken@ value isn\'t already associated with a
    --     version of the secret then a new version of the secret is created.
    --
    -- -   If a version with this value already exists and that version\'s
    --     @SecretString@ or @SecretBinary@ values are the same as those in the
    --     request then the request is ignored. The operation is idempotent.
    --
    -- -   If a version with this value already exists and the version of the
    --     @SecretString@ and @SecretBinary@ values are different from those in
    --     the request, then the request fails because you can\'t modify a
    --     secret version. You can only create new versions to store new secret
    --     values.
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The binary data to encrypt and store in the new version of the secret.
    -- To use this parameter in the command-line tools, we recommend that you
    -- store your binary data in a file and then pass the contents of the file
    -- as a parameter.
    --
    -- You must include @SecretBinary@ or @SecretString@, but not both.
    --
    -- You can\'t access this value from the Secrets Manager console.
    secretBinary :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | The text to encrypt and store in the new version of the secret.
    --
    -- You must include @SecretBinary@ or @SecretString@, but not both.
    --
    -- We recommend you create the secret string as JSON key\/value pairs, as
    -- shown in the example.
    secretString :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ARN or name of the secret to add a new version to.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    --
    -- If the secret doesn\'t already exist, use @CreateSecret@ instead.
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
-- 'versionStages', 'putSecretValue_versionStages' - A list of staging labels to attach to this version of the secret.
-- Secrets Manager uses staging labels to track versions of a secret
-- through the rotation process.
--
-- If you specify a staging label that\'s already associated with a
-- different version of the same secret, then Secrets Manager removes the
-- label from the other version and attaches it to this version. If you
-- specify @AWSCURRENT@, and it is already attached to another version,
-- then Secrets Manager also moves the staging label @AWSPREVIOUS@ to the
-- version that @AWSCURRENT@ was removed from.
--
-- If you don\'t include @VersionStages@, then Secrets Manager
-- automatically moves the staging label @AWSCURRENT@ to this version.
--
-- 'clientRequestToken', 'putSecretValue_clientRequestToken' - A unique identifier for the new version of the secret.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDKs to call this operation, then you can leave this parameter empty
-- because they generate a random UUID for you. If you don\'t use the SDK
-- and instead generate a raw HTTP request to the Secrets Manager service
-- endpoint, then you must generate a @ClientRequestToken@ yourself for new
-- versions and include that value in the request.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to
-- prevent the accidental creation of duplicate versions if there are
-- failures and retries during the Lambda rotation function processing. We
-- recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ or @SecretBinary@ values are the same as those in the
--     request then the request is ignored. The operation is idempotent.
--
-- -   If a version with this value already exists and the version of the
--     @SecretString@ and @SecretBinary@ values are different from those in
--     the request, then the request fails because you can\'t modify a
--     secret version. You can only create new versions to store new secret
--     values.
--
-- This value becomes the @VersionId@ of the new version.
--
-- 'secretBinary', 'putSecretValue_secretBinary' - The binary data to encrypt and store in the new version of the secret.
-- To use this parameter in the command-line tools, we recommend that you
-- store your binary data in a file and then pass the contents of the file
-- as a parameter.
--
-- You must include @SecretBinary@ or @SecretString@, but not both.
--
-- You can\'t access this value from the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'secretString', 'putSecretValue_secretString' - The text to encrypt and store in the new version of the secret.
--
-- You must include @SecretBinary@ or @SecretString@, but not both.
--
-- We recommend you create the secret string as JSON key\/value pairs, as
-- shown in the example.
--
-- 'secretId', 'putSecretValue_secretId' - The ARN or name of the secret to add a new version to.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
--
-- If the secret doesn\'t already exist, use @CreateSecret@ instead.
newPutSecretValue ::
  -- | 'secretId'
  Prelude.Text ->
  PutSecretValue
newPutSecretValue pSecretId_ =
  PutSecretValue'
    { versionStages = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      secretBinary = Prelude.Nothing,
      secretString = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | A list of staging labels to attach to this version of the secret.
-- Secrets Manager uses staging labels to track versions of a secret
-- through the rotation process.
--
-- If you specify a staging label that\'s already associated with a
-- different version of the same secret, then Secrets Manager removes the
-- label from the other version and attaches it to this version. If you
-- specify @AWSCURRENT@, and it is already attached to another version,
-- then Secrets Manager also moves the staging label @AWSPREVIOUS@ to the
-- version that @AWSCURRENT@ was removed from.
--
-- If you don\'t include @VersionStages@, then Secrets Manager
-- automatically moves the staging label @AWSCURRENT@ to this version.
putSecretValue_versionStages :: Lens.Lens' PutSecretValue (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
putSecretValue_versionStages = Lens.lens (\PutSecretValue' {versionStages} -> versionStages) (\s@PutSecretValue' {} a -> s {versionStages = a} :: PutSecretValue) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the new version of the secret.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDKs to call this operation, then you can leave this parameter empty
-- because they generate a random UUID for you. If you don\'t use the SDK
-- and instead generate a raw HTTP request to the Secrets Manager service
-- endpoint, then you must generate a @ClientRequestToken@ yourself for new
-- versions and include that value in the request.
--
-- This value helps ensure idempotency. Secrets Manager uses this value to
-- prevent the accidental creation of duplicate versions if there are
-- failures and retries during the Lambda rotation function processing. We
-- recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- -   If the @ClientRequestToken@ value isn\'t already associated with a
--     version of the secret then a new version of the secret is created.
--
-- -   If a version with this value already exists and that version\'s
--     @SecretString@ or @SecretBinary@ values are the same as those in the
--     request then the request is ignored. The operation is idempotent.
--
-- -   If a version with this value already exists and the version of the
--     @SecretString@ and @SecretBinary@ values are different from those in
--     the request, then the request fails because you can\'t modify a
--     secret version. You can only create new versions to store new secret
--     values.
--
-- This value becomes the @VersionId@ of the new version.
putSecretValue_clientRequestToken :: Lens.Lens' PutSecretValue (Prelude.Maybe Prelude.Text)
putSecretValue_clientRequestToken = Lens.lens (\PutSecretValue' {clientRequestToken} -> clientRequestToken) (\s@PutSecretValue' {} a -> s {clientRequestToken = a} :: PutSecretValue)

-- | The binary data to encrypt and store in the new version of the secret.
-- To use this parameter in the command-line tools, we recommend that you
-- store your binary data in a file and then pass the contents of the file
-- as a parameter.
--
-- You must include @SecretBinary@ or @SecretString@, but not both.
--
-- You can\'t access this value from the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
putSecretValue_secretBinary :: Lens.Lens' PutSecretValue (Prelude.Maybe Prelude.ByteString)
putSecretValue_secretBinary = Lens.lens (\PutSecretValue' {secretBinary} -> secretBinary) (\s@PutSecretValue' {} a -> s {secretBinary = a} :: PutSecretValue) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | The text to encrypt and store in the new version of the secret.
--
-- You must include @SecretBinary@ or @SecretString@, but not both.
--
-- We recommend you create the secret string as JSON key\/value pairs, as
-- shown in the example.
putSecretValue_secretString :: Lens.Lens' PutSecretValue (Prelude.Maybe Prelude.Text)
putSecretValue_secretString = Lens.lens (\PutSecretValue' {secretString} -> secretString) (\s@PutSecretValue' {} a -> s {secretString = a} :: PutSecretValue) Prelude.. Lens.mapping Core._Sensitive

-- | The ARN or name of the secret to add a new version to.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
--
-- If the secret doesn\'t already exist, use @CreateSecret@ instead.
putSecretValue_secretId :: Lens.Lens' PutSecretValue Prelude.Text
putSecretValue_secretId = Lens.lens (\PutSecretValue' {secretId} -> secretId) (\s@PutSecretValue' {} a -> s {secretId = a} :: PutSecretValue)

instance Core.AWSRequest PutSecretValue where
  type
    AWSResponse PutSecretValue =
      PutSecretValueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSecretValueResponse'
            Prelude.<$> (x Core..?> "VersionStages")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSecretValue where
  hashWithSalt _salt PutSecretValue' {..} =
    _salt `Prelude.hashWithSalt` versionStages
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` secretBinary
      `Prelude.hashWithSalt` secretString
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData PutSecretValue where
  rnf PutSecretValue' {..} =
    Prelude.rnf versionStages
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf secretBinary
      `Prelude.seq` Prelude.rnf secretString
      `Prelude.seq` Prelude.rnf secretId

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
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("SecretBinary" Core..=) Prelude.<$> secretBinary,
            ("SecretString" Core..=) Prelude.<$> secretString,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath PutSecretValue where
  toPath = Prelude.const "/"

instance Core.ToQuery PutSecretValue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSecretValueResponse' smart constructor.
data PutSecretValueResponse = PutSecretValueResponse'
  { -- | The list of staging labels that are currently attached to this version
    -- of the secret. Secrets Manager uses staging labels to track a version as
    -- it progresses through the secret rotation process.
    versionStages :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the version of the secret.
    versionId :: Prelude.Maybe Prelude.Text,
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
-- 'versionStages', 'putSecretValueResponse_versionStages' - The list of staging labels that are currently attached to this version
-- of the secret. Secrets Manager uses staging labels to track a version as
-- it progresses through the secret rotation process.
--
-- 'name', 'putSecretValueResponse_name' - The name of the secret.
--
-- 'arn', 'putSecretValueResponse_arn' - The ARN of the secret.
--
-- 'versionId', 'putSecretValueResponse_versionId' - The unique identifier of the version of the secret.
--
-- 'httpStatus', 'putSecretValueResponse_httpStatus' - The response's http status code.
newPutSecretValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSecretValueResponse
newPutSecretValueResponse pHttpStatus_ =
  PutSecretValueResponse'
    { versionStages =
        Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of staging labels that are currently attached to this version
-- of the secret. Secrets Manager uses staging labels to track a version as
-- it progresses through the secret rotation process.
putSecretValueResponse_versionStages :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
putSecretValueResponse_versionStages = Lens.lens (\PutSecretValueResponse' {versionStages} -> versionStages) (\s@PutSecretValueResponse' {} a -> s {versionStages = a} :: PutSecretValueResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the secret.
putSecretValueResponse_name :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe Prelude.Text)
putSecretValueResponse_name = Lens.lens (\PutSecretValueResponse' {name} -> name) (\s@PutSecretValueResponse' {} a -> s {name = a} :: PutSecretValueResponse)

-- | The ARN of the secret.
putSecretValueResponse_arn :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe Prelude.Text)
putSecretValueResponse_arn = Lens.lens (\PutSecretValueResponse' {arn} -> arn) (\s@PutSecretValueResponse' {} a -> s {arn = a} :: PutSecretValueResponse)

-- | The unique identifier of the version of the secret.
putSecretValueResponse_versionId :: Lens.Lens' PutSecretValueResponse (Prelude.Maybe Prelude.Text)
putSecretValueResponse_versionId = Lens.lens (\PutSecretValueResponse' {versionId} -> versionId) (\s@PutSecretValueResponse' {} a -> s {versionId = a} :: PutSecretValueResponse)

-- | The response's http status code.
putSecretValueResponse_httpStatus :: Lens.Lens' PutSecretValueResponse Prelude.Int
putSecretValueResponse_httpStatus = Lens.lens (\PutSecretValueResponse' {httpStatus} -> httpStatus) (\s@PutSecretValueResponse' {} a -> s {httpStatus = a} :: PutSecretValueResponse)

instance Prelude.NFData PutSecretValueResponse where
  rnf PutSecretValueResponse' {..} =
    Prelude.rnf versionStages
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
