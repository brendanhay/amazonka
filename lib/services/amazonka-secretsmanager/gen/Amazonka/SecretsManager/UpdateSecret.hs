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
-- Module      : Amazonka.SecretsManager.UpdateSecret
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the details of a secret, including metadata and the secret
-- value. To change the secret value, you can also use PutSecretValue.
--
-- To change the rotation configuration of a secret, use RotateSecret
-- instead.
--
-- We recommend you avoid calling @UpdateSecret@ at a sustained rate of
-- more than once every 10 minutes. When you call @UpdateSecret@ to update
-- the secret value, Secrets Manager creates a new version of the secret.
-- Secrets Manager removes outdated versions when there are more than 100,
-- but it does not remove versions created less than 24 hours ago. If you
-- update the secret value more than once every 10 minutes, you create more
-- versions than Secrets Manager removes, and you will reach the quota for
-- secret versions.
--
-- If you include @SecretString@ or @SecretBinary@ to create a new secret
-- version, Secrets Manager automatically moves the staging label
-- @AWSCURRENT@ to the new version. Then it attaches the label
-- @AWSPREVIOUS@ to the version that @AWSCURRENT@ was removed from.
--
-- If you call this operation with a @ClientRequestToken@ that matches an
-- existing version\'s @VersionId@, the operation results in an error. You
-- can\'t modify an existing version, you can only create a new version. To
-- remove a version, remove all staging labels from it. See
-- UpdateSecretVersionStage.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- except @SecretBinary@ or @SecretString@ because it might be logged. For
-- more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:UpdateSecret@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
-- If you use a customer managed key, you must also have
-- @kms:GenerateDataKey@ and @kms:Decrypt@ permissions on the key. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/security-encryption.html Secret encryption and decryption>.
module Amazonka.SecretsManager.UpdateSecret
  ( -- * Creating a Request
    UpdateSecret (..),
    newUpdateSecret,

    -- * Request Lenses
    updateSecret_clientRequestToken,
    updateSecret_description,
    updateSecret_kmsKeyId,
    updateSecret_secretBinary,
    updateSecret_secretString,
    updateSecret_secretId,

    -- * Destructuring the Response
    UpdateSecretResponse (..),
    newUpdateSecretResponse,

    -- * Response Lenses
    updateSecretResponse_arn,
    updateSecretResponse_name,
    updateSecretResponse_versionId,
    updateSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newUpdateSecret' smart constructor.
data UpdateSecret = UpdateSecret'
  { -- | If you include @SecretString@ or @SecretBinary@, then Secrets Manager
    -- creates a new version for the secret, and this parameter specifies the
    -- unique identifier for the new version.
    --
    -- If you use the Amazon Web Services CLI or one of the Amazon Web Services
    -- SDKs to call this operation, then you can leave this parameter empty.
    -- The CLI or SDK generates a random UUID for you and includes it as the
    -- value for this parameter in the request. If you don\'t use the SDK and
    -- instead generate a raw HTTP request to the Secrets Manager service
    -- endpoint, then you must generate a @ClientRequestToken@ yourself for the
    -- new version and include the value in the request.
    --
    -- This value becomes the @VersionId@ of the new version.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the secret.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN, key ID, or alias of the KMS key that Secrets Manager uses to
    -- encrypt new secret versions as well as any existing versions with the
    -- staging labels @AWSCURRENT@, @AWSPENDING@, or @AWSPREVIOUS@. For more
    -- information about versions and staging labels, see
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/getting-started.html#term_version Concepts: Version>.
    --
    -- A key alias is always prefixed by @alias\/@, for example
    -- @alias\/aws\/secretsmanager@. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/alias-about.html About aliases>.
    --
    -- If you set this to an empty string, Secrets Manager uses the Amazon Web
    -- Services managed key @aws\/secretsmanager@. If this key doesn\'t already
    -- exist in your account, then Secrets Manager creates it for you
    -- automatically. All users and roles in the Amazon Web Services account
    -- automatically have access to use @aws\/secretsmanager@. Creating
    -- @aws\/secretsmanager@ can result in a one-time significant delay in
    -- returning the result.
    --
    -- You can only use the Amazon Web Services managed key
    -- @aws\/secretsmanager@ if you call this operation using credentials from
    -- the same Amazon Web Services account that owns the secret. If the secret
    -- is in a different account, then you must use a customer managed key and
    -- provide the ARN of that KMS key in this field. The user making the call
    -- must have permissions to both the secret and the KMS key in their
    -- respective accounts.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The binary data to encrypt and store in the new version of the secret.
    -- We recommend that you store your binary data in a file and then pass the
    -- contents of the file as a parameter.
    --
    -- Either @SecretBinary@ or @SecretString@ must have a value, but not both.
    --
    -- You can\'t access this parameter in the Secrets Manager console.
    secretBinary :: Prelude.Maybe (Data.Sensitive Data.Base64),
    -- | The text data to encrypt and store in the new version of the secret. We
    -- recommend you use a JSON structure of key\/value pairs for your secret
    -- value.
    --
    -- Either @SecretBinary@ or @SecretString@ must have a value, but not both.
    secretString :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN or name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'updateSecret_clientRequestToken' - If you include @SecretString@ or @SecretBinary@, then Secrets Manager
-- creates a new version for the secret, and this parameter specifies the
-- unique identifier for the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDKs to call this operation, then you can leave this parameter empty.
-- The CLI or SDK generates a random UUID for you and includes it as the
-- value for this parameter in the request. If you don\'t use the SDK and
-- instead generate a raw HTTP request to the Secrets Manager service
-- endpoint, then you must generate a @ClientRequestToken@ yourself for the
-- new version and include the value in the request.
--
-- This value becomes the @VersionId@ of the new version.
--
-- 'description', 'updateSecret_description' - The description of the secret.
--
-- 'kmsKeyId', 'updateSecret_kmsKeyId' - The ARN, key ID, or alias of the KMS key that Secrets Manager uses to
-- encrypt new secret versions as well as any existing versions with the
-- staging labels @AWSCURRENT@, @AWSPENDING@, or @AWSPREVIOUS@. For more
-- information about versions and staging labels, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/getting-started.html#term_version Concepts: Version>.
--
-- A key alias is always prefixed by @alias\/@, for example
-- @alias\/aws\/secretsmanager@. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/alias-about.html About aliases>.
--
-- If you set this to an empty string, Secrets Manager uses the Amazon Web
-- Services managed key @aws\/secretsmanager@. If this key doesn\'t already
-- exist in your account, then Secrets Manager creates it for you
-- automatically. All users and roles in the Amazon Web Services account
-- automatically have access to use @aws\/secretsmanager@. Creating
-- @aws\/secretsmanager@ can result in a one-time significant delay in
-- returning the result.
--
-- You can only use the Amazon Web Services managed key
-- @aws\/secretsmanager@ if you call this operation using credentials from
-- the same Amazon Web Services account that owns the secret. If the secret
-- is in a different account, then you must use a customer managed key and
-- provide the ARN of that KMS key in this field. The user making the call
-- must have permissions to both the secret and the KMS key in their
-- respective accounts.
--
-- 'secretBinary', 'updateSecret_secretBinary' - The binary data to encrypt and store in the new version of the secret.
-- We recommend that you store your binary data in a file and then pass the
-- contents of the file as a parameter.
--
-- Either @SecretBinary@ or @SecretString@ must have a value, but not both.
--
-- You can\'t access this parameter in the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'secretString', 'updateSecret_secretString' - The text data to encrypt and store in the new version of the secret. We
-- recommend you use a JSON structure of key\/value pairs for your secret
-- value.
--
-- Either @SecretBinary@ or @SecretString@ must have a value, but not both.
--
-- 'secretId', 'updateSecret_secretId' - The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newUpdateSecret ::
  -- | 'secretId'
  Prelude.Text ->
  UpdateSecret
newUpdateSecret pSecretId_ =
  UpdateSecret'
    { clientRequestToken = Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      secretBinary = Prelude.Nothing,
      secretString = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | If you include @SecretString@ or @SecretBinary@, then Secrets Manager
-- creates a new version for the secret, and this parameter specifies the
-- unique identifier for the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDKs to call this operation, then you can leave this parameter empty.
-- The CLI or SDK generates a random UUID for you and includes it as the
-- value for this parameter in the request. If you don\'t use the SDK and
-- instead generate a raw HTTP request to the Secrets Manager service
-- endpoint, then you must generate a @ClientRequestToken@ yourself for the
-- new version and include the value in the request.
--
-- This value becomes the @VersionId@ of the new version.
updateSecret_clientRequestToken :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_clientRequestToken = Lens.lens (\UpdateSecret' {clientRequestToken} -> clientRequestToken) (\s@UpdateSecret' {} a -> s {clientRequestToken = a} :: UpdateSecret)

-- | The description of the secret.
updateSecret_description :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_description = Lens.lens (\UpdateSecret' {description} -> description) (\s@UpdateSecret' {} a -> s {description = a} :: UpdateSecret)

-- | The ARN, key ID, or alias of the KMS key that Secrets Manager uses to
-- encrypt new secret versions as well as any existing versions with the
-- staging labels @AWSCURRENT@, @AWSPENDING@, or @AWSPREVIOUS@. For more
-- information about versions and staging labels, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/getting-started.html#term_version Concepts: Version>.
--
-- A key alias is always prefixed by @alias\/@, for example
-- @alias\/aws\/secretsmanager@. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/alias-about.html About aliases>.
--
-- If you set this to an empty string, Secrets Manager uses the Amazon Web
-- Services managed key @aws\/secretsmanager@. If this key doesn\'t already
-- exist in your account, then Secrets Manager creates it for you
-- automatically. All users and roles in the Amazon Web Services account
-- automatically have access to use @aws\/secretsmanager@. Creating
-- @aws\/secretsmanager@ can result in a one-time significant delay in
-- returning the result.
--
-- You can only use the Amazon Web Services managed key
-- @aws\/secretsmanager@ if you call this operation using credentials from
-- the same Amazon Web Services account that owns the secret. If the secret
-- is in a different account, then you must use a customer managed key and
-- provide the ARN of that KMS key in this field. The user making the call
-- must have permissions to both the secret and the KMS key in their
-- respective accounts.
updateSecret_kmsKeyId :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_kmsKeyId = Lens.lens (\UpdateSecret' {kmsKeyId} -> kmsKeyId) (\s@UpdateSecret' {} a -> s {kmsKeyId = a} :: UpdateSecret)

-- | The binary data to encrypt and store in the new version of the secret.
-- We recommend that you store your binary data in a file and then pass the
-- contents of the file as a parameter.
--
-- Either @SecretBinary@ or @SecretString@ must have a value, but not both.
--
-- You can\'t access this parameter in the Secrets Manager console.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateSecret_secretBinary :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.ByteString)
updateSecret_secretBinary = Lens.lens (\UpdateSecret' {secretBinary} -> secretBinary) (\s@UpdateSecret' {} a -> s {secretBinary = a} :: UpdateSecret) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Data._Base64)

-- | The text data to encrypt and store in the new version of the secret. We
-- recommend you use a JSON structure of key\/value pairs for your secret
-- value.
--
-- Either @SecretBinary@ or @SecretString@ must have a value, but not both.
updateSecret_secretString :: Lens.Lens' UpdateSecret (Prelude.Maybe Prelude.Text)
updateSecret_secretString = Lens.lens (\UpdateSecret' {secretString} -> secretString) (\s@UpdateSecret' {} a -> s {secretString = a} :: UpdateSecret) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
updateSecret_secretId :: Lens.Lens' UpdateSecret Prelude.Text
updateSecret_secretId = Lens.lens (\UpdateSecret' {secretId} -> secretId) (\s@UpdateSecret' {} a -> s {secretId = a} :: UpdateSecret)

instance Core.AWSRequest UpdateSecret where
  type AWSResponse UpdateSecret = UpdateSecretResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecretResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecret where
  hashWithSalt _salt UpdateSecret' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` secretBinary
      `Prelude.hashWithSalt` secretString
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData UpdateSecret where
  rnf UpdateSecret' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf kmsKeyId `Prelude.seq`
          Prelude.rnf secretBinary `Prelude.seq`
            Prelude.rnf secretString `Prelude.seq`
              Prelude.rnf secretId

instance Data.ToHeaders UpdateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.UpdateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSecret where
  toJSON UpdateSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("SecretBinary" Data..=) Prelude.<$> secretBinary,
            ("SecretString" Data..=) Prelude.<$> secretString,
            Prelude.Just ("SecretId" Data..= secretId)
          ]
      )

instance Data.ToPath UpdateSecret where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecretResponse' smart constructor.
data UpdateSecretResponse = UpdateSecretResponse'
  { -- | The ARN of the secret that was updated.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the secret that was updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | If Secrets Manager created a new version of the secret during this
    -- operation, then @VersionId@ contains the unique identifier of the new
    -- version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateSecretResponse_arn' - The ARN of the secret that was updated.
--
-- 'name', 'updateSecretResponse_name' - The name of the secret that was updated.
--
-- 'versionId', 'updateSecretResponse_versionId' - If Secrets Manager created a new version of the secret during this
-- operation, then @VersionId@ contains the unique identifier of the new
-- version.
--
-- 'httpStatus', 'updateSecretResponse_httpStatus' - The response's http status code.
newUpdateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecretResponse
newUpdateSecretResponse pHttpStatus_ =
  UpdateSecretResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret that was updated.
updateSecretResponse_arn :: Lens.Lens' UpdateSecretResponse (Prelude.Maybe Prelude.Text)
updateSecretResponse_arn = Lens.lens (\UpdateSecretResponse' {arn} -> arn) (\s@UpdateSecretResponse' {} a -> s {arn = a} :: UpdateSecretResponse)

-- | The name of the secret that was updated.
updateSecretResponse_name :: Lens.Lens' UpdateSecretResponse (Prelude.Maybe Prelude.Text)
updateSecretResponse_name = Lens.lens (\UpdateSecretResponse' {name} -> name) (\s@UpdateSecretResponse' {} a -> s {name = a} :: UpdateSecretResponse)

-- | If Secrets Manager created a new version of the secret during this
-- operation, then @VersionId@ contains the unique identifier of the new
-- version.
updateSecretResponse_versionId :: Lens.Lens' UpdateSecretResponse (Prelude.Maybe Prelude.Text)
updateSecretResponse_versionId = Lens.lens (\UpdateSecretResponse' {versionId} -> versionId) (\s@UpdateSecretResponse' {} a -> s {versionId = a} :: UpdateSecretResponse)

-- | The response's http status code.
updateSecretResponse_httpStatus :: Lens.Lens' UpdateSecretResponse Prelude.Int
updateSecretResponse_httpStatus = Lens.lens (\UpdateSecretResponse' {httpStatus} -> httpStatus) (\s@UpdateSecretResponse' {} a -> s {httpStatus = a} :: UpdateSecretResponse)

instance Prelude.NFData UpdateSecretResponse where
  rnf UpdateSecretResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf versionId `Prelude.seq`
          Prelude.rnf httpStatus
