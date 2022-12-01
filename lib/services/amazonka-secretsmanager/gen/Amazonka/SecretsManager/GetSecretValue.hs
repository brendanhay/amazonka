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
-- Module      : Amazonka.SecretsManager.GetSecretValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of the encrypted fields @SecretString@ or
-- @SecretBinary@ from the specified version of a secret, whichever
-- contains content.
--
-- We recommend that you cache your secret values by using client-side
-- caching. Caching secrets improves speed and reduces your costs. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieving-secrets.html Cache secrets for your applications>.
--
-- To retrieve the previous version of a secret, use @VersionStage@ and
-- specify AWSPREVIOUS. To revert to the previous version of a secret, call
-- <https://docs.aws.amazon.com/cli/latest/reference/secretsmanager/update-secret-version-stage.html UpdateSecretVersionStage>.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:GetSecretValue@. If the secret
-- is encrypted using a customer-managed key instead of the Amazon Web
-- Services managed key @aws\/secretsmanager@, then you also need
-- @kms:Decrypt@ permissions for that key. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.GetSecretValue
  ( -- * Creating a Request
    GetSecretValue (..),
    newGetSecretValue,

    -- * Request Lenses
    getSecretValue_versionStage,
    getSecretValue_versionId,
    getSecretValue_secretId,

    -- * Destructuring the Response
    GetSecretValueResponse (..),
    newGetSecretValueResponse,

    -- * Response Lenses
    getSecretValueResponse_versionStages,
    getSecretValueResponse_name,
    getSecretValueResponse_arn,
    getSecretValueResponse_secretBinary,
    getSecretValueResponse_secretString,
    getSecretValueResponse_createdDate,
    getSecretValueResponse_versionId,
    getSecretValueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newGetSecretValue' smart constructor.
data GetSecretValue = GetSecretValue'
  { -- | The staging label of the version of the secret to retrieve.
    --
    -- Secrets Manager uses staging labels to keep track of different versions
    -- during the rotation process. If you include both this parameter and
    -- @VersionId@, the two parameters must refer to the same secret version.
    -- If you don\'t specify either a @VersionStage@ or @VersionId@, Secrets
    -- Manager returns the @AWSCURRENT@ version.
    versionStage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the version of the secret to retrieve. If you
    -- include both this parameter and @VersionStage@, the two parameters must
    -- refer to the same secret version. If you don\'t specify either a
    -- @VersionStage@ or @VersionId@, then Secrets Manager returns the
    -- @AWSCURRENT@ version.
    --
    -- This value is typically a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value with 32 hexadecimal digits.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The ARN or name of the secret to retrieve.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecretValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionStage', 'getSecretValue_versionStage' - The staging label of the version of the secret to retrieve.
--
-- Secrets Manager uses staging labels to keep track of different versions
-- during the rotation process. If you include both this parameter and
-- @VersionId@, the two parameters must refer to the same secret version.
-- If you don\'t specify either a @VersionStage@ or @VersionId@, Secrets
-- Manager returns the @AWSCURRENT@ version.
--
-- 'versionId', 'getSecretValue_versionId' - The unique identifier of the version of the secret to retrieve. If you
-- include both this parameter and @VersionStage@, the two parameters must
-- refer to the same secret version. If you don\'t specify either a
-- @VersionStage@ or @VersionId@, then Secrets Manager returns the
-- @AWSCURRENT@ version.
--
-- This value is typically a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value with 32 hexadecimal digits.
--
-- 'secretId', 'getSecretValue_secretId' - The ARN or name of the secret to retrieve.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newGetSecretValue ::
  -- | 'secretId'
  Prelude.Text ->
  GetSecretValue
newGetSecretValue pSecretId_ =
  GetSecretValue'
    { versionStage = Prelude.Nothing,
      versionId = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | The staging label of the version of the secret to retrieve.
--
-- Secrets Manager uses staging labels to keep track of different versions
-- during the rotation process. If you include both this parameter and
-- @VersionId@, the two parameters must refer to the same secret version.
-- If you don\'t specify either a @VersionStage@ or @VersionId@, Secrets
-- Manager returns the @AWSCURRENT@ version.
getSecretValue_versionStage :: Lens.Lens' GetSecretValue (Prelude.Maybe Prelude.Text)
getSecretValue_versionStage = Lens.lens (\GetSecretValue' {versionStage} -> versionStage) (\s@GetSecretValue' {} a -> s {versionStage = a} :: GetSecretValue)

-- | The unique identifier of the version of the secret to retrieve. If you
-- include both this parameter and @VersionStage@, the two parameters must
-- refer to the same secret version. If you don\'t specify either a
-- @VersionStage@ or @VersionId@, then Secrets Manager returns the
-- @AWSCURRENT@ version.
--
-- This value is typically a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value with 32 hexadecimal digits.
getSecretValue_versionId :: Lens.Lens' GetSecretValue (Prelude.Maybe Prelude.Text)
getSecretValue_versionId = Lens.lens (\GetSecretValue' {versionId} -> versionId) (\s@GetSecretValue' {} a -> s {versionId = a} :: GetSecretValue)

-- | The ARN or name of the secret to retrieve.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
getSecretValue_secretId :: Lens.Lens' GetSecretValue Prelude.Text
getSecretValue_secretId = Lens.lens (\GetSecretValue' {secretId} -> secretId) (\s@GetSecretValue' {} a -> s {secretId = a} :: GetSecretValue)

instance Core.AWSRequest GetSecretValue where
  type
    AWSResponse GetSecretValue =
      GetSecretValueResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecretValueResponse'
            Prelude.<$> (x Core..?> "VersionStages")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "SecretBinary")
            Prelude.<*> (x Core..?> "SecretString")
            Prelude.<*> (x Core..?> "CreatedDate")
            Prelude.<*> (x Core..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSecretValue where
  hashWithSalt _salt GetSecretValue' {..} =
    _salt `Prelude.hashWithSalt` versionStage
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData GetSecretValue where
  rnf GetSecretValue' {..} =
    Prelude.rnf versionStage
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf secretId

instance Core.ToHeaders GetSecretValue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.GetSecretValue" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetSecretValue where
  toJSON GetSecretValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VersionStage" Core..=) Prelude.<$> versionStage,
            ("VersionId" Core..=) Prelude.<$> versionId,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath GetSecretValue where
  toPath = Prelude.const "/"

instance Core.ToQuery GetSecretValue where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSecretValueResponse' smart constructor.
data GetSecretValueResponse = GetSecretValueResponse'
  { -- | A list of all of the staging labels currently attached to this version
    -- of the secret.
    versionStages :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The friendly name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The decrypted secret value, if the secret value was originally provided
    -- as binary data in the form of a byte array. The response parameter
    -- represents the binary data as a
    -- <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
    --
    -- If the secret was created by using the Secrets Manager console, or if
    -- the secret value was originally provided as a string, then this field is
    -- omitted. The secret value appears in @SecretString@ instead.
    secretBinary :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | The decrypted secret value, if the secret value was originally provided
    -- as a string or through the Secrets Manager console.
    --
    -- If this secret was created by using the console, then Secrets Manager
    -- stores the information as a JSON structure of key\/value pairs.
    secretString :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The date and time that this version of the secret was created. If you
    -- don\'t specify which version in @VersionId@ or @VersionStage@, then
    -- Secrets Manager uses the @AWSCURRENT@ version.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of this version of the secret.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecretValueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionStages', 'getSecretValueResponse_versionStages' - A list of all of the staging labels currently attached to this version
-- of the secret.
--
-- 'name', 'getSecretValueResponse_name' - The friendly name of the secret.
--
-- 'arn', 'getSecretValueResponse_arn' - The ARN of the secret.
--
-- 'secretBinary', 'getSecretValueResponse_secretBinary' - The decrypted secret value, if the secret value was originally provided
-- as binary data in the form of a byte array. The response parameter
-- represents the binary data as a
-- <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- If the secret was created by using the Secrets Manager console, or if
-- the secret value was originally provided as a string, then this field is
-- omitted. The secret value appears in @SecretString@ instead.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'secretString', 'getSecretValueResponse_secretString' - The decrypted secret value, if the secret value was originally provided
-- as a string or through the Secrets Manager console.
--
-- If this secret was created by using the console, then Secrets Manager
-- stores the information as a JSON structure of key\/value pairs.
--
-- 'createdDate', 'getSecretValueResponse_createdDate' - The date and time that this version of the secret was created. If you
-- don\'t specify which version in @VersionId@ or @VersionStage@, then
-- Secrets Manager uses the @AWSCURRENT@ version.
--
-- 'versionId', 'getSecretValueResponse_versionId' - The unique identifier of this version of the secret.
--
-- 'httpStatus', 'getSecretValueResponse_httpStatus' - The response's http status code.
newGetSecretValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSecretValueResponse
newGetSecretValueResponse pHttpStatus_ =
  GetSecretValueResponse'
    { versionStages =
        Prelude.Nothing,
      name = Prelude.Nothing,
      arn = Prelude.Nothing,
      secretBinary = Prelude.Nothing,
      secretString = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all of the staging labels currently attached to this version
-- of the secret.
getSecretValueResponse_versionStages :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getSecretValueResponse_versionStages = Lens.lens (\GetSecretValueResponse' {versionStages} -> versionStages) (\s@GetSecretValueResponse' {} a -> s {versionStages = a} :: GetSecretValueResponse) Prelude.. Lens.mapping Lens.coerced

-- | The friendly name of the secret.
getSecretValueResponse_name :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.Text)
getSecretValueResponse_name = Lens.lens (\GetSecretValueResponse' {name} -> name) (\s@GetSecretValueResponse' {} a -> s {name = a} :: GetSecretValueResponse)

-- | The ARN of the secret.
getSecretValueResponse_arn :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.Text)
getSecretValueResponse_arn = Lens.lens (\GetSecretValueResponse' {arn} -> arn) (\s@GetSecretValueResponse' {} a -> s {arn = a} :: GetSecretValueResponse)

-- | The decrypted secret value, if the secret value was originally provided
-- as binary data in the form of a byte array. The response parameter
-- represents the binary data as a
-- <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- If the secret was created by using the Secrets Manager console, or if
-- the secret value was originally provided as a string, then this field is
-- omitted. The secret value appears in @SecretString@ instead.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getSecretValueResponse_secretBinary :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.ByteString)
getSecretValueResponse_secretBinary = Lens.lens (\GetSecretValueResponse' {secretBinary} -> secretBinary) (\s@GetSecretValueResponse' {} a -> s {secretBinary = a} :: GetSecretValueResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | The decrypted secret value, if the secret value was originally provided
-- as a string or through the Secrets Manager console.
--
-- If this secret was created by using the console, then Secrets Manager
-- stores the information as a JSON structure of key\/value pairs.
getSecretValueResponse_secretString :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.Text)
getSecretValueResponse_secretString = Lens.lens (\GetSecretValueResponse' {secretString} -> secretString) (\s@GetSecretValueResponse' {} a -> s {secretString = a} :: GetSecretValueResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The date and time that this version of the secret was created. If you
-- don\'t specify which version in @VersionId@ or @VersionStage@, then
-- Secrets Manager uses the @AWSCURRENT@ version.
getSecretValueResponse_createdDate :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.UTCTime)
getSecretValueResponse_createdDate = Lens.lens (\GetSecretValueResponse' {createdDate} -> createdDate) (\s@GetSecretValueResponse' {} a -> s {createdDate = a} :: GetSecretValueResponse) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of this version of the secret.
getSecretValueResponse_versionId :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.Text)
getSecretValueResponse_versionId = Lens.lens (\GetSecretValueResponse' {versionId} -> versionId) (\s@GetSecretValueResponse' {} a -> s {versionId = a} :: GetSecretValueResponse)

-- | The response's http status code.
getSecretValueResponse_httpStatus :: Lens.Lens' GetSecretValueResponse Prelude.Int
getSecretValueResponse_httpStatus = Lens.lens (\GetSecretValueResponse' {httpStatus} -> httpStatus) (\s@GetSecretValueResponse' {} a -> s {httpStatus = a} :: GetSecretValueResponse)

instance Prelude.NFData GetSecretValueResponse where
  rnf GetSecretValueResponse' {..} =
    Prelude.rnf versionStages
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf secretBinary
      `Prelude.seq` Prelude.rnf secretString
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
