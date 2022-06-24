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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the contents of the encrypted fields @SecretString@ or
-- @SecretBinary@ from the specified version of a secret, whichever
-- contains content.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:GetSecretValue
--
-- -   kms:Decrypt - required only if you use a customer-managed Amazon Web
--     Services KMS key to encrypt the secret. You do not need this
--     permission to use the account\'s default Amazon Web Services managed
--     CMK for Secrets Manager.
--
-- __Related operations__
--
-- -   To create a new version of the secret with different encrypted
--     information, use PutSecretValue.
--
-- -   To retrieve the non-encrypted details for the secret, use
--     DescribeSecret.
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newGetSecretValue' smart constructor.
data GetSecretValue = GetSecretValue'
  { -- | Specifies the secret version that you want to retrieve by the staging
    -- label attached to the version.
    --
    -- Staging labels are used to keep track of different versions during the
    -- rotation process. If you specify both this parameter and @VersionId@,
    -- the two parameters must refer to the same secret version . If you don\'t
    -- specify either a @VersionStage@ or @VersionId@, then the default is to
    -- perform the operation on the version with the @VersionStage@ value of
    -- @AWSCURRENT@.
    versionStage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the unique identifier of the version of the secret that you
    -- want to retrieve. If you specify both this parameter and @VersionStage@,
    -- the two parameters must refer to the same secret version. If you don\'t
    -- specify either a @VersionStage@ or @VersionId@ then the default is to
    -- perform the operation on the version with the @VersionStage@ value of
    -- @AWSCURRENT@.
    --
    -- This value is typically a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value with 32 hexadecimal digits.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the secret containing the version that you want to retrieve.
    -- You can specify either the Amazon Resource Name (ARN) or the friendly
    -- name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
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
-- 'versionStage', 'getSecretValue_versionStage' - Specifies the secret version that you want to retrieve by the staging
-- label attached to the version.
--
-- Staging labels are used to keep track of different versions during the
-- rotation process. If you specify both this parameter and @VersionId@,
-- the two parameters must refer to the same secret version . If you don\'t
-- specify either a @VersionStage@ or @VersionId@, then the default is to
-- perform the operation on the version with the @VersionStage@ value of
-- @AWSCURRENT@.
--
-- 'versionId', 'getSecretValue_versionId' - Specifies the unique identifier of the version of the secret that you
-- want to retrieve. If you specify both this parameter and @VersionStage@,
-- the two parameters must refer to the same secret version. If you don\'t
-- specify either a @VersionStage@ or @VersionId@ then the default is to
-- perform the operation on the version with the @VersionStage@ value of
-- @AWSCURRENT@.
--
-- This value is typically a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value with 32 hexadecimal digits.
--
-- 'secretId', 'getSecretValue_secretId' - Specifies the secret containing the version that you want to retrieve.
-- You can specify either the Amazon Resource Name (ARN) or the friendly
-- name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
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

-- | Specifies the secret version that you want to retrieve by the staging
-- label attached to the version.
--
-- Staging labels are used to keep track of different versions during the
-- rotation process. If you specify both this parameter and @VersionId@,
-- the two parameters must refer to the same secret version . If you don\'t
-- specify either a @VersionStage@ or @VersionId@, then the default is to
-- perform the operation on the version with the @VersionStage@ value of
-- @AWSCURRENT@.
getSecretValue_versionStage :: Lens.Lens' GetSecretValue (Prelude.Maybe Prelude.Text)
getSecretValue_versionStage = Lens.lens (\GetSecretValue' {versionStage} -> versionStage) (\s@GetSecretValue' {} a -> s {versionStage = a} :: GetSecretValue)

-- | Specifies the unique identifier of the version of the secret that you
-- want to retrieve. If you specify both this parameter and @VersionStage@,
-- the two parameters must refer to the same secret version. If you don\'t
-- specify either a @VersionStage@ or @VersionId@ then the default is to
-- perform the operation on the version with the @VersionStage@ value of
-- @AWSCURRENT@.
--
-- This value is typically a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value with 32 hexadecimal digits.
getSecretValue_versionId :: Lens.Lens' GetSecretValue (Prelude.Maybe Prelude.Text)
getSecretValue_versionId = Lens.lens (\GetSecretValue' {versionId} -> versionId) (\s@GetSecretValue' {} a -> s {versionId = a} :: GetSecretValue)

-- | Specifies the secret containing the version that you want to retrieve.
-- You can specify either the Amazon Resource Name (ARN) or the friendly
-- name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
getSecretValue_secretId :: Lens.Lens' GetSecretValue Prelude.Text
getSecretValue_secretId = Lens.lens (\GetSecretValue' {secretId} -> secretId) (\s@GetSecretValue' {} a -> s {secretId = a} :: GetSecretValue)

instance Core.AWSRequest GetSecretValue where
  type
    AWSResponse GetSecretValue =
      GetSecretValueResponse
  request = Request.postJSON defaultService
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
    -- | The decrypted part of the protected secret information that was
    -- originally provided as binary data in the form of a byte array. The
    -- response parameter represents the binary data as a
    -- <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
    --
    -- This parameter is not used if the secret is created by the Secrets
    -- Manager console.
    --
    -- If you store custom information in this field of the secret, then you
    -- must code your Lambda rotation function to parse and interpret whatever
    -- you store in the @SecretString@ or @SecretBinary@ fields.
    secretBinary :: Prelude.Maybe (Core.Sensitive Core.Base64),
    -- | The decrypted part of the protected secret information that was
    -- originally provided as a string.
    --
    -- If you create this secret by using the Secrets Manager console then only
    -- the @SecretString@ parameter contains data. Secrets Manager stores the
    -- information as a JSON structure of key\/value pairs that the Lambda
    -- rotation function knows how to parse.
    --
    -- If you store custom information in the secret by using the CreateSecret,
    -- UpdateSecret, or PutSecretValue API operations instead of the Secrets
    -- Manager console, or by using the __Other secret type__ in the console,
    -- then you must code your Lambda rotation function to parse and interpret
    -- those values.
    secretString :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The date and time that this version of the secret was created.
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
-- 'secretBinary', 'getSecretValueResponse_secretBinary' - The decrypted part of the protected secret information that was
-- originally provided as binary data in the form of a byte array. The
-- response parameter represents the binary data as a
-- <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- This parameter is not used if the secret is created by the Secrets
-- Manager console.
--
-- If you store custom information in this field of the secret, then you
-- must code your Lambda rotation function to parse and interpret whatever
-- you store in the @SecretString@ or @SecretBinary@ fields.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'secretString', 'getSecretValueResponse_secretString' - The decrypted part of the protected secret information that was
-- originally provided as a string.
--
-- If you create this secret by using the Secrets Manager console then only
-- the @SecretString@ parameter contains data. Secrets Manager stores the
-- information as a JSON structure of key\/value pairs that the Lambda
-- rotation function knows how to parse.
--
-- If you store custom information in the secret by using the CreateSecret,
-- UpdateSecret, or PutSecretValue API operations instead of the Secrets
-- Manager console, or by using the __Other secret type__ in the console,
-- then you must code your Lambda rotation function to parse and interpret
-- those values.
--
-- 'createdDate', 'getSecretValueResponse_createdDate' - The date and time that this version of the secret was created.
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

-- | The decrypted part of the protected secret information that was
-- originally provided as binary data in the form of a byte array. The
-- response parameter represents the binary data as a
-- <https://tools.ietf.org/html/rfc4648#section-4 base64-encoded> string.
--
-- This parameter is not used if the secret is created by the Secrets
-- Manager console.
--
-- If you store custom information in this field of the secret, then you
-- must code your Lambda rotation function to parse and interpret whatever
-- you store in the @SecretString@ or @SecretBinary@ fields.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getSecretValueResponse_secretBinary :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.ByteString)
getSecretValueResponse_secretBinary = Lens.lens (\GetSecretValueResponse' {secretBinary} -> secretBinary) (\s@GetSecretValueResponse' {} a -> s {secretBinary = a} :: GetSecretValueResponse) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Core._Base64)

-- | The decrypted part of the protected secret information that was
-- originally provided as a string.
--
-- If you create this secret by using the Secrets Manager console then only
-- the @SecretString@ parameter contains data. Secrets Manager stores the
-- information as a JSON structure of key\/value pairs that the Lambda
-- rotation function knows how to parse.
--
-- If you store custom information in the secret by using the CreateSecret,
-- UpdateSecret, or PutSecretValue API operations instead of the Secrets
-- Manager console, or by using the __Other secret type__ in the console,
-- then you must code your Lambda rotation function to parse and interpret
-- those values.
getSecretValueResponse_secretString :: Lens.Lens' GetSecretValueResponse (Prelude.Maybe Prelude.Text)
getSecretValueResponse_secretString = Lens.lens (\GetSecretValueResponse' {secretString} -> secretString) (\s@GetSecretValueResponse' {} a -> s {secretString = a} :: GetSecretValueResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The date and time that this version of the secret was created.
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
