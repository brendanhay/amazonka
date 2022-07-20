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
-- Module      : Amazonka.SecretsManager.RotateSecret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures and starts the asynchronous process of rotating this secret.
-- If you include the configuration parameters, the operation sets those
-- values for the secret and then immediately starts a rotation. If you do
-- not include the configuration parameters, the operation starts a
-- rotation with the values already stored in the secret. After the
-- rotation completes, the protected service and its clients all use the
-- new version of the secret.
--
-- This required configuration information includes the ARN of an Amazon
-- Web Services Lambda function and optionally, the time between scheduled
-- rotations. The Lambda rotation function creates a new version of the
-- secret and creates or updates the credentials on the protected service
-- to match. After testing the new credentials, the function marks the new
-- secret with the staging label @AWSCURRENT@ so that your clients all
-- immediately begin to use the new version. For more information about
-- rotating secrets and how to configure a Lambda function to rotate the
-- secrets for your protected service, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets.html Rotating Secrets in Amazon Web Services Secrets Manager>
-- in the /Amazon Web Services Secrets Manager User Guide/.
--
-- Secrets Manager schedules the next rotation when the previous one
-- completes. Secrets Manager schedules the date by adding the rotation
-- interval (number of days) to the actual date of the last rotation. The
-- service chooses the hour within that 24-hour date window randomly. The
-- minute is also chosen somewhat randomly, but weighted towards the top of
-- the hour and influenced by a variety of factors that help distribute
-- load.
--
-- The rotation function must end with the versions of the secret in one of
-- two states:
--
-- -   The @AWSPENDING@ and @AWSCURRENT@ staging labels are attached to the
--     same version of the secret, or
--
-- -   The @AWSPENDING@ staging label is not attached to any version of the
--     secret.
--
-- If the @AWSPENDING@ staging label is present but not attached to the
-- same version as @AWSCURRENT@ then any later invocation of @RotateSecret@
-- assumes that a previous rotation request is still in progress and
-- returns an error.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:RotateSecret
--
-- -   lambda:InvokeFunction (on the function specified in the secret\'s
--     metadata)
--
-- __Related operations__
--
-- -   To list the secrets in your account, use ListSecrets.
--
-- -   To get the details for a version of a secret, use DescribeSecret.
--
-- -   To create a new version of a secret, use CreateSecret.
--
-- -   To attach staging labels to or remove staging labels from a version
--     of a secret, use UpdateSecretVersionStage.
module Amazonka.SecretsManager.RotateSecret
  ( -- * Creating a Request
    RotateSecret (..),
    newRotateSecret,

    -- * Request Lenses
    rotateSecret_rotationLambdaARN,
    rotateSecret_clientRequestToken,
    rotateSecret_rotationRules,
    rotateSecret_secretId,

    -- * Destructuring the Response
    RotateSecretResponse (..),
    newRotateSecretResponse,

    -- * Response Lenses
    rotateSecretResponse_name,
    rotateSecretResponse_arn,
    rotateSecretResponse_versionId,
    rotateSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newRotateSecret' smart constructor.
data RotateSecret = RotateSecret'
  { -- | (Optional) Specifies the ARN of the Lambda function that can rotate the
    -- secret.
    rotationLambdaARN :: Prelude.Maybe Prelude.Text,
    -- | (Optional) Specifies a unique identifier for the new version of the
    -- secret that helps ensure idempotency.
    --
    -- If you use the Amazon Web Services CLI or one of the Amazon Web Services
    -- SDK to call this operation, then you can leave this parameter empty. The
    -- CLI or SDK generates a random UUID for you and includes that in the
    -- request for this parameter. If you don\'t use the SDK and instead
    -- generate a raw HTTP request to the Secrets Manager service endpoint,
    -- then you must generate a @ClientRequestToken@ yourself for new versions
    -- and include that value in the request.
    --
    -- You only need to specify your own value if you implement your own retry
    -- logic and want to ensure that a given secret is not created twice. We
    -- recommend that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value to ensure uniqueness within the specified secret.
    --
    -- Secrets Manager uses this value to prevent the accidental creation of
    -- duplicate versions if there are failures and retries during the
    -- function\'s processing. This value becomes the @VersionId@ of the new
    -- version.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A structure that defines the rotation configuration for this secret.
    rotationRules :: Prelude.Maybe RotationRulesType,
    -- | Specifies the secret that you want to rotate. You can specify either the
    -- Amazon Resource Name (ARN) or the friendly name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotateSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotationLambdaARN', 'rotateSecret_rotationLambdaARN' - (Optional) Specifies the ARN of the Lambda function that can rotate the
-- secret.
--
-- 'clientRequestToken', 'rotateSecret_clientRequestToken' - (Optional) Specifies a unique identifier for the new version of the
-- secret that helps ensure idempotency.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes that in the
-- request for this parameter. If you don\'t use the SDK and instead
-- generate a raw HTTP request to the Secrets Manager service endpoint,
-- then you must generate a @ClientRequestToken@ yourself for new versions
-- and include that value in the request.
--
-- You only need to specify your own value if you implement your own retry
-- logic and want to ensure that a given secret is not created twice. We
-- recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- Secrets Manager uses this value to prevent the accidental creation of
-- duplicate versions if there are failures and retries during the
-- function\'s processing. This value becomes the @VersionId@ of the new
-- version.
--
-- 'rotationRules', 'rotateSecret_rotationRules' - A structure that defines the rotation configuration for this secret.
--
-- 'secretId', 'rotateSecret_secretId' - Specifies the secret that you want to rotate. You can specify either the
-- Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
newRotateSecret ::
  -- | 'secretId'
  Prelude.Text ->
  RotateSecret
newRotateSecret pSecretId_ =
  RotateSecret'
    { rotationLambdaARN = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      rotationRules = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | (Optional) Specifies the ARN of the Lambda function that can rotate the
-- secret.
rotateSecret_rotationLambdaARN :: Lens.Lens' RotateSecret (Prelude.Maybe Prelude.Text)
rotateSecret_rotationLambdaARN = Lens.lens (\RotateSecret' {rotationLambdaARN} -> rotationLambdaARN) (\s@RotateSecret' {} a -> s {rotationLambdaARN = a} :: RotateSecret)

-- | (Optional) Specifies a unique identifier for the new version of the
-- secret that helps ensure idempotency.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes that in the
-- request for this parameter. If you don\'t use the SDK and instead
-- generate a raw HTTP request to the Secrets Manager service endpoint,
-- then you must generate a @ClientRequestToken@ yourself for new versions
-- and include that value in the request.
--
-- You only need to specify your own value if you implement your own retry
-- logic and want to ensure that a given secret is not created twice. We
-- recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- Secrets Manager uses this value to prevent the accidental creation of
-- duplicate versions if there are failures and retries during the
-- function\'s processing. This value becomes the @VersionId@ of the new
-- version.
rotateSecret_clientRequestToken :: Lens.Lens' RotateSecret (Prelude.Maybe Prelude.Text)
rotateSecret_clientRequestToken = Lens.lens (\RotateSecret' {clientRequestToken} -> clientRequestToken) (\s@RotateSecret' {} a -> s {clientRequestToken = a} :: RotateSecret)

-- | A structure that defines the rotation configuration for this secret.
rotateSecret_rotationRules :: Lens.Lens' RotateSecret (Prelude.Maybe RotationRulesType)
rotateSecret_rotationRules = Lens.lens (\RotateSecret' {rotationRules} -> rotationRules) (\s@RotateSecret' {} a -> s {rotationRules = a} :: RotateSecret)

-- | Specifies the secret that you want to rotate. You can specify either the
-- Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
rotateSecret_secretId :: Lens.Lens' RotateSecret Prelude.Text
rotateSecret_secretId = Lens.lens (\RotateSecret' {secretId} -> secretId) (\s@RotateSecret' {} a -> s {secretId = a} :: RotateSecret)

instance Core.AWSRequest RotateSecret where
  type AWSResponse RotateSecret = RotateSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RotateSecretResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RotateSecret where
  hashWithSalt _salt RotateSecret' {..} =
    _salt `Prelude.hashWithSalt` rotationLambdaARN
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` rotationRules
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData RotateSecret where
  rnf RotateSecret' {..} =
    Prelude.rnf rotationLambdaARN
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf rotationRules
      `Prelude.seq` Prelude.rnf secretId

instance Core.ToHeaders RotateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.RotateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RotateSecret where
  toJSON RotateSecret' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RotationLambdaARN" Core..=)
              Prelude.<$> rotationLambdaARN,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("RotationRules" Core..=) Prelude.<$> rotationRules,
            Prelude.Just ("SecretId" Core..= secretId)
          ]
      )

instance Core.ToPath RotateSecret where
  toPath = Prelude.const "/"

instance Core.ToQuery RotateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRotateSecretResponse' smart constructor.
data RotateSecretResponse = RotateSecretResponse'
  { -- | The friendly name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the new version of the secret created by the rotation started
    -- by this request.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RotateSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'rotateSecretResponse_name' - The friendly name of the secret.
--
-- 'arn', 'rotateSecretResponse_arn' - The ARN of the secret.
--
-- 'versionId', 'rotateSecretResponse_versionId' - The ID of the new version of the secret created by the rotation started
-- by this request.
--
-- 'httpStatus', 'rotateSecretResponse_httpStatus' - The response's http status code.
newRotateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RotateSecretResponse
newRotateSecretResponse pHttpStatus_ =
  RotateSecretResponse'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The friendly name of the secret.
rotateSecretResponse_name :: Lens.Lens' RotateSecretResponse (Prelude.Maybe Prelude.Text)
rotateSecretResponse_name = Lens.lens (\RotateSecretResponse' {name} -> name) (\s@RotateSecretResponse' {} a -> s {name = a} :: RotateSecretResponse)

-- | The ARN of the secret.
rotateSecretResponse_arn :: Lens.Lens' RotateSecretResponse (Prelude.Maybe Prelude.Text)
rotateSecretResponse_arn = Lens.lens (\RotateSecretResponse' {arn} -> arn) (\s@RotateSecretResponse' {} a -> s {arn = a} :: RotateSecretResponse)

-- | The ID of the new version of the secret created by the rotation started
-- by this request.
rotateSecretResponse_versionId :: Lens.Lens' RotateSecretResponse (Prelude.Maybe Prelude.Text)
rotateSecretResponse_versionId = Lens.lens (\RotateSecretResponse' {versionId} -> versionId) (\s@RotateSecretResponse' {} a -> s {versionId = a} :: RotateSecretResponse)

-- | The response's http status code.
rotateSecretResponse_httpStatus :: Lens.Lens' RotateSecretResponse Prelude.Int
rotateSecretResponse_httpStatus = Lens.lens (\RotateSecretResponse' {httpStatus} -> httpStatus) (\s@RotateSecretResponse' {} a -> s {httpStatus = a} :: RotateSecretResponse)

instance Prelude.NFData RotateSecretResponse where
  rnf RotateSecretResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
