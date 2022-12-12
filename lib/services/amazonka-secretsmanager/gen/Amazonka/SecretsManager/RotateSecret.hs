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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures and starts the asynchronous process of rotating the secret.
-- For more information about rotation, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets.html Rotate secrets>.
--
-- If you include the configuration parameters, the operation sets the
-- values for the secret and then immediately starts a rotation. If you
-- don\'t include the configuration parameters, the operation starts a
-- rotation with the values already stored in the secret.
--
-- For database credentials you want to rotate, for Secrets Manager to be
-- able to rotate the secret, you must make sure the secret value is in the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_secret_json_structure.html JSON structure of a database secret>.
-- In particular, if you want to use the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets_strategies.html#rotating-secrets-two-users alternating users strategy>,
-- your secret must contain the ARN of a superuser secret.
--
-- To configure rotation, you also need the ARN of an Amazon Web Services
-- Lambda function and the schedule for the rotation. The Lambda rotation
-- function creates a new version of the secret and creates or updates the
-- credentials on the database or service to match. After testing the new
-- credentials, the function marks the new secret version with the staging
-- label @AWSCURRENT@. Then anyone who retrieves the secret gets the new
-- version. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html How rotation works>.
--
-- You can create the Lambda rotation function based on the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_available-rotation-templates.html rotation function templates>
-- that Secrets Manager provides. Choose a template that matches your
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets_strategies.html Rotation strategy>.
--
-- When rotation is successful, the @AWSPENDING@ staging label might be
-- attached to the same version as the @AWSCURRENT@ version, or it might
-- not be attached to any version. If the @AWSPENDING@ staging label is
-- present but not attached to the same version as @AWSCURRENT@, then any
-- later invocation of @RotateSecret@ assumes that a previous rotation
-- request is still in progress and returns an error.
--
-- When rotation is unsuccessful, the @AWSPENDING@ staging label might be
-- attached to an empty secret version. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot_rotation.html Troubleshoot rotation>
-- in the /Secrets Manager User Guide/.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:RotateSecret@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
-- You also need @lambda:InvokeFunction@ permissions on the rotation
-- function. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotating-secrets-required-permissions-function.html Permissions for rotation>.
module Amazonka.SecretsManager.RotateSecret
  ( -- * Creating a Request
    RotateSecret (..),
    newRotateSecret,

    -- * Request Lenses
    rotateSecret_clientRequestToken,
    rotateSecret_rotateImmediately,
    rotateSecret_rotationLambdaARN,
    rotateSecret_rotationRules,
    rotateSecret_secretId,

    -- * Destructuring the Response
    RotateSecretResponse (..),
    newRotateSecretResponse,

    -- * Response Lenses
    rotateSecretResponse_arn,
    rotateSecretResponse_name,
    rotateSecretResponse_versionId,
    rotateSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newRotateSecret' smart constructor.
data RotateSecret = RotateSecret'
  { -- | A unique identifier for the new version of the secret that helps ensure
    -- idempotency. Secrets Manager uses this value to prevent the accidental
    -- creation of duplicate versions if there are failures and retries during
    -- rotation. This value becomes the @VersionId@ of the new version.
    --
    -- If you use the Amazon Web Services CLI or one of the Amazon Web Services
    -- SDK to call this operation, then you can leave this parameter empty. The
    -- CLI or SDK generates a random UUID for you and includes that in the
    -- request for this parameter. If you don\'t use the SDK and instead
    -- generate a raw HTTP request to the Secrets Manager service endpoint,
    -- then you must generate a @ClientRequestToken@ yourself for new versions
    -- and include that value in the request.
    --
    -- You only need to specify this value if you implement your own retry
    -- logic and you want to ensure that Secrets Manager doesn\'t attempt to
    -- create a secret version twice. We recommend that you generate a
    -- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
    -- value to ensure uniqueness within the specified secret.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to rotate the secret immediately or wait until the
    -- next scheduled rotation window. The rotation schedule is defined in
    -- RotateSecretRequest$RotationRules.
    --
    -- If you don\'t immediately rotate the secret, Secrets Manager tests the
    -- rotation configuration by running the
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html testSecret step>
    -- of the Lambda rotation function. The test creates an @AWSPENDING@
    -- version of the secret and then removes it.
    --
    -- If you don\'t specify this value, then by default, Secrets Manager
    -- rotates the secret immediately.
    rotateImmediately :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the Lambda rotation function that can rotate the secret.
    rotationLambdaARN :: Prelude.Maybe Prelude.Text,
    -- | A structure that defines the rotation configuration for this secret.
    rotationRules :: Prelude.Maybe RotationRulesType,
    -- | The ARN or name of the secret to rotate.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
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
-- 'clientRequestToken', 'rotateSecret_clientRequestToken' - A unique identifier for the new version of the secret that helps ensure
-- idempotency. Secrets Manager uses this value to prevent the accidental
-- creation of duplicate versions if there are failures and retries during
-- rotation. This value becomes the @VersionId@ of the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes that in the
-- request for this parameter. If you don\'t use the SDK and instead
-- generate a raw HTTP request to the Secrets Manager service endpoint,
-- then you must generate a @ClientRequestToken@ yourself for new versions
-- and include that value in the request.
--
-- You only need to specify this value if you implement your own retry
-- logic and you want to ensure that Secrets Manager doesn\'t attempt to
-- create a secret version twice. We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
--
-- 'rotateImmediately', 'rotateSecret_rotateImmediately' - Specifies whether to rotate the secret immediately or wait until the
-- next scheduled rotation window. The rotation schedule is defined in
-- RotateSecretRequest$RotationRules.
--
-- If you don\'t immediately rotate the secret, Secrets Manager tests the
-- rotation configuration by running the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html testSecret step>
-- of the Lambda rotation function. The test creates an @AWSPENDING@
-- version of the secret and then removes it.
--
-- If you don\'t specify this value, then by default, Secrets Manager
-- rotates the secret immediately.
--
-- 'rotationLambdaARN', 'rotateSecret_rotationLambdaARN' - The ARN of the Lambda rotation function that can rotate the secret.
--
-- 'rotationRules', 'rotateSecret_rotationRules' - A structure that defines the rotation configuration for this secret.
--
-- 'secretId', 'rotateSecret_secretId' - The ARN or name of the secret to rotate.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newRotateSecret ::
  -- | 'secretId'
  Prelude.Text ->
  RotateSecret
newRotateSecret pSecretId_ =
  RotateSecret'
    { clientRequestToken = Prelude.Nothing,
      rotateImmediately = Prelude.Nothing,
      rotationLambdaARN = Prelude.Nothing,
      rotationRules = Prelude.Nothing,
      secretId = pSecretId_
    }

-- | A unique identifier for the new version of the secret that helps ensure
-- idempotency. Secrets Manager uses this value to prevent the accidental
-- creation of duplicate versions if there are failures and retries during
-- rotation. This value becomes the @VersionId@ of the new version.
--
-- If you use the Amazon Web Services CLI or one of the Amazon Web Services
-- SDK to call this operation, then you can leave this parameter empty. The
-- CLI or SDK generates a random UUID for you and includes that in the
-- request for this parameter. If you don\'t use the SDK and instead
-- generate a raw HTTP request to the Secrets Manager service endpoint,
-- then you must generate a @ClientRequestToken@ yourself for new versions
-- and include that value in the request.
--
-- You only need to specify this value if you implement your own retry
-- logic and you want to ensure that Secrets Manager doesn\'t attempt to
-- create a secret version twice. We recommend that you generate a
-- <https://wikipedia.org/wiki/Universally_unique_identifier UUID-type>
-- value to ensure uniqueness within the specified secret.
rotateSecret_clientRequestToken :: Lens.Lens' RotateSecret (Prelude.Maybe Prelude.Text)
rotateSecret_clientRequestToken = Lens.lens (\RotateSecret' {clientRequestToken} -> clientRequestToken) (\s@RotateSecret' {} a -> s {clientRequestToken = a} :: RotateSecret)

-- | Specifies whether to rotate the secret immediately or wait until the
-- next scheduled rotation window. The rotation schedule is defined in
-- RotateSecretRequest$RotationRules.
--
-- If you don\'t immediately rotate the secret, Secrets Manager tests the
-- rotation configuration by running the
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html testSecret step>
-- of the Lambda rotation function. The test creates an @AWSPENDING@
-- version of the secret and then removes it.
--
-- If you don\'t specify this value, then by default, Secrets Manager
-- rotates the secret immediately.
rotateSecret_rotateImmediately :: Lens.Lens' RotateSecret (Prelude.Maybe Prelude.Bool)
rotateSecret_rotateImmediately = Lens.lens (\RotateSecret' {rotateImmediately} -> rotateImmediately) (\s@RotateSecret' {} a -> s {rotateImmediately = a} :: RotateSecret)

-- | The ARN of the Lambda rotation function that can rotate the secret.
rotateSecret_rotationLambdaARN :: Lens.Lens' RotateSecret (Prelude.Maybe Prelude.Text)
rotateSecret_rotationLambdaARN = Lens.lens (\RotateSecret' {rotationLambdaARN} -> rotationLambdaARN) (\s@RotateSecret' {} a -> s {rotationLambdaARN = a} :: RotateSecret)

-- | A structure that defines the rotation configuration for this secret.
rotateSecret_rotationRules :: Lens.Lens' RotateSecret (Prelude.Maybe RotationRulesType)
rotateSecret_rotationRules = Lens.lens (\RotateSecret' {rotationRules} -> rotationRules) (\s@RotateSecret' {} a -> s {rotationRules = a} :: RotateSecret)

-- | The ARN or name of the secret to rotate.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
rotateSecret_secretId :: Lens.Lens' RotateSecret Prelude.Text
rotateSecret_secretId = Lens.lens (\RotateSecret' {secretId} -> secretId) (\s@RotateSecret' {} a -> s {secretId = a} :: RotateSecret)

instance Core.AWSRequest RotateSecret where
  type AWSResponse RotateSecret = RotateSecretResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RotateSecretResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RotateSecret where
  hashWithSalt _salt RotateSecret' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` rotateImmediately
      `Prelude.hashWithSalt` rotationLambdaARN
      `Prelude.hashWithSalt` rotationRules
      `Prelude.hashWithSalt` secretId

instance Prelude.NFData RotateSecret where
  rnf RotateSecret' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf rotateImmediately
      `Prelude.seq` Prelude.rnf rotationLambdaARN
      `Prelude.seq` Prelude.rnf rotationRules
      `Prelude.seq` Prelude.rnf secretId

instance Data.ToHeaders RotateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.RotateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RotateSecret where
  toJSON RotateSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("RotateImmediately" Data..=)
              Prelude.<$> rotateImmediately,
            ("RotationLambdaARN" Data..=)
              Prelude.<$> rotationLambdaARN,
            ("RotationRules" Data..=) Prelude.<$> rotationRules,
            Prelude.Just ("SecretId" Data..= secretId)
          ]
      )

instance Data.ToPath RotateSecret where
  toPath = Prelude.const "/"

instance Data.ToQuery RotateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRotateSecretResponse' smart constructor.
data RotateSecretResponse = RotateSecretResponse'
  { -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the new version of the secret.
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
-- 'arn', 'rotateSecretResponse_arn' - The ARN of the secret.
--
-- 'name', 'rotateSecretResponse_name' - The name of the secret.
--
-- 'versionId', 'rotateSecretResponse_versionId' - The ID of the new version of the secret.
--
-- 'httpStatus', 'rotateSecretResponse_httpStatus' - The response's http status code.
newRotateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RotateSecretResponse
newRotateSecretResponse pHttpStatus_ =
  RotateSecretResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret.
rotateSecretResponse_arn :: Lens.Lens' RotateSecretResponse (Prelude.Maybe Prelude.Text)
rotateSecretResponse_arn = Lens.lens (\RotateSecretResponse' {arn} -> arn) (\s@RotateSecretResponse' {} a -> s {arn = a} :: RotateSecretResponse)

-- | The name of the secret.
rotateSecretResponse_name :: Lens.Lens' RotateSecretResponse (Prelude.Maybe Prelude.Text)
rotateSecretResponse_name = Lens.lens (\RotateSecretResponse' {name} -> name) (\s@RotateSecretResponse' {} a -> s {name = a} :: RotateSecretResponse)

-- | The ID of the new version of the secret.
rotateSecretResponse_versionId :: Lens.Lens' RotateSecretResponse (Prelude.Maybe Prelude.Text)
rotateSecretResponse_versionId = Lens.lens (\RotateSecretResponse' {versionId} -> versionId) (\s@RotateSecretResponse' {} a -> s {versionId = a} :: RotateSecretResponse)

-- | The response's http status code.
rotateSecretResponse_httpStatus :: Lens.Lens' RotateSecretResponse Prelude.Int
rotateSecretResponse_httpStatus = Lens.lens (\RotateSecretResponse' {httpStatus} -> httpStatus) (\s@RotateSecretResponse' {} a -> s {httpStatus = a} :: RotateSecretResponse)

instance Prelude.NFData RotateSecretResponse where
  rnf RotateSecretResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
