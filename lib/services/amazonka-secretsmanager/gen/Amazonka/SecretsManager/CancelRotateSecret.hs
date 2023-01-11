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
-- Module      : Amazonka.SecretsManager.CancelRotateSecret
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Turns off automatic rotation, and if a rotation is currently in
-- progress, cancels the rotation.
--
-- If you cancel a rotation in progress, it can leave the @VersionStage@
-- labels in an unexpected state. You might need to remove the staging
-- label @AWSPENDING@ from the partially created version. You also need to
-- determine whether to roll back to the previous version of the secret by
-- moving the staging label @AWSCURRENT@ to the version that has
-- @AWSPENDING@. To determine which version has a specific staging label,
-- call ListSecretVersionIds. Then use UpdateSecretVersionStage to change
-- staging labels. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/rotate-secrets_how.html How rotation works>.
--
-- To turn on automatic rotation again, call RotateSecret.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:CancelRotateSecret@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.CancelRotateSecret
  ( -- * Creating a Request
    CancelRotateSecret (..),
    newCancelRotateSecret,

    -- * Request Lenses
    cancelRotateSecret_secretId,

    -- * Destructuring the Response
    CancelRotateSecretResponse (..),
    newCancelRotateSecretResponse,

    -- * Response Lenses
    cancelRotateSecretResponse_arn,
    cancelRotateSecretResponse_name,
    cancelRotateSecretResponse_versionId,
    cancelRotateSecretResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newCancelRotateSecret' smart constructor.
data CancelRotateSecret = CancelRotateSecret'
  { -- | The ARN or name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
    secretId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelRotateSecret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secretId', 'cancelRotateSecret_secretId' - The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
newCancelRotateSecret ::
  -- | 'secretId'
  Prelude.Text ->
  CancelRotateSecret
newCancelRotateSecret pSecretId_ =
  CancelRotateSecret' {secretId = pSecretId_}

-- | The ARN or name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
cancelRotateSecret_secretId :: Lens.Lens' CancelRotateSecret Prelude.Text
cancelRotateSecret_secretId = Lens.lens (\CancelRotateSecret' {secretId} -> secretId) (\s@CancelRotateSecret' {} a -> s {secretId = a} :: CancelRotateSecret)

instance Core.AWSRequest CancelRotateSecret where
  type
    AWSResponse CancelRotateSecret =
      CancelRotateSecretResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelRotateSecretResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "VersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelRotateSecret where
  hashWithSalt _salt CancelRotateSecret' {..} =
    _salt `Prelude.hashWithSalt` secretId

instance Prelude.NFData CancelRotateSecret where
  rnf CancelRotateSecret' {..} = Prelude.rnf secretId

instance Data.ToHeaders CancelRotateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.CancelRotateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelRotateSecret where
  toJSON CancelRotateSecret' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Data..= secretId)]
      )

instance Data.ToPath CancelRotateSecret where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelRotateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelRotateSecretResponse' smart constructor.
data CancelRotateSecretResponse = CancelRotateSecretResponse'
  { -- | The ARN of the secret.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the secret.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the version of the secret created during the
    -- rotation. This version might not be complete, and should be evaluated
    -- for possible deletion. We recommend that you remove the @VersionStage@
    -- value @AWSPENDING@ from this version so that Secrets Manager can delete
    -- it. Failing to clean up a cancelled rotation can block you from starting
    -- future rotations.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelRotateSecretResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'cancelRotateSecretResponse_arn' - The ARN of the secret.
--
-- 'name', 'cancelRotateSecretResponse_name' - The name of the secret.
--
-- 'versionId', 'cancelRotateSecretResponse_versionId' - The unique identifier of the version of the secret created during the
-- rotation. This version might not be complete, and should be evaluated
-- for possible deletion. We recommend that you remove the @VersionStage@
-- value @AWSPENDING@ from this version so that Secrets Manager can delete
-- it. Failing to clean up a cancelled rotation can block you from starting
-- future rotations.
--
-- 'httpStatus', 'cancelRotateSecretResponse_httpStatus' - The response's http status code.
newCancelRotateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelRotateSecretResponse
newCancelRotateSecretResponse pHttpStatus_ =
  CancelRotateSecretResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      versionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret.
cancelRotateSecretResponse_arn :: Lens.Lens' CancelRotateSecretResponse (Prelude.Maybe Prelude.Text)
cancelRotateSecretResponse_arn = Lens.lens (\CancelRotateSecretResponse' {arn} -> arn) (\s@CancelRotateSecretResponse' {} a -> s {arn = a} :: CancelRotateSecretResponse)

-- | The name of the secret.
cancelRotateSecretResponse_name :: Lens.Lens' CancelRotateSecretResponse (Prelude.Maybe Prelude.Text)
cancelRotateSecretResponse_name = Lens.lens (\CancelRotateSecretResponse' {name} -> name) (\s@CancelRotateSecretResponse' {} a -> s {name = a} :: CancelRotateSecretResponse)

-- | The unique identifier of the version of the secret created during the
-- rotation. This version might not be complete, and should be evaluated
-- for possible deletion. We recommend that you remove the @VersionStage@
-- value @AWSPENDING@ from this version so that Secrets Manager can delete
-- it. Failing to clean up a cancelled rotation can block you from starting
-- future rotations.
cancelRotateSecretResponse_versionId :: Lens.Lens' CancelRotateSecretResponse (Prelude.Maybe Prelude.Text)
cancelRotateSecretResponse_versionId = Lens.lens (\CancelRotateSecretResponse' {versionId} -> versionId) (\s@CancelRotateSecretResponse' {} a -> s {versionId = a} :: CancelRotateSecretResponse)

-- | The response's http status code.
cancelRotateSecretResponse_httpStatus :: Lens.Lens' CancelRotateSecretResponse Prelude.Int
cancelRotateSecretResponse_httpStatus = Lens.lens (\CancelRotateSecretResponse' {httpStatus} -> httpStatus) (\s@CancelRotateSecretResponse' {} a -> s {httpStatus = a} :: CancelRotateSecretResponse)

instance Prelude.NFData CancelRotateSecretResponse where
  rnf CancelRotateSecretResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
