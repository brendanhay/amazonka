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
-- Module      : Network.AWS.SecretsManager.CancelRotateSecret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables automatic scheduled rotation and cancels the rotation of a
-- secret if currently in progress.
--
-- To re-enable scheduled rotation, call RotateSecret with
-- @AutomaticallyRotateAfterDays@ set to a value greater than 0. This
-- immediately rotates your secret and then enables the automatic schedule.
--
-- If you cancel a rotation while in progress, it can leave the
-- @VersionStage@ labels in an unexpected state. Depending on the step of
-- the rotation in progress, you might need to remove the staging label
-- @AWSPENDING@ from the partially created version, specified by the
-- @VersionId@ response value. You should also evaluate the partially
-- rotated new version to see if it should be deleted, which you can do by
-- removing all staging labels from the new version @VersionStage@ field.
--
-- To successfully start a rotation, the staging label @AWSPENDING@ must be
-- in one of the following states:
--
-- -   Not attached to any version at all
--
-- -   Attached to the same version as the staging label @AWSCURRENT@
--
-- If the staging label @AWSPENDING@ attached to a different version than
-- the version with @AWSCURRENT@ then the attempt to rotate fails.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:CancelRotateSecret
--
-- __Related operations__
--
-- -   To configure rotation for a secret or to manually trigger a
--     rotation, use RotateSecret.
--
-- -   To get the rotation configuration details for a secret, use
--     DescribeSecret.
--
-- -   To list all of the currently available secrets, use ListSecrets.
--
-- -   To list all of the versions currently associated with a secret, use
--     ListSecretVersionIds.
module Network.AWS.SecretsManager.CancelRotateSecret
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
    cancelRotateSecretResponse_versionId,
    cancelRotateSecretResponse_name,
    cancelRotateSecretResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newCancelRotateSecret' smart constructor.
data CancelRotateSecret = CancelRotateSecret'
  { -- | Specifies the secret to cancel a rotation request. You can specify
    -- either the Amazon Resource Name (ARN) or the friendly name of the
    -- secret.
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
-- 'secretId', 'cancelRotateSecret_secretId' - Specifies the secret to cancel a rotation request. You can specify
-- either the Amazon Resource Name (ARN) or the friendly name of the
-- secret.
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
newCancelRotateSecret ::
  -- | 'secretId'
  Prelude.Text ->
  CancelRotateSecret
newCancelRotateSecret pSecretId_ =
  CancelRotateSecret' {secretId = pSecretId_}

-- | Specifies the secret to cancel a rotation request. You can specify
-- either the Amazon Resource Name (ARN) or the friendly name of the
-- secret.
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
cancelRotateSecret_secretId :: Lens.Lens' CancelRotateSecret Prelude.Text
cancelRotateSecret_secretId = Lens.lens (\CancelRotateSecret' {secretId} -> secretId) (\s@CancelRotateSecret' {} a -> s {secretId = a} :: CancelRotateSecret)

instance Core.AWSRequest CancelRotateSecret where
  type
    AWSResponse CancelRotateSecret =
      CancelRotateSecretResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelRotateSecretResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "VersionId")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelRotateSecret

instance Prelude.NFData CancelRotateSecret

instance Core.ToHeaders CancelRotateSecret where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.CancelRotateSecret" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CancelRotateSecret where
  toJSON CancelRotateSecret' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("SecretId" Core..= secretId)]
      )

instance Core.ToPath CancelRotateSecret where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelRotateSecret where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelRotateSecretResponse' smart constructor.
data CancelRotateSecretResponse = CancelRotateSecretResponse'
  { -- | The ARN of the secret for which rotation was canceled.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the version of the secret created during the
    -- rotation. This version might not be complete, and should be evaluated
    -- for possible deletion. At the very least, you should remove the
    -- @VersionStage@ value @AWSPENDING@ to enable this version to be deleted.
    -- Failing to clean up a cancelled rotation can block you from successfully
    -- starting future rotations.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret for which rotation was canceled.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'arn', 'cancelRotateSecretResponse_arn' - The ARN of the secret for which rotation was canceled.
--
-- 'versionId', 'cancelRotateSecretResponse_versionId' - The unique identifier of the version of the secret created during the
-- rotation. This version might not be complete, and should be evaluated
-- for possible deletion. At the very least, you should remove the
-- @VersionStage@ value @AWSPENDING@ to enable this version to be deleted.
-- Failing to clean up a cancelled rotation can block you from successfully
-- starting future rotations.
--
-- 'name', 'cancelRotateSecretResponse_name' - The friendly name of the secret for which rotation was canceled.
--
-- 'httpStatus', 'cancelRotateSecretResponse_httpStatus' - The response's http status code.
newCancelRotateSecretResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelRotateSecretResponse
newCancelRotateSecretResponse pHttpStatus_ =
  CancelRotateSecretResponse'
    { arn = Prelude.Nothing,
      versionId = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret for which rotation was canceled.
cancelRotateSecretResponse_arn :: Lens.Lens' CancelRotateSecretResponse (Prelude.Maybe Prelude.Text)
cancelRotateSecretResponse_arn = Lens.lens (\CancelRotateSecretResponse' {arn} -> arn) (\s@CancelRotateSecretResponse' {} a -> s {arn = a} :: CancelRotateSecretResponse)

-- | The unique identifier of the version of the secret created during the
-- rotation. This version might not be complete, and should be evaluated
-- for possible deletion. At the very least, you should remove the
-- @VersionStage@ value @AWSPENDING@ to enable this version to be deleted.
-- Failing to clean up a cancelled rotation can block you from successfully
-- starting future rotations.
cancelRotateSecretResponse_versionId :: Lens.Lens' CancelRotateSecretResponse (Prelude.Maybe Prelude.Text)
cancelRotateSecretResponse_versionId = Lens.lens (\CancelRotateSecretResponse' {versionId} -> versionId) (\s@CancelRotateSecretResponse' {} a -> s {versionId = a} :: CancelRotateSecretResponse)

-- | The friendly name of the secret for which rotation was canceled.
cancelRotateSecretResponse_name :: Lens.Lens' CancelRotateSecretResponse (Prelude.Maybe Prelude.Text)
cancelRotateSecretResponse_name = Lens.lens (\CancelRotateSecretResponse' {name} -> name) (\s@CancelRotateSecretResponse' {} a -> s {name = a} :: CancelRotateSecretResponse)

-- | The response's http status code.
cancelRotateSecretResponse_httpStatus :: Lens.Lens' CancelRotateSecretResponse Prelude.Int
cancelRotateSecretResponse_httpStatus = Lens.lens (\CancelRotateSecretResponse' {httpStatus} -> httpStatus) (\s@CancelRotateSecretResponse' {} a -> s {httpStatus = a} :: CancelRotateSecretResponse)

instance Prelude.NFData CancelRotateSecretResponse
