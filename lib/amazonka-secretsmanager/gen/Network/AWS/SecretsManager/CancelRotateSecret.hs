{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.CancelRotateSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables automatic scheduled rotation and cancels the rotation of a secret if currently in progress.
--
-- To re-enable scheduled rotation, call 'RotateSecret' with @AutomaticallyRotateAfterDays@ set to a value greater than 0. This immediately rotates your secret and then enables the automatic schedule.
-- To successfully start a rotation, the staging label @AWSPENDING@ must be in one of the following states:
--
--     * Not attached to any version at all
--
--
--     * Attached to the same version as the staging label @AWSCURRENT@
--
--
-- If the staging label @AWSPENDING@ attached to a different version than the version with @AWSCURRENT@ then the attempt to rotate fails.
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:CancelRotateSecret
--
--
-- __Related operations__
--
--     * To configure rotation for a secret or to manually trigger a rotation, use 'RotateSecret' .
--
--
--     * To get the rotation configuration details for a secret, use 'DescribeSecret' .
--
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
--
--
--     * To list all of the versions currently associated with a secret, use 'ListSecretVersionIds' .
module Network.AWS.SecretsManager.CancelRotateSecret
  ( -- * Creating a request
    CancelRotateSecret (..),
    mkCancelRotateSecret,

    -- ** Request lenses
    crsSecretId,

    -- * Destructuring the response
    CancelRotateSecretResponse (..),
    mkCancelRotateSecretResponse,

    -- ** Response lenses
    crsrsVersionId,
    crsrsARN,
    crsrsName,
    crsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkCancelRotateSecret' smart constructor.
newtype CancelRotateSecret = CancelRotateSecret'
  { -- | Specifies the secret to cancel a rotation request. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelRotateSecret' with the minimum fields required to make a request.
--
-- * 'secretId' - Specifies the secret to cancel a rotation request. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
mkCancelRotateSecret ::
  -- | 'secretId'
  Lude.Text ->
  CancelRotateSecret
mkCancelRotateSecret pSecretId_ =
  CancelRotateSecret' {secretId = pSecretId_}

-- | Specifies the secret to cancel a rotation request. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSecretId :: Lens.Lens' CancelRotateSecret Lude.Text
crsSecretId = Lens.lens (secretId :: CancelRotateSecret -> Lude.Text) (\s a -> s {secretId = a} :: CancelRotateSecret)
{-# DEPRECATED crsSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Lude.AWSRequest CancelRotateSecret where
  type Rs CancelRotateSecret = CancelRotateSecretResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelRotateSecretResponse'
            Lude.<$> (x Lude..?> "VersionId")
            Lude.<*> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelRotateSecret where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.CancelRotateSecret" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelRotateSecret where
  toJSON CancelRotateSecret' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SecretId" Lude..= secretId)])

instance Lude.ToPath CancelRotateSecret where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelRotateSecret where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelRotateSecretResponse' smart constructor.
data CancelRotateSecretResponse = CancelRotateSecretResponse'
  { -- | The unique identifier of the version of the secret created during the rotation. This version might not be complete, and should be evaluated for possible deletion. At the very least, you should remove the @VersionStage@ value @AWSPENDING@ to enable this version to be deleted. Failing to clean up a cancelled rotation can block you from successfully starting future rotations.
    versionId :: Lude.Maybe Lude.Text,
    -- | The ARN of the secret for which rotation was canceled.
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the secret for which rotation was canceled.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelRotateSecretResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The unique identifier of the version of the secret created during the rotation. This version might not be complete, and should be evaluated for possible deletion. At the very least, you should remove the @VersionStage@ value @AWSPENDING@ to enable this version to be deleted. Failing to clean up a cancelled rotation can block you from successfully starting future rotations.
-- * 'arn' - The ARN of the secret for which rotation was canceled.
-- * 'name' - The friendly name of the secret for which rotation was canceled.
-- * 'responseStatus' - The response status code.
mkCancelRotateSecretResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelRotateSecretResponse
mkCancelRotateSecretResponse pResponseStatus_ =
  CancelRotateSecretResponse'
    { versionId = Lude.Nothing,
      arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier of the version of the secret created during the rotation. This version might not be complete, and should be evaluated for possible deletion. At the very least, you should remove the @VersionStage@ value @AWSPENDING@ to enable this version to be deleted. Failing to clean up a cancelled rotation can block you from successfully starting future rotations.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrsVersionId :: Lens.Lens' CancelRotateSecretResponse (Lude.Maybe Lude.Text)
crsrsVersionId = Lens.lens (versionId :: CancelRotateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: CancelRotateSecretResponse)
{-# DEPRECATED crsrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ARN of the secret for which rotation was canceled.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrsARN :: Lens.Lens' CancelRotateSecretResponse (Lude.Maybe Lude.Text)
crsrsARN = Lens.lens (arn :: CancelRotateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CancelRotateSecretResponse)
{-# DEPRECATED crsrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret for which rotation was canceled.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrsName :: Lens.Lens' CancelRotateSecretResponse (Lude.Maybe Lude.Text)
crsrsName = Lens.lens (name :: CancelRotateSecretResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CancelRotateSecretResponse)
{-# DEPRECATED crsrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrsResponseStatus :: Lens.Lens' CancelRotateSecretResponse Lude.Int
crsrsResponseStatus = Lens.lens (responseStatus :: CancelRotateSecretResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelRotateSecretResponse)
{-# DEPRECATED crsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
