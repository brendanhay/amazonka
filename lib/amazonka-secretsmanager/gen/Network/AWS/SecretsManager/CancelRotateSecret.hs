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
    crsrrsARN,
    crsrrsName,
    crsrrsVersionId,
    crsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkCancelRotateSecret' smart constructor.
newtype CancelRotateSecret = CancelRotateSecret'
  { -- | Specifies the secret to cancel a rotation request. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelRotateSecret' value with any optional fields omitted.
mkCancelRotateSecret ::
  -- | 'secretId'
  Types.SecretId ->
  CancelRotateSecret
mkCancelRotateSecret secretId = CancelRotateSecret' {secretId}

-- | Specifies the secret to cancel a rotation request. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsSecretId :: Lens.Lens' CancelRotateSecret Types.SecretId
crsSecretId = Lens.field @"secretId"
{-# DEPRECATED crsSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Core.FromJSON CancelRotateSecret where
  toJSON CancelRotateSecret {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SecretId" Core..= secretId)])

instance Core.AWSRequest CancelRotateSecret where
  type Rs CancelRotateSecret = CancelRotateSecretResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.CancelRotateSecret")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelRotateSecretResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "VersionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCancelRotateSecretResponse' smart constructor.
data CancelRotateSecretResponse = CancelRotateSecretResponse'
  { -- | The ARN of the secret for which rotation was canceled.
    arn :: Core.Maybe Types.ARN,
    -- | The friendly name of the secret for which rotation was canceled.
    name :: Core.Maybe Types.Name,
    -- | The unique identifier of the version of the secret created during the rotation. This version might not be complete, and should be evaluated for possible deletion. At the very least, you should remove the @VersionStage@ value @AWSPENDING@ to enable this version to be deleted. Failing to clean up a cancelled rotation can block you from successfully starting future rotations.
    versionId :: Core.Maybe Types.VersionId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelRotateSecretResponse' value with any optional fields omitted.
mkCancelRotateSecretResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CancelRotateSecretResponse
mkCancelRotateSecretResponse responseStatus =
  CancelRotateSecretResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      versionId = Core.Nothing,
      responseStatus
    }

-- | The ARN of the secret for which rotation was canceled.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrrsARN :: Lens.Lens' CancelRotateSecretResponse (Core.Maybe Types.ARN)
crsrrsARN = Lens.field @"arn"
{-# DEPRECATED crsrrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret for which rotation was canceled.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrrsName :: Lens.Lens' CancelRotateSecretResponse (Core.Maybe Types.Name)
crsrrsName = Lens.field @"name"
{-# DEPRECATED crsrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier of the version of the secret created during the rotation. This version might not be complete, and should be evaluated for possible deletion. At the very least, you should remove the @VersionStage@ value @AWSPENDING@ to enable this version to be deleted. Failing to clean up a cancelled rotation can block you from successfully starting future rotations.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrrsVersionId :: Lens.Lens' CancelRotateSecretResponse (Core.Maybe Types.VersionId)
crsrrsVersionId = Lens.field @"versionId"
{-# DEPRECATED crsrrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrrsResponseStatus :: Lens.Lens' CancelRotateSecretResponse Core.Int
crsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
