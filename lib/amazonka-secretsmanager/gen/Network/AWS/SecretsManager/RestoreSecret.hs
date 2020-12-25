{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.RestoreSecret
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the scheduled deletion of a secret by removing the @DeletedDate@ time stamp. This makes the secret accessible to query once again.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:RestoreSecret
--
--
-- __Related operations__
--
--     * To delete a secret, use 'DeleteSecret' .
module Network.AWS.SecretsManager.RestoreSecret
  ( -- * Creating a request
    RestoreSecret (..),
    mkRestoreSecret,

    -- ** Request lenses
    rSecretId,

    -- * Destructuring the response
    RestoreSecretResponse (..),
    mkRestoreSecretResponse,

    -- ** Response lenses
    rrsARN,
    rrsName,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkRestoreSecret' smart constructor.
newtype RestoreSecret = RestoreSecret'
  { -- | Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreSecret' value with any optional fields omitted.
mkRestoreSecret ::
  -- | 'secretId'
  Types.SecretId ->
  RestoreSecret
mkRestoreSecret secretId = RestoreSecret' {secretId}

-- | Specifies the secret that you want to restore from a previously scheduled deletion. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rSecretId :: Lens.Lens' RestoreSecret Types.SecretId
rSecretId = Lens.field @"secretId"
{-# DEPRECATED rSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Core.FromJSON RestoreSecret where
  toJSON RestoreSecret {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SecretId" Core..= secretId)])

instance Core.AWSRequest RestoreSecret where
  type Rs RestoreSecret = RestoreSecretResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.RestoreSecret")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreSecretResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRestoreSecretResponse' smart constructor.
data RestoreSecretResponse = RestoreSecretResponse'
  { -- | The ARN of the secret that was restored.
    arn :: Core.Maybe Types.SecretARNType,
    -- | The friendly name of the secret that was restored.
    name :: Core.Maybe Types.Name,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RestoreSecretResponse' value with any optional fields omitted.
mkRestoreSecretResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RestoreSecretResponse
mkRestoreSecretResponse responseStatus =
  RestoreSecretResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The ARN of the secret that was restored.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsARN :: Lens.Lens' RestoreSecretResponse (Core.Maybe Types.SecretARNType)
rrsARN = Lens.field @"arn"
{-# DEPRECATED rrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that was restored.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsName :: Lens.Lens' RestoreSecretResponse (Core.Maybe Types.Name)
rrsName = Lens.field @"name"
{-# DEPRECATED rrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RestoreSecretResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
