{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.DeleteResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource-based permission policy attached to the secret.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:DeleteResourcePolicy
--
--
-- __Related operations__
--
--     * To attach a resource policy to a secret, use 'PutResourcePolicy' .
--
--
--     * To retrieve the current resource-based policy that's attached to a secret, use 'GetResourcePolicy' .
--
--
--     * To list all of the currently available secrets, use 'ListSecrets' .
module Network.AWS.SecretsManager.DeleteResourcePolicy
  ( -- * Creating a request
    DeleteResourcePolicy (..),
    mkDeleteResourcePolicy,

    -- ** Request lenses
    drpSecretId,

    -- * Destructuring the response
    DeleteResourcePolicyResponse (..),
    mkDeleteResourcePolicyResponse,

    -- ** Response lenses
    drprrsARN,
    drprrsName,
    drprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkDeleteResourcePolicy' smart constructor.
newtype DeleteResourcePolicy = DeleteResourcePolicy'
  { -- | Specifies the secret that you want to delete the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcePolicy' value with any optional fields omitted.
mkDeleteResourcePolicy ::
  -- | 'secretId'
  Types.SecretId ->
  DeleteResourcePolicy
mkDeleteResourcePolicy secretId = DeleteResourcePolicy' {secretId}

-- | Specifies the secret that you want to delete the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpSecretId :: Lens.Lens' DeleteResourcePolicy Types.SecretId
drpSecretId = Lens.field @"secretId"
{-# DEPRECATED drpSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

instance Core.FromJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy {..} =
    Core.object
      (Core.catMaybes [Core.Just ("SecretId" Core..= secretId)])

instance Core.AWSRequest DeleteResourcePolicy where
  type Rs DeleteResourcePolicy = DeleteResourcePolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.DeleteResourcePolicy")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteResourcePolicyResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { -- | The ARN of the secret that the resource-based policy was deleted for.
    arn :: Core.Maybe Types.SecretARNType,
    -- | The friendly name of the secret that the resource-based policy was deleted for.
    name :: Core.Maybe Types.NameType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcePolicyResponse' value with any optional fields omitted.
mkDeleteResourcePolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteResourcePolicyResponse
mkDeleteResourcePolicyResponse responseStatus =
  DeleteResourcePolicyResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The ARN of the secret that the resource-based policy was deleted for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsARN :: Lens.Lens' DeleteResourcePolicyResponse (Core.Maybe Types.SecretARNType)
drprrsARN = Lens.field @"arn"
{-# DEPRECATED drprrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret that the resource-based policy was deleted for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsName :: Lens.Lens' DeleteResourcePolicyResponse (Core.Maybe Types.NameType)
drprrsName = Lens.field @"name"
{-# DEPRECATED drprrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DeleteResourcePolicyResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
