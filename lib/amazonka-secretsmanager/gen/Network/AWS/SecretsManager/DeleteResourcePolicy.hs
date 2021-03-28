{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.SecretsManager.DeleteResourcePolicy
    (
    -- * Creating a request
      DeleteResourcePolicy (..)
    , mkDeleteResourcePolicy
    -- ** Request lenses
    , drpSecretId

    -- * Destructuring the response
    , DeleteResourcePolicyResponse (..)
    , mkDeleteResourcePolicyResponse
    -- ** Response lenses
    , drprrsARN
    , drprrsName
    , drprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkDeleteResourcePolicy' smart constructor.
newtype DeleteResourcePolicy = DeleteResourcePolicy'
  { secretId :: Types.SecretId
    -- ^ Specifies the secret that you want to delete the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcePolicy' value with any optional fields omitted.
mkDeleteResourcePolicy
    :: Types.SecretId -- ^ 'secretId'
    -> DeleteResourcePolicy
mkDeleteResourcePolicy secretId = DeleteResourcePolicy'{secretId}

-- | Specifies the secret that you want to delete the attached resource-based policy for. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpSecretId :: Lens.Lens' DeleteResourcePolicy Types.SecretId
drpSecretId = Lens.field @"secretId"
{-# INLINEABLE drpSecretId #-}
{-# DEPRECATED secretId "Use generic-lens or generic-optics with 'secretId' instead"  #-}

instance Core.ToQuery DeleteResourcePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteResourcePolicy where
        toHeaders DeleteResourcePolicy{..}
          = Core.pure ("X-Amz-Target", "secretsmanager.DeleteResourcePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteResourcePolicy where
        toJSON DeleteResourcePolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SecretId" Core..= secretId)])

instance Core.AWSRequest DeleteResourcePolicy where
        type Rs DeleteResourcePolicy = DeleteResourcePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteResourcePolicyResponse' Core.<$>
                   (x Core..:? "ARN") Core.<*> x Core..:? "Name" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteResourcePolicyResponse' smart constructor.
data DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { arn :: Core.Maybe Types.SecretARNType
    -- ^ The ARN of the secret that the resource-based policy was deleted for.
  , name :: Core.Maybe Types.NameType
    -- ^ The friendly name of the secret that the resource-based policy was deleted for.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteResourcePolicyResponse' value with any optional fields omitted.
mkDeleteResourcePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteResourcePolicyResponse
mkDeleteResourcePolicyResponse responseStatus
  = DeleteResourcePolicyResponse'{arn = Core.Nothing,
                                  name = Core.Nothing, responseStatus}

-- | The ARN of the secret that the resource-based policy was deleted for.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsARN :: Lens.Lens' DeleteResourcePolicyResponse (Core.Maybe Types.SecretARNType)
drprrsARN = Lens.field @"arn"
{-# INLINEABLE drprrsARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The friendly name of the secret that the resource-based policy was deleted for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsName :: Lens.Lens' DeleteResourcePolicyResponse (Core.Maybe Types.NameType)
drprrsName = Lens.field @"name"
{-# INLINEABLE drprrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DeleteResourcePolicyResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
