{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeletePermissionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an IAM policy from the specified RuleGroup.
--
-- The user making the request must be the owner of the RuleGroup.
module Network.AWS.WAFRegional.DeletePermissionPolicy
    (
    -- * Creating a request
      DeletePermissionPolicy (..)
    , mkDeletePermissionPolicy
    -- ** Request lenses
    , dppResourceArn

    -- * Destructuring the response
    , DeletePermissionPolicyResponse (..)
    , mkDeletePermissionPolicyResponse
    -- ** Response lenses
    , dpprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeletePermissionPolicy' smart constructor.
newtype DeletePermissionPolicy = DeletePermissionPolicy'
  { resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the RuleGroup from which you want to delete the policy.
--
-- The user making the request must be the owner of the RuleGroup.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePermissionPolicy' value with any optional fields omitted.
mkDeletePermissionPolicy
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> DeletePermissionPolicy
mkDeletePermissionPolicy resourceArn
  = DeletePermissionPolicy'{resourceArn}

-- | The Amazon Resource Name (ARN) of the RuleGroup from which you want to delete the policy.
--
-- The user making the request must be the owner of the RuleGroup.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dppResourceArn :: Lens.Lens' DeletePermissionPolicy Types.ResourceArn
dppResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE dppResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery DeletePermissionPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeletePermissionPolicy where
        toHeaders DeletePermissionPolicy{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.DeletePermissionPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeletePermissionPolicy where
        toJSON DeletePermissionPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest DeletePermissionPolicy where
        type Rs DeletePermissionPolicy = DeletePermissionPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeletePermissionPolicyResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeletePermissionPolicyResponse' smart constructor.
newtype DeletePermissionPolicyResponse = DeletePermissionPolicyResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeletePermissionPolicyResponse' value with any optional fields omitted.
mkDeletePermissionPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeletePermissionPolicyResponse
mkDeletePermissionPolicyResponse responseStatus
  = DeletePermissionPolicyResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpprrsResponseStatus :: Lens.Lens' DeletePermissionPolicyResponse Core.Int
dpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
