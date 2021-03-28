{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetPermissionPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM policy attached to the RuleGroup.
module Network.AWS.WAFRegional.GetPermissionPolicy
    (
    -- * Creating a request
      GetPermissionPolicy (..)
    , mkGetPermissionPolicy
    -- ** Request lenses
    , gppResourceArn

    -- * Destructuring the response
    , GetPermissionPolicyResponse (..)
    , mkGetPermissionPolicyResponse
    -- ** Response lenses
    , gpprrsPolicy
    , gpprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkGetPermissionPolicy' smart constructor.
newtype GetPermissionPolicy = GetPermissionPolicy'
  { resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the RuleGroup for which you want to get the policy.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPermissionPolicy' value with any optional fields omitted.
mkGetPermissionPolicy
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> GetPermissionPolicy
mkGetPermissionPolicy resourceArn
  = GetPermissionPolicy'{resourceArn}

-- | The Amazon Resource Name (ARN) of the RuleGroup for which you want to get the policy.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gppResourceArn :: Lens.Lens' GetPermissionPolicy Types.ResourceArn
gppResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE gppResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery GetPermissionPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPermissionPolicy where
        toHeaders GetPermissionPolicy{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.GetPermissionPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPermissionPolicy where
        toJSON GetPermissionPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetPermissionPolicy where
        type Rs GetPermissionPolicy = GetPermissionPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPermissionPolicyResponse' Core.<$>
                   (x Core..:? "Policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPermissionPolicyResponse' smart constructor.
data GetPermissionPolicyResponse = GetPermissionPolicyResponse'
  { policy :: Core.Maybe Types.Policy
    -- ^ The IAM policy attached to the specified RuleGroup.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetPermissionPolicyResponse' value with any optional fields omitted.
mkGetPermissionPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPermissionPolicyResponse
mkGetPermissionPolicyResponse responseStatus
  = GetPermissionPolicyResponse'{policy = Core.Nothing,
                                 responseStatus}

-- | The IAM policy attached to the specified RuleGroup.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpprrsPolicy :: Lens.Lens' GetPermissionPolicyResponse (Core.Maybe Types.Policy)
gpprrsPolicy = Lens.field @"policy"
{-# INLINEABLE gpprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpprrsResponseStatus :: Lens.Lens' GetPermissionPolicyResponse Core.Int
gpprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
