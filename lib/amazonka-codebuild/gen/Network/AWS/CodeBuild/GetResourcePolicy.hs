{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a resource policy that is identified by its resource ARN. 
module Network.AWS.CodeBuild.GetResourcePolicy
    (
    -- * Creating a request
      GetResourcePolicy (..)
    , mkGetResourcePolicy
    -- ** Request lenses
    , grpResourceArn

    -- * Destructuring the response
    , GetResourcePolicyResponse (..)
    , mkGetResourcePolicyResponse
    -- ** Response lenses
    , grprrsPolicy
    , grprrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { resourceArn :: Types.ResourceArn
    -- ^ The ARN of the resource that is associated with the resource policy. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicy' value with any optional fields omitted.
mkGetResourcePolicy
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> GetResourcePolicy
mkGetResourcePolicy resourceArn = GetResourcePolicy'{resourceArn}

-- | The ARN of the resource that is associated with the resource policy. 
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpResourceArn :: Lens.Lens' GetResourcePolicy Types.ResourceArn
grpResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE grpResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery GetResourcePolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetResourcePolicy where
        toHeaders GetResourcePolicy{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.GetResourcePolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetResourcePolicy where
        toJSON GetResourcePolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("resourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetResourcePolicy where
        type Rs GetResourcePolicy = GetResourcePolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetResourcePolicyResponse' Core.<$>
                   (x Core..:? "policy") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { policy :: Core.Maybe Types.Policy
    -- ^ The resource policy for the resource identified by the input ARN parameter. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourcePolicyResponse' value with any optional fields omitted.
mkGetResourcePolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetResourcePolicyResponse
mkGetResourcePolicyResponse responseStatus
  = GetResourcePolicyResponse'{policy = Core.Nothing, responseStatus}

-- | The resource policy for the resource identified by the input ARN parameter. 
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsPolicy :: Lens.Lens' GetResourcePolicyResponse (Core.Maybe Types.Policy)
grprrsPolicy = Lens.field @"policy"
{-# INLINEABLE grprrsPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrsResponseStatus :: Lens.Lens' GetResourcePolicyResponse Core.Int
grprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
