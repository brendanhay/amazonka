{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.GetManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the attached managed scaling policy for an Amazon EMR cluster. 
module Network.AWS.EMR.GetManagedScalingPolicy
    (
    -- * Creating a request
      GetManagedScalingPolicy (..)
    , mkGetManagedScalingPolicy
    -- ** Request lenses
    , gmspClusterId

    -- * Destructuring the response
    , GetManagedScalingPolicyResponse (..)
    , mkGetManagedScalingPolicyResponse
    -- ** Response lenses
    , gmsprrsManagedScalingPolicy
    , gmsprrsResponseStatus
    ) where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetManagedScalingPolicy' smart constructor.
newtype GetManagedScalingPolicy = GetManagedScalingPolicy'
  { clusterId :: Types.ClusterId
    -- ^ Specifies the ID of the cluster for which the managed scaling policy will be fetched. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetManagedScalingPolicy' value with any optional fields omitted.
mkGetManagedScalingPolicy
    :: Types.ClusterId -- ^ 'clusterId'
    -> GetManagedScalingPolicy
mkGetManagedScalingPolicy clusterId
  = GetManagedScalingPolicy'{clusterId}

-- | Specifies the ID of the cluster for which the managed scaling policy will be fetched. 
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmspClusterId :: Lens.Lens' GetManagedScalingPolicy Types.ClusterId
gmspClusterId = Lens.field @"clusterId"
{-# INLINEABLE gmspClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

instance Core.ToQuery GetManagedScalingPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetManagedScalingPolicy where
        toHeaders GetManagedScalingPolicy{..}
          = Core.pure
              ("X-Amz-Target", "ElasticMapReduce.GetManagedScalingPolicy")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetManagedScalingPolicy where
        toJSON GetManagedScalingPolicy{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ClusterId" Core..= clusterId)])

instance Core.AWSRequest GetManagedScalingPolicy where
        type Rs GetManagedScalingPolicy = GetManagedScalingPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetManagedScalingPolicyResponse' Core.<$>
                   (x Core..:? "ManagedScalingPolicy") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetManagedScalingPolicyResponse' smart constructor.
data GetManagedScalingPolicyResponse = GetManagedScalingPolicyResponse'
  { managedScalingPolicy :: Core.Maybe Types.ManagedScalingPolicy
    -- ^ Specifies the managed scaling policy that is attached to an Amazon EMR cluster. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetManagedScalingPolicyResponse' value with any optional fields omitted.
mkGetManagedScalingPolicyResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetManagedScalingPolicyResponse
mkGetManagedScalingPolicyResponse responseStatus
  = GetManagedScalingPolicyResponse'{managedScalingPolicy =
                                       Core.Nothing,
                                     responseStatus}

-- | Specifies the managed scaling policy that is attached to an Amazon EMR cluster. 
--
-- /Note:/ Consider using 'managedScalingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsprrsManagedScalingPolicy :: Lens.Lens' GetManagedScalingPolicyResponse (Core.Maybe Types.ManagedScalingPolicy)
gmsprrsManagedScalingPolicy = Lens.field @"managedScalingPolicy"
{-# INLINEABLE gmsprrsManagedScalingPolicy #-}
{-# DEPRECATED managedScalingPolicy "Use generic-lens or generic-optics with 'managedScalingPolicy' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsprrsResponseStatus :: Lens.Lens' GetManagedScalingPolicyResponse Core.Int
gmsprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmsprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
