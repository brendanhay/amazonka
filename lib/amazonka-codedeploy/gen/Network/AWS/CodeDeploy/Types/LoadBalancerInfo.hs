{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LoadBalancerInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.LoadBalancerInfo
  ( LoadBalancerInfo (..)
  -- * Smart constructor
  , mkLoadBalancerInfo
  -- * Lenses
  , lbiElbInfoList
  , lbiTargetGroupInfoList
  , lbiTargetGroupPairInfoList
  ) where

import qualified Network.AWS.CodeDeploy.Types.ELBInfo as Types
import qualified Network.AWS.CodeDeploy.Types.TargetGroupInfo as Types
import qualified Network.AWS.CodeDeploy.Types.TargetGroupPairInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the Elastic Load Balancing load balancer or target group used in a deployment.
--
-- /See:/ 'mkLoadBalancerInfo' smart constructor.
data LoadBalancerInfo = LoadBalancerInfo'
  { elbInfoList :: Core.Maybe [Types.ELBInfo]
    -- ^ An array that contains information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
  , targetGroupInfoList :: Core.Maybe [Types.TargetGroupInfo]
    -- ^ An array that contains information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
  , targetGroupPairInfoList :: Core.Maybe [Types.TargetGroupPairInfo]
    -- ^ The target group pair information. This is an array of @TargeGroupPairInfo@ objects with a maximum size of one. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerInfo' value with any optional fields omitted.
mkLoadBalancerInfo
    :: LoadBalancerInfo
mkLoadBalancerInfo
  = LoadBalancerInfo'{elbInfoList = Core.Nothing,
                      targetGroupInfoList = Core.Nothing,
                      targetGroupPairInfoList = Core.Nothing}

-- | An array that contains information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
--
-- /Note:/ Consider using 'elbInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbiElbInfoList :: Lens.Lens' LoadBalancerInfo (Core.Maybe [Types.ELBInfo])
lbiElbInfoList = Lens.field @"elbInfoList"
{-# INLINEABLE lbiElbInfoList #-}
{-# DEPRECATED elbInfoList "Use generic-lens or generic-optics with 'elbInfoList' instead"  #-}

-- | An array that contains information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
--
-- /Note:/ Consider using 'targetGroupInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbiTargetGroupInfoList :: Lens.Lens' LoadBalancerInfo (Core.Maybe [Types.TargetGroupInfo])
lbiTargetGroupInfoList = Lens.field @"targetGroupInfoList"
{-# INLINEABLE lbiTargetGroupInfoList #-}
{-# DEPRECATED targetGroupInfoList "Use generic-lens or generic-optics with 'targetGroupInfoList' instead"  #-}

-- | The target group pair information. This is an array of @TargeGroupPairInfo@ objects with a maximum size of one. 
--
-- /Note:/ Consider using 'targetGroupPairInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbiTargetGroupPairInfoList :: Lens.Lens' LoadBalancerInfo (Core.Maybe [Types.TargetGroupPairInfo])
lbiTargetGroupPairInfoList = Lens.field @"targetGroupPairInfoList"
{-# INLINEABLE lbiTargetGroupPairInfoList #-}
{-# DEPRECATED targetGroupPairInfoList "Use generic-lens or generic-optics with 'targetGroupPairInfoList' instead"  #-}

instance Core.FromJSON LoadBalancerInfo where
        toJSON LoadBalancerInfo{..}
          = Core.object
              (Core.catMaybes
                 [("elbInfoList" Core..=) Core.<$> elbInfoList,
                  ("targetGroupInfoList" Core..=) Core.<$> targetGroupInfoList,
                  ("targetGroupPairInfoList" Core..=) Core.<$>
                    targetGroupPairInfoList])

instance Core.FromJSON LoadBalancerInfo where
        parseJSON
          = Core.withObject "LoadBalancerInfo" Core.$
              \ x ->
                LoadBalancerInfo' Core.<$>
                  (x Core..:? "elbInfoList") Core.<*>
                    x Core..:? "targetGroupInfoList"
                    Core.<*> x Core..:? "targetGroupPairInfoList"
