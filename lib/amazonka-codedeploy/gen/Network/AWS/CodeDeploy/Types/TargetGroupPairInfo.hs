{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
  ( TargetGroupPairInfo (..)
  -- * Smart constructor
  , mkTargetGroupPairInfo
  -- * Lenses
  , tgpiProdTrafficRoute
  , tgpiTargetGroups
  , tgpiTestTrafficRoute
  ) where

import qualified Network.AWS.CodeDeploy.Types.TargetGroupInfo as Types
import qualified Network.AWS.CodeDeploy.Types.TrafficRoute as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about two target groups and how traffic is routed during an Amazon ECS deployment. An optional test traffic route can be specified. 
--
-- /See:/ 'mkTargetGroupPairInfo' smart constructor.
data TargetGroupPairInfo = TargetGroupPairInfo'
  { prodTrafficRoute :: Core.Maybe Types.TrafficRoute
    -- ^ The path used by a load balancer to route production traffic when an Amazon ECS deployment is complete. 
  , targetGroups :: Core.Maybe [Types.TargetGroupInfo]
    -- ^ One pair of target groups. One is associated with the original task set. The second is associated with the task set that serves traffic after the deployment is complete. 
  , testTrafficRoute :: Core.Maybe Types.TrafficRoute
    -- ^ An optional path used by a load balancer to route test traffic after an Amazon ECS deployment. Validation can occur while test traffic is served during a deployment. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetGroupPairInfo' value with any optional fields omitted.
mkTargetGroupPairInfo
    :: TargetGroupPairInfo
mkTargetGroupPairInfo
  = TargetGroupPairInfo'{prodTrafficRoute = Core.Nothing,
                         targetGroups = Core.Nothing, testTrafficRoute = Core.Nothing}

-- | The path used by a load balancer to route production traffic when an Amazon ECS deployment is complete. 
--
-- /Note:/ Consider using 'prodTrafficRoute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpiProdTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Core.Maybe Types.TrafficRoute)
tgpiProdTrafficRoute = Lens.field @"prodTrafficRoute"
{-# INLINEABLE tgpiProdTrafficRoute #-}
{-# DEPRECATED prodTrafficRoute "Use generic-lens or generic-optics with 'prodTrafficRoute' instead"  #-}

-- | One pair of target groups. One is associated with the original task set. The second is associated with the task set that serves traffic after the deployment is complete. 
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpiTargetGroups :: Lens.Lens' TargetGroupPairInfo (Core.Maybe [Types.TargetGroupInfo])
tgpiTargetGroups = Lens.field @"targetGroups"
{-# INLINEABLE tgpiTargetGroups #-}
{-# DEPRECATED targetGroups "Use generic-lens or generic-optics with 'targetGroups' instead"  #-}

-- | An optional path used by a load balancer to route test traffic after an Amazon ECS deployment. Validation can occur while test traffic is served during a deployment. 
--
-- /Note:/ Consider using 'testTrafficRoute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpiTestTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Core.Maybe Types.TrafficRoute)
tgpiTestTrafficRoute = Lens.field @"testTrafficRoute"
{-# INLINEABLE tgpiTestTrafficRoute #-}
{-# DEPRECATED testTrafficRoute "Use generic-lens or generic-optics with 'testTrafficRoute' instead"  #-}

instance Core.FromJSON TargetGroupPairInfo where
        toJSON TargetGroupPairInfo{..}
          = Core.object
              (Core.catMaybes
                 [("prodTrafficRoute" Core..=) Core.<$> prodTrafficRoute,
                  ("targetGroups" Core..=) Core.<$> targetGroups,
                  ("testTrafficRoute" Core..=) Core.<$> testTrafficRoute])

instance Core.FromJSON TargetGroupPairInfo where
        parseJSON
          = Core.withObject "TargetGroupPairInfo" Core.$
              \ x ->
                TargetGroupPairInfo' Core.<$>
                  (x Core..:? "prodTrafficRoute") Core.<*> x Core..:? "targetGroups"
                    Core.<*> x Core..:? "testTrafficRoute"
