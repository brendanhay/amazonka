{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficMirrorTarget
  ( TrafficMirrorTarget (..)
  -- * Smart constructor
  , mkTrafficMirrorTarget
  -- * Lenses
  , tmtDescription
  , tmtNetworkInterfaceId
  , tmtNetworkLoadBalancerArn
  , tmtOwnerId
  , tmtTags
  , tmtTrafficMirrorTargetId
  , tmtType
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorTargetType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Traffic Mirror target.
--
-- /See:/ 'mkTrafficMirrorTarget' smart constructor.
data TrafficMirrorTarget = TrafficMirrorTarget'
  { description :: Core.Maybe Core.Text
    -- ^ Information about the Traffic Mirror target.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The network interface ID that is attached to the target.
  , networkLoadBalancerArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Network Load Balancer.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the account that owns the Traffic Mirror target.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags assigned to the Traffic Mirror target.
  , trafficMirrorTargetId :: Core.Maybe Core.Text
    -- ^ The ID of the Traffic Mirror target.
  , type' :: Core.Maybe Types.TrafficMirrorTargetType
    -- ^ The type of Traffic Mirror target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorTarget' value with any optional fields omitted.
mkTrafficMirrorTarget
    :: TrafficMirrorTarget
mkTrafficMirrorTarget
  = TrafficMirrorTarget'{description = Core.Nothing,
                         networkInterfaceId = Core.Nothing,
                         networkLoadBalancerArn = Core.Nothing, ownerId = Core.Nothing,
                         tags = Core.Nothing, trafficMirrorTargetId = Core.Nothing,
                         type' = Core.Nothing}

-- | Information about the Traffic Mirror target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtDescription :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
tmtDescription = Lens.field @"description"
{-# INLINEABLE tmtDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The network interface ID that is attached to the target.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtNetworkInterfaceId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
tmtNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE tmtNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Network Load Balancer.
--
-- /Note:/ Consider using 'networkLoadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtNetworkLoadBalancerArn :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
tmtNetworkLoadBalancerArn = Lens.field @"networkLoadBalancerArn"
{-# INLINEABLE tmtNetworkLoadBalancerArn #-}
{-# DEPRECATED networkLoadBalancerArn "Use generic-lens or generic-optics with 'networkLoadBalancerArn' instead"  #-}

-- | The ID of the account that owns the Traffic Mirror target.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtOwnerId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
tmtOwnerId = Lens.field @"ownerId"
{-# INLINEABLE tmtOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The tags assigned to the Traffic Mirror target.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtTags :: Lens.Lens' TrafficMirrorTarget (Core.Maybe [Types.Tag])
tmtTags = Lens.field @"tags"
{-# INLINEABLE tmtTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtTrafficMirrorTargetId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Core.Text)
tmtTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# INLINEABLE tmtTrafficMirrorTargetId #-}
{-# DEPRECATED trafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead"  #-}

-- | The type of Traffic Mirror target.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtType :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Types.TrafficMirrorTargetType)
tmtType = Lens.field @"type'"
{-# INLINEABLE tmtType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromXML TrafficMirrorTarget where
        parseXML x
          = TrafficMirrorTarget' Core.<$>
              (x Core..@? "description") Core.<*> x Core..@? "networkInterfaceId"
                Core.<*> x Core..@? "networkLoadBalancerArn"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "trafficMirrorTargetId"
                Core.<*> x Core..@? "type"
