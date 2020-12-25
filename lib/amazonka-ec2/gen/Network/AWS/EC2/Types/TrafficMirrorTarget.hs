{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorTarget
  ( TrafficMirrorTarget (..),

    -- * Smart constructor
    mkTrafficMirrorTarget,

    -- * Lenses
    tmtDescription,
    tmtNetworkInterfaceId,
    tmtNetworkLoadBalancerArn,
    tmtOwnerId,
    tmtTags,
    tmtTrafficMirrorTargetId,
    tmtType,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorTargetType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Traffic Mirror target.
--
-- /See:/ 'mkTrafficMirrorTarget' smart constructor.
data TrafficMirrorTarget = TrafficMirrorTarget'
  { -- | Information about the Traffic Mirror target.
    description :: Core.Maybe Types.String,
    -- | The network interface ID that is attached to the target.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the Network Load Balancer.
    networkLoadBalancerArn :: Core.Maybe Types.String,
    -- | The ID of the account that owns the Traffic Mirror target.
    ownerId :: Core.Maybe Types.String,
    -- | The tags assigned to the Traffic Mirror target.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Core.Maybe Types.String,
    -- | The type of Traffic Mirror target.
    type' :: Core.Maybe Types.TrafficMirrorTargetType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorTarget' value with any optional fields omitted.
mkTrafficMirrorTarget ::
  TrafficMirrorTarget
mkTrafficMirrorTarget =
  TrafficMirrorTarget'
    { description = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      networkLoadBalancerArn = Core.Nothing,
      ownerId = Core.Nothing,
      tags = Core.Nothing,
      trafficMirrorTargetId = Core.Nothing,
      type' = Core.Nothing
    }

-- | Information about the Traffic Mirror target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtDescription :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Types.String)
tmtDescription = Lens.field @"description"
{-# DEPRECATED tmtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The network interface ID that is attached to the target.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtNetworkInterfaceId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Types.String)
tmtNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED tmtNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Network Load Balancer.
--
-- /Note:/ Consider using 'networkLoadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtNetworkLoadBalancerArn :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Types.String)
tmtNetworkLoadBalancerArn = Lens.field @"networkLoadBalancerArn"
{-# DEPRECATED tmtNetworkLoadBalancerArn "Use generic-lens or generic-optics with 'networkLoadBalancerArn' instead." #-}

-- | The ID of the account that owns the Traffic Mirror target.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtOwnerId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Types.String)
tmtOwnerId = Lens.field @"ownerId"
{-# DEPRECATED tmtOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The tags assigned to the Traffic Mirror target.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtTags :: Lens.Lens' TrafficMirrorTarget (Core.Maybe [Types.Tag])
tmtTags = Lens.field @"tags"
{-# DEPRECATED tmtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtTrafficMirrorTargetId :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Types.String)
tmtTrafficMirrorTargetId = Lens.field @"trafficMirrorTargetId"
{-# DEPRECATED tmtTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The type of Traffic Mirror target.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtType :: Lens.Lens' TrafficMirrorTarget (Core.Maybe Types.TrafficMirrorTargetType)
tmtType = Lens.field @"type'"
{-# DEPRECATED tmtType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromXML TrafficMirrorTarget where
  parseXML x =
    TrafficMirrorTarget'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "networkLoadBalancerArn")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "trafficMirrorTargetId")
      Core.<*> (x Core..@? "type")
