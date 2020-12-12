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
    tmtTrafficMirrorTargetId,
    tmtNetworkInterfaceId,
    tmtNetworkLoadBalancerARN,
    tmtOwnerId,
    tmtType,
    tmtDescription,
    tmtTags,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficMirrorTargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Traffic Mirror target.
--
-- /See:/ 'mkTrafficMirrorTarget' smart constructor.
data TrafficMirrorTarget = TrafficMirrorTarget'
  { trafficMirrorTargetId ::
      Lude.Maybe Lude.Text,
    networkInterfaceId :: Lude.Maybe Lude.Text,
    networkLoadBalancerARN :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe TrafficMirrorTargetType,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficMirrorTarget' with the minimum fields required to make a request.
--
-- * 'description' - Information about the Traffic Mirror target.
-- * 'networkInterfaceId' - The network interface ID that is attached to the target.
-- * 'networkLoadBalancerARN' - The Amazon Resource Name (ARN) of the Network Load Balancer.
-- * 'ownerId' - The ID of the account that owns the Traffic Mirror target.
-- * 'tags' - The tags assigned to the Traffic Mirror target.
-- * 'trafficMirrorTargetId' - The ID of the Traffic Mirror target.
-- * 'type'' - The type of Traffic Mirror target.
mkTrafficMirrorTarget ::
  TrafficMirrorTarget
mkTrafficMirrorTarget =
  TrafficMirrorTarget'
    { trafficMirrorTargetId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      networkLoadBalancerARN = Lude.Nothing,
      ownerId = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the Traffic Mirror target.
--
-- /Note:/ Consider using 'trafficMirrorTargetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtTrafficMirrorTargetId :: Lens.Lens' TrafficMirrorTarget (Lude.Maybe Lude.Text)
tmtTrafficMirrorTargetId = Lens.lens (trafficMirrorTargetId :: TrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorTargetId = a} :: TrafficMirrorTarget)
{-# DEPRECATED tmtTrafficMirrorTargetId "Use generic-lens or generic-optics with 'trafficMirrorTargetId' instead." #-}

-- | The network interface ID that is attached to the target.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtNetworkInterfaceId :: Lens.Lens' TrafficMirrorTarget (Lude.Maybe Lude.Text)
tmtNetworkInterfaceId = Lens.lens (networkInterfaceId :: TrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: TrafficMirrorTarget)
{-# DEPRECATED tmtNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Network Load Balancer.
--
-- /Note:/ Consider using 'networkLoadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtNetworkLoadBalancerARN :: Lens.Lens' TrafficMirrorTarget (Lude.Maybe Lude.Text)
tmtNetworkLoadBalancerARN = Lens.lens (networkLoadBalancerARN :: TrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {networkLoadBalancerARN = a} :: TrafficMirrorTarget)
{-# DEPRECATED tmtNetworkLoadBalancerARN "Use generic-lens or generic-optics with 'networkLoadBalancerARN' instead." #-}

-- | The ID of the account that owns the Traffic Mirror target.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtOwnerId :: Lens.Lens' TrafficMirrorTarget (Lude.Maybe Lude.Text)
tmtOwnerId = Lens.lens (ownerId :: TrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: TrafficMirrorTarget)
{-# DEPRECATED tmtOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The type of Traffic Mirror target.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtType :: Lens.Lens' TrafficMirrorTarget (Lude.Maybe TrafficMirrorTargetType)
tmtType = Lens.lens (type' :: TrafficMirrorTarget -> Lude.Maybe TrafficMirrorTargetType) (\s a -> s {type' = a} :: TrafficMirrorTarget)
{-# DEPRECATED tmtType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Information about the Traffic Mirror target.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtDescription :: Lens.Lens' TrafficMirrorTarget (Lude.Maybe Lude.Text)
tmtDescription = Lens.lens (description :: TrafficMirrorTarget -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TrafficMirrorTarget)
{-# DEPRECATED tmtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags assigned to the Traffic Mirror target.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmtTags :: Lens.Lens' TrafficMirrorTarget (Lude.Maybe [Tag])
tmtTags = Lens.lens (tags :: TrafficMirrorTarget -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TrafficMirrorTarget)
{-# DEPRECATED tmtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TrafficMirrorTarget where
  parseXML x =
    TrafficMirrorTarget'
      Lude.<$> (x Lude..@? "trafficMirrorTargetId")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "networkLoadBalancerArn")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "type")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
