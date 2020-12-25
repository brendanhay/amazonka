{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilter
  ( TrafficMirrorFilter (..),

    -- * Smart constructor
    mkTrafficMirrorFilter,

    -- * Lenses
    tmfDescription,
    tmfEgressFilterRules,
    tmfIngressFilterRules,
    tmfNetworkServices,
    tmfTags,
    tmfTrafficMirrorFilterId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorFilterRule as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorNetworkService as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Traffic Mirror filter.
--
-- /See:/ 'mkTrafficMirrorFilter' smart constructor.
data TrafficMirrorFilter = TrafficMirrorFilter'
  { -- | The description of the Traffic Mirror filter.
    description :: Core.Maybe Types.String,
    -- | Information about the egress rules that are associated with the Traffic Mirror filter.
    egressFilterRules :: Core.Maybe [Types.TrafficMirrorFilterRule],
    -- | Information about the ingress rules that are associated with the Traffic Mirror filter.
    ingressFilterRules :: Core.Maybe [Types.TrafficMirrorFilterRule],
    -- | The network service traffic that is associated with the Traffic Mirror filter.
    networkServices :: Core.Maybe [Types.TrafficMirrorNetworkService],
    -- | The tags assigned to the Traffic Mirror filter.
    tags :: Core.Maybe [Types.Tag],
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorFilter' value with any optional fields omitted.
mkTrafficMirrorFilter ::
  TrafficMirrorFilter
mkTrafficMirrorFilter =
  TrafficMirrorFilter'
    { description = Core.Nothing,
      egressFilterRules = Core.Nothing,
      ingressFilterRules = Core.Nothing,
      networkServices = Core.Nothing,
      tags = Core.Nothing,
      trafficMirrorFilterId = Core.Nothing
    }

-- | The description of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfDescription :: Lens.Lens' TrafficMirrorFilter (Core.Maybe Types.String)
tmfDescription = Lens.field @"description"
{-# DEPRECATED tmfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Information about the egress rules that are associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'egressFilterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfEgressFilterRules :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.TrafficMirrorFilterRule])
tmfEgressFilterRules = Lens.field @"egressFilterRules"
{-# DEPRECATED tmfEgressFilterRules "Use generic-lens or generic-optics with 'egressFilterRules' instead." #-}

-- | Information about the ingress rules that are associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'ingressFilterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfIngressFilterRules :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.TrafficMirrorFilterRule])
tmfIngressFilterRules = Lens.field @"ingressFilterRules"
{-# DEPRECATED tmfIngressFilterRules "Use generic-lens or generic-optics with 'ingressFilterRules' instead." #-}

-- | The network service traffic that is associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'networkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfNetworkServices :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.TrafficMirrorNetworkService])
tmfNetworkServices = Lens.field @"networkServices"
{-# DEPRECATED tmfNetworkServices "Use generic-lens or generic-optics with 'networkServices' instead." #-}

-- | The tags assigned to the Traffic Mirror filter.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfTags :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.Tag])
tmfTags = Lens.field @"tags"
{-# DEPRECATED tmfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilter (Core.Maybe Types.String)
tmfTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# DEPRECATED tmfTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

instance Core.FromXML TrafficMirrorFilter where
  parseXML x =
    TrafficMirrorFilter'
      Core.<$> (x Core..@? "description")
      Core.<*> ( x Core..@? "egressFilterRuleSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "ingressFilterRuleSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "networkServiceSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "trafficMirrorFilterId")
