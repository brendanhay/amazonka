{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TrafficMirrorFilter
  ( TrafficMirrorFilter (..)
  -- * Smart constructor
  , mkTrafficMirrorFilter
  -- * Lenses
  , tmfDescription
  , tmfEgressFilterRules
  , tmfIngressFilterRules
  , tmfNetworkServices
  , tmfTags
  , tmfTrafficMirrorFilterId
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorFilterRule as Types
import qualified Network.AWS.EC2.Types.TrafficMirrorNetworkService as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Traffic Mirror filter.
--
-- /See:/ 'mkTrafficMirrorFilter' smart constructor.
data TrafficMirrorFilter = TrafficMirrorFilter'
  { description :: Core.Maybe Core.Text
    -- ^ The description of the Traffic Mirror filter.
  , egressFilterRules :: Core.Maybe [Types.TrafficMirrorFilterRule]
    -- ^ Information about the egress rules that are associated with the Traffic Mirror filter.
  , ingressFilterRules :: Core.Maybe [Types.TrafficMirrorFilterRule]
    -- ^ Information about the ingress rules that are associated with the Traffic Mirror filter.
  , networkServices :: Core.Maybe [Types.TrafficMirrorNetworkService]
    -- ^ The network service traffic that is associated with the Traffic Mirror filter.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tags assigned to the Traffic Mirror filter.
  , trafficMirrorFilterId :: Core.Maybe Core.Text
    -- ^ The ID of the Traffic Mirror filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrafficMirrorFilter' value with any optional fields omitted.
mkTrafficMirrorFilter
    :: TrafficMirrorFilter
mkTrafficMirrorFilter
  = TrafficMirrorFilter'{description = Core.Nothing,
                         egressFilterRules = Core.Nothing,
                         ingressFilterRules = Core.Nothing, networkServices = Core.Nothing,
                         tags = Core.Nothing, trafficMirrorFilterId = Core.Nothing}

-- | The description of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfDescription :: Lens.Lens' TrafficMirrorFilter (Core.Maybe Core.Text)
tmfDescription = Lens.field @"description"
{-# INLINEABLE tmfDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Information about the egress rules that are associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'egressFilterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfEgressFilterRules :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.TrafficMirrorFilterRule])
tmfEgressFilterRules = Lens.field @"egressFilterRules"
{-# INLINEABLE tmfEgressFilterRules #-}
{-# DEPRECATED egressFilterRules "Use generic-lens or generic-optics with 'egressFilterRules' instead"  #-}

-- | Information about the ingress rules that are associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'ingressFilterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfIngressFilterRules :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.TrafficMirrorFilterRule])
tmfIngressFilterRules = Lens.field @"ingressFilterRules"
{-# INLINEABLE tmfIngressFilterRules #-}
{-# DEPRECATED ingressFilterRules "Use generic-lens or generic-optics with 'ingressFilterRules' instead"  #-}

-- | The network service traffic that is associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'networkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfNetworkServices :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.TrafficMirrorNetworkService])
tmfNetworkServices = Lens.field @"networkServices"
{-# INLINEABLE tmfNetworkServices #-}
{-# DEPRECATED networkServices "Use generic-lens or generic-optics with 'networkServices' instead"  #-}

-- | The tags assigned to the Traffic Mirror filter.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfTags :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Types.Tag])
tmfTags = Lens.field @"tags"
{-# INLINEABLE tmfTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilter (Core.Maybe Core.Text)
tmfTrafficMirrorFilterId = Lens.field @"trafficMirrorFilterId"
{-# INLINEABLE tmfTrafficMirrorFilterId #-}
{-# DEPRECATED trafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead"  #-}

instance Core.FromXML TrafficMirrorFilter where
        parseXML x
          = TrafficMirrorFilter' Core.<$>
              (x Core..@? "description") Core.<*>
                x Core..@? "egressFilterRuleSet" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "ingressFilterRuleSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*>
                x Core..@? "networkServiceSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "trafficMirrorFilterId"
