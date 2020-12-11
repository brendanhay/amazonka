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
    tmfTrafficMirrorFilterId,
    tmfIngressFilterRules,
    tmfNetworkServices,
    tmfEgressFilterRules,
    tmfDescription,
    tmfTags,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficMirrorFilterRule
import Network.AWS.EC2.Types.TrafficMirrorNetworkService
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Traffic Mirror filter.
--
-- /See:/ 'mkTrafficMirrorFilter' smart constructor.
data TrafficMirrorFilter = TrafficMirrorFilter'
  { trafficMirrorFilterId ::
      Lude.Maybe Lude.Text,
    ingressFilterRules ::
      Lude.Maybe [TrafficMirrorFilterRule],
    networkServices ::
      Lude.Maybe [TrafficMirrorNetworkService],
    egressFilterRules ::
      Lude.Maybe [TrafficMirrorFilterRule],
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

-- | Creates a value of 'TrafficMirrorFilter' with the minimum fields required to make a request.
--
-- * 'description' - The description of the Traffic Mirror filter.
-- * 'egressFilterRules' - Information about the egress rules that are associated with the Traffic Mirror filter.
-- * 'ingressFilterRules' - Information about the ingress rules that are associated with the Traffic Mirror filter.
-- * 'networkServices' - The network service traffic that is associated with the Traffic Mirror filter.
-- * 'tags' - The tags assigned to the Traffic Mirror filter.
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
mkTrafficMirrorFilter ::
  TrafficMirrorFilter
mkTrafficMirrorFilter =
  TrafficMirrorFilter'
    { trafficMirrorFilterId = Lude.Nothing,
      ingressFilterRules = Lude.Nothing,
      networkServices = Lude.Nothing,
      egressFilterRules = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfTrafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilter (Lude.Maybe Lude.Text)
tmfTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: TrafficMirrorFilter -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: TrafficMirrorFilter)
{-# DEPRECATED tmfTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | Information about the ingress rules that are associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'ingressFilterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfIngressFilterRules :: Lens.Lens' TrafficMirrorFilter (Lude.Maybe [TrafficMirrorFilterRule])
tmfIngressFilterRules = Lens.lens (ingressFilterRules :: TrafficMirrorFilter -> Lude.Maybe [TrafficMirrorFilterRule]) (\s a -> s {ingressFilterRules = a} :: TrafficMirrorFilter)
{-# DEPRECATED tmfIngressFilterRules "Use generic-lens or generic-optics with 'ingressFilterRules' instead." #-}

-- | The network service traffic that is associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'networkServices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfNetworkServices :: Lens.Lens' TrafficMirrorFilter (Lude.Maybe [TrafficMirrorNetworkService])
tmfNetworkServices = Lens.lens (networkServices :: TrafficMirrorFilter -> Lude.Maybe [TrafficMirrorNetworkService]) (\s a -> s {networkServices = a} :: TrafficMirrorFilter)
{-# DEPRECATED tmfNetworkServices "Use generic-lens or generic-optics with 'networkServices' instead." #-}

-- | Information about the egress rules that are associated with the Traffic Mirror filter.
--
-- /Note:/ Consider using 'egressFilterRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfEgressFilterRules :: Lens.Lens' TrafficMirrorFilter (Lude.Maybe [TrafficMirrorFilterRule])
tmfEgressFilterRules = Lens.lens (egressFilterRules :: TrafficMirrorFilter -> Lude.Maybe [TrafficMirrorFilterRule]) (\s a -> s {egressFilterRules = a} :: TrafficMirrorFilter)
{-# DEPRECATED tmfEgressFilterRules "Use generic-lens or generic-optics with 'egressFilterRules' instead." #-}

-- | The description of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfDescription :: Lens.Lens' TrafficMirrorFilter (Lude.Maybe Lude.Text)
tmfDescription = Lens.lens (description :: TrafficMirrorFilter -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TrafficMirrorFilter)
{-# DEPRECATED tmfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags assigned to the Traffic Mirror filter.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tmfTags :: Lens.Lens' TrafficMirrorFilter (Lude.Maybe [Tag])
tmfTags = Lens.lens (tags :: TrafficMirrorFilter -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: TrafficMirrorFilter)
{-# DEPRECATED tmfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML TrafficMirrorFilter where
  parseXML x =
    TrafficMirrorFilter'
      Lude.<$> (x Lude..@? "trafficMirrorFilterId")
      Lude.<*> ( x Lude..@? "ingressFilterRuleSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "networkServiceSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "egressFilterRuleSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
