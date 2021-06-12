{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TrafficMirrorFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TrafficMirrorFilter where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.TrafficMirrorFilterRule
import Network.AWS.EC2.Types.TrafficMirrorNetworkService
import qualified Network.AWS.Lens as Lens

-- | Describes the Traffic Mirror filter.
--
-- /See:/ 'newTrafficMirrorFilter' smart constructor.
data TrafficMirrorFilter = TrafficMirrorFilter'
  { -- | Information about the egress rules that are associated with the Traffic
    -- Mirror filter.
    egressFilterRules :: Core.Maybe [TrafficMirrorFilterRule],
    -- | The network service traffic that is associated with the Traffic Mirror
    -- filter.
    networkServices :: Core.Maybe [TrafficMirrorNetworkService],
    -- | The tags assigned to the Traffic Mirror filter.
    tags :: Core.Maybe [Tag],
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Maybe Core.Text,
    -- | The description of the Traffic Mirror filter.
    description :: Core.Maybe Core.Text,
    -- | Information about the ingress rules that are associated with the Traffic
    -- Mirror filter.
    ingressFilterRules :: Core.Maybe [TrafficMirrorFilterRule]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrafficMirrorFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'egressFilterRules', 'trafficMirrorFilter_egressFilterRules' - Information about the egress rules that are associated with the Traffic
-- Mirror filter.
--
-- 'networkServices', 'trafficMirrorFilter_networkServices' - The network service traffic that is associated with the Traffic Mirror
-- filter.
--
-- 'tags', 'trafficMirrorFilter_tags' - The tags assigned to the Traffic Mirror filter.
--
-- 'trafficMirrorFilterId', 'trafficMirrorFilter_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- 'description', 'trafficMirrorFilter_description' - The description of the Traffic Mirror filter.
--
-- 'ingressFilterRules', 'trafficMirrorFilter_ingressFilterRules' - Information about the ingress rules that are associated with the Traffic
-- Mirror filter.
newTrafficMirrorFilter ::
  TrafficMirrorFilter
newTrafficMirrorFilter =
  TrafficMirrorFilter'
    { egressFilterRules =
        Core.Nothing,
      networkServices = Core.Nothing,
      tags = Core.Nothing,
      trafficMirrorFilterId = Core.Nothing,
      description = Core.Nothing,
      ingressFilterRules = Core.Nothing
    }

-- | Information about the egress rules that are associated with the Traffic
-- Mirror filter.
trafficMirrorFilter_egressFilterRules :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [TrafficMirrorFilterRule])
trafficMirrorFilter_egressFilterRules = Lens.lens (\TrafficMirrorFilter' {egressFilterRules} -> egressFilterRules) (\s@TrafficMirrorFilter' {} a -> s {egressFilterRules = a} :: TrafficMirrorFilter) Core.. Lens.mapping Lens._Coerce

-- | The network service traffic that is associated with the Traffic Mirror
-- filter.
trafficMirrorFilter_networkServices :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [TrafficMirrorNetworkService])
trafficMirrorFilter_networkServices = Lens.lens (\TrafficMirrorFilter' {networkServices} -> networkServices) (\s@TrafficMirrorFilter' {} a -> s {networkServices = a} :: TrafficMirrorFilter) Core.. Lens.mapping Lens._Coerce

-- | The tags assigned to the Traffic Mirror filter.
trafficMirrorFilter_tags :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [Tag])
trafficMirrorFilter_tags = Lens.lens (\TrafficMirrorFilter' {tags} -> tags) (\s@TrafficMirrorFilter' {} a -> s {tags = a} :: TrafficMirrorFilter) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Traffic Mirror filter.
trafficMirrorFilter_trafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilter (Core.Maybe Core.Text)
trafficMirrorFilter_trafficMirrorFilterId = Lens.lens (\TrafficMirrorFilter' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@TrafficMirrorFilter' {} a -> s {trafficMirrorFilterId = a} :: TrafficMirrorFilter)

-- | The description of the Traffic Mirror filter.
trafficMirrorFilter_description :: Lens.Lens' TrafficMirrorFilter (Core.Maybe Core.Text)
trafficMirrorFilter_description = Lens.lens (\TrafficMirrorFilter' {description} -> description) (\s@TrafficMirrorFilter' {} a -> s {description = a} :: TrafficMirrorFilter)

-- | Information about the ingress rules that are associated with the Traffic
-- Mirror filter.
trafficMirrorFilter_ingressFilterRules :: Lens.Lens' TrafficMirrorFilter (Core.Maybe [TrafficMirrorFilterRule])
trafficMirrorFilter_ingressFilterRules = Lens.lens (\TrafficMirrorFilter' {ingressFilterRules} -> ingressFilterRules) (\s@TrafficMirrorFilter' {} a -> s {ingressFilterRules = a} :: TrafficMirrorFilter) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML TrafficMirrorFilter where
  parseXML x =
    TrafficMirrorFilter'
      Core.<$> ( x Core..@? "egressFilterRuleSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "networkServiceSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "trafficMirrorFilterId")
      Core.<*> (x Core..@? "description")
      Core.<*> ( x Core..@? "ingressFilterRuleSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable TrafficMirrorFilter

instance Core.NFData TrafficMirrorFilter
