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
-- Module      : Amazonka.EC2.Types.TrafficMirrorFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TrafficMirrorFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TrafficMirrorFilterRule
import Amazonka.EC2.Types.TrafficMirrorNetworkService
import qualified Amazonka.Prelude as Prelude

-- | Describes the Traffic Mirror filter.
--
-- /See:/ 'newTrafficMirrorFilter' smart constructor.
data TrafficMirrorFilter = TrafficMirrorFilter'
  { -- | The tags assigned to the Traffic Mirror filter.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the egress rules that are associated with the Traffic
    -- Mirror filter.
    egressFilterRules :: Prelude.Maybe [TrafficMirrorFilterRule],
    -- | Information about the ingress rules that are associated with the Traffic
    -- Mirror filter.
    ingressFilterRules :: Prelude.Maybe [TrafficMirrorFilterRule],
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Prelude.Maybe Prelude.Text,
    -- | The description of the Traffic Mirror filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The network service traffic that is associated with the Traffic Mirror
    -- filter.
    networkServices :: Prelude.Maybe [TrafficMirrorNetworkService]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficMirrorFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'trafficMirrorFilter_tags' - The tags assigned to the Traffic Mirror filter.
--
-- 'egressFilterRules', 'trafficMirrorFilter_egressFilterRules' - Information about the egress rules that are associated with the Traffic
-- Mirror filter.
--
-- 'ingressFilterRules', 'trafficMirrorFilter_ingressFilterRules' - Information about the ingress rules that are associated with the Traffic
-- Mirror filter.
--
-- 'trafficMirrorFilterId', 'trafficMirrorFilter_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- 'description', 'trafficMirrorFilter_description' - The description of the Traffic Mirror filter.
--
-- 'networkServices', 'trafficMirrorFilter_networkServices' - The network service traffic that is associated with the Traffic Mirror
-- filter.
newTrafficMirrorFilter ::
  TrafficMirrorFilter
newTrafficMirrorFilter =
  TrafficMirrorFilter'
    { tags = Prelude.Nothing,
      egressFilterRules = Prelude.Nothing,
      ingressFilterRules = Prelude.Nothing,
      trafficMirrorFilterId = Prelude.Nothing,
      description = Prelude.Nothing,
      networkServices = Prelude.Nothing
    }

-- | The tags assigned to the Traffic Mirror filter.
trafficMirrorFilter_tags :: Lens.Lens' TrafficMirrorFilter (Prelude.Maybe [Tag])
trafficMirrorFilter_tags = Lens.lens (\TrafficMirrorFilter' {tags} -> tags) (\s@TrafficMirrorFilter' {} a -> s {tags = a} :: TrafficMirrorFilter) Prelude.. Lens.mapping Lens.coerced

-- | Information about the egress rules that are associated with the Traffic
-- Mirror filter.
trafficMirrorFilter_egressFilterRules :: Lens.Lens' TrafficMirrorFilter (Prelude.Maybe [TrafficMirrorFilterRule])
trafficMirrorFilter_egressFilterRules = Lens.lens (\TrafficMirrorFilter' {egressFilterRules} -> egressFilterRules) (\s@TrafficMirrorFilter' {} a -> s {egressFilterRules = a} :: TrafficMirrorFilter) Prelude.. Lens.mapping Lens.coerced

-- | Information about the ingress rules that are associated with the Traffic
-- Mirror filter.
trafficMirrorFilter_ingressFilterRules :: Lens.Lens' TrafficMirrorFilter (Prelude.Maybe [TrafficMirrorFilterRule])
trafficMirrorFilter_ingressFilterRules = Lens.lens (\TrafficMirrorFilter' {ingressFilterRules} -> ingressFilterRules) (\s@TrafficMirrorFilter' {} a -> s {ingressFilterRules = a} :: TrafficMirrorFilter) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Traffic Mirror filter.
trafficMirrorFilter_trafficMirrorFilterId :: Lens.Lens' TrafficMirrorFilter (Prelude.Maybe Prelude.Text)
trafficMirrorFilter_trafficMirrorFilterId = Lens.lens (\TrafficMirrorFilter' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@TrafficMirrorFilter' {} a -> s {trafficMirrorFilterId = a} :: TrafficMirrorFilter)

-- | The description of the Traffic Mirror filter.
trafficMirrorFilter_description :: Lens.Lens' TrafficMirrorFilter (Prelude.Maybe Prelude.Text)
trafficMirrorFilter_description = Lens.lens (\TrafficMirrorFilter' {description} -> description) (\s@TrafficMirrorFilter' {} a -> s {description = a} :: TrafficMirrorFilter)

-- | The network service traffic that is associated with the Traffic Mirror
-- filter.
trafficMirrorFilter_networkServices :: Lens.Lens' TrafficMirrorFilter (Prelude.Maybe [TrafficMirrorNetworkService])
trafficMirrorFilter_networkServices = Lens.lens (\TrafficMirrorFilter' {networkServices} -> networkServices) (\s@TrafficMirrorFilter' {} a -> s {networkServices = a} :: TrafficMirrorFilter) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML TrafficMirrorFilter where
  parseXML x =
    TrafficMirrorFilter'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "egressFilterRuleSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "ingressFilterRuleSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "trafficMirrorFilterId")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> ( x Core..@? "networkServiceSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable TrafficMirrorFilter where
  hashWithSalt _salt TrafficMirrorFilter' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` egressFilterRules
      `Prelude.hashWithSalt` ingressFilterRules
      `Prelude.hashWithSalt` trafficMirrorFilterId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` networkServices

instance Prelude.NFData TrafficMirrorFilter where
  rnf TrafficMirrorFilter' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf egressFilterRules
      `Prelude.seq` Prelude.rnf ingressFilterRules
      `Prelude.seq` Prelude.rnf trafficMirrorFilterId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf networkServices
