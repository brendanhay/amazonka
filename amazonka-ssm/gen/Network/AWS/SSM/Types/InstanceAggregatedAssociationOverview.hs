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
-- Module      : Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Status information about the aggregated associations.
--
-- /See:/ 'newInstanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
  { -- | Detailed status information about the aggregated associations.
    detailedStatus :: Core.Maybe Core.Text,
    -- | The number of associations for the instance(s).
    instanceAssociationStatusAggregatedCount :: Core.Maybe (Core.HashMap Core.Text Core.Int)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceAggregatedAssociationOverview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detailedStatus', 'instanceAggregatedAssociationOverview_detailedStatus' - Detailed status information about the aggregated associations.
--
-- 'instanceAssociationStatusAggregatedCount', 'instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount' - The number of associations for the instance(s).
newInstanceAggregatedAssociationOverview ::
  InstanceAggregatedAssociationOverview
newInstanceAggregatedAssociationOverview =
  InstanceAggregatedAssociationOverview'
    { detailedStatus =
        Core.Nothing,
      instanceAssociationStatusAggregatedCount =
        Core.Nothing
    }

-- | Detailed status information about the aggregated associations.
instanceAggregatedAssociationOverview_detailedStatus :: Lens.Lens' InstanceAggregatedAssociationOverview (Core.Maybe Core.Text)
instanceAggregatedAssociationOverview_detailedStatus = Lens.lens (\InstanceAggregatedAssociationOverview' {detailedStatus} -> detailedStatus) (\s@InstanceAggregatedAssociationOverview' {} a -> s {detailedStatus = a} :: InstanceAggregatedAssociationOverview)

-- | The number of associations for the instance(s).
instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount :: Lens.Lens' InstanceAggregatedAssociationOverview (Core.Maybe (Core.HashMap Core.Text Core.Int))
instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount = Lens.lens (\InstanceAggregatedAssociationOverview' {instanceAssociationStatusAggregatedCount} -> instanceAssociationStatusAggregatedCount) (\s@InstanceAggregatedAssociationOverview' {} a -> s {instanceAssociationStatusAggregatedCount = a} :: InstanceAggregatedAssociationOverview) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    InstanceAggregatedAssociationOverview
  where
  parseJSON =
    Core.withObject
      "InstanceAggregatedAssociationOverview"
      ( \x ->
          InstanceAggregatedAssociationOverview'
            Core.<$> (x Core..:? "DetailedStatus")
            Core.<*> ( x
                         Core..:? "InstanceAssociationStatusAggregatedCount"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    InstanceAggregatedAssociationOverview

instance
  Core.NFData
    InstanceAggregatedAssociationOverview
