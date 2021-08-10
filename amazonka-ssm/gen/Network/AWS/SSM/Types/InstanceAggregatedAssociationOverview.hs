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
import qualified Network.AWS.Prelude as Prelude

-- | Status information about the aggregated associations.
--
-- /See:/ 'newInstanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
  { -- | Detailed status information about the aggregated associations.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The number of associations for the instance(s).
    instanceAssociationStatusAggregatedCount :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      instanceAssociationStatusAggregatedCount =
        Prelude.Nothing
    }

-- | Detailed status information about the aggregated associations.
instanceAggregatedAssociationOverview_detailedStatus :: Lens.Lens' InstanceAggregatedAssociationOverview (Prelude.Maybe Prelude.Text)
instanceAggregatedAssociationOverview_detailedStatus = Lens.lens (\InstanceAggregatedAssociationOverview' {detailedStatus} -> detailedStatus) (\s@InstanceAggregatedAssociationOverview' {} a -> s {detailedStatus = a} :: InstanceAggregatedAssociationOverview)

-- | The number of associations for the instance(s).
instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount :: Lens.Lens' InstanceAggregatedAssociationOverview (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount = Lens.lens (\InstanceAggregatedAssociationOverview' {instanceAssociationStatusAggregatedCount} -> instanceAssociationStatusAggregatedCount) (\s@InstanceAggregatedAssociationOverview' {} a -> s {instanceAssociationStatusAggregatedCount = a} :: InstanceAggregatedAssociationOverview) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    InstanceAggregatedAssociationOverview
  where
  parseJSON =
    Core.withObject
      "InstanceAggregatedAssociationOverview"
      ( \x ->
          InstanceAggregatedAssociationOverview'
            Prelude.<$> (x Core..:? "DetailedStatus")
            Prelude.<*> ( x
                            Core..:? "InstanceAssociationStatusAggregatedCount"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    InstanceAggregatedAssociationOverview

instance
  Prelude.NFData
    InstanceAggregatedAssociationOverview
