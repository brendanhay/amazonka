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
-- Module      : Amazonka.SSM.Types.InstanceAggregatedAssociationOverview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceAggregatedAssociationOverview where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Status information about the aggregated associations.
--
-- /See:/ 'newInstanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
  { -- | Detailed status information about the aggregated associations.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | The number of associations for the managed node(s).
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
-- 'instanceAssociationStatusAggregatedCount', 'instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount' - The number of associations for the managed node(s).
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

-- | The number of associations for the managed node(s).
instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount :: Lens.Lens' InstanceAggregatedAssociationOverview (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Int))
instanceAggregatedAssociationOverview_instanceAssociationStatusAggregatedCount = Lens.lens (\InstanceAggregatedAssociationOverview' {instanceAssociationStatusAggregatedCount} -> instanceAssociationStatusAggregatedCount) (\s@InstanceAggregatedAssociationOverview' {} a -> s {instanceAssociationStatusAggregatedCount = a} :: InstanceAggregatedAssociationOverview) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    InstanceAggregatedAssociationOverview
  where
  parseJSON =
    Data.withObject
      "InstanceAggregatedAssociationOverview"
      ( \x ->
          InstanceAggregatedAssociationOverview'
            Prelude.<$> (x Data..:? "DetailedStatus")
            Prelude.<*> ( x
                            Data..:? "InstanceAssociationStatusAggregatedCount"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    InstanceAggregatedAssociationOverview
  where
  hashWithSalt
    _salt
    InstanceAggregatedAssociationOverview' {..} =
      _salt `Prelude.hashWithSalt` detailedStatus
        `Prelude.hashWithSalt` instanceAssociationStatusAggregatedCount

instance
  Prelude.NFData
    InstanceAggregatedAssociationOverview
  where
  rnf InstanceAggregatedAssociationOverview' {..} =
    Prelude.rnf detailedStatus
      `Prelude.seq` Prelude.rnf instanceAssociationStatusAggregatedCount
