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
-- Module      : Amazonka.QuickSight.Types.DataPointDrillUpDownOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataPointDrillUpDownOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- | The drill down options for data points in a dashbaord.
--
-- /See:/ 'newDataPointDrillUpDownOption' smart constructor.
data DataPointDrillUpDownOption = DataPointDrillUpDownOption'
  { -- | The status of the drill down options of data points.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPointDrillUpDownOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'dataPointDrillUpDownOption_availabilityStatus' - The status of the drill down options of data points.
newDataPointDrillUpDownOption ::
  DataPointDrillUpDownOption
newDataPointDrillUpDownOption =
  DataPointDrillUpDownOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | The status of the drill down options of data points.
dataPointDrillUpDownOption_availabilityStatus :: Lens.Lens' DataPointDrillUpDownOption (Prelude.Maybe DashboardBehavior)
dataPointDrillUpDownOption_availabilityStatus = Lens.lens (\DataPointDrillUpDownOption' {availabilityStatus} -> availabilityStatus) (\s@DataPointDrillUpDownOption' {} a -> s {availabilityStatus = a} :: DataPointDrillUpDownOption)

instance Data.FromJSON DataPointDrillUpDownOption where
  parseJSON =
    Data.withObject
      "DataPointDrillUpDownOption"
      ( \x ->
          DataPointDrillUpDownOption'
            Prelude.<$> (x Data..:? "AvailabilityStatus")
      )

instance Prelude.Hashable DataPointDrillUpDownOption where
  hashWithSalt _salt DataPointDrillUpDownOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData DataPointDrillUpDownOption where
  rnf DataPointDrillUpDownOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON DataPointDrillUpDownOption where
  toJSON DataPointDrillUpDownOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
