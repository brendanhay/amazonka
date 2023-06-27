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
-- Module      : Amazonka.QuickSight.Types.DataPointTooltipOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataPointTooltipOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- | The data point tooltip options.
--
-- /See:/ 'newDataPointTooltipOption' smart constructor.
data DataPointTooltipOption = DataPointTooltipOption'
  { -- | The status of the data point tool tip options.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPointTooltipOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'dataPointTooltipOption_availabilityStatus' - The status of the data point tool tip options.
newDataPointTooltipOption ::
  DataPointTooltipOption
newDataPointTooltipOption =
  DataPointTooltipOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | The status of the data point tool tip options.
dataPointTooltipOption_availabilityStatus :: Lens.Lens' DataPointTooltipOption (Prelude.Maybe DashboardBehavior)
dataPointTooltipOption_availabilityStatus = Lens.lens (\DataPointTooltipOption' {availabilityStatus} -> availabilityStatus) (\s@DataPointTooltipOption' {} a -> s {availabilityStatus = a} :: DataPointTooltipOption)

instance Data.FromJSON DataPointTooltipOption where
  parseJSON =
    Data.withObject
      "DataPointTooltipOption"
      ( \x ->
          DataPointTooltipOption'
            Prelude.<$> (x Data..:? "AvailabilityStatus")
      )

instance Prelude.Hashable DataPointTooltipOption where
  hashWithSalt _salt DataPointTooltipOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData DataPointTooltipOption where
  rnf DataPointTooltipOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON DataPointTooltipOption where
  toJSON DataPointTooltipOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
