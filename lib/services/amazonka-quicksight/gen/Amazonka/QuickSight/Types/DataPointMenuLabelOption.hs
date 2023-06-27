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
-- Module      : Amazonka.QuickSight.Types.DataPointMenuLabelOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataPointMenuLabelOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- | The data point menu options of a dashboard.
--
-- /See:/ 'newDataPointMenuLabelOption' smart constructor.
data DataPointMenuLabelOption = DataPointMenuLabelOption'
  { -- | The status of the data point menu options.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataPointMenuLabelOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'dataPointMenuLabelOption_availabilityStatus' - The status of the data point menu options.
newDataPointMenuLabelOption ::
  DataPointMenuLabelOption
newDataPointMenuLabelOption =
  DataPointMenuLabelOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | The status of the data point menu options.
dataPointMenuLabelOption_availabilityStatus :: Lens.Lens' DataPointMenuLabelOption (Prelude.Maybe DashboardBehavior)
dataPointMenuLabelOption_availabilityStatus = Lens.lens (\DataPointMenuLabelOption' {availabilityStatus} -> availabilityStatus) (\s@DataPointMenuLabelOption' {} a -> s {availabilityStatus = a} :: DataPointMenuLabelOption)

instance Data.FromJSON DataPointMenuLabelOption where
  parseJSON =
    Data.withObject
      "DataPointMenuLabelOption"
      ( \x ->
          DataPointMenuLabelOption'
            Prelude.<$> (x Data..:? "AvailabilityStatus")
      )

instance Prelude.Hashable DataPointMenuLabelOption where
  hashWithSalt _salt DataPointMenuLabelOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData DataPointMenuLabelOption where
  rnf DataPointMenuLabelOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON DataPointMenuLabelOption where
  toJSON DataPointMenuLabelOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
