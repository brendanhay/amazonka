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
-- Module      : Amazonka.QuickSight.Types.SheetLayoutElementMaximizationOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetLayoutElementMaximizationOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- | The sheet layout maximization options of a dashbaord.
--
-- /See:/ 'newSheetLayoutElementMaximizationOption' smart constructor.
data SheetLayoutElementMaximizationOption = SheetLayoutElementMaximizationOption'
  { -- | The status of the sheet layout maximization options of a dashbaord.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetLayoutElementMaximizationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'sheetLayoutElementMaximizationOption_availabilityStatus' - The status of the sheet layout maximization options of a dashbaord.
newSheetLayoutElementMaximizationOption ::
  SheetLayoutElementMaximizationOption
newSheetLayoutElementMaximizationOption =
  SheetLayoutElementMaximizationOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | The status of the sheet layout maximization options of a dashbaord.
sheetLayoutElementMaximizationOption_availabilityStatus :: Lens.Lens' SheetLayoutElementMaximizationOption (Prelude.Maybe DashboardBehavior)
sheetLayoutElementMaximizationOption_availabilityStatus = Lens.lens (\SheetLayoutElementMaximizationOption' {availabilityStatus} -> availabilityStatus) (\s@SheetLayoutElementMaximizationOption' {} a -> s {availabilityStatus = a} :: SheetLayoutElementMaximizationOption)

instance
  Data.FromJSON
    SheetLayoutElementMaximizationOption
  where
  parseJSON =
    Data.withObject
      "SheetLayoutElementMaximizationOption"
      ( \x ->
          SheetLayoutElementMaximizationOption'
            Prelude.<$> (x Data..:? "AvailabilityStatus")
      )

instance
  Prelude.Hashable
    SheetLayoutElementMaximizationOption
  where
  hashWithSalt
    _salt
    SheetLayoutElementMaximizationOption' {..} =
      _salt `Prelude.hashWithSalt` availabilityStatus

instance
  Prelude.NFData
    SheetLayoutElementMaximizationOption
  where
  rnf SheetLayoutElementMaximizationOption' {..} =
    Prelude.rnf availabilityStatus

instance
  Data.ToJSON
    SheetLayoutElementMaximizationOption
  where
  toJSON SheetLayoutElementMaximizationOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
