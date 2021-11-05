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
-- Module      : Network.AWS.QuickSight.Types.ExportToCSVOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.ExportToCSVOption where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.DashboardBehavior

-- | Export to .csv option.
--
-- /See:/ 'newExportToCSVOption' smart constructor.
data ExportToCSVOption = ExportToCSVOption'
  { -- | Availability status.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportToCSVOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'exportToCSVOption_availabilityStatus' - Availability status.
newExportToCSVOption ::
  ExportToCSVOption
newExportToCSVOption =
  ExportToCSVOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | Availability status.
exportToCSVOption_availabilityStatus :: Lens.Lens' ExportToCSVOption (Prelude.Maybe DashboardBehavior)
exportToCSVOption_availabilityStatus = Lens.lens (\ExportToCSVOption' {availabilityStatus} -> availabilityStatus) (\s@ExportToCSVOption' {} a -> s {availabilityStatus = a} :: ExportToCSVOption)

instance Prelude.Hashable ExportToCSVOption

instance Prelude.NFData ExportToCSVOption

instance Core.ToJSON ExportToCSVOption where
  toJSON ExportToCSVOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Core..=)
              Prelude.<$> availabilityStatus
          ]
      )
