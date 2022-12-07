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
-- Module      : Amazonka.QuickSight.Types.ExportToCSVOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ExportToCSVOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

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

instance Prelude.Hashable ExportToCSVOption where
  hashWithSalt _salt ExportToCSVOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData ExportToCSVOption where
  rnf ExportToCSVOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON ExportToCSVOption where
  toJSON ExportToCSVOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
