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
-- Module      : Amazonka.QuickSight.Types.ExportWithHiddenFieldsOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ExportWithHiddenFieldsOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardBehavior

-- | Determines whether or not hidden fields are visible on exported
-- dashbaords.
--
-- /See:/ 'newExportWithHiddenFieldsOption' smart constructor.
data ExportWithHiddenFieldsOption = ExportWithHiddenFieldsOption'
  { -- | The status of the export with hidden fields options.
    availabilityStatus :: Prelude.Maybe DashboardBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportWithHiddenFieldsOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityStatus', 'exportWithHiddenFieldsOption_availabilityStatus' - The status of the export with hidden fields options.
newExportWithHiddenFieldsOption ::
  ExportWithHiddenFieldsOption
newExportWithHiddenFieldsOption =
  ExportWithHiddenFieldsOption'
    { availabilityStatus =
        Prelude.Nothing
    }

-- | The status of the export with hidden fields options.
exportWithHiddenFieldsOption_availabilityStatus :: Lens.Lens' ExportWithHiddenFieldsOption (Prelude.Maybe DashboardBehavior)
exportWithHiddenFieldsOption_availabilityStatus = Lens.lens (\ExportWithHiddenFieldsOption' {availabilityStatus} -> availabilityStatus) (\s@ExportWithHiddenFieldsOption' {} a -> s {availabilityStatus = a} :: ExportWithHiddenFieldsOption)

instance Data.FromJSON ExportWithHiddenFieldsOption where
  parseJSON =
    Data.withObject
      "ExportWithHiddenFieldsOption"
      ( \x ->
          ExportWithHiddenFieldsOption'
            Prelude.<$> (x Data..:? "AvailabilityStatus")
      )

instance
  Prelude.Hashable
    ExportWithHiddenFieldsOption
  where
  hashWithSalt _salt ExportWithHiddenFieldsOption' {..} =
    _salt `Prelude.hashWithSalt` availabilityStatus

instance Prelude.NFData ExportWithHiddenFieldsOption where
  rnf ExportWithHiddenFieldsOption' {..} =
    Prelude.rnf availabilityStatus

instance Data.ToJSON ExportWithHiddenFieldsOption where
  toJSON ExportWithHiddenFieldsOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityStatus" Data..=)
              Prelude.<$> availabilityStatus
          ]
      )
