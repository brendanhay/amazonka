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
-- Module      : Amazonka.QuickSight.Types.DashboardPublishOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardPublishOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AdHocFilteringOption
import Amazonka.QuickSight.Types.ExportToCSVOption
import Amazonka.QuickSight.Types.SheetControlsOption

-- | Dashboard publish options.
--
-- /See:/ 'newDashboardPublishOptions' smart constructor.
data DashboardPublishOptions = DashboardPublishOptions'
  { -- | Ad hoc (one-time) filtering option.
    adHocFilteringOption :: Prelude.Maybe AdHocFilteringOption,
    -- | Sheet controls option.
    sheetControlsOption :: Prelude.Maybe SheetControlsOption,
    -- | Export to .csv option.
    exportToCSVOption :: Prelude.Maybe ExportToCSVOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardPublishOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adHocFilteringOption', 'dashboardPublishOptions_adHocFilteringOption' - Ad hoc (one-time) filtering option.
--
-- 'sheetControlsOption', 'dashboardPublishOptions_sheetControlsOption' - Sheet controls option.
--
-- 'exportToCSVOption', 'dashboardPublishOptions_exportToCSVOption' - Export to .csv option.
newDashboardPublishOptions ::
  DashboardPublishOptions
newDashboardPublishOptions =
  DashboardPublishOptions'
    { adHocFilteringOption =
        Prelude.Nothing,
      sheetControlsOption = Prelude.Nothing,
      exportToCSVOption = Prelude.Nothing
    }

-- | Ad hoc (one-time) filtering option.
dashboardPublishOptions_adHocFilteringOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe AdHocFilteringOption)
dashboardPublishOptions_adHocFilteringOption = Lens.lens (\DashboardPublishOptions' {adHocFilteringOption} -> adHocFilteringOption) (\s@DashboardPublishOptions' {} a -> s {adHocFilteringOption = a} :: DashboardPublishOptions)

-- | Sheet controls option.
dashboardPublishOptions_sheetControlsOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe SheetControlsOption)
dashboardPublishOptions_sheetControlsOption = Lens.lens (\DashboardPublishOptions' {sheetControlsOption} -> sheetControlsOption) (\s@DashboardPublishOptions' {} a -> s {sheetControlsOption = a} :: DashboardPublishOptions)

-- | Export to .csv option.
dashboardPublishOptions_exportToCSVOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe ExportToCSVOption)
dashboardPublishOptions_exportToCSVOption = Lens.lens (\DashboardPublishOptions' {exportToCSVOption} -> exportToCSVOption) (\s@DashboardPublishOptions' {} a -> s {exportToCSVOption = a} :: DashboardPublishOptions)

instance Prelude.Hashable DashboardPublishOptions where
  hashWithSalt _salt DashboardPublishOptions' {..} =
    _salt `Prelude.hashWithSalt` adHocFilteringOption
      `Prelude.hashWithSalt` sheetControlsOption
      `Prelude.hashWithSalt` exportToCSVOption

instance Prelude.NFData DashboardPublishOptions where
  rnf DashboardPublishOptions' {..} =
    Prelude.rnf adHocFilteringOption
      `Prelude.seq` Prelude.rnf sheetControlsOption
      `Prelude.seq` Prelude.rnf exportToCSVOption

instance Data.ToJSON DashboardPublishOptions where
  toJSON DashboardPublishOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdHocFilteringOption" Data..=)
              Prelude.<$> adHocFilteringOption,
            ("SheetControlsOption" Data..=)
              Prelude.<$> sheetControlsOption,
            ("ExportToCSVOption" Data..=)
              Prelude.<$> exportToCSVOption
          ]
      )
