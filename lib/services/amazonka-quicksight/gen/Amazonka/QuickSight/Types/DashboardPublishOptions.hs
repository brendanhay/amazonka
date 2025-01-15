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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardPublishOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AdHocFilteringOption
import Amazonka.QuickSight.Types.DashboardVisualPublishOptions
import Amazonka.QuickSight.Types.ExportToCSVOption
import Amazonka.QuickSight.Types.SheetControlsOption

-- | Dashboard publish options.
--
-- /See:/ 'newDashboardPublishOptions' smart constructor.
data DashboardPublishOptions = DashboardPublishOptions'
  { -- | Ad hoc (one-time) filtering option.
    adHocFilteringOption :: Prelude.Maybe AdHocFilteringOption,
    -- | Export to .csv option.
    exportToCSVOption :: Prelude.Maybe ExportToCSVOption,
    -- | Sheet controls option.
    sheetControlsOption :: Prelude.Maybe SheetControlsOption,
    visualPublishOptions :: Prelude.Maybe DashboardVisualPublishOptions
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
-- 'exportToCSVOption', 'dashboardPublishOptions_exportToCSVOption' - Export to .csv option.
--
-- 'sheetControlsOption', 'dashboardPublishOptions_sheetControlsOption' - Sheet controls option.
--
-- 'visualPublishOptions', 'dashboardPublishOptions_visualPublishOptions' -
newDashboardPublishOptions ::
  DashboardPublishOptions
newDashboardPublishOptions =
  DashboardPublishOptions'
    { adHocFilteringOption =
        Prelude.Nothing,
      exportToCSVOption = Prelude.Nothing,
      sheetControlsOption = Prelude.Nothing,
      visualPublishOptions = Prelude.Nothing
    }

-- | Ad hoc (one-time) filtering option.
dashboardPublishOptions_adHocFilteringOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe AdHocFilteringOption)
dashboardPublishOptions_adHocFilteringOption = Lens.lens (\DashboardPublishOptions' {adHocFilteringOption} -> adHocFilteringOption) (\s@DashboardPublishOptions' {} a -> s {adHocFilteringOption = a} :: DashboardPublishOptions)

-- | Export to .csv option.
dashboardPublishOptions_exportToCSVOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe ExportToCSVOption)
dashboardPublishOptions_exportToCSVOption = Lens.lens (\DashboardPublishOptions' {exportToCSVOption} -> exportToCSVOption) (\s@DashboardPublishOptions' {} a -> s {exportToCSVOption = a} :: DashboardPublishOptions)

-- | Sheet controls option.
dashboardPublishOptions_sheetControlsOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe SheetControlsOption)
dashboardPublishOptions_sheetControlsOption = Lens.lens (\DashboardPublishOptions' {sheetControlsOption} -> sheetControlsOption) (\s@DashboardPublishOptions' {} a -> s {sheetControlsOption = a} :: DashboardPublishOptions)

dashboardPublishOptions_visualPublishOptions :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe DashboardVisualPublishOptions)
dashboardPublishOptions_visualPublishOptions = Lens.lens (\DashboardPublishOptions' {visualPublishOptions} -> visualPublishOptions) (\s@DashboardPublishOptions' {} a -> s {visualPublishOptions = a} :: DashboardPublishOptions)

instance Prelude.Hashable DashboardPublishOptions where
  hashWithSalt _salt DashboardPublishOptions' {..} =
    _salt
      `Prelude.hashWithSalt` adHocFilteringOption
      `Prelude.hashWithSalt` exportToCSVOption
      `Prelude.hashWithSalt` sheetControlsOption
      `Prelude.hashWithSalt` visualPublishOptions

instance Prelude.NFData DashboardPublishOptions where
  rnf DashboardPublishOptions' {..} =
    Prelude.rnf adHocFilteringOption `Prelude.seq`
      Prelude.rnf exportToCSVOption `Prelude.seq`
        Prelude.rnf sheetControlsOption `Prelude.seq`
          Prelude.rnf visualPublishOptions

instance Data.ToJSON DashboardPublishOptions where
  toJSON DashboardPublishOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdHocFilteringOption" Data..=)
              Prelude.<$> adHocFilteringOption,
            ("ExportToCSVOption" Data..=)
              Prelude.<$> exportToCSVOption,
            ("SheetControlsOption" Data..=)
              Prelude.<$> sheetControlsOption,
            ("VisualPublishOptions" Data..=)
              Prelude.<$> visualPublishOptions
          ]
      )
