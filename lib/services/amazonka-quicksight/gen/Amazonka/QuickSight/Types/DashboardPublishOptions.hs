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
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardPublishOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AdHocFilteringOption
import Amazonka.QuickSight.Types.DashboardVisualPublishOptions
import Amazonka.QuickSight.Types.DataPointDrillUpDownOption
import Amazonka.QuickSight.Types.DataPointMenuLabelOption
import Amazonka.QuickSight.Types.DataPointTooltipOption
import Amazonka.QuickSight.Types.ExportToCSVOption
import Amazonka.QuickSight.Types.ExportWithHiddenFieldsOption
import Amazonka.QuickSight.Types.SheetControlsOption
import Amazonka.QuickSight.Types.SheetLayoutElementMaximizationOption
import Amazonka.QuickSight.Types.VisualAxisSortOption
import Amazonka.QuickSight.Types.VisualMenuOption

-- | Dashboard publish options.
--
-- /See:/ 'newDashboardPublishOptions' smart constructor.
data DashboardPublishOptions = DashboardPublishOptions'
  { -- | Ad hoc (one-time) filtering option.
    adHocFilteringOption :: Prelude.Maybe AdHocFilteringOption,
    -- | The drill-down options of data points in a dashboard.
    dataPointDrillUpDownOption :: Prelude.Maybe DataPointDrillUpDownOption,
    -- | The data point menu label options of a dashboard.
    dataPointMenuLabelOption :: Prelude.Maybe DataPointMenuLabelOption,
    -- | The data point tool tip options of a dashboard.
    dataPointTooltipOption :: Prelude.Maybe DataPointTooltipOption,
    -- | Export to .csv option.
    exportToCSVOption :: Prelude.Maybe ExportToCSVOption,
    -- | Determines if hidden fields are exported with a dashboard.
    exportWithHiddenFieldsOption :: Prelude.Maybe ExportWithHiddenFieldsOption,
    -- | Sheet controls option.
    sheetControlsOption :: Prelude.Maybe SheetControlsOption,
    -- | The sheet layout maximization options of a dashbaord.
    sheetLayoutElementMaximizationOption :: Prelude.Maybe SheetLayoutElementMaximizationOption,
    -- | The axis sort options of a dashboard.
    visualAxisSortOption :: Prelude.Maybe VisualAxisSortOption,
    -- | The menu options of a visual in a dashboard.
    visualMenuOption :: Prelude.Maybe VisualMenuOption,
    -- | The visual publish options of a visual in a dashboard.
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
-- 'dataPointDrillUpDownOption', 'dashboardPublishOptions_dataPointDrillUpDownOption' - The drill-down options of data points in a dashboard.
--
-- 'dataPointMenuLabelOption', 'dashboardPublishOptions_dataPointMenuLabelOption' - The data point menu label options of a dashboard.
--
-- 'dataPointTooltipOption', 'dashboardPublishOptions_dataPointTooltipOption' - The data point tool tip options of a dashboard.
--
-- 'exportToCSVOption', 'dashboardPublishOptions_exportToCSVOption' - Export to .csv option.
--
-- 'exportWithHiddenFieldsOption', 'dashboardPublishOptions_exportWithHiddenFieldsOption' - Determines if hidden fields are exported with a dashboard.
--
-- 'sheetControlsOption', 'dashboardPublishOptions_sheetControlsOption' - Sheet controls option.
--
-- 'sheetLayoutElementMaximizationOption', 'dashboardPublishOptions_sheetLayoutElementMaximizationOption' - The sheet layout maximization options of a dashbaord.
--
-- 'visualAxisSortOption', 'dashboardPublishOptions_visualAxisSortOption' - The axis sort options of a dashboard.
--
-- 'visualMenuOption', 'dashboardPublishOptions_visualMenuOption' - The menu options of a visual in a dashboard.
--
-- 'visualPublishOptions', 'dashboardPublishOptions_visualPublishOptions' - The visual publish options of a visual in a dashboard.
newDashboardPublishOptions ::
  DashboardPublishOptions
newDashboardPublishOptions =
  DashboardPublishOptions'
    { adHocFilteringOption =
        Prelude.Nothing,
      dataPointDrillUpDownOption = Prelude.Nothing,
      dataPointMenuLabelOption = Prelude.Nothing,
      dataPointTooltipOption = Prelude.Nothing,
      exportToCSVOption = Prelude.Nothing,
      exportWithHiddenFieldsOption = Prelude.Nothing,
      sheetControlsOption = Prelude.Nothing,
      sheetLayoutElementMaximizationOption =
        Prelude.Nothing,
      visualAxisSortOption = Prelude.Nothing,
      visualMenuOption = Prelude.Nothing,
      visualPublishOptions = Prelude.Nothing
    }

-- | Ad hoc (one-time) filtering option.
dashboardPublishOptions_adHocFilteringOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe AdHocFilteringOption)
dashboardPublishOptions_adHocFilteringOption = Lens.lens (\DashboardPublishOptions' {adHocFilteringOption} -> adHocFilteringOption) (\s@DashboardPublishOptions' {} a -> s {adHocFilteringOption = a} :: DashboardPublishOptions)

-- | The drill-down options of data points in a dashboard.
dashboardPublishOptions_dataPointDrillUpDownOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe DataPointDrillUpDownOption)
dashboardPublishOptions_dataPointDrillUpDownOption = Lens.lens (\DashboardPublishOptions' {dataPointDrillUpDownOption} -> dataPointDrillUpDownOption) (\s@DashboardPublishOptions' {} a -> s {dataPointDrillUpDownOption = a} :: DashboardPublishOptions)

-- | The data point menu label options of a dashboard.
dashboardPublishOptions_dataPointMenuLabelOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe DataPointMenuLabelOption)
dashboardPublishOptions_dataPointMenuLabelOption = Lens.lens (\DashboardPublishOptions' {dataPointMenuLabelOption} -> dataPointMenuLabelOption) (\s@DashboardPublishOptions' {} a -> s {dataPointMenuLabelOption = a} :: DashboardPublishOptions)

-- | The data point tool tip options of a dashboard.
dashboardPublishOptions_dataPointTooltipOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe DataPointTooltipOption)
dashboardPublishOptions_dataPointTooltipOption = Lens.lens (\DashboardPublishOptions' {dataPointTooltipOption} -> dataPointTooltipOption) (\s@DashboardPublishOptions' {} a -> s {dataPointTooltipOption = a} :: DashboardPublishOptions)

-- | Export to .csv option.
dashboardPublishOptions_exportToCSVOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe ExportToCSVOption)
dashboardPublishOptions_exportToCSVOption = Lens.lens (\DashboardPublishOptions' {exportToCSVOption} -> exportToCSVOption) (\s@DashboardPublishOptions' {} a -> s {exportToCSVOption = a} :: DashboardPublishOptions)

-- | Determines if hidden fields are exported with a dashboard.
dashboardPublishOptions_exportWithHiddenFieldsOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe ExportWithHiddenFieldsOption)
dashboardPublishOptions_exportWithHiddenFieldsOption = Lens.lens (\DashboardPublishOptions' {exportWithHiddenFieldsOption} -> exportWithHiddenFieldsOption) (\s@DashboardPublishOptions' {} a -> s {exportWithHiddenFieldsOption = a} :: DashboardPublishOptions)

-- | Sheet controls option.
dashboardPublishOptions_sheetControlsOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe SheetControlsOption)
dashboardPublishOptions_sheetControlsOption = Lens.lens (\DashboardPublishOptions' {sheetControlsOption} -> sheetControlsOption) (\s@DashboardPublishOptions' {} a -> s {sheetControlsOption = a} :: DashboardPublishOptions)

-- | The sheet layout maximization options of a dashbaord.
dashboardPublishOptions_sheetLayoutElementMaximizationOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe SheetLayoutElementMaximizationOption)
dashboardPublishOptions_sheetLayoutElementMaximizationOption = Lens.lens (\DashboardPublishOptions' {sheetLayoutElementMaximizationOption} -> sheetLayoutElementMaximizationOption) (\s@DashboardPublishOptions' {} a -> s {sheetLayoutElementMaximizationOption = a} :: DashboardPublishOptions)

-- | The axis sort options of a dashboard.
dashboardPublishOptions_visualAxisSortOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe VisualAxisSortOption)
dashboardPublishOptions_visualAxisSortOption = Lens.lens (\DashboardPublishOptions' {visualAxisSortOption} -> visualAxisSortOption) (\s@DashboardPublishOptions' {} a -> s {visualAxisSortOption = a} :: DashboardPublishOptions)

-- | The menu options of a visual in a dashboard.
dashboardPublishOptions_visualMenuOption :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe VisualMenuOption)
dashboardPublishOptions_visualMenuOption = Lens.lens (\DashboardPublishOptions' {visualMenuOption} -> visualMenuOption) (\s@DashboardPublishOptions' {} a -> s {visualMenuOption = a} :: DashboardPublishOptions)

-- | The visual publish options of a visual in a dashboard.
dashboardPublishOptions_visualPublishOptions :: Lens.Lens' DashboardPublishOptions (Prelude.Maybe DashboardVisualPublishOptions)
dashboardPublishOptions_visualPublishOptions = Lens.lens (\DashboardPublishOptions' {visualPublishOptions} -> visualPublishOptions) (\s@DashboardPublishOptions' {} a -> s {visualPublishOptions = a} :: DashboardPublishOptions)

instance Data.FromJSON DashboardPublishOptions where
  parseJSON =
    Data.withObject
      "DashboardPublishOptions"
      ( \x ->
          DashboardPublishOptions'
            Prelude.<$> (x Data..:? "AdHocFilteringOption")
            Prelude.<*> (x Data..:? "DataPointDrillUpDownOption")
            Prelude.<*> (x Data..:? "DataPointMenuLabelOption")
            Prelude.<*> (x Data..:? "DataPointTooltipOption")
            Prelude.<*> (x Data..:? "ExportToCSVOption")
            Prelude.<*> (x Data..:? "ExportWithHiddenFieldsOption")
            Prelude.<*> (x Data..:? "SheetControlsOption")
            Prelude.<*> (x Data..:? "SheetLayoutElementMaximizationOption")
            Prelude.<*> (x Data..:? "VisualAxisSortOption")
            Prelude.<*> (x Data..:? "VisualMenuOption")
            Prelude.<*> (x Data..:? "VisualPublishOptions")
      )

instance Prelude.Hashable DashboardPublishOptions where
  hashWithSalt _salt DashboardPublishOptions' {..} =
    _salt
      `Prelude.hashWithSalt` adHocFilteringOption
      `Prelude.hashWithSalt` dataPointDrillUpDownOption
      `Prelude.hashWithSalt` dataPointMenuLabelOption
      `Prelude.hashWithSalt` dataPointTooltipOption
      `Prelude.hashWithSalt` exportToCSVOption
      `Prelude.hashWithSalt` exportWithHiddenFieldsOption
      `Prelude.hashWithSalt` sheetControlsOption
      `Prelude.hashWithSalt` sheetLayoutElementMaximizationOption
      `Prelude.hashWithSalt` visualAxisSortOption
      `Prelude.hashWithSalt` visualMenuOption
      `Prelude.hashWithSalt` visualPublishOptions

instance Prelude.NFData DashboardPublishOptions where
  rnf DashboardPublishOptions' {..} =
    Prelude.rnf adHocFilteringOption
      `Prelude.seq` Prelude.rnf dataPointDrillUpDownOption
      `Prelude.seq` Prelude.rnf dataPointMenuLabelOption
      `Prelude.seq` Prelude.rnf dataPointTooltipOption
      `Prelude.seq` Prelude.rnf exportToCSVOption
      `Prelude.seq` Prelude.rnf exportWithHiddenFieldsOption
      `Prelude.seq` Prelude.rnf sheetControlsOption
      `Prelude.seq` Prelude.rnf sheetLayoutElementMaximizationOption
      `Prelude.seq` Prelude.rnf visualAxisSortOption
      `Prelude.seq` Prelude.rnf visualMenuOption
      `Prelude.seq` Prelude.rnf visualPublishOptions

instance Data.ToJSON DashboardPublishOptions where
  toJSON DashboardPublishOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdHocFilteringOption" Data..=)
              Prelude.<$> adHocFilteringOption,
            ("DataPointDrillUpDownOption" Data..=)
              Prelude.<$> dataPointDrillUpDownOption,
            ("DataPointMenuLabelOption" Data..=)
              Prelude.<$> dataPointMenuLabelOption,
            ("DataPointTooltipOption" Data..=)
              Prelude.<$> dataPointTooltipOption,
            ("ExportToCSVOption" Data..=)
              Prelude.<$> exportToCSVOption,
            ("ExportWithHiddenFieldsOption" Data..=)
              Prelude.<$> exportWithHiddenFieldsOption,
            ("SheetControlsOption" Data..=)
              Prelude.<$> sheetControlsOption,
            ("SheetLayoutElementMaximizationOption" Data..=)
              Prelude.<$> sheetLayoutElementMaximizationOption,
            ("VisualAxisSortOption" Data..=)
              Prelude.<$> visualAxisSortOption,
            ("VisualMenuOption" Data..=)
              Prelude.<$> visualMenuOption,
            ("VisualPublishOptions" Data..=)
              Prelude.<$> visualPublishOptions
          ]
      )
