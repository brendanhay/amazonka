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
-- Module      : Amazonka.QuickSight.Types.KPIVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.KPIConditionalFormatting
import Amazonka.QuickSight.Types.KPIConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A key performance indicator (KPI).
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/kpi.html Using KPIs>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newKPIVisual' smart constructor.
data KPIVisual = KPIVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a KPI visual.
    chartConfiguration :: Prelude.Maybe KPIConfiguration,
    -- | The column hierarchy that is used during drill-downs and drill-ups.
    columnHierarchies :: Prelude.Maybe [ColumnHierarchy],
    -- | The conditional formatting of a KPI visual.
    conditionalFormatting :: Prelude.Maybe KPIConditionalFormatting,
    -- | The subtitle that is displayed on the visual.
    subtitle :: Prelude.Maybe VisualSubtitleLabelOptions,
    -- | The title that is displayed on the visual.
    title :: Prelude.Maybe VisualTitleLabelOptions,
    -- | The unique identifier of a visual. This identifier must be unique within
    -- the context of a dashboard, template, or analysis. Two dashboards,
    -- analyses, or templates can have visuals with the same identifiers.
    visualId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'kPIVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'kPIVisual_chartConfiguration' - The configuration of a KPI visual.
--
-- 'columnHierarchies', 'kPIVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'conditionalFormatting', 'kPIVisual_conditionalFormatting' - The conditional formatting of a KPI visual.
--
-- 'subtitle', 'kPIVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'kPIVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'kPIVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
newKPIVisual ::
  -- | 'visualId'
  Prelude.Text ->
  KPIVisual
newKPIVisual pVisualId_ =
  KPIVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      conditionalFormatting = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
kPIVisual_actions :: Lens.Lens' KPIVisual (Prelude.Maybe [VisualCustomAction])
kPIVisual_actions = Lens.lens (\KPIVisual' {actions} -> actions) (\s@KPIVisual' {} a -> s {actions = a} :: KPIVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a KPI visual.
kPIVisual_chartConfiguration :: Lens.Lens' KPIVisual (Prelude.Maybe KPIConfiguration)
kPIVisual_chartConfiguration = Lens.lens (\KPIVisual' {chartConfiguration} -> chartConfiguration) (\s@KPIVisual' {} a -> s {chartConfiguration = a} :: KPIVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
kPIVisual_columnHierarchies :: Lens.Lens' KPIVisual (Prelude.Maybe [ColumnHierarchy])
kPIVisual_columnHierarchies = Lens.lens (\KPIVisual' {columnHierarchies} -> columnHierarchies) (\s@KPIVisual' {} a -> s {columnHierarchies = a} :: KPIVisual) Prelude.. Lens.mapping Lens.coerced

-- | The conditional formatting of a KPI visual.
kPIVisual_conditionalFormatting :: Lens.Lens' KPIVisual (Prelude.Maybe KPIConditionalFormatting)
kPIVisual_conditionalFormatting = Lens.lens (\KPIVisual' {conditionalFormatting} -> conditionalFormatting) (\s@KPIVisual' {} a -> s {conditionalFormatting = a} :: KPIVisual)

-- | The subtitle that is displayed on the visual.
kPIVisual_subtitle :: Lens.Lens' KPIVisual (Prelude.Maybe VisualSubtitleLabelOptions)
kPIVisual_subtitle = Lens.lens (\KPIVisual' {subtitle} -> subtitle) (\s@KPIVisual' {} a -> s {subtitle = a} :: KPIVisual)

-- | The title that is displayed on the visual.
kPIVisual_title :: Lens.Lens' KPIVisual (Prelude.Maybe VisualTitleLabelOptions)
kPIVisual_title = Lens.lens (\KPIVisual' {title} -> title) (\s@KPIVisual' {} a -> s {title = a} :: KPIVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
kPIVisual_visualId :: Lens.Lens' KPIVisual Prelude.Text
kPIVisual_visualId = Lens.lens (\KPIVisual' {visualId} -> visualId) (\s@KPIVisual' {} a -> s {visualId = a} :: KPIVisual)

instance Data.FromJSON KPIVisual where
  parseJSON =
    Data.withObject
      "KPIVisual"
      ( \x ->
          KPIVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x
                            Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ConditionalFormatting")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable KPIVisual where
  hashWithSalt _salt KPIVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` conditionalFormatting
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData KPIVisual where
  rnf KPIVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf conditionalFormatting
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON KPIVisual where
  toJSON KPIVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("ChartConfiguration" Data..=)
              Prelude.<$> chartConfiguration,
            ("ColumnHierarchies" Data..=)
              Prelude.<$> columnHierarchies,
            ("ConditionalFormatting" Data..=)
              Prelude.<$> conditionalFormatting,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId)
          ]
      )
