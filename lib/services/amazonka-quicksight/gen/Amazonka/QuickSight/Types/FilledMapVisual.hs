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
-- Module      : Amazonka.QuickSight.Types.FilledMapVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.FilledMapConditionalFormatting
import Amazonka.QuickSight.Types.FilledMapConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A filled map.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/filled-maps.html Creating filled maps>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newFilledMapVisual' smart constructor.
data FilledMapVisual = FilledMapVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe FilledMapConfiguration,
    -- | The column hierarchy that is used during drill-downs and drill-ups.
    columnHierarchies :: Prelude.Maybe [ColumnHierarchy],
    -- | The conditional formatting of a @FilledMapVisual@.
    conditionalFormatting :: Prelude.Maybe FilledMapConditionalFormatting,
    -- | The subtitle that is displayed on the visual.
    subtitle :: Prelude.Maybe VisualSubtitleLabelOptions,
    -- | The title that is displayed on the visual.
    title :: Prelude.Maybe VisualTitleLabelOptions,
    -- | The unique identifier of a visual. This identifier must be unique within
    -- the context of a dashboard, template, or analysis. Two dashboards,
    -- analyses, or templates can have visuals with the same identifiers..
    visualId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'filledMapVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'filledMapVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'filledMapVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'conditionalFormatting', 'filledMapVisual_conditionalFormatting' - The conditional formatting of a @FilledMapVisual@.
--
-- 'subtitle', 'filledMapVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'filledMapVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'filledMapVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newFilledMapVisual ::
  -- | 'visualId'
  Prelude.Text ->
  FilledMapVisual
newFilledMapVisual pVisualId_ =
  FilledMapVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      conditionalFormatting = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
filledMapVisual_actions :: Lens.Lens' FilledMapVisual (Prelude.Maybe [VisualCustomAction])
filledMapVisual_actions = Lens.lens (\FilledMapVisual' {actions} -> actions) (\s@FilledMapVisual' {} a -> s {actions = a} :: FilledMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
filledMapVisual_chartConfiguration :: Lens.Lens' FilledMapVisual (Prelude.Maybe FilledMapConfiguration)
filledMapVisual_chartConfiguration = Lens.lens (\FilledMapVisual' {chartConfiguration} -> chartConfiguration) (\s@FilledMapVisual' {} a -> s {chartConfiguration = a} :: FilledMapVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
filledMapVisual_columnHierarchies :: Lens.Lens' FilledMapVisual (Prelude.Maybe [ColumnHierarchy])
filledMapVisual_columnHierarchies = Lens.lens (\FilledMapVisual' {columnHierarchies} -> columnHierarchies) (\s@FilledMapVisual' {} a -> s {columnHierarchies = a} :: FilledMapVisual) Prelude.. Lens.mapping Lens.coerced

-- | The conditional formatting of a @FilledMapVisual@.
filledMapVisual_conditionalFormatting :: Lens.Lens' FilledMapVisual (Prelude.Maybe FilledMapConditionalFormatting)
filledMapVisual_conditionalFormatting = Lens.lens (\FilledMapVisual' {conditionalFormatting} -> conditionalFormatting) (\s@FilledMapVisual' {} a -> s {conditionalFormatting = a} :: FilledMapVisual)

-- | The subtitle that is displayed on the visual.
filledMapVisual_subtitle :: Lens.Lens' FilledMapVisual (Prelude.Maybe VisualSubtitleLabelOptions)
filledMapVisual_subtitle = Lens.lens (\FilledMapVisual' {subtitle} -> subtitle) (\s@FilledMapVisual' {} a -> s {subtitle = a} :: FilledMapVisual)

-- | The title that is displayed on the visual.
filledMapVisual_title :: Lens.Lens' FilledMapVisual (Prelude.Maybe VisualTitleLabelOptions)
filledMapVisual_title = Lens.lens (\FilledMapVisual' {title} -> title) (\s@FilledMapVisual' {} a -> s {title = a} :: FilledMapVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
filledMapVisual_visualId :: Lens.Lens' FilledMapVisual Prelude.Text
filledMapVisual_visualId = Lens.lens (\FilledMapVisual' {visualId} -> visualId) (\s@FilledMapVisual' {} a -> s {visualId = a} :: FilledMapVisual)

instance Data.FromJSON FilledMapVisual where
  parseJSON =
    Data.withObject
      "FilledMapVisual"
      ( \x ->
          FilledMapVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ConditionalFormatting")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable FilledMapVisual where
  hashWithSalt _salt FilledMapVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` conditionalFormatting
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData FilledMapVisual where
  rnf FilledMapVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf conditionalFormatting
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON FilledMapVisual where
  toJSON FilledMapVisual' {..} =
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
