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
-- Module      : Amazonka.QuickSight.Types.WordCloudVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WordCloudVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnHierarchy
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions
import Amazonka.QuickSight.Types.WordCloudChartConfiguration

-- | A word cloud.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/word-cloud.html Using word clouds>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newWordCloudVisual' smart constructor.
data WordCloudVisual = WordCloudVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe WordCloudChartConfiguration,
    -- | The column hierarchy that is used during drill-downs and drill-ups.
    columnHierarchies :: Prelude.Maybe [ColumnHierarchy],
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
-- Create a value of 'WordCloudVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'wordCloudVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'wordCloudVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'columnHierarchies', 'wordCloudVisual_columnHierarchies' - The column hierarchy that is used during drill-downs and drill-ups.
--
-- 'subtitle', 'wordCloudVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'wordCloudVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'wordCloudVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newWordCloudVisual ::
  -- | 'visualId'
  Prelude.Text ->
  WordCloudVisual
newWordCloudVisual pVisualId_ =
  WordCloudVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      columnHierarchies = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
wordCloudVisual_actions :: Lens.Lens' WordCloudVisual (Prelude.Maybe [VisualCustomAction])
wordCloudVisual_actions = Lens.lens (\WordCloudVisual' {actions} -> actions) (\s@WordCloudVisual' {} a -> s {actions = a} :: WordCloudVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
wordCloudVisual_chartConfiguration :: Lens.Lens' WordCloudVisual (Prelude.Maybe WordCloudChartConfiguration)
wordCloudVisual_chartConfiguration = Lens.lens (\WordCloudVisual' {chartConfiguration} -> chartConfiguration) (\s@WordCloudVisual' {} a -> s {chartConfiguration = a} :: WordCloudVisual)

-- | The column hierarchy that is used during drill-downs and drill-ups.
wordCloudVisual_columnHierarchies :: Lens.Lens' WordCloudVisual (Prelude.Maybe [ColumnHierarchy])
wordCloudVisual_columnHierarchies = Lens.lens (\WordCloudVisual' {columnHierarchies} -> columnHierarchies) (\s@WordCloudVisual' {} a -> s {columnHierarchies = a} :: WordCloudVisual) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle that is displayed on the visual.
wordCloudVisual_subtitle :: Lens.Lens' WordCloudVisual (Prelude.Maybe VisualSubtitleLabelOptions)
wordCloudVisual_subtitle = Lens.lens (\WordCloudVisual' {subtitle} -> subtitle) (\s@WordCloudVisual' {} a -> s {subtitle = a} :: WordCloudVisual)

-- | The title that is displayed on the visual.
wordCloudVisual_title :: Lens.Lens' WordCloudVisual (Prelude.Maybe VisualTitleLabelOptions)
wordCloudVisual_title = Lens.lens (\WordCloudVisual' {title} -> title) (\s@WordCloudVisual' {} a -> s {title = a} :: WordCloudVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
wordCloudVisual_visualId :: Lens.Lens' WordCloudVisual Prelude.Text
wordCloudVisual_visualId = Lens.lens (\WordCloudVisual' {visualId} -> visualId) (\s@WordCloudVisual' {} a -> s {visualId = a} :: WordCloudVisual)

instance Data.FromJSON WordCloudVisual where
  parseJSON =
    Data.withObject
      "WordCloudVisual"
      ( \x ->
          WordCloudVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> ( x Data..:? "ColumnHierarchies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable WordCloudVisual where
  hashWithSalt _salt WordCloudVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` columnHierarchies
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData WordCloudVisual where
  rnf WordCloudVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf columnHierarchies
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON WordCloudVisual where
  toJSON WordCloudVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("ChartConfiguration" Data..=)
              Prelude.<$> chartConfiguration,
            ("ColumnHierarchies" Data..=)
              Prelude.<$> columnHierarchies,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId)
          ]
      )
