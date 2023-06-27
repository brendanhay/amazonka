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
-- Module      : Amazonka.QuickSight.Types.PivotTableVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableConditionalFormatting
import Amazonka.QuickSight.Types.PivotTableConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A pivot table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/pivot-table.html Using pivot tables>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newPivotTableVisual' smart constructor.
data PivotTableVisual = PivotTableVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe PivotTableConfiguration,
    -- | The conditional formatting for a @PivotTableVisual@.
    conditionalFormatting :: Prelude.Maybe PivotTableConditionalFormatting,
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
-- Create a value of 'PivotTableVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'pivotTableVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'pivotTableVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'conditionalFormatting', 'pivotTableVisual_conditionalFormatting' - The conditional formatting for a @PivotTableVisual@.
--
-- 'subtitle', 'pivotTableVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'pivotTableVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'pivotTableVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newPivotTableVisual ::
  -- | 'visualId'
  Prelude.Text ->
  PivotTableVisual
newPivotTableVisual pVisualId_ =
  PivotTableVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      conditionalFormatting = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
pivotTableVisual_actions :: Lens.Lens' PivotTableVisual (Prelude.Maybe [VisualCustomAction])
pivotTableVisual_actions = Lens.lens (\PivotTableVisual' {actions} -> actions) (\s@PivotTableVisual' {} a -> s {actions = a} :: PivotTableVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
pivotTableVisual_chartConfiguration :: Lens.Lens' PivotTableVisual (Prelude.Maybe PivotTableConfiguration)
pivotTableVisual_chartConfiguration = Lens.lens (\PivotTableVisual' {chartConfiguration} -> chartConfiguration) (\s@PivotTableVisual' {} a -> s {chartConfiguration = a} :: PivotTableVisual)

-- | The conditional formatting for a @PivotTableVisual@.
pivotTableVisual_conditionalFormatting :: Lens.Lens' PivotTableVisual (Prelude.Maybe PivotTableConditionalFormatting)
pivotTableVisual_conditionalFormatting = Lens.lens (\PivotTableVisual' {conditionalFormatting} -> conditionalFormatting) (\s@PivotTableVisual' {} a -> s {conditionalFormatting = a} :: PivotTableVisual)

-- | The subtitle that is displayed on the visual.
pivotTableVisual_subtitle :: Lens.Lens' PivotTableVisual (Prelude.Maybe VisualSubtitleLabelOptions)
pivotTableVisual_subtitle = Lens.lens (\PivotTableVisual' {subtitle} -> subtitle) (\s@PivotTableVisual' {} a -> s {subtitle = a} :: PivotTableVisual)

-- | The title that is displayed on the visual.
pivotTableVisual_title :: Lens.Lens' PivotTableVisual (Prelude.Maybe VisualTitleLabelOptions)
pivotTableVisual_title = Lens.lens (\PivotTableVisual' {title} -> title) (\s@PivotTableVisual' {} a -> s {title = a} :: PivotTableVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
pivotTableVisual_visualId :: Lens.Lens' PivotTableVisual Prelude.Text
pivotTableVisual_visualId = Lens.lens (\PivotTableVisual' {visualId} -> visualId) (\s@PivotTableVisual' {} a -> s {visualId = a} :: PivotTableVisual)

instance Data.FromJSON PivotTableVisual where
  parseJSON =
    Data.withObject
      "PivotTableVisual"
      ( \x ->
          PivotTableVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> (x Data..:? "ConditionalFormatting")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable PivotTableVisual where
  hashWithSalt _salt PivotTableVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` conditionalFormatting
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData PivotTableVisual where
  rnf PivotTableVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf conditionalFormatting
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId

instance Data.ToJSON PivotTableVisual where
  toJSON PivotTableVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("ChartConfiguration" Data..=)
              Prelude.<$> chartConfiguration,
            ("ConditionalFormatting" Data..=)
              Prelude.<$> conditionalFormatting,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId)
          ]
      )
