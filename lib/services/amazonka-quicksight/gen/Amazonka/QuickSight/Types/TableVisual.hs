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
-- Module      : Amazonka.QuickSight.Types.TableVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableConditionalFormatting
import Amazonka.QuickSight.Types.TableConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A table visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/tabular.html Using tables as visuals>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newTableVisual' smart constructor.
data TableVisual = TableVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration settings of the visual.
    chartConfiguration :: Prelude.Maybe TableConfiguration,
    -- | The conditional formatting for a @PivotTableVisual@.
    conditionalFormatting :: Prelude.Maybe TableConditionalFormatting,
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
-- Create a value of 'TableVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'tableVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'tableVisual_chartConfiguration' - The configuration settings of the visual.
--
-- 'conditionalFormatting', 'tableVisual_conditionalFormatting' - The conditional formatting for a @PivotTableVisual@.
--
-- 'subtitle', 'tableVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'tableVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'tableVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
newTableVisual ::
  -- | 'visualId'
  Prelude.Text ->
  TableVisual
newTableVisual pVisualId_ =
  TableVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      conditionalFormatting = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_
    }

-- | The list of custom actions that are configured for a visual.
tableVisual_actions :: Lens.Lens' TableVisual (Prelude.Maybe [VisualCustomAction])
tableVisual_actions = Lens.lens (\TableVisual' {actions} -> actions) (\s@TableVisual' {} a -> s {actions = a} :: TableVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration settings of the visual.
tableVisual_chartConfiguration :: Lens.Lens' TableVisual (Prelude.Maybe TableConfiguration)
tableVisual_chartConfiguration = Lens.lens (\TableVisual' {chartConfiguration} -> chartConfiguration) (\s@TableVisual' {} a -> s {chartConfiguration = a} :: TableVisual)

-- | The conditional formatting for a @PivotTableVisual@.
tableVisual_conditionalFormatting :: Lens.Lens' TableVisual (Prelude.Maybe TableConditionalFormatting)
tableVisual_conditionalFormatting = Lens.lens (\TableVisual' {conditionalFormatting} -> conditionalFormatting) (\s@TableVisual' {} a -> s {conditionalFormatting = a} :: TableVisual)

-- | The subtitle that is displayed on the visual.
tableVisual_subtitle :: Lens.Lens' TableVisual (Prelude.Maybe VisualSubtitleLabelOptions)
tableVisual_subtitle = Lens.lens (\TableVisual' {subtitle} -> subtitle) (\s@TableVisual' {} a -> s {subtitle = a} :: TableVisual)

-- | The title that is displayed on the visual.
tableVisual_title :: Lens.Lens' TableVisual (Prelude.Maybe VisualTitleLabelOptions)
tableVisual_title = Lens.lens (\TableVisual' {title} -> title) (\s@TableVisual' {} a -> s {title = a} :: TableVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers..
tableVisual_visualId :: Lens.Lens' TableVisual Prelude.Text
tableVisual_visualId = Lens.lens (\TableVisual' {visualId} -> visualId) (\s@TableVisual' {} a -> s {visualId = a} :: TableVisual)

instance Data.FromJSON TableVisual where
  parseJSON =
    Data.withObject
      "TableVisual"
      ( \x ->
          TableVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> (x Data..:? "ConditionalFormatting")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
      )

instance Prelude.Hashable TableVisual where
  hashWithSalt _salt TableVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` conditionalFormatting
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId

instance Prelude.NFData TableVisual where
  rnf TableVisual' {..} =
    Prelude.rnf actions `Prelude.seq`
      Prelude.rnf chartConfiguration `Prelude.seq`
        Prelude.rnf conditionalFormatting `Prelude.seq`
          Prelude.rnf subtitle `Prelude.seq`
            Prelude.rnf title `Prelude.seq`
              Prelude.rnf visualId

instance Data.ToJSON TableVisual where
  toJSON TableVisual' {..} =
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
