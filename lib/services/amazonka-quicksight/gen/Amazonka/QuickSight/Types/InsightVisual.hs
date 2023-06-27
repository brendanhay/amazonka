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
-- Module      : Amazonka.QuickSight.Types.InsightVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.InsightVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.InsightConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | An insight visual.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/computational-insights.html Working with insights>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newInsightVisual' smart constructor.
data InsightVisual = InsightVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of an insight visual.
    insightConfiguration :: Prelude.Maybe InsightConfiguration,
    -- | The subtitle that is displayed on the visual.
    subtitle :: Prelude.Maybe VisualSubtitleLabelOptions,
    -- | The title that is displayed on the visual.
    title :: Prelude.Maybe VisualTitleLabelOptions,
    -- | The unique identifier of a visual. This identifier must be unique within
    -- the context of a dashboard, template, or analysis. Two dashboards,
    -- analyses, or templates can have visuals with the same identifiers.
    visualId :: Prelude.Text,
    -- | The dataset that is used in the insight visual.
    dataSetIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'insightVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'insightConfiguration', 'insightVisual_insightConfiguration' - The configuration of an insight visual.
--
-- 'subtitle', 'insightVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'insightVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'insightVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
--
-- 'dataSetIdentifier', 'insightVisual_dataSetIdentifier' - The dataset that is used in the insight visual.
newInsightVisual ::
  -- | 'visualId'
  Prelude.Text ->
  -- | 'dataSetIdentifier'
  Prelude.Text ->
  InsightVisual
newInsightVisual pVisualId_ pDataSetIdentifier_ =
  InsightVisual'
    { actions = Prelude.Nothing,
      insightConfiguration = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_,
      dataSetIdentifier = pDataSetIdentifier_
    }

-- | The list of custom actions that are configured for a visual.
insightVisual_actions :: Lens.Lens' InsightVisual (Prelude.Maybe [VisualCustomAction])
insightVisual_actions = Lens.lens (\InsightVisual' {actions} -> actions) (\s@InsightVisual' {} a -> s {actions = a} :: InsightVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of an insight visual.
insightVisual_insightConfiguration :: Lens.Lens' InsightVisual (Prelude.Maybe InsightConfiguration)
insightVisual_insightConfiguration = Lens.lens (\InsightVisual' {insightConfiguration} -> insightConfiguration) (\s@InsightVisual' {} a -> s {insightConfiguration = a} :: InsightVisual)

-- | The subtitle that is displayed on the visual.
insightVisual_subtitle :: Lens.Lens' InsightVisual (Prelude.Maybe VisualSubtitleLabelOptions)
insightVisual_subtitle = Lens.lens (\InsightVisual' {subtitle} -> subtitle) (\s@InsightVisual' {} a -> s {subtitle = a} :: InsightVisual)

-- | The title that is displayed on the visual.
insightVisual_title :: Lens.Lens' InsightVisual (Prelude.Maybe VisualTitleLabelOptions)
insightVisual_title = Lens.lens (\InsightVisual' {title} -> title) (\s@InsightVisual' {} a -> s {title = a} :: InsightVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
insightVisual_visualId :: Lens.Lens' InsightVisual Prelude.Text
insightVisual_visualId = Lens.lens (\InsightVisual' {visualId} -> visualId) (\s@InsightVisual' {} a -> s {visualId = a} :: InsightVisual)

-- | The dataset that is used in the insight visual.
insightVisual_dataSetIdentifier :: Lens.Lens' InsightVisual Prelude.Text
insightVisual_dataSetIdentifier = Lens.lens (\InsightVisual' {dataSetIdentifier} -> dataSetIdentifier) (\s@InsightVisual' {} a -> s {dataSetIdentifier = a} :: InsightVisual)

instance Data.FromJSON InsightVisual where
  parseJSON =
    Data.withObject
      "InsightVisual"
      ( \x ->
          InsightVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "InsightConfiguration")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
            Prelude.<*> (x Data..: "DataSetIdentifier")
      )

instance Prelude.Hashable InsightVisual where
  hashWithSalt _salt InsightVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` insightConfiguration
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId
      `Prelude.hashWithSalt` dataSetIdentifier

instance Prelude.NFData InsightVisual where
  rnf InsightVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf insightConfiguration
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId
      `Prelude.seq` Prelude.rnf dataSetIdentifier

instance Data.ToJSON InsightVisual where
  toJSON InsightVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("InsightConfiguration" Data..=)
              Prelude.<$> insightConfiguration,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId),
            Prelude.Just
              ("DataSetIdentifier" Data..= dataSetIdentifier)
          ]
      )
