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
-- Module      : Amazonka.QuickSight.Types.CustomContentVisual
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomContentVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CustomContentConfiguration
import Amazonka.QuickSight.Types.VisualCustomAction
import Amazonka.QuickSight.Types.VisualSubtitleLabelOptions
import Amazonka.QuickSight.Types.VisualTitleLabelOptions

-- | A visual that contains custom content.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/custom-visual-content.html Using custom visual content>
-- in the /Amazon QuickSight User Guide/.
--
-- /See:/ 'newCustomContentVisual' smart constructor.
data CustomContentVisual = CustomContentVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The configuration of a @CustomContentVisual@.
    chartConfiguration :: Prelude.Maybe CustomContentConfiguration,
    -- | The subtitle that is displayed on the visual.
    subtitle :: Prelude.Maybe VisualSubtitleLabelOptions,
    -- | The title that is displayed on the visual.
    title :: Prelude.Maybe VisualTitleLabelOptions,
    -- | The unique identifier of a visual. This identifier must be unique within
    -- the context of a dashboard, template, or analysis. Two dashboards,
    -- analyses, or templates can have visuals with the same identifiers.
    visualId :: Prelude.Text,
    -- | The dataset that is used to create the custom content visual. You can\'t
    -- create a visual without a dataset.
    dataSetIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomContentVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'customContentVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'chartConfiguration', 'customContentVisual_chartConfiguration' - The configuration of a @CustomContentVisual@.
--
-- 'subtitle', 'customContentVisual_subtitle' - The subtitle that is displayed on the visual.
--
-- 'title', 'customContentVisual_title' - The title that is displayed on the visual.
--
-- 'visualId', 'customContentVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
--
-- 'dataSetIdentifier', 'customContentVisual_dataSetIdentifier' - The dataset that is used to create the custom content visual. You can\'t
-- create a visual without a dataset.
newCustomContentVisual ::
  -- | 'visualId'
  Prelude.Text ->
  -- | 'dataSetIdentifier'
  Prelude.Text ->
  CustomContentVisual
newCustomContentVisual pVisualId_ pDataSetIdentifier_ =
  CustomContentVisual'
    { actions = Prelude.Nothing,
      chartConfiguration = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      title = Prelude.Nothing,
      visualId = pVisualId_,
      dataSetIdentifier = pDataSetIdentifier_
    }

-- | The list of custom actions that are configured for a visual.
customContentVisual_actions :: Lens.Lens' CustomContentVisual (Prelude.Maybe [VisualCustomAction])
customContentVisual_actions = Lens.lens (\CustomContentVisual' {actions} -> actions) (\s@CustomContentVisual' {} a -> s {actions = a} :: CustomContentVisual) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of a @CustomContentVisual@.
customContentVisual_chartConfiguration :: Lens.Lens' CustomContentVisual (Prelude.Maybe CustomContentConfiguration)
customContentVisual_chartConfiguration = Lens.lens (\CustomContentVisual' {chartConfiguration} -> chartConfiguration) (\s@CustomContentVisual' {} a -> s {chartConfiguration = a} :: CustomContentVisual)

-- | The subtitle that is displayed on the visual.
customContentVisual_subtitle :: Lens.Lens' CustomContentVisual (Prelude.Maybe VisualSubtitleLabelOptions)
customContentVisual_subtitle = Lens.lens (\CustomContentVisual' {subtitle} -> subtitle) (\s@CustomContentVisual' {} a -> s {subtitle = a} :: CustomContentVisual)

-- | The title that is displayed on the visual.
customContentVisual_title :: Lens.Lens' CustomContentVisual (Prelude.Maybe VisualTitleLabelOptions)
customContentVisual_title = Lens.lens (\CustomContentVisual' {title} -> title) (\s@CustomContentVisual' {} a -> s {title = a} :: CustomContentVisual)

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
customContentVisual_visualId :: Lens.Lens' CustomContentVisual Prelude.Text
customContentVisual_visualId = Lens.lens (\CustomContentVisual' {visualId} -> visualId) (\s@CustomContentVisual' {} a -> s {visualId = a} :: CustomContentVisual)

-- | The dataset that is used to create the custom content visual. You can\'t
-- create a visual without a dataset.
customContentVisual_dataSetIdentifier :: Lens.Lens' CustomContentVisual Prelude.Text
customContentVisual_dataSetIdentifier = Lens.lens (\CustomContentVisual' {dataSetIdentifier} -> dataSetIdentifier) (\s@CustomContentVisual' {} a -> s {dataSetIdentifier = a} :: CustomContentVisual)

instance Data.FromJSON CustomContentVisual where
  parseJSON =
    Data.withObject
      "CustomContentVisual"
      ( \x ->
          CustomContentVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ChartConfiguration")
            Prelude.<*> (x Data..:? "Subtitle")
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..: "VisualId")
            Prelude.<*> (x Data..: "DataSetIdentifier")
      )

instance Prelude.Hashable CustomContentVisual where
  hashWithSalt _salt CustomContentVisual' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chartConfiguration
      `Prelude.hashWithSalt` subtitle
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visualId
      `Prelude.hashWithSalt` dataSetIdentifier

instance Prelude.NFData CustomContentVisual where
  rnf CustomContentVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chartConfiguration
      `Prelude.seq` Prelude.rnf subtitle
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf visualId
      `Prelude.seq` Prelude.rnf dataSetIdentifier

instance Data.ToJSON CustomContentVisual where
  toJSON CustomContentVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            ("ChartConfiguration" Data..=)
              Prelude.<$> chartConfiguration,
            ("Subtitle" Data..=) Prelude.<$> subtitle,
            ("Title" Data..=) Prelude.<$> title,
            Prelude.Just ("VisualId" Data..= visualId),
            Prelude.Just
              ("DataSetIdentifier" Data..= dataSetIdentifier)
          ]
      )
