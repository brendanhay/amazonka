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
-- Module      : Amazonka.QuickSight.Types.DataLabelOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataLabelOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataLabelContent
import Amazonka.QuickSight.Types.DataLabelOverlap
import Amazonka.QuickSight.Types.DataLabelPosition
import Amazonka.QuickSight.Types.DataLabelType
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.Visibility

-- | The options that determine the presentation of the data labels.
--
-- /See:/ 'newDataLabelOptions' smart constructor.
data DataLabelOptions = DataLabelOptions'
  { -- | Determines the visibility of the category field labels.
    categoryLabelVisibility :: Prelude.Maybe Visibility,
    -- | The option that determines the data label type.
    dataLabelTypes :: Prelude.Maybe [DataLabelType],
    -- | Determines the color of the data labels.
    labelColor :: Prelude.Maybe Prelude.Text,
    -- | Determines the content of the data labels.
    labelContent :: Prelude.Maybe DataLabelContent,
    -- | Determines the font configuration of the data labels.
    labelFontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | Determines the visibility of the measure field labels.
    measureLabelVisibility :: Prelude.Maybe Visibility,
    -- | Determines whether overlap is enabled or disabled for the data labels.
    overlap :: Prelude.Maybe DataLabelOverlap,
    -- | Determines the position of the data labels.
    position :: Prelude.Maybe DataLabelPosition,
    -- | Determines the visibility of the data labels.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLabelOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryLabelVisibility', 'dataLabelOptions_categoryLabelVisibility' - Determines the visibility of the category field labels.
--
-- 'dataLabelTypes', 'dataLabelOptions_dataLabelTypes' - The option that determines the data label type.
--
-- 'labelColor', 'dataLabelOptions_labelColor' - Determines the color of the data labels.
--
-- 'labelContent', 'dataLabelOptions_labelContent' - Determines the content of the data labels.
--
-- 'labelFontConfiguration', 'dataLabelOptions_labelFontConfiguration' - Determines the font configuration of the data labels.
--
-- 'measureLabelVisibility', 'dataLabelOptions_measureLabelVisibility' - Determines the visibility of the measure field labels.
--
-- 'overlap', 'dataLabelOptions_overlap' - Determines whether overlap is enabled or disabled for the data labels.
--
-- 'position', 'dataLabelOptions_position' - Determines the position of the data labels.
--
-- 'visibility', 'dataLabelOptions_visibility' - Determines the visibility of the data labels.
newDataLabelOptions ::
  DataLabelOptions
newDataLabelOptions =
  DataLabelOptions'
    { categoryLabelVisibility =
        Prelude.Nothing,
      dataLabelTypes = Prelude.Nothing,
      labelColor = Prelude.Nothing,
      labelContent = Prelude.Nothing,
      labelFontConfiguration = Prelude.Nothing,
      measureLabelVisibility = Prelude.Nothing,
      overlap = Prelude.Nothing,
      position = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | Determines the visibility of the category field labels.
dataLabelOptions_categoryLabelVisibility :: Lens.Lens' DataLabelOptions (Prelude.Maybe Visibility)
dataLabelOptions_categoryLabelVisibility = Lens.lens (\DataLabelOptions' {categoryLabelVisibility} -> categoryLabelVisibility) (\s@DataLabelOptions' {} a -> s {categoryLabelVisibility = a} :: DataLabelOptions)

-- | The option that determines the data label type.
dataLabelOptions_dataLabelTypes :: Lens.Lens' DataLabelOptions (Prelude.Maybe [DataLabelType])
dataLabelOptions_dataLabelTypes = Lens.lens (\DataLabelOptions' {dataLabelTypes} -> dataLabelTypes) (\s@DataLabelOptions' {} a -> s {dataLabelTypes = a} :: DataLabelOptions) Prelude.. Lens.mapping Lens.coerced

-- | Determines the color of the data labels.
dataLabelOptions_labelColor :: Lens.Lens' DataLabelOptions (Prelude.Maybe Prelude.Text)
dataLabelOptions_labelColor = Lens.lens (\DataLabelOptions' {labelColor} -> labelColor) (\s@DataLabelOptions' {} a -> s {labelColor = a} :: DataLabelOptions)

-- | Determines the content of the data labels.
dataLabelOptions_labelContent :: Lens.Lens' DataLabelOptions (Prelude.Maybe DataLabelContent)
dataLabelOptions_labelContent = Lens.lens (\DataLabelOptions' {labelContent} -> labelContent) (\s@DataLabelOptions' {} a -> s {labelContent = a} :: DataLabelOptions)

-- | Determines the font configuration of the data labels.
dataLabelOptions_labelFontConfiguration :: Lens.Lens' DataLabelOptions (Prelude.Maybe FontConfiguration)
dataLabelOptions_labelFontConfiguration = Lens.lens (\DataLabelOptions' {labelFontConfiguration} -> labelFontConfiguration) (\s@DataLabelOptions' {} a -> s {labelFontConfiguration = a} :: DataLabelOptions)

-- | Determines the visibility of the measure field labels.
dataLabelOptions_measureLabelVisibility :: Lens.Lens' DataLabelOptions (Prelude.Maybe Visibility)
dataLabelOptions_measureLabelVisibility = Lens.lens (\DataLabelOptions' {measureLabelVisibility} -> measureLabelVisibility) (\s@DataLabelOptions' {} a -> s {measureLabelVisibility = a} :: DataLabelOptions)

-- | Determines whether overlap is enabled or disabled for the data labels.
dataLabelOptions_overlap :: Lens.Lens' DataLabelOptions (Prelude.Maybe DataLabelOverlap)
dataLabelOptions_overlap = Lens.lens (\DataLabelOptions' {overlap} -> overlap) (\s@DataLabelOptions' {} a -> s {overlap = a} :: DataLabelOptions)

-- | Determines the position of the data labels.
dataLabelOptions_position :: Lens.Lens' DataLabelOptions (Prelude.Maybe DataLabelPosition)
dataLabelOptions_position = Lens.lens (\DataLabelOptions' {position} -> position) (\s@DataLabelOptions' {} a -> s {position = a} :: DataLabelOptions)

-- | Determines the visibility of the data labels.
dataLabelOptions_visibility :: Lens.Lens' DataLabelOptions (Prelude.Maybe Visibility)
dataLabelOptions_visibility = Lens.lens (\DataLabelOptions' {visibility} -> visibility) (\s@DataLabelOptions' {} a -> s {visibility = a} :: DataLabelOptions)

instance Data.FromJSON DataLabelOptions where
  parseJSON =
    Data.withObject
      "DataLabelOptions"
      ( \x ->
          DataLabelOptions'
            Prelude.<$> (x Data..:? "CategoryLabelVisibility")
            Prelude.<*> (x Data..:? "DataLabelTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LabelColor")
            Prelude.<*> (x Data..:? "LabelContent")
            Prelude.<*> (x Data..:? "LabelFontConfiguration")
            Prelude.<*> (x Data..:? "MeasureLabelVisibility")
            Prelude.<*> (x Data..:? "Overlap")
            Prelude.<*> (x Data..:? "Position")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable DataLabelOptions where
  hashWithSalt _salt DataLabelOptions' {..} =
    _salt
      `Prelude.hashWithSalt` categoryLabelVisibility
      `Prelude.hashWithSalt` dataLabelTypes
      `Prelude.hashWithSalt` labelColor
      `Prelude.hashWithSalt` labelContent
      `Prelude.hashWithSalt` labelFontConfiguration
      `Prelude.hashWithSalt` measureLabelVisibility
      `Prelude.hashWithSalt` overlap
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData DataLabelOptions where
  rnf DataLabelOptions' {..} =
    Prelude.rnf categoryLabelVisibility
      `Prelude.seq` Prelude.rnf dataLabelTypes
      `Prelude.seq` Prelude.rnf labelColor
      `Prelude.seq` Prelude.rnf labelContent
      `Prelude.seq` Prelude.rnf labelFontConfiguration
      `Prelude.seq` Prelude.rnf measureLabelVisibility
      `Prelude.seq` Prelude.rnf overlap
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON DataLabelOptions where
  toJSON DataLabelOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryLabelVisibility" Data..=)
              Prelude.<$> categoryLabelVisibility,
            ("DataLabelTypes" Data..=)
              Prelude.<$> dataLabelTypes,
            ("LabelColor" Data..=) Prelude.<$> labelColor,
            ("LabelContent" Data..=) Prelude.<$> labelContent,
            ("LabelFontConfiguration" Data..=)
              Prelude.<$> labelFontConfiguration,
            ("MeasureLabelVisibility" Data..=)
              Prelude.<$> measureLabelVisibility,
            ("Overlap" Data..=) Prelude.<$> overlap,
            ("Position" Data..=) Prelude.<$> position,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
