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
-- Module      : Amazonka.QuickSight.Types.FilterSliderControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterSliderControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SheetControlSliderType
import Amazonka.QuickSight.Types.SliderControlDisplayOptions

-- | A control to display a horizontal toggle bar. This is used to change a
-- value by sliding the toggle.
--
-- /See:/ 'newFilterSliderControl' smart constructor.
data FilterSliderControl = FilterSliderControl'
  { -- | The display options of a control.
    displayOptions :: Prelude.Maybe SliderControlDisplayOptions,
    -- | The type of @FilterSliderControl@. Choose one of the following options:
    --
    -- -   @SINGLE_POINT@: Filter against(equals) a single data point.
    --
    -- -   @RANGE@: Filter data that is in a specified range.
    type' :: Prelude.Maybe SheetControlSliderType,
    -- | The ID of the @FilterSliderControl@.
    filterControlId :: Prelude.Text,
    -- | The title of the @FilterSliderControl@.
    title :: Prelude.Text,
    -- | The source filter ID of the @FilterSliderControl@.
    sourceFilterId :: Prelude.Text,
    -- | The smaller value that is displayed at the left of the slider.
    maximumValue :: Prelude.Double,
    -- | The larger value that is displayed at the right of the slider.
    minimumValue :: Prelude.Double,
    -- | The number of increments that the slider bar is divided into.
    stepSize :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterSliderControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayOptions', 'filterSliderControl_displayOptions' - The display options of a control.
--
-- 'type'', 'filterSliderControl_type' - The type of @FilterSliderControl@. Choose one of the following options:
--
-- -   @SINGLE_POINT@: Filter against(equals) a single data point.
--
-- -   @RANGE@: Filter data that is in a specified range.
--
-- 'filterControlId', 'filterSliderControl_filterControlId' - The ID of the @FilterSliderControl@.
--
-- 'title', 'filterSliderControl_title' - The title of the @FilterSliderControl@.
--
-- 'sourceFilterId', 'filterSliderControl_sourceFilterId' - The source filter ID of the @FilterSliderControl@.
--
-- 'maximumValue', 'filterSliderControl_maximumValue' - The smaller value that is displayed at the left of the slider.
--
-- 'minimumValue', 'filterSliderControl_minimumValue' - The larger value that is displayed at the right of the slider.
--
-- 'stepSize', 'filterSliderControl_stepSize' - The number of increments that the slider bar is divided into.
newFilterSliderControl ::
  -- | 'filterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceFilterId'
  Prelude.Text ->
  -- | 'maximumValue'
  Prelude.Double ->
  -- | 'minimumValue'
  Prelude.Double ->
  -- | 'stepSize'
  Prelude.Double ->
  FilterSliderControl
newFilterSliderControl
  pFilterControlId_
  pTitle_
  pSourceFilterId_
  pMaximumValue_
  pMinimumValue_
  pStepSize_ =
    FilterSliderControl'
      { displayOptions =
          Prelude.Nothing,
        type' = Prelude.Nothing,
        filterControlId = pFilterControlId_,
        title = pTitle_,
        sourceFilterId = pSourceFilterId_,
        maximumValue = pMaximumValue_,
        minimumValue = pMinimumValue_,
        stepSize = pStepSize_
      }

-- | The display options of a control.
filterSliderControl_displayOptions :: Lens.Lens' FilterSliderControl (Prelude.Maybe SliderControlDisplayOptions)
filterSliderControl_displayOptions = Lens.lens (\FilterSliderControl' {displayOptions} -> displayOptions) (\s@FilterSliderControl' {} a -> s {displayOptions = a} :: FilterSliderControl)

-- | The type of @FilterSliderControl@. Choose one of the following options:
--
-- -   @SINGLE_POINT@: Filter against(equals) a single data point.
--
-- -   @RANGE@: Filter data that is in a specified range.
filterSliderControl_type :: Lens.Lens' FilterSliderControl (Prelude.Maybe SheetControlSliderType)
filterSliderControl_type = Lens.lens (\FilterSliderControl' {type'} -> type') (\s@FilterSliderControl' {} a -> s {type' = a} :: FilterSliderControl)

-- | The ID of the @FilterSliderControl@.
filterSliderControl_filterControlId :: Lens.Lens' FilterSliderControl Prelude.Text
filterSliderControl_filterControlId = Lens.lens (\FilterSliderControl' {filterControlId} -> filterControlId) (\s@FilterSliderControl' {} a -> s {filterControlId = a} :: FilterSliderControl)

-- | The title of the @FilterSliderControl@.
filterSliderControl_title :: Lens.Lens' FilterSliderControl Prelude.Text
filterSliderControl_title = Lens.lens (\FilterSliderControl' {title} -> title) (\s@FilterSliderControl' {} a -> s {title = a} :: FilterSliderControl)

-- | The source filter ID of the @FilterSliderControl@.
filterSliderControl_sourceFilterId :: Lens.Lens' FilterSliderControl Prelude.Text
filterSliderControl_sourceFilterId = Lens.lens (\FilterSliderControl' {sourceFilterId} -> sourceFilterId) (\s@FilterSliderControl' {} a -> s {sourceFilterId = a} :: FilterSliderControl)

-- | The smaller value that is displayed at the left of the slider.
filterSliderControl_maximumValue :: Lens.Lens' FilterSliderControl Prelude.Double
filterSliderControl_maximumValue = Lens.lens (\FilterSliderControl' {maximumValue} -> maximumValue) (\s@FilterSliderControl' {} a -> s {maximumValue = a} :: FilterSliderControl)

-- | The larger value that is displayed at the right of the slider.
filterSliderControl_minimumValue :: Lens.Lens' FilterSliderControl Prelude.Double
filterSliderControl_minimumValue = Lens.lens (\FilterSliderControl' {minimumValue} -> minimumValue) (\s@FilterSliderControl' {} a -> s {minimumValue = a} :: FilterSliderControl)

-- | The number of increments that the slider bar is divided into.
filterSliderControl_stepSize :: Lens.Lens' FilterSliderControl Prelude.Double
filterSliderControl_stepSize = Lens.lens (\FilterSliderControl' {stepSize} -> stepSize) (\s@FilterSliderControl' {} a -> s {stepSize = a} :: FilterSliderControl)

instance Data.FromJSON FilterSliderControl where
  parseJSON =
    Data.withObject
      "FilterSliderControl"
      ( \x ->
          FilterSliderControl'
            Prelude.<$> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "FilterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceFilterId")
            Prelude.<*> (x Data..: "MaximumValue")
            Prelude.<*> (x Data..: "MinimumValue")
            Prelude.<*> (x Data..: "StepSize")
      )

instance Prelude.Hashable FilterSliderControl where
  hashWithSalt _salt FilterSliderControl' {..} =
    _salt
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` filterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceFilterId
      `Prelude.hashWithSalt` maximumValue
      `Prelude.hashWithSalt` minimumValue
      `Prelude.hashWithSalt` stepSize

instance Prelude.NFData FilterSliderControl where
  rnf FilterSliderControl' {..} =
    Prelude.rnf displayOptions `Prelude.seq`
      Prelude.rnf type' `Prelude.seq`
        Prelude.rnf filterControlId `Prelude.seq`
          Prelude.rnf title `Prelude.seq`
            Prelude.rnf sourceFilterId `Prelude.seq`
              Prelude.rnf maximumValue `Prelude.seq`
                Prelude.rnf minimumValue `Prelude.seq`
                  Prelude.rnf stepSize

instance Data.ToJSON FilterSliderControl where
  toJSON FilterSliderControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just
              ("FilterControlId" Data..= filterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceFilterId" Data..= sourceFilterId),
            Prelude.Just ("MaximumValue" Data..= maximumValue),
            Prelude.Just ("MinimumValue" Data..= minimumValue),
            Prelude.Just ("StepSize" Data..= stepSize)
          ]
      )
