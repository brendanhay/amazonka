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
-- Module      : Amazonka.QuickSight.Types.ParameterSliderControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterSliderControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SliderControlDisplayOptions

-- | A control to display a horizontal toggle bar. This is used to change a
-- value by sliding the toggle.
--
-- /See:/ 'newParameterSliderControl' smart constructor.
data ParameterSliderControl = ParameterSliderControl'
  { -- | The display options of a control.
    displayOptions :: Prelude.Maybe SliderControlDisplayOptions,
    -- | The ID of the @ParameterSliderControl@.
    parameterControlId :: Prelude.Text,
    -- | The title of the @ParameterSliderControl@.
    title :: Prelude.Text,
    -- | The source parameter name of the @ParameterSliderControl@.
    sourceParameterName :: Prelude.Text,
    -- | The smaller value that is displayed at the left of the slider.
    maximumValue :: Prelude.Double,
    -- | The larger value that is displayed at the right of the slider.
    minimumValue :: Prelude.Double,
    -- | The number of increments that the slider bar is divided into.
    stepSize :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterSliderControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayOptions', 'parameterSliderControl_displayOptions' - The display options of a control.
--
-- 'parameterControlId', 'parameterSliderControl_parameterControlId' - The ID of the @ParameterSliderControl@.
--
-- 'title', 'parameterSliderControl_title' - The title of the @ParameterSliderControl@.
--
-- 'sourceParameterName', 'parameterSliderControl_sourceParameterName' - The source parameter name of the @ParameterSliderControl@.
--
-- 'maximumValue', 'parameterSliderControl_maximumValue' - The smaller value that is displayed at the left of the slider.
--
-- 'minimumValue', 'parameterSliderControl_minimumValue' - The larger value that is displayed at the right of the slider.
--
-- 'stepSize', 'parameterSliderControl_stepSize' - The number of increments that the slider bar is divided into.
newParameterSliderControl ::
  -- | 'parameterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceParameterName'
  Prelude.Text ->
  -- | 'maximumValue'
  Prelude.Double ->
  -- | 'minimumValue'
  Prelude.Double ->
  -- | 'stepSize'
  Prelude.Double ->
  ParameterSliderControl
newParameterSliderControl
  pParameterControlId_
  pTitle_
  pSourceParameterName_
  pMaximumValue_
  pMinimumValue_
  pStepSize_ =
    ParameterSliderControl'
      { displayOptions =
          Prelude.Nothing,
        parameterControlId = pParameterControlId_,
        title = pTitle_,
        sourceParameterName = pSourceParameterName_,
        maximumValue = pMaximumValue_,
        minimumValue = pMinimumValue_,
        stepSize = pStepSize_
      }

-- | The display options of a control.
parameterSliderControl_displayOptions :: Lens.Lens' ParameterSliderControl (Prelude.Maybe SliderControlDisplayOptions)
parameterSliderControl_displayOptions = Lens.lens (\ParameterSliderControl' {displayOptions} -> displayOptions) (\s@ParameterSliderControl' {} a -> s {displayOptions = a} :: ParameterSliderControl)

-- | The ID of the @ParameterSliderControl@.
parameterSliderControl_parameterControlId :: Lens.Lens' ParameterSliderControl Prelude.Text
parameterSliderControl_parameterControlId = Lens.lens (\ParameterSliderControl' {parameterControlId} -> parameterControlId) (\s@ParameterSliderControl' {} a -> s {parameterControlId = a} :: ParameterSliderControl)

-- | The title of the @ParameterSliderControl@.
parameterSliderControl_title :: Lens.Lens' ParameterSliderControl Prelude.Text
parameterSliderControl_title = Lens.lens (\ParameterSliderControl' {title} -> title) (\s@ParameterSliderControl' {} a -> s {title = a} :: ParameterSliderControl)

-- | The source parameter name of the @ParameterSliderControl@.
parameterSliderControl_sourceParameterName :: Lens.Lens' ParameterSliderControl Prelude.Text
parameterSliderControl_sourceParameterName = Lens.lens (\ParameterSliderControl' {sourceParameterName} -> sourceParameterName) (\s@ParameterSliderControl' {} a -> s {sourceParameterName = a} :: ParameterSliderControl)

-- | The smaller value that is displayed at the left of the slider.
parameterSliderControl_maximumValue :: Lens.Lens' ParameterSliderControl Prelude.Double
parameterSliderControl_maximumValue = Lens.lens (\ParameterSliderControl' {maximumValue} -> maximumValue) (\s@ParameterSliderControl' {} a -> s {maximumValue = a} :: ParameterSliderControl)

-- | The larger value that is displayed at the right of the slider.
parameterSliderControl_minimumValue :: Lens.Lens' ParameterSliderControl Prelude.Double
parameterSliderControl_minimumValue = Lens.lens (\ParameterSliderControl' {minimumValue} -> minimumValue) (\s@ParameterSliderControl' {} a -> s {minimumValue = a} :: ParameterSliderControl)

-- | The number of increments that the slider bar is divided into.
parameterSliderControl_stepSize :: Lens.Lens' ParameterSliderControl Prelude.Double
parameterSliderControl_stepSize = Lens.lens (\ParameterSliderControl' {stepSize} -> stepSize) (\s@ParameterSliderControl' {} a -> s {stepSize = a} :: ParameterSliderControl)

instance Data.FromJSON ParameterSliderControl where
  parseJSON =
    Data.withObject
      "ParameterSliderControl"
      ( \x ->
          ParameterSliderControl'
            Prelude.<$> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..: "ParameterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceParameterName")
            Prelude.<*> (x Data..: "MaximumValue")
            Prelude.<*> (x Data..: "MinimumValue")
            Prelude.<*> (x Data..: "StepSize")
      )

instance Prelude.Hashable ParameterSliderControl where
  hashWithSalt _salt ParameterSliderControl' {..} =
    _salt `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` parameterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceParameterName
      `Prelude.hashWithSalt` maximumValue
      `Prelude.hashWithSalt` minimumValue
      `Prelude.hashWithSalt` stepSize

instance Prelude.NFData ParameterSliderControl where
  rnf ParameterSliderControl' {..} =
    Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf parameterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceParameterName
      `Prelude.seq` Prelude.rnf maximumValue
      `Prelude.seq` Prelude.rnf minimumValue
      `Prelude.seq` Prelude.rnf stepSize

instance Data.ToJSON ParameterSliderControl where
  toJSON ParameterSliderControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            Prelude.Just
              ("ParameterControlId" Data..= parameterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceParameterName" Data..= sourceParameterName),
            Prelude.Just ("MaximumValue" Data..= maximumValue),
            Prelude.Just ("MinimumValue" Data..= minimumValue),
            Prelude.Just ("StepSize" Data..= stepSize)
          ]
      )
