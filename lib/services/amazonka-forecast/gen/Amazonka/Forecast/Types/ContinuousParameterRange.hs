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
-- Module      : Amazonka.Forecast.Types.ContinuousParameterRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ContinuousParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types.ScalingType
import qualified Amazonka.Prelude as Prelude

-- | Specifies a continuous hyperparameter and it\'s range of tunable values.
-- This object is part of the ParameterRanges object.
--
-- /See:/ 'newContinuousParameterRange' smart constructor.
data ContinuousParameterRange = ContinuousParameterRange'
  { -- | The scale that hyperparameter tuning uses to search the hyperparameter
    -- range. Valid values:
    --
    -- [Auto]
    --     Amazon Forecast hyperparameter tuning chooses the best scale for the
    --     hyperparameter.
    --
    -- [Linear]
    --     Hyperparameter tuning searches the values in the hyperparameter
    --     range by using a linear scale.
    --
    -- [Logarithmic]
    --     Hyperparameter tuning searches the values in the hyperparameter
    --     range by using a logarithmic scale.
    --
    --     Logarithmic scaling works only for ranges that have values greater
    --     than 0.
    --
    -- [ReverseLogarithmic]
    --     hyperparameter tuning searches the values in the hyperparameter
    --     range by using a reverse logarithmic scale.
    --
    --     Reverse logarithmic scaling works only for ranges that are entirely
    --     within the range 0 \<= x \< 1.0.
    --
    -- For information about choosing a hyperparameter scale, see
    -- <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
    -- One of the following values:
    scalingType :: Prelude.Maybe ScalingType,
    -- | The name of the hyperparameter to tune.
    name :: Prelude.Text,
    -- | The maximum tunable value of the hyperparameter.
    maxValue :: Prelude.Double,
    -- | The minimum tunable value of the hyperparameter.
    minValue :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinuousParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingType', 'continuousParameterRange_scalingType' - The scale that hyperparameter tuning uses to search the hyperparameter
-- range. Valid values:
--
-- [Auto]
--     Amazon Forecast hyperparameter tuning chooses the best scale for the
--     hyperparameter.
--
-- [Linear]
--     Hyperparameter tuning searches the values in the hyperparameter
--     range by using a linear scale.
--
-- [Logarithmic]
--     Hyperparameter tuning searches the values in the hyperparameter
--     range by using a logarithmic scale.
--
--     Logarithmic scaling works only for ranges that have values greater
--     than 0.
--
-- [ReverseLogarithmic]
--     hyperparameter tuning searches the values in the hyperparameter
--     range by using a reverse logarithmic scale.
--
--     Reverse logarithmic scaling works only for ranges that are entirely
--     within the range 0 \<= x \< 1.0.
--
-- For information about choosing a hyperparameter scale, see
-- <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
-- One of the following values:
--
-- 'name', 'continuousParameterRange_name' - The name of the hyperparameter to tune.
--
-- 'maxValue', 'continuousParameterRange_maxValue' - The maximum tunable value of the hyperparameter.
--
-- 'minValue', 'continuousParameterRange_minValue' - The minimum tunable value of the hyperparameter.
newContinuousParameterRange ::
  -- | 'name'
  Prelude.Text ->
  -- | 'maxValue'
  Prelude.Double ->
  -- | 'minValue'
  Prelude.Double ->
  ContinuousParameterRange
newContinuousParameterRange
  pName_
  pMaxValue_
  pMinValue_ =
    ContinuousParameterRange'
      { scalingType =
          Prelude.Nothing,
        name = pName_,
        maxValue = pMaxValue_,
        minValue = pMinValue_
      }

-- | The scale that hyperparameter tuning uses to search the hyperparameter
-- range. Valid values:
--
-- [Auto]
--     Amazon Forecast hyperparameter tuning chooses the best scale for the
--     hyperparameter.
--
-- [Linear]
--     Hyperparameter tuning searches the values in the hyperparameter
--     range by using a linear scale.
--
-- [Logarithmic]
--     Hyperparameter tuning searches the values in the hyperparameter
--     range by using a logarithmic scale.
--
--     Logarithmic scaling works only for ranges that have values greater
--     than 0.
--
-- [ReverseLogarithmic]
--     hyperparameter tuning searches the values in the hyperparameter
--     range by using a reverse logarithmic scale.
--
--     Reverse logarithmic scaling works only for ranges that are entirely
--     within the range 0 \<= x \< 1.0.
--
-- For information about choosing a hyperparameter scale, see
-- <http://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
-- One of the following values:
continuousParameterRange_scalingType :: Lens.Lens' ContinuousParameterRange (Prelude.Maybe ScalingType)
continuousParameterRange_scalingType = Lens.lens (\ContinuousParameterRange' {scalingType} -> scalingType) (\s@ContinuousParameterRange' {} a -> s {scalingType = a} :: ContinuousParameterRange)

-- | The name of the hyperparameter to tune.
continuousParameterRange_name :: Lens.Lens' ContinuousParameterRange Prelude.Text
continuousParameterRange_name = Lens.lens (\ContinuousParameterRange' {name} -> name) (\s@ContinuousParameterRange' {} a -> s {name = a} :: ContinuousParameterRange)

-- | The maximum tunable value of the hyperparameter.
continuousParameterRange_maxValue :: Lens.Lens' ContinuousParameterRange Prelude.Double
continuousParameterRange_maxValue = Lens.lens (\ContinuousParameterRange' {maxValue} -> maxValue) (\s@ContinuousParameterRange' {} a -> s {maxValue = a} :: ContinuousParameterRange)

-- | The minimum tunable value of the hyperparameter.
continuousParameterRange_minValue :: Lens.Lens' ContinuousParameterRange Prelude.Double
continuousParameterRange_minValue = Lens.lens (\ContinuousParameterRange' {minValue} -> minValue) (\s@ContinuousParameterRange' {} a -> s {minValue = a} :: ContinuousParameterRange)

instance Core.FromJSON ContinuousParameterRange where
  parseJSON =
    Core.withObject
      "ContinuousParameterRange"
      ( \x ->
          ContinuousParameterRange'
            Prelude.<$> (x Core..:? "ScalingType")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "MaxValue")
            Prelude.<*> (x Core..: "MinValue")
      )

instance Prelude.Hashable ContinuousParameterRange where
  hashWithSalt _salt ContinuousParameterRange' {..} =
    _salt `Prelude.hashWithSalt` scalingType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` maxValue
      `Prelude.hashWithSalt` minValue

instance Prelude.NFData ContinuousParameterRange where
  rnf ContinuousParameterRange' {..} =
    Prelude.rnf scalingType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minValue

instance Core.ToJSON ContinuousParameterRange where
  toJSON ContinuousParameterRange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ScalingType" Core..=) Prelude.<$> scalingType,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("MaxValue" Core..= maxValue),
            Prelude.Just ("MinValue" Core..= minValue)
          ]
      )
