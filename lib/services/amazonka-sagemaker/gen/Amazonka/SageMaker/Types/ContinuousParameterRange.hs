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
-- Module      : Amazonka.SageMaker.Types.ContinuousParameterRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ContinuousParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterScalingType

-- | A list of continuous hyperparameters to tune.
--
-- /See:/ 'newContinuousParameterRange' smart constructor.
data ContinuousParameterRange = ContinuousParameterRange'
  { -- | The scale that hyperparameter tuning uses to search the hyperparameter
    -- range. For information about choosing a hyperparameter scale, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
    -- One of the following values:
    --
    -- [Auto]
    --     SageMaker hyperparameter tuning chooses the best scale for the
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
    --     Logarithmic scaling works only for ranges that have only values
    --     greater than 0.
    --
    -- [ReverseLogarithmic]
    --     Hyperparameter tuning searches the values in the hyperparameter
    --     range by using a reverse logarithmic scale.
    --
    --     Reverse logarithmic scaling works only for ranges that are entirely
    --     within the range 0\<=x\<1.0.
    scalingType :: Prelude.Maybe HyperParameterScalingType,
    -- | The name of the continuous hyperparameter to tune.
    name :: Prelude.Text,
    -- | The minimum value for the hyperparameter. The tuning job uses
    -- floating-point values between this value and @MaxValue@for tuning.
    minValue :: Prelude.Text,
    -- | The maximum value for the hyperparameter. The tuning job uses
    -- floating-point values between @MinValue@ value and this value for
    -- tuning.
    maxValue :: Prelude.Text
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
-- range. For information about choosing a hyperparameter scale, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
-- One of the following values:
--
-- [Auto]
--     SageMaker hyperparameter tuning chooses the best scale for the
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
--     Logarithmic scaling works only for ranges that have only values
--     greater than 0.
--
-- [ReverseLogarithmic]
--     Hyperparameter tuning searches the values in the hyperparameter
--     range by using a reverse logarithmic scale.
--
--     Reverse logarithmic scaling works only for ranges that are entirely
--     within the range 0\<=x\<1.0.
--
-- 'name', 'continuousParameterRange_name' - The name of the continuous hyperparameter to tune.
--
-- 'minValue', 'continuousParameterRange_minValue' - The minimum value for the hyperparameter. The tuning job uses
-- floating-point values between this value and @MaxValue@for tuning.
--
-- 'maxValue', 'continuousParameterRange_maxValue' - The maximum value for the hyperparameter. The tuning job uses
-- floating-point values between @MinValue@ value and this value for
-- tuning.
newContinuousParameterRange ::
  -- | 'name'
  Prelude.Text ->
  -- | 'minValue'
  Prelude.Text ->
  -- | 'maxValue'
  Prelude.Text ->
  ContinuousParameterRange
newContinuousParameterRange
  pName_
  pMinValue_
  pMaxValue_ =
    ContinuousParameterRange'
      { scalingType =
          Prelude.Nothing,
        name = pName_,
        minValue = pMinValue_,
        maxValue = pMaxValue_
      }

-- | The scale that hyperparameter tuning uses to search the hyperparameter
-- range. For information about choosing a hyperparameter scale, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
-- One of the following values:
--
-- [Auto]
--     SageMaker hyperparameter tuning chooses the best scale for the
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
--     Logarithmic scaling works only for ranges that have only values
--     greater than 0.
--
-- [ReverseLogarithmic]
--     Hyperparameter tuning searches the values in the hyperparameter
--     range by using a reverse logarithmic scale.
--
--     Reverse logarithmic scaling works only for ranges that are entirely
--     within the range 0\<=x\<1.0.
continuousParameterRange_scalingType :: Lens.Lens' ContinuousParameterRange (Prelude.Maybe HyperParameterScalingType)
continuousParameterRange_scalingType = Lens.lens (\ContinuousParameterRange' {scalingType} -> scalingType) (\s@ContinuousParameterRange' {} a -> s {scalingType = a} :: ContinuousParameterRange)

-- | The name of the continuous hyperparameter to tune.
continuousParameterRange_name :: Lens.Lens' ContinuousParameterRange Prelude.Text
continuousParameterRange_name = Lens.lens (\ContinuousParameterRange' {name} -> name) (\s@ContinuousParameterRange' {} a -> s {name = a} :: ContinuousParameterRange)

-- | The minimum value for the hyperparameter. The tuning job uses
-- floating-point values between this value and @MaxValue@for tuning.
continuousParameterRange_minValue :: Lens.Lens' ContinuousParameterRange Prelude.Text
continuousParameterRange_minValue = Lens.lens (\ContinuousParameterRange' {minValue} -> minValue) (\s@ContinuousParameterRange' {} a -> s {minValue = a} :: ContinuousParameterRange)

-- | The maximum value for the hyperparameter. The tuning job uses
-- floating-point values between @MinValue@ value and this value for
-- tuning.
continuousParameterRange_maxValue :: Lens.Lens' ContinuousParameterRange Prelude.Text
continuousParameterRange_maxValue = Lens.lens (\ContinuousParameterRange' {maxValue} -> maxValue) (\s@ContinuousParameterRange' {} a -> s {maxValue = a} :: ContinuousParameterRange)

instance Data.FromJSON ContinuousParameterRange where
  parseJSON =
    Data.withObject
      "ContinuousParameterRange"
      ( \x ->
          ContinuousParameterRange'
            Prelude.<$> (x Data..:? "ScalingType")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "MinValue")
            Prelude.<*> (x Data..: "MaxValue")
      )

instance Prelude.Hashable ContinuousParameterRange where
  hashWithSalt _salt ContinuousParameterRange' {..} =
    _salt
      `Prelude.hashWithSalt` scalingType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` minValue
      `Prelude.hashWithSalt` maxValue

instance Prelude.NFData ContinuousParameterRange where
  rnf ContinuousParameterRange' {..} =
    Prelude.rnf scalingType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf maxValue

instance Data.ToJSON ContinuousParameterRange where
  toJSON ContinuousParameterRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ScalingType" Data..=) Prelude.<$> scalingType,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("MinValue" Data..= minValue),
            Prelude.Just ("MaxValue" Data..= maxValue)
          ]
      )
