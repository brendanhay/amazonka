{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.IntegerParameterRange where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.HyperParameterScalingType

-- | For a hyperparameter of the integer type, specifies the range that a
-- hyperparameter tuning job searches.
--
-- /See:/ 'newIntegerParameterRange' smart constructor.
data IntegerParameterRange = IntegerParameterRange'
  { -- | The scale that hyperparameter tuning uses to search the hyperparameter
    -- range. For information about choosing a hyperparameter scale, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
    -- One of the following values:
    --
    -- [Auto]
    --     Amazon SageMaker hyperparameter tuning chooses the best scale for
    --     the hyperparameter.
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
    scalingType :: Prelude.Maybe HyperParameterScalingType,
    -- | The name of the hyperparameter to search.
    name :: Prelude.Text,
    -- | The minimum value of the hyperparameter to search.
    minValue :: Prelude.Text,
    -- | The maximum value of the hyperparameter to search.
    maxValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IntegerParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scalingType', 'integerParameterRange_scalingType' - The scale that hyperparameter tuning uses to search the hyperparameter
-- range. For information about choosing a hyperparameter scale, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling>.
-- One of the following values:
--
-- [Auto]
--     Amazon SageMaker hyperparameter tuning chooses the best scale for
--     the hyperparameter.
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
-- 'name', 'integerParameterRange_name' - The name of the hyperparameter to search.
--
-- 'minValue', 'integerParameterRange_minValue' - The minimum value of the hyperparameter to search.
--
-- 'maxValue', 'integerParameterRange_maxValue' - The maximum value of the hyperparameter to search.
newIntegerParameterRange ::
  -- | 'name'
  Prelude.Text ->
  -- | 'minValue'
  Prelude.Text ->
  -- | 'maxValue'
  Prelude.Text ->
  IntegerParameterRange
newIntegerParameterRange pName_ pMinValue_ pMaxValue_ =
  IntegerParameterRange'
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
--     Amazon SageMaker hyperparameter tuning chooses the best scale for
--     the hyperparameter.
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
integerParameterRange_scalingType :: Lens.Lens' IntegerParameterRange (Prelude.Maybe HyperParameterScalingType)
integerParameterRange_scalingType = Lens.lens (\IntegerParameterRange' {scalingType} -> scalingType) (\s@IntegerParameterRange' {} a -> s {scalingType = a} :: IntegerParameterRange)

-- | The name of the hyperparameter to search.
integerParameterRange_name :: Lens.Lens' IntegerParameterRange Prelude.Text
integerParameterRange_name = Lens.lens (\IntegerParameterRange' {name} -> name) (\s@IntegerParameterRange' {} a -> s {name = a} :: IntegerParameterRange)

-- | The minimum value of the hyperparameter to search.
integerParameterRange_minValue :: Lens.Lens' IntegerParameterRange Prelude.Text
integerParameterRange_minValue = Lens.lens (\IntegerParameterRange' {minValue} -> minValue) (\s@IntegerParameterRange' {} a -> s {minValue = a} :: IntegerParameterRange)

-- | The maximum value of the hyperparameter to search.
integerParameterRange_maxValue :: Lens.Lens' IntegerParameterRange Prelude.Text
integerParameterRange_maxValue = Lens.lens (\IntegerParameterRange' {maxValue} -> maxValue) (\s@IntegerParameterRange' {} a -> s {maxValue = a} :: IntegerParameterRange)

instance Prelude.FromJSON IntegerParameterRange where
  parseJSON =
    Prelude.withObject
      "IntegerParameterRange"
      ( \x ->
          IntegerParameterRange'
            Prelude.<$> (x Prelude..:? "ScalingType")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "MinValue")
            Prelude.<*> (x Prelude..: "MaxValue")
      )

instance Prelude.Hashable IntegerParameterRange

instance Prelude.NFData IntegerParameterRange

instance Prelude.ToJSON IntegerParameterRange where
  toJSON IntegerParameterRange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ScalingType" Prelude..=) Prelude.<$> scalingType,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("MinValue" Prelude..= minValue),
            Prelude.Just ("MaxValue" Prelude..= maxValue)
          ]
      )
