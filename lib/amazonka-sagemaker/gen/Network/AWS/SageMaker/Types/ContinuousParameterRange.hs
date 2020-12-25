{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ContinuousParameterRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContinuousParameterRange
  ( ContinuousParameterRange (..),

    -- * Smart constructor
    mkContinuousParameterRange,

    -- * Lenses
    cName,
    cMinValue,
    cMaxValue,
    cScalingType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterScalingType as Types
import qualified Network.AWS.SageMaker.Types.ParameterKey as Types
import qualified Network.AWS.SageMaker.Types.ParameterValue as Types

-- | A list of continuous hyperparameters to tune.
--
-- /See:/ 'mkContinuousParameterRange' smart constructor.
data ContinuousParameterRange = ContinuousParameterRange'
  { -- | The name of the continuous hyperparameter to tune.
    name :: Types.ParameterKey,
    -- | The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
    minValue :: Types.ParameterValue,
    -- | The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
    maxValue :: Types.ParameterValue,
    -- | The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling> . One of the following values:
    --
    --
    --     * Auto
    --
    --     * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.
    --
    --
    --     * Linear
    --
    --     * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.
    --
    --
    --     * Logarithmic
    --
    --     * Hyperparameter tuning searches the values in the hyperparameter range by using a logarithmic scale.
    -- Logarithmic scaling works only for ranges that have only values greater than 0.
    --
    --
    --     * ReverseLogarithmic
    --
    --     * Hyperparameter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale.
    -- Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
    scalingType :: Core.Maybe Types.HyperParameterScalingType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContinuousParameterRange' value with any optional fields omitted.
mkContinuousParameterRange ::
  -- | 'name'
  Types.ParameterKey ->
  -- | 'minValue'
  Types.ParameterValue ->
  -- | 'maxValue'
  Types.ParameterValue ->
  ContinuousParameterRange
mkContinuousParameterRange name minValue maxValue =
  ContinuousParameterRange'
    { name,
      minValue,
      maxValue,
      scalingType = Core.Nothing
    }

-- | The name of the continuous hyperparameter to tune.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' ContinuousParameterRange Types.ParameterKey
cName = Lens.field @"name"
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMinValue :: Lens.Lens' ContinuousParameterRange Types.ParameterValue
cMinValue = Lens.field @"minValue"
{-# DEPRECATED cMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

-- | The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaxValue :: Lens.Lens' ContinuousParameterRange Types.ParameterValue
cMaxValue = Lens.field @"maxValue"
{-# DEPRECATED cMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

-- | The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling> . One of the following values:
--
--
--     * Auto
--
--     * Amazon SageMaker hyperparameter tuning chooses the best scale for the hyperparameter.
--
--
--     * Linear
--
--     * Hyperparameter tuning searches the values in the hyperparameter range by using a linear scale.
--
--
--     * Logarithmic
--
--     * Hyperparameter tuning searches the values in the hyperparameter range by using a logarithmic scale.
-- Logarithmic scaling works only for ranges that have only values greater than 0.
--
--
--     * ReverseLogarithmic
--
--     * Hyperparameter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale.
-- Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
--
--
--
-- /Note:/ Consider using 'scalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cScalingType :: Lens.Lens' ContinuousParameterRange (Core.Maybe Types.HyperParameterScalingType)
cScalingType = Lens.field @"scalingType"
{-# DEPRECATED cScalingType "Use generic-lens or generic-optics with 'scalingType' instead." #-}

instance Core.FromJSON ContinuousParameterRange where
  toJSON ContinuousParameterRange {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("MinValue" Core..= minValue),
            Core.Just ("MaxValue" Core..= maxValue),
            ("ScalingType" Core..=) Core.<$> scalingType
          ]
      )

instance Core.FromJSON ContinuousParameterRange where
  parseJSON =
    Core.withObject "ContinuousParameterRange" Core.$
      \x ->
        ContinuousParameterRange'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "MinValue")
          Core.<*> (x Core..: "MaxValue")
          Core.<*> (x Core..:? "ScalingType")
