{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.IntegerParameterRange
  ( IntegerParameterRange (..),

    -- * Smart constructor
    mkIntegerParameterRange,

    -- * Lenses
    iprName,
    iprMinValue,
    iprMaxValue,
    iprScalingType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterScalingType as Types
import qualified Network.AWS.SageMaker.Types.ParameterKey as Types
import qualified Network.AWS.SageMaker.Types.ParameterValue as Types

-- | For a hyperparameter of the integer type, specifies the range that a hyperparameter tuning job searches.
--
-- /See:/ 'mkIntegerParameterRange' smart constructor.
data IntegerParameterRange = IntegerParameterRange'
  { -- | The name of the hyperparameter to search.
    name :: Types.ParameterKey,
    -- | The minimum value of the hyperparameter to search.
    minValue :: Types.ParameterValue,
    -- | The maximum value of the hyperparameter to search.
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
    scalingType :: Core.Maybe Types.HyperParameterScalingType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntegerParameterRange' value with any optional fields omitted.
mkIntegerParameterRange ::
  -- | 'name'
  Types.ParameterKey ->
  -- | 'minValue'
  Types.ParameterValue ->
  -- | 'maxValue'
  Types.ParameterValue ->
  IntegerParameterRange
mkIntegerParameterRange name minValue maxValue =
  IntegerParameterRange'
    { name,
      minValue,
      maxValue,
      scalingType = Core.Nothing
    }

-- | The name of the hyperparameter to search.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprName :: Lens.Lens' IntegerParameterRange Types.ParameterKey
iprName = Lens.field @"name"
{-# DEPRECATED iprName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The minimum value of the hyperparameter to search.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprMinValue :: Lens.Lens' IntegerParameterRange Types.ParameterValue
iprMinValue = Lens.field @"minValue"
{-# DEPRECATED iprMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

-- | The maximum value of the hyperparameter to search.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprMaxValue :: Lens.Lens' IntegerParameterRange Types.ParameterValue
iprMaxValue = Lens.field @"maxValue"
{-# DEPRECATED iprMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

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
--
-- /Note:/ Consider using 'scalingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprScalingType :: Lens.Lens' IntegerParameterRange (Core.Maybe Types.HyperParameterScalingType)
iprScalingType = Lens.field @"scalingType"
{-# DEPRECATED iprScalingType "Use generic-lens or generic-optics with 'scalingType' instead." #-}

instance Core.FromJSON IntegerParameterRange where
  toJSON IntegerParameterRange {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("MinValue" Core..= minValue),
            Core.Just ("MaxValue" Core..= maxValue),
            ("ScalingType" Core..=) Core.<$> scalingType
          ]
      )

instance Core.FromJSON IntegerParameterRange where
  parseJSON =
    Core.withObject "IntegerParameterRange" Core.$
      \x ->
        IntegerParameterRange'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "MinValue")
          Core.<*> (x Core..: "MaxValue")
          Core.<*> (x Core..:? "ScalingType")
