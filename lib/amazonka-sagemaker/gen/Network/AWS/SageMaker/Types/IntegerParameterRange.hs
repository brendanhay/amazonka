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
    iprMaxValue,
    iprScalingType,
    iprName,
    iprMinValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HyperParameterScalingType

-- | For a hyperparameter of the integer type, specifies the range that a hyperparameter tuning job searches.
--
-- /See:/ 'mkIntegerParameterRange' smart constructor.
data IntegerParameterRange = IntegerParameterRange'
  { -- | The maximum value of the hyperparameter to search.
    maxValue :: Lude.Text,
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
    scalingType :: Lude.Maybe HyperParameterScalingType,
    -- | The name of the hyperparameter to search.
    name :: Lude.Text,
    -- | The minimum value of the hyperparameter to search.
    minValue :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntegerParameterRange' with the minimum fields required to make a request.
--
-- * 'maxValue' - The maximum value of the hyperparameter to search.
-- * 'scalingType' - The scale that hyperparameter tuning uses to search the hyperparameter range. For information about choosing a hyperparameter scale, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-define-ranges.html#scaling-type Hyperparameter Scaling> . One of the following values:
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
-- * 'name' - The name of the hyperparameter to search.
-- * 'minValue' - The minimum value of the hyperparameter to search.
mkIntegerParameterRange ::
  -- | 'maxValue'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'minValue'
  Lude.Text ->
  IntegerParameterRange
mkIntegerParameterRange pMaxValue_ pName_ pMinValue_ =
  IntegerParameterRange'
    { maxValue = pMaxValue_,
      scalingType = Lude.Nothing,
      name = pName_,
      minValue = pMinValue_
    }

-- | The maximum value of the hyperparameter to search.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprMaxValue :: Lens.Lens' IntegerParameterRange Lude.Text
iprMaxValue = Lens.lens (maxValue :: IntegerParameterRange -> Lude.Text) (\s a -> s {maxValue = a} :: IntegerParameterRange)
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
iprScalingType :: Lens.Lens' IntegerParameterRange (Lude.Maybe HyperParameterScalingType)
iprScalingType = Lens.lens (scalingType :: IntegerParameterRange -> Lude.Maybe HyperParameterScalingType) (\s a -> s {scalingType = a} :: IntegerParameterRange)
{-# DEPRECATED iprScalingType "Use generic-lens or generic-optics with 'scalingType' instead." #-}

-- | The name of the hyperparameter to search.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprName :: Lens.Lens' IntegerParameterRange Lude.Text
iprName = Lens.lens (name :: IntegerParameterRange -> Lude.Text) (\s a -> s {name = a} :: IntegerParameterRange)
{-# DEPRECATED iprName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The minimum value of the hyperparameter to search.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprMinValue :: Lens.Lens' IntegerParameterRange Lude.Text
iprMinValue = Lens.lens (minValue :: IntegerParameterRange -> Lude.Text) (\s a -> s {minValue = a} :: IntegerParameterRange)
{-# DEPRECATED iprMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

instance Lude.FromJSON IntegerParameterRange where
  parseJSON =
    Lude.withObject
      "IntegerParameterRange"
      ( \x ->
          IntegerParameterRange'
            Lude.<$> (x Lude..: "MaxValue")
            Lude.<*> (x Lude..:? "ScalingType")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "MinValue")
      )

instance Lude.ToJSON IntegerParameterRange where
  toJSON IntegerParameterRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MaxValue" Lude..= maxValue),
            ("ScalingType" Lude..=) Lude.<$> scalingType,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("MinValue" Lude..= minValue)
          ]
      )
