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
    cScalingType,
    cName,
    cMinValue,
    cMaxValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HyperParameterScalingType

-- | A list of continuous hyperparameters to tune.
--
-- /See:/ 'mkContinuousParameterRange' smart constructor.
data ContinuousParameterRange = ContinuousParameterRange'
  { scalingType ::
      Lude.Maybe HyperParameterScalingType,
    name :: Lude.Text,
    minValue :: Lude.Text,
    maxValue :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinuousParameterRange' with the minimum fields required to make a request.
--
-- * 'maxValue' - The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
-- * 'minValue' - The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
-- * 'name' - The name of the continuous hyperparameter to tune.
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
--     * ReverseLogarithmic
--
--     * Hyperparameter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale.
-- Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
mkContinuousParameterRange ::
  -- | 'name'
  Lude.Text ->
  -- | 'minValue'
  Lude.Text ->
  -- | 'maxValue'
  Lude.Text ->
  ContinuousParameterRange
mkContinuousParameterRange pName_ pMinValue_ pMaxValue_ =
  ContinuousParameterRange'
    { scalingType = Lude.Nothing,
      name = pName_,
      minValue = pMinValue_,
      maxValue = pMaxValue_
    }

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
cScalingType :: Lens.Lens' ContinuousParameterRange (Lude.Maybe HyperParameterScalingType)
cScalingType = Lens.lens (scalingType :: ContinuousParameterRange -> Lude.Maybe HyperParameterScalingType) (\s a -> s {scalingType = a} :: ContinuousParameterRange)
{-# DEPRECATED cScalingType "Use generic-lens or generic-optics with 'scalingType' instead." #-}

-- | The name of the continuous hyperparameter to tune.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cName :: Lens.Lens' ContinuousParameterRange Lude.Text
cName = Lens.lens (name :: ContinuousParameterRange -> Lude.Text) (\s a -> s {name = a} :: ContinuousParameterRange)
{-# DEPRECATED cName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMinValue :: Lens.Lens' ContinuousParameterRange Lude.Text
cMinValue = Lens.lens (minValue :: ContinuousParameterRange -> Lude.Text) (\s a -> s {minValue = a} :: ContinuousParameterRange)
{-# DEPRECATED cMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

-- | The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaxValue :: Lens.Lens' ContinuousParameterRange Lude.Text
cMaxValue = Lens.lens (maxValue :: ContinuousParameterRange -> Lude.Text) (\s a -> s {maxValue = a} :: ContinuousParameterRange)
{-# DEPRECATED cMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

instance Lude.FromJSON ContinuousParameterRange where
  parseJSON =
    Lude.withObject
      "ContinuousParameterRange"
      ( \x ->
          ContinuousParameterRange'
            Lude.<$> (x Lude..:? "ScalingType")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "MinValue")
            Lude.<*> (x Lude..: "MaxValue")
      )

instance Lude.ToJSON ContinuousParameterRange where
  toJSON ContinuousParameterRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ScalingType" Lude..=) Lude.<$> scalingType,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("MinValue" Lude..= minValue),
            Lude.Just ("MaxValue" Lude..= maxValue)
          ]
      )
