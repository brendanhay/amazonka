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
    cMaxValue,
    cScalingType,
    cName,
    cMinValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HyperParameterScalingType

-- | A list of continuous hyperparameters to tune.
--
-- /See:/ 'mkContinuousParameterRange' smart constructor.
data ContinuousParameterRange = ContinuousParameterRange'
  { -- | The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
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
    --
    --
    --     * ReverseLogarithmic
    --
    --     * Hyperparameter tuning searches the values in the hyperparameter range by using a reverse logarithmic scale.
    -- Reverse logarithmic scaling works only for ranges that are entirely within the range 0<=x<1.0.
    scalingType :: Lude.Maybe HyperParameterScalingType,
    -- | The name of the continuous hyperparameter to tune.
    name :: Lude.Text,
    -- | The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
    minValue :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContinuousParameterRange' with the minimum fields required to make a request.
--
-- * 'maxValue' - The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
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
--
--
-- * 'name' - The name of the continuous hyperparameter to tune.
-- * 'minValue' - The minimum value for the hyperparameter. The tuning job uses floating-point values between this value and @MaxValue@ for tuning.
mkContinuousParameterRange ::
  -- | 'maxValue'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'minValue'
  Lude.Text ->
  ContinuousParameterRange
mkContinuousParameterRange pMaxValue_ pName_ pMinValue_ =
  ContinuousParameterRange'
    { maxValue = pMaxValue_,
      scalingType = Lude.Nothing,
      name = pName_,
      minValue = pMinValue_
    }

-- | The maximum value for the hyperparameter. The tuning job uses floating-point values between @MinValue@ value and this value for tuning.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cMaxValue :: Lens.Lens' ContinuousParameterRange Lude.Text
cMaxValue = Lens.lens (maxValue :: ContinuousParameterRange -> Lude.Text) (\s a -> s {maxValue = a} :: ContinuousParameterRange)
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

instance Lude.FromJSON ContinuousParameterRange where
  parseJSON =
    Lude.withObject
      "ContinuousParameterRange"
      ( \x ->
          ContinuousParameterRange'
            Lude.<$> (x Lude..: "MaxValue")
            Lude.<*> (x Lude..:? "ScalingType")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "MinValue")
      )

instance Lude.ToJSON ContinuousParameterRange where
  toJSON ContinuousParameterRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MaxValue" Lude..= maxValue),
            ("ScalingType" Lude..=) Lude.<$> scalingType,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("MinValue" Lude..= minValue)
          ]
      )
