-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
  ( IntegerParameterRangeSpecification (..),

    -- * Smart constructor
    mkIntegerParameterRangeSpecification,

    -- * Lenses
    iprsMinValue,
    iprsMaxValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines the possible values for an integer hyperparameter.
--
-- /See:/ 'mkIntegerParameterRangeSpecification' smart constructor.
data IntegerParameterRangeSpecification = IntegerParameterRangeSpecification'
  { minValue ::
      Lude.Text,
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

-- | Creates a value of 'IntegerParameterRangeSpecification' with the minimum fields required to make a request.
--
-- * 'maxValue' - The maximum integer value allowed.
-- * 'minValue' - The minimum integer value allowed.
mkIntegerParameterRangeSpecification ::
  -- | 'minValue'
  Lude.Text ->
  -- | 'maxValue'
  Lude.Text ->
  IntegerParameterRangeSpecification
mkIntegerParameterRangeSpecification pMinValue_ pMaxValue_ =
  IntegerParameterRangeSpecification'
    { minValue = pMinValue_,
      maxValue = pMaxValue_
    }

-- | The minimum integer value allowed.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprsMinValue :: Lens.Lens' IntegerParameterRangeSpecification Lude.Text
iprsMinValue = Lens.lens (minValue :: IntegerParameterRangeSpecification -> Lude.Text) (\s a -> s {minValue = a} :: IntegerParameterRangeSpecification)
{-# DEPRECATED iprsMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

-- | The maximum integer value allowed.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprsMaxValue :: Lens.Lens' IntegerParameterRangeSpecification Lude.Text
iprsMaxValue = Lens.lens (maxValue :: IntegerParameterRangeSpecification -> Lude.Text) (\s a -> s {maxValue = a} :: IntegerParameterRangeSpecification)
{-# DEPRECATED iprsMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

instance Lude.FromJSON IntegerParameterRangeSpecification where
  parseJSON =
    Lude.withObject
      "IntegerParameterRangeSpecification"
      ( \x ->
          IntegerParameterRangeSpecification'
            Lude.<$> (x Lude..: "MinValue") Lude.<*> (x Lude..: "MaxValue")
      )

instance Lude.ToJSON IntegerParameterRangeSpecification where
  toJSON IntegerParameterRangeSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MinValue" Lude..= minValue),
            Lude.Just ("MaxValue" Lude..= maxValue)
          ]
      )
