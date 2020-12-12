{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParameterRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParameterRange
  ( ParameterRange (..),

    -- * Smart constructor
    mkParameterRange,

    -- * Lenses
    prCategoricalParameterRangeSpecification,
    prIntegerParameterRangeSpecification,
    prContinuousParameterRangeSpecification,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
import Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
import Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification

-- | Defines the possible values for categorical, continuous, and integer hyperparameters to be used by an algorithm.
--
-- /See:/ 'mkParameterRange' smart constructor.
data ParameterRange = ParameterRange'
  { categoricalParameterRangeSpecification ::
      Lude.Maybe CategoricalParameterRangeSpecification,
    integerParameterRangeSpecification ::
      Lude.Maybe IntegerParameterRangeSpecification,
    continuousParameterRangeSpecification ::
      Lude.Maybe ContinuousParameterRangeSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterRange' with the minimum fields required to make a request.
--
-- * 'categoricalParameterRangeSpecification' - A @CategoricalParameterRangeSpecification@ object that defines the possible values for a categorical hyperparameter.
-- * 'continuousParameterRangeSpecification' - A @ContinuousParameterRangeSpecification@ object that defines the possible values for a continuous hyperparameter.
-- * 'integerParameterRangeSpecification' - A @IntegerParameterRangeSpecification@ object that defines the possible values for an integer hyperparameter.
mkParameterRange ::
  ParameterRange
mkParameterRange =
  ParameterRange'
    { categoricalParameterRangeSpecification =
        Lude.Nothing,
      integerParameterRangeSpecification = Lude.Nothing,
      continuousParameterRangeSpecification = Lude.Nothing
    }

-- | A @CategoricalParameterRangeSpecification@ object that defines the possible values for a categorical hyperparameter.
--
-- /Note:/ Consider using 'categoricalParameterRangeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prCategoricalParameterRangeSpecification :: Lens.Lens' ParameterRange (Lude.Maybe CategoricalParameterRangeSpecification)
prCategoricalParameterRangeSpecification = Lens.lens (categoricalParameterRangeSpecification :: ParameterRange -> Lude.Maybe CategoricalParameterRangeSpecification) (\s a -> s {categoricalParameterRangeSpecification = a} :: ParameterRange)
{-# DEPRECATED prCategoricalParameterRangeSpecification "Use generic-lens or generic-optics with 'categoricalParameterRangeSpecification' instead." #-}

-- | A @IntegerParameterRangeSpecification@ object that defines the possible values for an integer hyperparameter.
--
-- /Note:/ Consider using 'integerParameterRangeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prIntegerParameterRangeSpecification :: Lens.Lens' ParameterRange (Lude.Maybe IntegerParameterRangeSpecification)
prIntegerParameterRangeSpecification = Lens.lens (integerParameterRangeSpecification :: ParameterRange -> Lude.Maybe IntegerParameterRangeSpecification) (\s a -> s {integerParameterRangeSpecification = a} :: ParameterRange)
{-# DEPRECATED prIntegerParameterRangeSpecification "Use generic-lens or generic-optics with 'integerParameterRangeSpecification' instead." #-}

-- | A @ContinuousParameterRangeSpecification@ object that defines the possible values for a continuous hyperparameter.
--
-- /Note:/ Consider using 'continuousParameterRangeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prContinuousParameterRangeSpecification :: Lens.Lens' ParameterRange (Lude.Maybe ContinuousParameterRangeSpecification)
prContinuousParameterRangeSpecification = Lens.lens (continuousParameterRangeSpecification :: ParameterRange -> Lude.Maybe ContinuousParameterRangeSpecification) (\s a -> s {continuousParameterRangeSpecification = a} :: ParameterRange)
{-# DEPRECATED prContinuousParameterRangeSpecification "Use generic-lens or generic-optics with 'continuousParameterRangeSpecification' instead." #-}

instance Lude.FromJSON ParameterRange where
  parseJSON =
    Lude.withObject
      "ParameterRange"
      ( \x ->
          ParameterRange'
            Lude.<$> (x Lude..:? "CategoricalParameterRangeSpecification")
            Lude.<*> (x Lude..:? "IntegerParameterRangeSpecification")
            Lude.<*> (x Lude..:? "ContinuousParameterRangeSpecification")
      )

instance Lude.ToJSON ParameterRange where
  toJSON ParameterRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CategoricalParameterRangeSpecification" Lude..=)
              Lude.<$> categoricalParameterRangeSpecification,
            ("IntegerParameterRangeSpecification" Lude..=)
              Lude.<$> integerParameterRangeSpecification,
            ("ContinuousParameterRangeSpecification" Lude..=)
              Lude.<$> continuousParameterRangeSpecification
          ]
      )
