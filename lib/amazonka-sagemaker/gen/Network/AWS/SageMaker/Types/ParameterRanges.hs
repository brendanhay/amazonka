{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParameterRanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ParameterRanges
  ( ParameterRanges (..),

    -- * Smart constructor
    mkParameterRanges,

    -- * Lenses
    prCategoricalParameterRanges,
    prIntegerParameterRanges,
    prContinuousParameterRanges,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CategoricalParameterRange
import Network.AWS.SageMaker.Types.ContinuousParameterRange
import Network.AWS.SageMaker.Types.IntegerParameterRange

-- | Specifies ranges of integer, continuous, and categorical hyperparameters that a hyperparameter tuning job searches. The hyperparameter tuning job launches training jobs with hyperparameter values within these ranges to find the combination of values that result in the training job with the best performance as measured by the objective metric of the hyperparameter tuning job.
--
-- /See:/ 'mkParameterRanges' smart constructor.
data ParameterRanges = ParameterRanges'
  { categoricalParameterRanges ::
      Lude.Maybe [CategoricalParameterRange],
    integerParameterRanges ::
      Lude.Maybe [IntegerParameterRange],
    continuousParameterRanges ::
      Lude.Maybe [ContinuousParameterRange]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterRanges' with the minimum fields required to make a request.
--
-- * 'categoricalParameterRanges' - The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
-- * 'continuousParameterRanges' - The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
-- * 'integerParameterRanges' - The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
mkParameterRanges ::
  ParameterRanges
mkParameterRanges =
  ParameterRanges'
    { categoricalParameterRanges = Lude.Nothing,
      integerParameterRanges = Lude.Nothing,
      continuousParameterRanges = Lude.Nothing
    }

-- | The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
--
-- /Note:/ Consider using 'categoricalParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prCategoricalParameterRanges :: Lens.Lens' ParameterRanges (Lude.Maybe [CategoricalParameterRange])
prCategoricalParameterRanges = Lens.lens (categoricalParameterRanges :: ParameterRanges -> Lude.Maybe [CategoricalParameterRange]) (\s a -> s {categoricalParameterRanges = a} :: ParameterRanges)
{-# DEPRECATED prCategoricalParameterRanges "Use generic-lens or generic-optics with 'categoricalParameterRanges' instead." #-}

-- | The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
--
-- /Note:/ Consider using 'integerParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prIntegerParameterRanges :: Lens.Lens' ParameterRanges (Lude.Maybe [IntegerParameterRange])
prIntegerParameterRanges = Lens.lens (integerParameterRanges :: ParameterRanges -> Lude.Maybe [IntegerParameterRange]) (\s a -> s {integerParameterRanges = a} :: ParameterRanges)
{-# DEPRECATED prIntegerParameterRanges "Use generic-lens or generic-optics with 'integerParameterRanges' instead." #-}

-- | The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
--
-- /Note:/ Consider using 'continuousParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prContinuousParameterRanges :: Lens.Lens' ParameterRanges (Lude.Maybe [ContinuousParameterRange])
prContinuousParameterRanges = Lens.lens (continuousParameterRanges :: ParameterRanges -> Lude.Maybe [ContinuousParameterRange]) (\s a -> s {continuousParameterRanges = a} :: ParameterRanges)
{-# DEPRECATED prContinuousParameterRanges "Use generic-lens or generic-optics with 'continuousParameterRanges' instead." #-}

instance Lude.FromJSON ParameterRanges where
  parseJSON =
    Lude.withObject
      "ParameterRanges"
      ( \x ->
          ParameterRanges'
            Lude.<$> (x Lude..:? "CategoricalParameterRanges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "IntegerParameterRanges" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ContinuousParameterRanges" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON ParameterRanges where
  toJSON ParameterRanges' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CategoricalParameterRanges" Lude..=)
              Lude.<$> categoricalParameterRanges,
            ("IntegerParameterRanges" Lude..=) Lude.<$> integerParameterRanges,
            ("ContinuousParameterRanges" Lude..=)
              Lude.<$> continuousParameterRanges
          ]
      )
