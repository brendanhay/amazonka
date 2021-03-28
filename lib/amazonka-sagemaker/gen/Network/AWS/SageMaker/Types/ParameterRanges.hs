{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ParameterRanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ParameterRanges
  ( ParameterRanges (..)
  -- * Smart constructor
  , mkParameterRanges
  -- * Lenses
  , prCategoricalParameterRanges
  , prContinuousParameterRanges
  , prIntegerParameterRanges
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CategoricalParameterRange as Types
import qualified Network.AWS.SageMaker.Types.ContinuousParameterRange as Types
import qualified Network.AWS.SageMaker.Types.IntegerParameterRange as Types

-- | Specifies ranges of integer, continuous, and categorical hyperparameters that a hyperparameter tuning job searches. The hyperparameter tuning job launches training jobs with hyperparameter values within these ranges to find the combination of values that result in the training job with the best performance as measured by the objective metric of the hyperparameter tuning job.
--
-- /See:/ 'mkParameterRanges' smart constructor.
data ParameterRanges = ParameterRanges'
  { categoricalParameterRanges :: Core.Maybe [Types.CategoricalParameterRange]
    -- ^ The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
  , continuousParameterRanges :: Core.Maybe [Types.ContinuousParameterRange]
    -- ^ The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
  , integerParameterRanges :: Core.Maybe [Types.IntegerParameterRange]
    -- ^ The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterRanges' value with any optional fields omitted.
mkParameterRanges
    :: ParameterRanges
mkParameterRanges
  = ParameterRanges'{categoricalParameterRanges = Core.Nothing,
                     continuousParameterRanges = Core.Nothing,
                     integerParameterRanges = Core.Nothing}

-- | The array of 'CategoricalParameterRange' objects that specify ranges of categorical hyperparameters that a hyperparameter tuning job searches.
--
-- /Note:/ Consider using 'categoricalParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prCategoricalParameterRanges :: Lens.Lens' ParameterRanges (Core.Maybe [Types.CategoricalParameterRange])
prCategoricalParameterRanges = Lens.field @"categoricalParameterRanges"
{-# INLINEABLE prCategoricalParameterRanges #-}
{-# DEPRECATED categoricalParameterRanges "Use generic-lens or generic-optics with 'categoricalParameterRanges' instead"  #-}

-- | The array of 'ContinuousParameterRange' objects that specify ranges of continuous hyperparameters that a hyperparameter tuning job searches.
--
-- /Note:/ Consider using 'continuousParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prContinuousParameterRanges :: Lens.Lens' ParameterRanges (Core.Maybe [Types.ContinuousParameterRange])
prContinuousParameterRanges = Lens.field @"continuousParameterRanges"
{-# INLINEABLE prContinuousParameterRanges #-}
{-# DEPRECATED continuousParameterRanges "Use generic-lens or generic-optics with 'continuousParameterRanges' instead"  #-}

-- | The array of 'IntegerParameterRange' objects that specify ranges of integer hyperparameters that a hyperparameter tuning job searches.
--
-- /Note:/ Consider using 'integerParameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prIntegerParameterRanges :: Lens.Lens' ParameterRanges (Core.Maybe [Types.IntegerParameterRange])
prIntegerParameterRanges = Lens.field @"integerParameterRanges"
{-# INLINEABLE prIntegerParameterRanges #-}
{-# DEPRECATED integerParameterRanges "Use generic-lens or generic-optics with 'integerParameterRanges' instead"  #-}

instance Core.FromJSON ParameterRanges where
        toJSON ParameterRanges{..}
          = Core.object
              (Core.catMaybes
                 [("CategoricalParameterRanges" Core..=) Core.<$>
                    categoricalParameterRanges,
                  ("ContinuousParameterRanges" Core..=) Core.<$>
                    continuousParameterRanges,
                  ("IntegerParameterRanges" Core..=) Core.<$>
                    integerParameterRanges])

instance Core.FromJSON ParameterRanges where
        parseJSON
          = Core.withObject "ParameterRanges" Core.$
              \ x ->
                ParameterRanges' Core.<$>
                  (x Core..:? "CategoricalParameterRanges") Core.<*>
                    x Core..:? "ContinuousParameterRanges"
                    Core.<*> x Core..:? "IntegerParameterRanges"
