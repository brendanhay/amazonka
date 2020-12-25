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
    prContinuousParameterRangeSpecification,
    prIntegerParameterRangeSpecification,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification as Types
import qualified Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification as Types
import qualified Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification as Types

-- | Defines the possible values for categorical, continuous, and integer hyperparameters to be used by an algorithm.
--
-- /See:/ 'mkParameterRange' smart constructor.
data ParameterRange = ParameterRange'
  { -- | A @CategoricalParameterRangeSpecification@ object that defines the possible values for a categorical hyperparameter.
    categoricalParameterRangeSpecification :: Core.Maybe Types.CategoricalParameterRangeSpecification,
    -- | A @ContinuousParameterRangeSpecification@ object that defines the possible values for a continuous hyperparameter.
    continuousParameterRangeSpecification :: Core.Maybe Types.ContinuousParameterRangeSpecification,
    -- | A @IntegerParameterRangeSpecification@ object that defines the possible values for an integer hyperparameter.
    integerParameterRangeSpecification :: Core.Maybe Types.IntegerParameterRangeSpecification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ParameterRange' value with any optional fields omitted.
mkParameterRange ::
  ParameterRange
mkParameterRange =
  ParameterRange'
    { categoricalParameterRangeSpecification =
        Core.Nothing,
      continuousParameterRangeSpecification = Core.Nothing,
      integerParameterRangeSpecification = Core.Nothing
    }

-- | A @CategoricalParameterRangeSpecification@ object that defines the possible values for a categorical hyperparameter.
--
-- /Note:/ Consider using 'categoricalParameterRangeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prCategoricalParameterRangeSpecification :: Lens.Lens' ParameterRange (Core.Maybe Types.CategoricalParameterRangeSpecification)
prCategoricalParameterRangeSpecification = Lens.field @"categoricalParameterRangeSpecification"
{-# DEPRECATED prCategoricalParameterRangeSpecification "Use generic-lens or generic-optics with 'categoricalParameterRangeSpecification' instead." #-}

-- | A @ContinuousParameterRangeSpecification@ object that defines the possible values for a continuous hyperparameter.
--
-- /Note:/ Consider using 'continuousParameterRangeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prContinuousParameterRangeSpecification :: Lens.Lens' ParameterRange (Core.Maybe Types.ContinuousParameterRangeSpecification)
prContinuousParameterRangeSpecification = Lens.field @"continuousParameterRangeSpecification"
{-# DEPRECATED prContinuousParameterRangeSpecification "Use generic-lens or generic-optics with 'continuousParameterRangeSpecification' instead." #-}

-- | A @IntegerParameterRangeSpecification@ object that defines the possible values for an integer hyperparameter.
--
-- /Note:/ Consider using 'integerParameterRangeSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prIntegerParameterRangeSpecification :: Lens.Lens' ParameterRange (Core.Maybe Types.IntegerParameterRangeSpecification)
prIntegerParameterRangeSpecification = Lens.field @"integerParameterRangeSpecification"
{-# DEPRECATED prIntegerParameterRangeSpecification "Use generic-lens or generic-optics with 'integerParameterRangeSpecification' instead." #-}

instance Core.FromJSON ParameterRange where
  toJSON ParameterRange {..} =
    Core.object
      ( Core.catMaybes
          [ ("CategoricalParameterRangeSpecification" Core..=)
              Core.<$> categoricalParameterRangeSpecification,
            ("ContinuousParameterRangeSpecification" Core..=)
              Core.<$> continuousParameterRangeSpecification,
            ("IntegerParameterRangeSpecification" Core..=)
              Core.<$> integerParameterRangeSpecification
          ]
      )

instance Core.FromJSON ParameterRange where
  parseJSON =
    Core.withObject "ParameterRange" Core.$
      \x ->
        ParameterRange'
          Core.<$> (x Core..:? "CategoricalParameterRangeSpecification")
          Core.<*> (x Core..:? "ContinuousParameterRangeSpecification")
          Core.<*> (x Core..:? "IntegerParameterRangeSpecification")
