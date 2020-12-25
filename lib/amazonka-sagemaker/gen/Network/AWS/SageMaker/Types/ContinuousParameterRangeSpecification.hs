{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
  ( ContinuousParameterRangeSpecification (..),

    -- * Smart constructor
    mkContinuousParameterRangeSpecification,

    -- * Lenses
    cprsMinValue,
    cprsMaxValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ParameterValue as Types

-- | Defines the possible values for a continuous hyperparameter.
--
-- /See:/ 'mkContinuousParameterRangeSpecification' smart constructor.
data ContinuousParameterRangeSpecification = ContinuousParameterRangeSpecification'
  { -- | The minimum floating-point value allowed.
    minValue :: Types.ParameterValue,
    -- | The maximum floating-point value allowed.
    maxValue :: Types.ParameterValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContinuousParameterRangeSpecification' value with any optional fields omitted.
mkContinuousParameterRangeSpecification ::
  -- | 'minValue'
  Types.ParameterValue ->
  -- | 'maxValue'
  Types.ParameterValue ->
  ContinuousParameterRangeSpecification
mkContinuousParameterRangeSpecification minValue maxValue =
  ContinuousParameterRangeSpecification' {minValue, maxValue}

-- | The minimum floating-point value allowed.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsMinValue :: Lens.Lens' ContinuousParameterRangeSpecification Types.ParameterValue
cprsMinValue = Lens.field @"minValue"
{-# DEPRECATED cprsMinValue "Use generic-lens or generic-optics with 'minValue' instead." #-}

-- | The maximum floating-point value allowed.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsMaxValue :: Lens.Lens' ContinuousParameterRangeSpecification Types.ParameterValue
cprsMaxValue = Lens.field @"maxValue"
{-# DEPRECATED cprsMaxValue "Use generic-lens or generic-optics with 'maxValue' instead." #-}

instance Core.FromJSON ContinuousParameterRangeSpecification where
  toJSON ContinuousParameterRangeSpecification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MinValue" Core..= minValue),
            Core.Just ("MaxValue" Core..= maxValue)
          ]
      )

instance Core.FromJSON ContinuousParameterRangeSpecification where
  parseJSON =
    Core.withObject "ContinuousParameterRangeSpecification" Core.$
      \x ->
        ContinuousParameterRangeSpecification'
          Core.<$> (x Core..: "MinValue") Core.<*> (x Core..: "MaxValue")
