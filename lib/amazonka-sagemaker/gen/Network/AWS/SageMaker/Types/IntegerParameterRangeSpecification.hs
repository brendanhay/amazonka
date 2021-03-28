{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
  ( IntegerParameterRangeSpecification (..)
  -- * Smart constructor
  , mkIntegerParameterRangeSpecification
  -- * Lenses
  , iprsMinValue
  , iprsMaxValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ParameterValue as Types

-- | Defines the possible values for an integer hyperparameter.
--
-- /See:/ 'mkIntegerParameterRangeSpecification' smart constructor.
data IntegerParameterRangeSpecification = IntegerParameterRangeSpecification'
  { minValue :: Types.ParameterValue
    -- ^ The minimum integer value allowed.
  , maxValue :: Types.ParameterValue
    -- ^ The maximum integer value allowed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IntegerParameterRangeSpecification' value with any optional fields omitted.
mkIntegerParameterRangeSpecification
    :: Types.ParameterValue -- ^ 'minValue'
    -> Types.ParameterValue -- ^ 'maxValue'
    -> IntegerParameterRangeSpecification
mkIntegerParameterRangeSpecification minValue maxValue
  = IntegerParameterRangeSpecification'{minValue, maxValue}

-- | The minimum integer value allowed.
--
-- /Note:/ Consider using 'minValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprsMinValue :: Lens.Lens' IntegerParameterRangeSpecification Types.ParameterValue
iprsMinValue = Lens.field @"minValue"
{-# INLINEABLE iprsMinValue #-}
{-# DEPRECATED minValue "Use generic-lens or generic-optics with 'minValue' instead"  #-}

-- | The maximum integer value allowed.
--
-- /Note:/ Consider using 'maxValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iprsMaxValue :: Lens.Lens' IntegerParameterRangeSpecification Types.ParameterValue
iprsMaxValue = Lens.field @"maxValue"
{-# INLINEABLE iprsMaxValue #-}
{-# DEPRECATED maxValue "Use generic-lens or generic-optics with 'maxValue' instead"  #-}

instance Core.FromJSON IntegerParameterRangeSpecification where
        toJSON IntegerParameterRangeSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MinValue" Core..= minValue),
                  Core.Just ("MaxValue" Core..= maxValue)])

instance Core.FromJSON IntegerParameterRangeSpecification where
        parseJSON
          = Core.withObject "IntegerParameterRangeSpecification" Core.$
              \ x ->
                IntegerParameterRangeSpecification' Core.<$>
                  (x Core..: "MinValue") Core.<*> x Core..: "MaxValue"
