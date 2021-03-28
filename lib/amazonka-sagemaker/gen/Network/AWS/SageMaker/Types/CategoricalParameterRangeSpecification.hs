{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
  ( CategoricalParameterRangeSpecification (..)
  -- * Smart constructor
  , mkCategoricalParameterRangeSpecification
  -- * Lenses
  , cprsValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ParameterValue as Types

-- | Defines the possible values for a categorical hyperparameter.
--
-- /See:/ 'mkCategoricalParameterRangeSpecification' smart constructor.
newtype CategoricalParameterRangeSpecification = CategoricalParameterRangeSpecification'
  { values :: Core.NonEmpty Types.ParameterValue
    -- ^ The allowed categories for the hyperparameter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CategoricalParameterRangeSpecification' value with any optional fields omitted.
mkCategoricalParameterRangeSpecification
    :: Core.NonEmpty Types.ParameterValue -- ^ 'values'
    -> CategoricalParameterRangeSpecification
mkCategoricalParameterRangeSpecification values
  = CategoricalParameterRangeSpecification'{values}

-- | The allowed categories for the hyperparameter.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsValues :: Lens.Lens' CategoricalParameterRangeSpecification (Core.NonEmpty Types.ParameterValue)
cprsValues = Lens.field @"values"
{-# INLINEABLE cprsValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON CategoricalParameterRangeSpecification where
        toJSON CategoricalParameterRangeSpecification{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Values" Core..= values)])

instance Core.FromJSON CategoricalParameterRangeSpecification where
        parseJSON
          = Core.withObject "CategoricalParameterRangeSpecification" Core.$
              \ x ->
                CategoricalParameterRangeSpecification' Core.<$>
                  (x Core..: "Values")
