{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.TotalImpactFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TotalImpactFilter
  ( TotalImpactFilter (..),

    -- * Smart constructor
    mkTotalImpactFilter,

    -- * Lenses
    tifEndValue,
    tifNumericOperator,
    tifStartValue,
  )
where

import Network.AWS.CostExplorer.Types.NumericOperator
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters cost anomalies based on the total impact.
--
-- /See:/ 'mkTotalImpactFilter' smart constructor.
data TotalImpactFilter = TotalImpactFilter'
  { endValue ::
      Lude.Maybe Lude.Double,
    numericOperator :: NumericOperator,
    startValue :: Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TotalImpactFilter' with the minimum fields required to make a request.
--
-- * 'endValue' - The upper bound dollar value used in the filter.
-- * 'numericOperator' - The comparing value used in the filter.
-- * 'startValue' - The lower bound dollar value used in the filter.
mkTotalImpactFilter ::
  -- | 'numericOperator'
  NumericOperator ->
  -- | 'startValue'
  Lude.Double ->
  TotalImpactFilter
mkTotalImpactFilter pNumericOperator_ pStartValue_ =
  TotalImpactFilter'
    { endValue = Lude.Nothing,
      numericOperator = pNumericOperator_,
      startValue = pStartValue_
    }

-- | The upper bound dollar value used in the filter.
--
-- /Note:/ Consider using 'endValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifEndValue :: Lens.Lens' TotalImpactFilter (Lude.Maybe Lude.Double)
tifEndValue = Lens.lens (endValue :: TotalImpactFilter -> Lude.Maybe Lude.Double) (\s a -> s {endValue = a} :: TotalImpactFilter)
{-# DEPRECATED tifEndValue "Use generic-lens or generic-optics with 'endValue' instead." #-}

-- | The comparing value used in the filter.
--
-- /Note:/ Consider using 'numericOperator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifNumericOperator :: Lens.Lens' TotalImpactFilter NumericOperator
tifNumericOperator = Lens.lens (numericOperator :: TotalImpactFilter -> NumericOperator) (\s a -> s {numericOperator = a} :: TotalImpactFilter)
{-# DEPRECATED tifNumericOperator "Use generic-lens or generic-optics with 'numericOperator' instead." #-}

-- | The lower bound dollar value used in the filter.
--
-- /Note:/ Consider using 'startValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tifStartValue :: Lens.Lens' TotalImpactFilter Lude.Double
tifStartValue = Lens.lens (startValue :: TotalImpactFilter -> Lude.Double) (\s a -> s {startValue = a} :: TotalImpactFilter)
{-# DEPRECATED tifStartValue "Use generic-lens or generic-optics with 'startValue' instead." #-}

instance Lude.ToJSON TotalImpactFilter where
  toJSON TotalImpactFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndValue" Lude..=) Lude.<$> endValue,
            Lude.Just ("NumericOperator" Lude..= numericOperator),
            Lude.Just ("StartValue" Lude..= startValue)
          ]
      )
