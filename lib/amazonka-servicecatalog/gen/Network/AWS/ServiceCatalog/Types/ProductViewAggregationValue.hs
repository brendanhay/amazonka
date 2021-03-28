{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
  ( ProductViewAggregationValue (..)
  -- * Smart constructor
  , mkProductViewAggregationValue
  -- * Lenses
  , pvavApproximateCount
  , pvavValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.AttributeValue as Types

-- | A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.
--
-- /See:/ 'mkProductViewAggregationValue' smart constructor.
data ProductViewAggregationValue = ProductViewAggregationValue'
  { approximateCount :: Core.Maybe Core.Int
    -- ^ An approximate count of the products that match the value.
  , value :: Core.Maybe Types.AttributeValue
    -- ^ The value of the product view aggregation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProductViewAggregationValue' value with any optional fields omitted.
mkProductViewAggregationValue
    :: ProductViewAggregationValue
mkProductViewAggregationValue
  = ProductViewAggregationValue'{approximateCount = Core.Nothing,
                                 value = Core.Nothing}

-- | An approximate count of the products that match the value.
--
-- /Note:/ Consider using 'approximateCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvavApproximateCount :: Lens.Lens' ProductViewAggregationValue (Core.Maybe Core.Int)
pvavApproximateCount = Lens.field @"approximateCount"
{-# INLINEABLE pvavApproximateCount #-}
{-# DEPRECATED approximateCount "Use generic-lens or generic-optics with 'approximateCount' instead"  #-}

-- | The value of the product view aggregation.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvavValue :: Lens.Lens' ProductViewAggregationValue (Core.Maybe Types.AttributeValue)
pvavValue = Lens.field @"value"
{-# INLINEABLE pvavValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON ProductViewAggregationValue where
        parseJSON
          = Core.withObject "ProductViewAggregationValue" Core.$
              \ x ->
                ProductViewAggregationValue' Core.<$>
                  (x Core..:? "ApproximateCount") Core.<*> x Core..:? "Value"
