{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
  ( ProductViewAggregationValue (..),

    -- * Smart constructor
    mkProductViewAggregationValue,

    -- * Lenses
    pvavValue,
    pvavApproximateCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A single product view aggregation value/count pair, containing metadata about each product to which the calling user has access.
--
-- /See:/ 'mkProductViewAggregationValue' smart constructor.
data ProductViewAggregationValue = ProductViewAggregationValue'
  { -- | The value of the product view aggregation.
    value :: Lude.Maybe Lude.Text,
    -- | An approximate count of the products that match the value.
    approximateCount :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProductViewAggregationValue' with the minimum fields required to make a request.
--
-- * 'value' - The value of the product view aggregation.
-- * 'approximateCount' - An approximate count of the products that match the value.
mkProductViewAggregationValue ::
  ProductViewAggregationValue
mkProductViewAggregationValue =
  ProductViewAggregationValue'
    { value = Lude.Nothing,
      approximateCount = Lude.Nothing
    }

-- | The value of the product view aggregation.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvavValue :: Lens.Lens' ProductViewAggregationValue (Lude.Maybe Lude.Text)
pvavValue = Lens.lens (value :: ProductViewAggregationValue -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ProductViewAggregationValue)
{-# DEPRECATED pvavValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | An approximate count of the products that match the value.
--
-- /Note:/ Consider using 'approximateCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvavApproximateCount :: Lens.Lens' ProductViewAggregationValue (Lude.Maybe Lude.Int)
pvavApproximateCount = Lens.lens (approximateCount :: ProductViewAggregationValue -> Lude.Maybe Lude.Int) (\s a -> s {approximateCount = a} :: ProductViewAggregationValue)
{-# DEPRECATED pvavApproximateCount "Use generic-lens or generic-optics with 'approximateCount' instead." #-}

instance Lude.FromJSON ProductViewAggregationValue where
  parseJSON =
    Lude.withObject
      "ProductViewAggregationValue"
      ( \x ->
          ProductViewAggregationValue'
            Lude.<$> (x Lude..:? "Value") Lude.<*> (x Lude..:? "ApproximateCount")
      )
