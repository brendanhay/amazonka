{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewAggregationValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A single product view aggregation value\/count pair, containing metadata
-- about each product to which the calling user has access.
--
-- /See:/ 'newProductViewAggregationValue' smart constructor.
data ProductViewAggregationValue = ProductViewAggregationValue'
  { -- | The value of the product view aggregation.
    value :: Core.Maybe Core.Text,
    -- | An approximate count of the products that match the value.
    approximateCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProductViewAggregationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'productViewAggregationValue_value' - The value of the product view aggregation.
--
-- 'approximateCount', 'productViewAggregationValue_approximateCount' - An approximate count of the products that match the value.
newProductViewAggregationValue ::
  ProductViewAggregationValue
newProductViewAggregationValue =
  ProductViewAggregationValue'
    { value = Core.Nothing,
      approximateCount = Core.Nothing
    }

-- | The value of the product view aggregation.
productViewAggregationValue_value :: Lens.Lens' ProductViewAggregationValue (Core.Maybe Core.Text)
productViewAggregationValue_value = Lens.lens (\ProductViewAggregationValue' {value} -> value) (\s@ProductViewAggregationValue' {} a -> s {value = a} :: ProductViewAggregationValue)

-- | An approximate count of the products that match the value.
productViewAggregationValue_approximateCount :: Lens.Lens' ProductViewAggregationValue (Core.Maybe Core.Int)
productViewAggregationValue_approximateCount = Lens.lens (\ProductViewAggregationValue' {approximateCount} -> approximateCount) (\s@ProductViewAggregationValue' {} a -> s {approximateCount = a} :: ProductViewAggregationValue)

instance Core.FromJSON ProductViewAggregationValue where
  parseJSON =
    Core.withObject
      "ProductViewAggregationValue"
      ( \x ->
          ProductViewAggregationValue'
            Core.<$> (x Core..:? "Value")
            Core.<*> (x Core..:? "ApproximateCount")
      )

instance Core.Hashable ProductViewAggregationValue

instance Core.NFData ProductViewAggregationValue
