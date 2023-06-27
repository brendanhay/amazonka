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
-- Module      : Amazonka.ServiceCatalog.Types.ProductViewAggregationValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProductViewAggregationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A single product view aggregation value\/count pair, containing metadata
-- about each product to which the calling user has access.
--
-- /See:/ 'newProductViewAggregationValue' smart constructor.
data ProductViewAggregationValue = ProductViewAggregationValue'
  { -- | An approximate count of the products that match the value.
    approximateCount :: Prelude.Maybe Prelude.Int,
    -- | The value of the product view aggregation.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductViewAggregationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateCount', 'productViewAggregationValue_approximateCount' - An approximate count of the products that match the value.
--
-- 'value', 'productViewAggregationValue_value' - The value of the product view aggregation.
newProductViewAggregationValue ::
  ProductViewAggregationValue
newProductViewAggregationValue =
  ProductViewAggregationValue'
    { approximateCount =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | An approximate count of the products that match the value.
productViewAggregationValue_approximateCount :: Lens.Lens' ProductViewAggregationValue (Prelude.Maybe Prelude.Int)
productViewAggregationValue_approximateCount = Lens.lens (\ProductViewAggregationValue' {approximateCount} -> approximateCount) (\s@ProductViewAggregationValue' {} a -> s {approximateCount = a} :: ProductViewAggregationValue)

-- | The value of the product view aggregation.
productViewAggregationValue_value :: Lens.Lens' ProductViewAggregationValue (Prelude.Maybe Prelude.Text)
productViewAggregationValue_value = Lens.lens (\ProductViewAggregationValue' {value} -> value) (\s@ProductViewAggregationValue' {} a -> s {value = a} :: ProductViewAggregationValue)

instance Data.FromJSON ProductViewAggregationValue where
  parseJSON =
    Data.withObject
      "ProductViewAggregationValue"
      ( \x ->
          ProductViewAggregationValue'
            Prelude.<$> (x Data..:? "ApproximateCount")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable ProductViewAggregationValue where
  hashWithSalt _salt ProductViewAggregationValue' {..} =
    _salt
      `Prelude.hashWithSalt` approximateCount
      `Prelude.hashWithSalt` value

instance Prelude.NFData ProductViewAggregationValue where
  rnf ProductViewAggregationValue' {..} =
    Prelude.rnf approximateCount
      `Prelude.seq` Prelude.rnf value
