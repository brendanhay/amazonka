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
-- Module      : Amazonka.LicenseManager.Types.ProductInformationFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ProductInformationFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes product information filters.
--
-- /See:/ 'newProductInformationFilter' smart constructor.
data ProductInformationFilter = ProductInformationFilter'
  { -- | Filter value.
    productInformationFilterValue :: Prelude.Maybe [Prelude.Text],
    -- | Filter name.
    productInformationFilterName :: Prelude.Text,
    -- | Logical operator.
    productInformationFilterComparator :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductInformationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productInformationFilterValue', 'productInformationFilter_productInformationFilterValue' - Filter value.
--
-- 'productInformationFilterName', 'productInformationFilter_productInformationFilterName' - Filter name.
--
-- 'productInformationFilterComparator', 'productInformationFilter_productInformationFilterComparator' - Logical operator.
newProductInformationFilter ::
  -- | 'productInformationFilterName'
  Prelude.Text ->
  -- | 'productInformationFilterComparator'
  Prelude.Text ->
  ProductInformationFilter
newProductInformationFilter
  pProductInformationFilterName_
  pProductInformationFilterComparator_ =
    ProductInformationFilter'
      { productInformationFilterValue =
          Prelude.Nothing,
        productInformationFilterName =
          pProductInformationFilterName_,
        productInformationFilterComparator =
          pProductInformationFilterComparator_
      }

-- | Filter value.
productInformationFilter_productInformationFilterValue :: Lens.Lens' ProductInformationFilter (Prelude.Maybe [Prelude.Text])
productInformationFilter_productInformationFilterValue = Lens.lens (\ProductInformationFilter' {productInformationFilterValue} -> productInformationFilterValue) (\s@ProductInformationFilter' {} a -> s {productInformationFilterValue = a} :: ProductInformationFilter) Prelude.. Lens.mapping Lens.coerced

-- | Filter name.
productInformationFilter_productInformationFilterName :: Lens.Lens' ProductInformationFilter Prelude.Text
productInformationFilter_productInformationFilterName = Lens.lens (\ProductInformationFilter' {productInformationFilterName} -> productInformationFilterName) (\s@ProductInformationFilter' {} a -> s {productInformationFilterName = a} :: ProductInformationFilter)

-- | Logical operator.
productInformationFilter_productInformationFilterComparator :: Lens.Lens' ProductInformationFilter Prelude.Text
productInformationFilter_productInformationFilterComparator = Lens.lens (\ProductInformationFilter' {productInformationFilterComparator} -> productInformationFilterComparator) (\s@ProductInformationFilter' {} a -> s {productInformationFilterComparator = a} :: ProductInformationFilter)

instance Data.FromJSON ProductInformationFilter where
  parseJSON =
    Data.withObject
      "ProductInformationFilter"
      ( \x ->
          ProductInformationFilter'
            Prelude.<$> ( x
                            Data..:? "ProductInformationFilterValue"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "ProductInformationFilterName")
            Prelude.<*> (x Data..: "ProductInformationFilterComparator")
      )

instance Prelude.Hashable ProductInformationFilter where
  hashWithSalt _salt ProductInformationFilter' {..} =
    _salt
      `Prelude.hashWithSalt` productInformationFilterValue
      `Prelude.hashWithSalt` productInformationFilterName
      `Prelude.hashWithSalt` productInformationFilterComparator

instance Prelude.NFData ProductInformationFilter where
  rnf ProductInformationFilter' {..} =
    Prelude.rnf productInformationFilterValue
      `Prelude.seq` Prelude.rnf productInformationFilterName
      `Prelude.seq` Prelude.rnf productInformationFilterComparator

instance Data.ToJSON ProductInformationFilter where
  toJSON ProductInformationFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProductInformationFilterValue" Data..=)
              Prelude.<$> productInformationFilterValue,
            Prelude.Just
              ( "ProductInformationFilterName"
                  Data..= productInformationFilterName
              ),
            Prelude.Just
              ( "ProductInformationFilterComparator"
                  Data..= productInformationFilterComparator
              )
          ]
      )
