{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.Types.ProductCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ProductCode where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the product code for the EC2 instance.
--
-- /See:/ 'newProductCode' smart constructor.
data ProductCode = ProductCode'
  { -- | The product code information.
    code :: Prelude.Maybe Prelude.Text,
    -- | The product code type.
    productType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProductCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'productCode_code' - The product code information.
--
-- 'productType', 'productCode_productType' - The product code type.
newProductCode ::
  ProductCode
newProductCode =
  ProductCode'
    { code = Prelude.Nothing,
      productType = Prelude.Nothing
    }

-- | The product code information.
productCode_code :: Lens.Lens' ProductCode (Prelude.Maybe Prelude.Text)
productCode_code = Lens.lens (\ProductCode' {code} -> code) (\s@ProductCode' {} a -> s {code = a} :: ProductCode)

-- | The product code type.
productCode_productType :: Lens.Lens' ProductCode (Prelude.Maybe Prelude.Text)
productCode_productType = Lens.lens (\ProductCode' {productType} -> productType) (\s@ProductCode' {} a -> s {productType = a} :: ProductCode)

instance Prelude.FromJSON ProductCode where
  parseJSON =
    Prelude.withObject
      "ProductCode"
      ( \x ->
          ProductCode'
            Prelude.<$> (x Prelude..:? "code")
            Prelude.<*> (x Prelude..:? "productType")
      )

instance Prelude.Hashable ProductCode

instance Prelude.NFData ProductCode
