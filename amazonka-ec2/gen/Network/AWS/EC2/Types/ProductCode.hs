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
-- Module      : Network.AWS.EC2.Types.ProductCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProductCode where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ProductCodeValues
import qualified Network.AWS.Lens as Lens

-- | Describes a product code.
--
-- /See:/ 'newProductCode' smart constructor.
data ProductCode = ProductCode'
  { -- | The type of product code.
    productCodeType :: Core.Maybe ProductCodeValues,
    -- | The product code.
    productCodeId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProductCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productCodeType', 'productCode_productCodeType' - The type of product code.
--
-- 'productCodeId', 'productCode_productCodeId' - The product code.
newProductCode ::
  ProductCode
newProductCode =
  ProductCode'
    { productCodeType = Core.Nothing,
      productCodeId = Core.Nothing
    }

-- | The type of product code.
productCode_productCodeType :: Lens.Lens' ProductCode (Core.Maybe ProductCodeValues)
productCode_productCodeType = Lens.lens (\ProductCode' {productCodeType} -> productCodeType) (\s@ProductCode' {} a -> s {productCodeType = a} :: ProductCode)

-- | The product code.
productCode_productCodeId :: Lens.Lens' ProductCode (Core.Maybe Core.Text)
productCode_productCodeId = Lens.lens (\ProductCode' {productCodeId} -> productCodeId) (\s@ProductCode' {} a -> s {productCodeId = a} :: ProductCode)

instance Core.FromXML ProductCode where
  parseXML x =
    ProductCode'
      Core.<$> (x Core..@? "type")
      Core.<*> (x Core..@? "productCode")

instance Core.Hashable ProductCode

instance Core.NFData ProductCode
