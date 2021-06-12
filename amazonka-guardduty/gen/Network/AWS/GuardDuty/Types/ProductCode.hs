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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the product code for the EC2 instance.
--
-- /See:/ 'newProductCode' smart constructor.
data ProductCode = ProductCode'
  { -- | The product code information.
    code :: Core.Maybe Core.Text,
    -- | The product code type.
    productType :: Core.Maybe Core.Text
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
-- 'code', 'productCode_code' - The product code information.
--
-- 'productType', 'productCode_productType' - The product code type.
newProductCode ::
  ProductCode
newProductCode =
  ProductCode'
    { code = Core.Nothing,
      productType = Core.Nothing
    }

-- | The product code information.
productCode_code :: Lens.Lens' ProductCode (Core.Maybe Core.Text)
productCode_code = Lens.lens (\ProductCode' {code} -> code) (\s@ProductCode' {} a -> s {code = a} :: ProductCode)

-- | The product code type.
productCode_productType :: Lens.Lens' ProductCode (Core.Maybe Core.Text)
productCode_productType = Lens.lens (\ProductCode' {productType} -> productType) (\s@ProductCode' {} a -> s {productType = a} :: ProductCode)

instance Core.FromJSON ProductCode where
  parseJSON =
    Core.withObject
      "ProductCode"
      ( \x ->
          ProductCode'
            Core.<$> (x Core..:? "code")
            Core.<*> (x Core..:? "productType")
      )

instance Core.Hashable ProductCode

instance Core.NFData ProductCode
