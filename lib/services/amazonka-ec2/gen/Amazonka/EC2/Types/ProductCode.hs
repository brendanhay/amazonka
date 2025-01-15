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
-- Module      : Amazonka.EC2.Types.ProductCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ProductCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ProductCodeValues
import qualified Amazonka.Prelude as Prelude

-- | Describes a product code.
--
-- /See:/ 'newProductCode' smart constructor.
data ProductCode = ProductCode'
  { -- | The product code.
    productCodeId :: Prelude.Maybe Prelude.Text,
    -- | The type of product code.
    productCodeType :: Prelude.Maybe ProductCodeValues
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productCodeId', 'productCode_productCodeId' - The product code.
--
-- 'productCodeType', 'productCode_productCodeType' - The type of product code.
newProductCode ::
  ProductCode
newProductCode =
  ProductCode'
    { productCodeId = Prelude.Nothing,
      productCodeType = Prelude.Nothing
    }

-- | The product code.
productCode_productCodeId :: Lens.Lens' ProductCode (Prelude.Maybe Prelude.Text)
productCode_productCodeId = Lens.lens (\ProductCode' {productCodeId} -> productCodeId) (\s@ProductCode' {} a -> s {productCodeId = a} :: ProductCode)

-- | The type of product code.
productCode_productCodeType :: Lens.Lens' ProductCode (Prelude.Maybe ProductCodeValues)
productCode_productCodeType = Lens.lens (\ProductCode' {productCodeType} -> productCodeType) (\s@ProductCode' {} a -> s {productCodeType = a} :: ProductCode)

instance Data.FromXML ProductCode where
  parseXML x =
    ProductCode'
      Prelude.<$> (x Data..@? "productCode")
      Prelude.<*> (x Data..@? "type")

instance Prelude.Hashable ProductCode where
  hashWithSalt _salt ProductCode' {..} =
    _salt
      `Prelude.hashWithSalt` productCodeId
      `Prelude.hashWithSalt` productCodeType

instance Prelude.NFData ProductCode where
  rnf ProductCode' {..} =
    Prelude.rnf productCodeId `Prelude.seq`
      Prelude.rnf productCodeType
