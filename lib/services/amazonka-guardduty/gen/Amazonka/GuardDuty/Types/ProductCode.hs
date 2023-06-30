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
-- Module      : Amazonka.GuardDuty.Types.ProductCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ProductCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the product code for the EC2 instance.
--
-- /See:/ 'newProductCode' smart constructor.
data ProductCode = ProductCode'
  { -- | The product code information.
    code :: Prelude.Maybe Prelude.Text,
    -- | The product code type.
    productType :: Prelude.Maybe Prelude.Text
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

instance Data.FromJSON ProductCode where
  parseJSON =
    Data.withObject
      "ProductCode"
      ( \x ->
          ProductCode'
            Prelude.<$> (x Data..:? "productCodeId")
            Prelude.<*> (x Data..:? "productCodeType")
      )

instance Prelude.Hashable ProductCode where
  hashWithSalt _salt ProductCode' {..} =
    _salt
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` productType

instance Prelude.NFData ProductCode where
  rnf ProductCode' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf productType
