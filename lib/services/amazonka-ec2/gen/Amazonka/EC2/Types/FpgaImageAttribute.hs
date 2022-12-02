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
-- Module      : Amazonka.EC2.Types.FpgaImageAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FpgaImageAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.LoadPermission
import Amazonka.EC2.Types.ProductCode
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon FPGA image (AFI) attribute.
--
-- /See:/ 'newFpgaImageAttribute' smart constructor.
data FpgaImageAttribute = FpgaImageAttribute'
  { -- | The name of the AFI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The load permissions.
    loadPermissions :: Prelude.Maybe [LoadPermission],
    -- | The product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The description of the AFI.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AFI.
    fpgaImageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FpgaImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'fpgaImageAttribute_name' - The name of the AFI.
--
-- 'loadPermissions', 'fpgaImageAttribute_loadPermissions' - The load permissions.
--
-- 'productCodes', 'fpgaImageAttribute_productCodes' - The product codes.
--
-- 'description', 'fpgaImageAttribute_description' - The description of the AFI.
--
-- 'fpgaImageId', 'fpgaImageAttribute_fpgaImageId' - The ID of the AFI.
newFpgaImageAttribute ::
  FpgaImageAttribute
newFpgaImageAttribute =
  FpgaImageAttribute'
    { name = Prelude.Nothing,
      loadPermissions = Prelude.Nothing,
      productCodes = Prelude.Nothing,
      description = Prelude.Nothing,
      fpgaImageId = Prelude.Nothing
    }

-- | The name of the AFI.
fpgaImageAttribute_name :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe Prelude.Text)
fpgaImageAttribute_name = Lens.lens (\FpgaImageAttribute' {name} -> name) (\s@FpgaImageAttribute' {} a -> s {name = a} :: FpgaImageAttribute)

-- | The load permissions.
fpgaImageAttribute_loadPermissions :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe [LoadPermission])
fpgaImageAttribute_loadPermissions = Lens.lens (\FpgaImageAttribute' {loadPermissions} -> loadPermissions) (\s@FpgaImageAttribute' {} a -> s {loadPermissions = a} :: FpgaImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The product codes.
fpgaImageAttribute_productCodes :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe [ProductCode])
fpgaImageAttribute_productCodes = Lens.lens (\FpgaImageAttribute' {productCodes} -> productCodes) (\s@FpgaImageAttribute' {} a -> s {productCodes = a} :: FpgaImageAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The description of the AFI.
fpgaImageAttribute_description :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe Prelude.Text)
fpgaImageAttribute_description = Lens.lens (\FpgaImageAttribute' {description} -> description) (\s@FpgaImageAttribute' {} a -> s {description = a} :: FpgaImageAttribute)

-- | The ID of the AFI.
fpgaImageAttribute_fpgaImageId :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe Prelude.Text)
fpgaImageAttribute_fpgaImageId = Lens.lens (\FpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@FpgaImageAttribute' {} a -> s {fpgaImageId = a} :: FpgaImageAttribute)

instance Data.FromXML FpgaImageAttribute where
  parseXML x =
    FpgaImageAttribute'
      Prelude.<$> (x Data..@? "name")
      Prelude.<*> ( x Data..@? "loadPermissions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "productCodes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "fpgaImageId")

instance Prelude.Hashable FpgaImageAttribute where
  hashWithSalt _salt FpgaImageAttribute' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` loadPermissions
      `Prelude.hashWithSalt` productCodes
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fpgaImageId

instance Prelude.NFData FpgaImageAttribute where
  rnf FpgaImageAttribute' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf loadPermissions
      `Prelude.seq` Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fpgaImageId
