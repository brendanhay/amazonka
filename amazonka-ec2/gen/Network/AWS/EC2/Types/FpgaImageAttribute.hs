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
-- Module      : Network.AWS.EC2.Types.FpgaImageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageAttribute where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LoadPermission
import Network.AWS.EC2.Types.ProductCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon FPGA image (AFI) attribute.
--
-- /See:/ 'newFpgaImageAttribute' smart constructor.
data FpgaImageAttribute = FpgaImageAttribute'
  { -- | The product codes.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The name of the AFI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The load permissions.
    loadPermissions :: Prelude.Maybe [LoadPermission],
    -- | The description of the AFI.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AFI.
    fpgaImageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FpgaImageAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productCodes', 'fpgaImageAttribute_productCodes' - The product codes.
--
-- 'name', 'fpgaImageAttribute_name' - The name of the AFI.
--
-- 'loadPermissions', 'fpgaImageAttribute_loadPermissions' - The load permissions.
--
-- 'description', 'fpgaImageAttribute_description' - The description of the AFI.
--
-- 'fpgaImageId', 'fpgaImageAttribute_fpgaImageId' - The ID of the AFI.
newFpgaImageAttribute ::
  FpgaImageAttribute
newFpgaImageAttribute =
  FpgaImageAttribute'
    { productCodes = Prelude.Nothing,
      name = Prelude.Nothing,
      loadPermissions = Prelude.Nothing,
      description = Prelude.Nothing,
      fpgaImageId = Prelude.Nothing
    }

-- | The product codes.
fpgaImageAttribute_productCodes :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe [ProductCode])
fpgaImageAttribute_productCodes = Lens.lens (\FpgaImageAttribute' {productCodes} -> productCodes) (\s@FpgaImageAttribute' {} a -> s {productCodes = a} :: FpgaImageAttribute) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the AFI.
fpgaImageAttribute_name :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe Prelude.Text)
fpgaImageAttribute_name = Lens.lens (\FpgaImageAttribute' {name} -> name) (\s@FpgaImageAttribute' {} a -> s {name = a} :: FpgaImageAttribute)

-- | The load permissions.
fpgaImageAttribute_loadPermissions :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe [LoadPermission])
fpgaImageAttribute_loadPermissions = Lens.lens (\FpgaImageAttribute' {loadPermissions} -> loadPermissions) (\s@FpgaImageAttribute' {} a -> s {loadPermissions = a} :: FpgaImageAttribute) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the AFI.
fpgaImageAttribute_description :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe Prelude.Text)
fpgaImageAttribute_description = Lens.lens (\FpgaImageAttribute' {description} -> description) (\s@FpgaImageAttribute' {} a -> s {description = a} :: FpgaImageAttribute)

-- | The ID of the AFI.
fpgaImageAttribute_fpgaImageId :: Lens.Lens' FpgaImageAttribute (Prelude.Maybe Prelude.Text)
fpgaImageAttribute_fpgaImageId = Lens.lens (\FpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@FpgaImageAttribute' {} a -> s {fpgaImageId = a} :: FpgaImageAttribute)

instance Prelude.FromXML FpgaImageAttribute where
  parseXML x =
    FpgaImageAttribute'
      Prelude.<$> ( x Prelude..@? "productCodes"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "name")
      Prelude.<*> ( x Prelude..@? "loadPermissions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "description")
      Prelude.<*> (x Prelude..@? "fpgaImageId")

instance Prelude.Hashable FpgaImageAttribute

instance Prelude.NFData FpgaImageAttribute
