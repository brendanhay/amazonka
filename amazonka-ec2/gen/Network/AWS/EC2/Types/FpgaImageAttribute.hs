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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.LoadPermission
import Network.AWS.EC2.Types.ProductCode
import qualified Network.AWS.Lens as Lens

-- | Describes an Amazon FPGA image (AFI) attribute.
--
-- /See:/ 'newFpgaImageAttribute' smart constructor.
data FpgaImageAttribute = FpgaImageAttribute'
  { -- | The product codes.
    productCodes :: Core.Maybe [ProductCode],
    -- | The name of the AFI.
    name :: Core.Maybe Core.Text,
    -- | The load permissions.
    loadPermissions :: Core.Maybe [LoadPermission],
    -- | The description of the AFI.
    description :: Core.Maybe Core.Text,
    -- | The ID of the AFI.
    fpgaImageId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { productCodes = Core.Nothing,
      name = Core.Nothing,
      loadPermissions = Core.Nothing,
      description = Core.Nothing,
      fpgaImageId = Core.Nothing
    }

-- | The product codes.
fpgaImageAttribute_productCodes :: Lens.Lens' FpgaImageAttribute (Core.Maybe [ProductCode])
fpgaImageAttribute_productCodes = Lens.lens (\FpgaImageAttribute' {productCodes} -> productCodes) (\s@FpgaImageAttribute' {} a -> s {productCodes = a} :: FpgaImageAttribute) Core.. Lens.mapping Lens._Coerce

-- | The name of the AFI.
fpgaImageAttribute_name :: Lens.Lens' FpgaImageAttribute (Core.Maybe Core.Text)
fpgaImageAttribute_name = Lens.lens (\FpgaImageAttribute' {name} -> name) (\s@FpgaImageAttribute' {} a -> s {name = a} :: FpgaImageAttribute)

-- | The load permissions.
fpgaImageAttribute_loadPermissions :: Lens.Lens' FpgaImageAttribute (Core.Maybe [LoadPermission])
fpgaImageAttribute_loadPermissions = Lens.lens (\FpgaImageAttribute' {loadPermissions} -> loadPermissions) (\s@FpgaImageAttribute' {} a -> s {loadPermissions = a} :: FpgaImageAttribute) Core.. Lens.mapping Lens._Coerce

-- | The description of the AFI.
fpgaImageAttribute_description :: Lens.Lens' FpgaImageAttribute (Core.Maybe Core.Text)
fpgaImageAttribute_description = Lens.lens (\FpgaImageAttribute' {description} -> description) (\s@FpgaImageAttribute' {} a -> s {description = a} :: FpgaImageAttribute)

-- | The ID of the AFI.
fpgaImageAttribute_fpgaImageId :: Lens.Lens' FpgaImageAttribute (Core.Maybe Core.Text)
fpgaImageAttribute_fpgaImageId = Lens.lens (\FpgaImageAttribute' {fpgaImageId} -> fpgaImageId) (\s@FpgaImageAttribute' {} a -> s {fpgaImageId = a} :: FpgaImageAttribute)

instance Core.FromXML FpgaImageAttribute where
  parseXML x =
    FpgaImageAttribute'
      Core.<$> ( x Core..@? "productCodes" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "name")
      Core.<*> ( x Core..@? "loadPermissions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "fpgaImageId")

instance Core.Hashable FpgaImageAttribute

instance Core.NFData FpgaImageAttribute
