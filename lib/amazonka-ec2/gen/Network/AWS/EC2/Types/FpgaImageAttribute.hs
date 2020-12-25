{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImageAttribute
  ( FpgaImageAttribute (..),

    -- * Smart constructor
    mkFpgaImageAttribute,

    -- * Lenses
    fiaDescription,
    fiaFpgaImageId,
    fiaLoadPermissions,
    fiaName,
    fiaProductCodes,
  )
where

import qualified Network.AWS.EC2.Types.LoadPermission as Types
import qualified Network.AWS.EC2.Types.ProductCode as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon FPGA image (AFI) attribute.
--
-- /See:/ 'mkFpgaImageAttribute' smart constructor.
data FpgaImageAttribute = FpgaImageAttribute'
  { -- | The description of the AFI.
    description :: Core.Maybe Types.String,
    -- | The ID of the AFI.
    fpgaImageId :: Core.Maybe Types.String,
    -- | The load permissions.
    loadPermissions :: Core.Maybe [Types.LoadPermission],
    -- | The name of the AFI.
    name :: Core.Maybe Types.String,
    -- | The product codes.
    productCodes :: Core.Maybe [Types.ProductCode]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FpgaImageAttribute' value with any optional fields omitted.
mkFpgaImageAttribute ::
  FpgaImageAttribute
mkFpgaImageAttribute =
  FpgaImageAttribute'
    { description = Core.Nothing,
      fpgaImageId = Core.Nothing,
      loadPermissions = Core.Nothing,
      name = Core.Nothing,
      productCodes = Core.Nothing
    }

-- | The description of the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaDescription :: Lens.Lens' FpgaImageAttribute (Core.Maybe Types.String)
fiaDescription = Lens.field @"description"
{-# DEPRECATED fiaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaFpgaImageId :: Lens.Lens' FpgaImageAttribute (Core.Maybe Types.String)
fiaFpgaImageId = Lens.field @"fpgaImageId"
{-# DEPRECATED fiaFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The load permissions.
--
-- /Note:/ Consider using 'loadPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaLoadPermissions :: Lens.Lens' FpgaImageAttribute (Core.Maybe [Types.LoadPermission])
fiaLoadPermissions = Lens.field @"loadPermissions"
{-# DEPRECATED fiaLoadPermissions "Use generic-lens or generic-optics with 'loadPermissions' instead." #-}

-- | The name of the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaName :: Lens.Lens' FpgaImageAttribute (Core.Maybe Types.String)
fiaName = Lens.field @"name"
{-# DEPRECATED fiaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaProductCodes :: Lens.Lens' FpgaImageAttribute (Core.Maybe [Types.ProductCode])
fiaProductCodes = Lens.field @"productCodes"
{-# DEPRECATED fiaProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

instance Core.FromXML FpgaImageAttribute where
  parseXML x =
    FpgaImageAttribute'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "fpgaImageId")
      Core.<*> (x Core..@? "loadPermissions" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
