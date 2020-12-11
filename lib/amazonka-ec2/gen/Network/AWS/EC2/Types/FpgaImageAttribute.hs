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
    fiaFpgaImageId,
    fiaName,
    fiaProductCodes,
    fiaDescription,
    fiaLoadPermissions,
  )
where

import Network.AWS.EC2.Types.LoadPermission
import Network.AWS.EC2.Types.ProductCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon FPGA image (AFI) attribute.
--
-- /See:/ 'mkFpgaImageAttribute' smart constructor.
data FpgaImageAttribute = FpgaImageAttribute'
  { fpgaImageId ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    productCodes :: Lude.Maybe [ProductCode],
    description :: Lude.Maybe Lude.Text,
    loadPermissions :: Lude.Maybe [LoadPermission]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FpgaImageAttribute' with the minimum fields required to make a request.
--
-- * 'description' - The description of the AFI.
-- * 'fpgaImageId' - The ID of the AFI.
-- * 'loadPermissions' - The load permissions.
-- * 'name' - The name of the AFI.
-- * 'productCodes' - The product codes.
mkFpgaImageAttribute ::
  FpgaImageAttribute
mkFpgaImageAttribute =
  FpgaImageAttribute'
    { fpgaImageId = Lude.Nothing,
      name = Lude.Nothing,
      productCodes = Lude.Nothing,
      description = Lude.Nothing,
      loadPermissions = Lude.Nothing
    }

-- | The ID of the AFI.
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaFpgaImageId :: Lens.Lens' FpgaImageAttribute (Lude.Maybe Lude.Text)
fiaFpgaImageId = Lens.lens (fpgaImageId :: FpgaImageAttribute -> Lude.Maybe Lude.Text) (\s a -> s {fpgaImageId = a} :: FpgaImageAttribute)
{-# DEPRECATED fiaFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The name of the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaName :: Lens.Lens' FpgaImageAttribute (Lude.Maybe Lude.Text)
fiaName = Lens.lens (name :: FpgaImageAttribute -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FpgaImageAttribute)
{-# DEPRECATED fiaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaProductCodes :: Lens.Lens' FpgaImageAttribute (Lude.Maybe [ProductCode])
fiaProductCodes = Lens.lens (productCodes :: FpgaImageAttribute -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: FpgaImageAttribute)
{-# DEPRECATED fiaProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The description of the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaDescription :: Lens.Lens' FpgaImageAttribute (Lude.Maybe Lude.Text)
fiaDescription = Lens.lens (description :: FpgaImageAttribute -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: FpgaImageAttribute)
{-# DEPRECATED fiaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The load permissions.
--
-- /Note:/ Consider using 'loadPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiaLoadPermissions :: Lens.Lens' FpgaImageAttribute (Lude.Maybe [LoadPermission])
fiaLoadPermissions = Lens.lens (loadPermissions :: FpgaImageAttribute -> Lude.Maybe [LoadPermission]) (\s a -> s {loadPermissions = a} :: FpgaImageAttribute)
{-# DEPRECATED fiaLoadPermissions "Use generic-lens or generic-optics with 'loadPermissions' instead." #-}

instance Lude.FromXML FpgaImageAttribute where
  parseXML x =
    FpgaImageAttribute'
      Lude.<$> (x Lude..@? "fpgaImageId")
      Lude.<*> (x Lude..@? "name")
      Lude.<*> ( x Lude..@? "productCodes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "loadPermissions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
