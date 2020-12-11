-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ProductCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProductCode
  ( ProductCode (..),

    -- * Smart constructor
    mkProductCode,

    -- * Lenses
    pcProductCodeType,
    pcProductCodeId,
  )
where

import Network.AWS.EC2.Types.ProductCodeValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a product code.
--
-- /See:/ 'mkProductCode' smart constructor.
data ProductCode = ProductCode'
  { productCodeType ::
      Lude.Maybe ProductCodeValues,
    productCodeId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProductCode' with the minimum fields required to make a request.
--
-- * 'productCodeId' - The product code.
-- * 'productCodeType' - The type of product code.
mkProductCode ::
  ProductCode
mkProductCode =
  ProductCode'
    { productCodeType = Lude.Nothing,
      productCodeId = Lude.Nothing
    }

-- | The type of product code.
--
-- /Note:/ Consider using 'productCodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProductCodeType :: Lens.Lens' ProductCode (Lude.Maybe ProductCodeValues)
pcProductCodeType = Lens.lens (productCodeType :: ProductCode -> Lude.Maybe ProductCodeValues) (\s a -> s {productCodeType = a} :: ProductCode)
{-# DEPRECATED pcProductCodeType "Use generic-lens or generic-optics with 'productCodeType' instead." #-}

-- | The product code.
--
-- /Note:/ Consider using 'productCodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProductCodeId :: Lens.Lens' ProductCode (Lude.Maybe Lude.Text)
pcProductCodeId = Lens.lens (productCodeId :: ProductCode -> Lude.Maybe Lude.Text) (\s a -> s {productCodeId = a} :: ProductCode)
{-# DEPRECATED pcProductCodeId "Use generic-lens or generic-optics with 'productCodeId' instead." #-}

instance Lude.FromXML ProductCode where
  parseXML x =
    ProductCode'
      Lude.<$> (x Lude..@? "type") Lude.<*> (x Lude..@? "productCode")
