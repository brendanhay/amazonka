{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ProductCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.ProductCode
  ( ProductCode (..),

    -- * Smart constructor
    mkProductCode,

    -- * Lenses
    pcProductType,
    pcCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the product code for the EC2 instance.
--
-- /See:/ 'mkProductCode' smart constructor.
data ProductCode = ProductCode'
  { productType ::
      Lude.Maybe Lude.Text,
    code :: Lude.Maybe Lude.Text
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
-- * 'code' - The product code information.
-- * 'productType' - The product code type.
mkProductCode ::
  ProductCode
mkProductCode =
  ProductCode' {productType = Lude.Nothing, code = Lude.Nothing}

-- | The product code type.
--
-- /Note:/ Consider using 'productType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProductType :: Lens.Lens' ProductCode (Lude.Maybe Lude.Text)
pcProductType = Lens.lens (productType :: ProductCode -> Lude.Maybe Lude.Text) (\s a -> s {productType = a} :: ProductCode)
{-# DEPRECATED pcProductType "Use generic-lens or generic-optics with 'productType' instead." #-}

-- | The product code information.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcCode :: Lens.Lens' ProductCode (Lude.Maybe Lude.Text)
pcCode = Lens.lens (code :: ProductCode -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: ProductCode)
{-# DEPRECATED pcCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON ProductCode where
  parseJSON =
    Lude.withObject
      "ProductCode"
      ( \x ->
          ProductCode'
            Lude.<$> (x Lude..:? "productType") Lude.<*> (x Lude..:? "code")
      )
