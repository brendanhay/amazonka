{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    pcProductCodeId,
    pcProductCodeType,
  )
where

import qualified Network.AWS.EC2.Types.ProductCodeValues as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a product code.
--
-- /See:/ 'mkProductCode' smart constructor.
data ProductCode = ProductCode'
  { -- | The product code.
    productCodeId :: Core.Maybe Types.String,
    -- | The type of product code.
    productCodeType :: Core.Maybe Types.ProductCodeValues
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProductCode' value with any optional fields omitted.
mkProductCode ::
  ProductCode
mkProductCode =
  ProductCode'
    { productCodeId = Core.Nothing,
      productCodeType = Core.Nothing
    }

-- | The product code.
--
-- /Note:/ Consider using 'productCodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProductCodeId :: Lens.Lens' ProductCode (Core.Maybe Types.String)
pcProductCodeId = Lens.field @"productCodeId"
{-# DEPRECATED pcProductCodeId "Use generic-lens or generic-optics with 'productCodeId' instead." #-}

-- | The type of product code.
--
-- /Note:/ Consider using 'productCodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProductCodeType :: Lens.Lens' ProductCode (Core.Maybe Types.ProductCodeValues)
pcProductCodeType = Lens.field @"productCodeType"
{-# DEPRECATED pcProductCodeType "Use generic-lens or generic-optics with 'productCodeType' instead." #-}

instance Core.FromXML ProductCode where
  parseXML x =
    ProductCode'
      Core.<$> (x Core..@? "productCode") Core.<*> (x Core..@? "type")
