{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.ProductCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.ProductCode
  ( ProductCode (..)
  -- * Smart constructor
  , mkProductCode
  -- * Lenses
  , pcCode
  , pcProductType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the product code for the EC2 instance.
--
-- /See:/ 'mkProductCode' smart constructor.
data ProductCode = ProductCode'
  { code :: Core.Maybe Core.Text
    -- ^ The product code information.
  , productType :: Core.Maybe Core.Text
    -- ^ The product code type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProductCode' value with any optional fields omitted.
mkProductCode
    :: ProductCode
mkProductCode
  = ProductCode'{code = Core.Nothing, productType = Core.Nothing}

-- | The product code information.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcCode :: Lens.Lens' ProductCode (Core.Maybe Core.Text)
pcCode = Lens.field @"code"
{-# INLINEABLE pcCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | The product code type.
--
-- /Note:/ Consider using 'productType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcProductType :: Lens.Lens' ProductCode (Core.Maybe Core.Text)
pcProductType = Lens.field @"productType"
{-# INLINEABLE pcProductType #-}
{-# DEPRECATED productType "Use generic-lens or generic-optics with 'productType' instead"  #-}

instance Core.FromJSON ProductCode where
        parseJSON
          = Core.withObject "ProductCode" Core.$
              \ x ->
                ProductCode' Core.<$>
                  (x Core..:? "code") Core.<*> x Core..:? "productType"
