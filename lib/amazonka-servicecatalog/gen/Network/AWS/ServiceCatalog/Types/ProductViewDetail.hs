{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewDetail
  ( ProductViewDetail (..),

    -- * Smart constructor
    mkProductViewDetail,

    -- * Lenses
    pvdCreatedTime,
    pvdProductARN,
    pvdProductViewSummary,
    pvdStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.ProductViewSummary as Types
import qualified Network.AWS.ServiceCatalog.Types.RequestStatus as Types
import qualified Network.AWS.ServiceCatalog.Types.ResourceARN as Types

-- | Information about a product view.
--
-- /See:/ 'mkProductViewDetail' smart constructor.
data ProductViewDetail = ProductViewDetail'
  { -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.NominalDiffTime,
    -- | The ARN of the product.
    productARN :: Core.Maybe Types.ResourceARN,
    -- | Summary information about the product view.
    productViewSummary :: Core.Maybe Types.ProductViewSummary,
    -- | The status of the product.
    --
    --
    --     * @AVAILABLE@ - The product is ready for use.
    --
    --
    --     * @CREATING@ - Product creation has started; the product is not ready for use.
    --
    --
    --     * @FAILED@ - An action failed.
    status :: Core.Maybe Types.RequestStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ProductViewDetail' value with any optional fields omitted.
mkProductViewDetail ::
  ProductViewDetail
mkProductViewDetail =
  ProductViewDetail'
    { createdTime = Core.Nothing,
      productARN = Core.Nothing,
      productViewSummary = Core.Nothing,
      status = Core.Nothing
    }

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdCreatedTime :: Lens.Lens' ProductViewDetail (Core.Maybe Core.NominalDiffTime)
pvdCreatedTime = Lens.field @"createdTime"
{-# DEPRECATED pvdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The ARN of the product.
--
-- /Note:/ Consider using 'productARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdProductARN :: Lens.Lens' ProductViewDetail (Core.Maybe Types.ResourceARN)
pvdProductARN = Lens.field @"productARN"
{-# DEPRECATED pvdProductARN "Use generic-lens or generic-optics with 'productARN' instead." #-}

-- | Summary information about the product view.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdProductViewSummary :: Lens.Lens' ProductViewDetail (Core.Maybe Types.ProductViewSummary)
pvdProductViewSummary = Lens.field @"productViewSummary"
{-# DEPRECATED pvdProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | The status of the product.
--
--
--     * @AVAILABLE@ - The product is ready for use.
--
--
--     * @CREATING@ - Product creation has started; the product is not ready for use.
--
--
--     * @FAILED@ - An action failed.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdStatus :: Lens.Lens' ProductViewDetail (Core.Maybe Types.RequestStatus)
pvdStatus = Lens.field @"status"
{-# DEPRECATED pvdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ProductViewDetail where
  parseJSON =
    Core.withObject "ProductViewDetail" Core.$
      \x ->
        ProductViewDetail'
          Core.<$> (x Core..:? "CreatedTime")
          Core.<*> (x Core..:? "ProductARN")
          Core.<*> (x Core..:? "ProductViewSummary")
          Core.<*> (x Core..:? "Status")
