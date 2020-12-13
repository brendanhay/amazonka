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
    pvdStatus,
    pvdProductViewSummary,
    pvdCreatedTime,
    pvdProductARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.RequestStatus

-- | Information about a product view.
--
-- /See:/ 'mkProductViewDetail' smart constructor.
data ProductViewDetail = ProductViewDetail'
  { -- | The status of the product.
    --
    --
    --     * @AVAILABLE@ - The product is ready for use.
    --
    --
    --     * @CREATING@ - Product creation has started; the product is not ready for use.
    --
    --
    --     * @FAILED@ - An action failed.
    status :: Lude.Maybe RequestStatus,
    -- | Summary information about the product view.
    productViewSummary :: Lude.Maybe ProductViewSummary,
    -- | The UTC time stamp of the creation time.
    createdTime :: Lude.Maybe Lude.Timestamp,
    -- | The ARN of the product.
    productARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProductViewDetail' with the minimum fields required to make a request.
--
-- * 'status' - The status of the product.
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
-- * 'productViewSummary' - Summary information about the product view.
-- * 'createdTime' - The UTC time stamp of the creation time.
-- * 'productARN' - The ARN of the product.
mkProductViewDetail ::
  ProductViewDetail
mkProductViewDetail =
  ProductViewDetail'
    { status = Lude.Nothing,
      productViewSummary = Lude.Nothing,
      createdTime = Lude.Nothing,
      productARN = Lude.Nothing
    }

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
pvdStatus :: Lens.Lens' ProductViewDetail (Lude.Maybe RequestStatus)
pvdStatus = Lens.lens (status :: ProductViewDetail -> Lude.Maybe RequestStatus) (\s a -> s {status = a} :: ProductViewDetail)
{-# DEPRECATED pvdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Summary information about the product view.
--
-- /Note:/ Consider using 'productViewSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdProductViewSummary :: Lens.Lens' ProductViewDetail (Lude.Maybe ProductViewSummary)
pvdProductViewSummary = Lens.lens (productViewSummary :: ProductViewDetail -> Lude.Maybe ProductViewSummary) (\s a -> s {productViewSummary = a} :: ProductViewDetail)
{-# DEPRECATED pvdProductViewSummary "Use generic-lens or generic-optics with 'productViewSummary' instead." #-}

-- | The UTC time stamp of the creation time.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdCreatedTime :: Lens.Lens' ProductViewDetail (Lude.Maybe Lude.Timestamp)
pvdCreatedTime = Lens.lens (createdTime :: ProductViewDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTime = a} :: ProductViewDetail)
{-# DEPRECATED pvdCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The ARN of the product.
--
-- /Note:/ Consider using 'productARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pvdProductARN :: Lens.Lens' ProductViewDetail (Lude.Maybe Lude.Text)
pvdProductARN = Lens.lens (productARN :: ProductViewDetail -> Lude.Maybe Lude.Text) (\s a -> s {productARN = a} :: ProductViewDetail)
{-# DEPRECATED pvdProductARN "Use generic-lens or generic-optics with 'productARN' instead." #-}

instance Lude.FromJSON ProductViewDetail where
  parseJSON =
    Lude.withObject
      "ProductViewDetail"
      ( \x ->
          ProductViewDetail'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ProductViewSummary")
            Lude.<*> (x Lude..:? "CreatedTime")
            Lude.<*> (x Lude..:? "ProductARN")
      )
