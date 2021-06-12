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
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ProductViewSummary
import Network.AWS.ServiceCatalog.Types.RequestStatus

-- | Information about a product view.
--
-- /See:/ 'newProductViewDetail' smart constructor.
data ProductViewDetail = ProductViewDetail'
  { -- | The status of the product.
    --
    -- -   @AVAILABLE@ - The product is ready for use.
    --
    -- -   @CREATING@ - Product creation has started; the product is not ready
    --     for use.
    --
    -- -   @FAILED@ - An action failed.
    status :: Core.Maybe RequestStatus,
    -- | The ARN of the product.
    productARN :: Core.Maybe Core.Text,
    -- | The UTC time stamp of the creation time.
    createdTime :: Core.Maybe Core.POSIX,
    -- | Summary information about the product view.
    productViewSummary :: Core.Maybe ProductViewSummary
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProductViewDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'productViewDetail_status' - The status of the product.
--
-- -   @AVAILABLE@ - The product is ready for use.
--
-- -   @CREATING@ - Product creation has started; the product is not ready
--     for use.
--
-- -   @FAILED@ - An action failed.
--
-- 'productARN', 'productViewDetail_productARN' - The ARN of the product.
--
-- 'createdTime', 'productViewDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'productViewSummary', 'productViewDetail_productViewSummary' - Summary information about the product view.
newProductViewDetail ::
  ProductViewDetail
newProductViewDetail =
  ProductViewDetail'
    { status = Core.Nothing,
      productARN = Core.Nothing,
      createdTime = Core.Nothing,
      productViewSummary = Core.Nothing
    }

-- | The status of the product.
--
-- -   @AVAILABLE@ - The product is ready for use.
--
-- -   @CREATING@ - Product creation has started; the product is not ready
--     for use.
--
-- -   @FAILED@ - An action failed.
productViewDetail_status :: Lens.Lens' ProductViewDetail (Core.Maybe RequestStatus)
productViewDetail_status = Lens.lens (\ProductViewDetail' {status} -> status) (\s@ProductViewDetail' {} a -> s {status = a} :: ProductViewDetail)

-- | The ARN of the product.
productViewDetail_productARN :: Lens.Lens' ProductViewDetail (Core.Maybe Core.Text)
productViewDetail_productARN = Lens.lens (\ProductViewDetail' {productARN} -> productARN) (\s@ProductViewDetail' {} a -> s {productARN = a} :: ProductViewDetail)

-- | The UTC time stamp of the creation time.
productViewDetail_createdTime :: Lens.Lens' ProductViewDetail (Core.Maybe Core.UTCTime)
productViewDetail_createdTime = Lens.lens (\ProductViewDetail' {createdTime} -> createdTime) (\s@ProductViewDetail' {} a -> s {createdTime = a} :: ProductViewDetail) Core.. Lens.mapping Core._Time

-- | Summary information about the product view.
productViewDetail_productViewSummary :: Lens.Lens' ProductViewDetail (Core.Maybe ProductViewSummary)
productViewDetail_productViewSummary = Lens.lens (\ProductViewDetail' {productViewSummary} -> productViewSummary) (\s@ProductViewDetail' {} a -> s {productViewSummary = a} :: ProductViewDetail)

instance Core.FromJSON ProductViewDetail where
  parseJSON =
    Core.withObject
      "ProductViewDetail"
      ( \x ->
          ProductViewDetail'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "ProductARN")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "ProductViewSummary")
      )

instance Core.Hashable ProductViewDetail

instance Core.NFData ProductViewDetail
