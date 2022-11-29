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
-- Module      : Amazonka.ServiceCatalog.Types.ProductViewDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProductViewDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProductViewSummary
import Amazonka.ServiceCatalog.Types.RequestStatus
import Amazonka.ServiceCatalog.Types.SourceConnectionDetail

-- | Information about a product view.
--
-- /See:/ 'newProductViewDetail' smart constructor.
data ProductViewDetail = ProductViewDetail'
  { -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the product.
    --
    -- -   @AVAILABLE@ - The product is ready for use.
    --
    -- -   @CREATING@ - Product creation has started; the product is not ready
    --     for use.
    --
    -- -   @FAILED@ - An action failed.
    status :: Prelude.Maybe RequestStatus,
    -- | Summary information about the product view.
    productViewSummary :: Prelude.Maybe ProductViewSummary,
    -- | The ARN of the product.
    productARN :: Prelude.Maybe Prelude.Text,
    -- | A top level @ProductViewDetail@ response containing details about the
    -- product’s connection. Service Catalog returns this field for the
    -- @CreateProduct@, @UpdateProduct@, @DescribeProductAsAdmin@, and
    -- @SearchProductAsAdmin@ APIs. This response contains the same fields as
    -- the @ConnectionParameters@ request, with the addition of the @LastSync@
    -- response.
    sourceConnection :: Prelude.Maybe SourceConnectionDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductViewDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'productViewDetail_createdTime' - The UTC time stamp of the creation time.
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
-- 'productViewSummary', 'productViewDetail_productViewSummary' - Summary information about the product view.
--
-- 'productARN', 'productViewDetail_productARN' - The ARN of the product.
--
-- 'sourceConnection', 'productViewDetail_sourceConnection' - A top level @ProductViewDetail@ response containing details about the
-- product’s connection. Service Catalog returns this field for the
-- @CreateProduct@, @UpdateProduct@, @DescribeProductAsAdmin@, and
-- @SearchProductAsAdmin@ APIs. This response contains the same fields as
-- the @ConnectionParameters@ request, with the addition of the @LastSync@
-- response.
newProductViewDetail ::
  ProductViewDetail
newProductViewDetail =
  ProductViewDetail'
    { createdTime = Prelude.Nothing,
      status = Prelude.Nothing,
      productViewSummary = Prelude.Nothing,
      productARN = Prelude.Nothing,
      sourceConnection = Prelude.Nothing
    }

-- | The UTC time stamp of the creation time.
productViewDetail_createdTime :: Lens.Lens' ProductViewDetail (Prelude.Maybe Prelude.UTCTime)
productViewDetail_createdTime = Lens.lens (\ProductViewDetail' {createdTime} -> createdTime) (\s@ProductViewDetail' {} a -> s {createdTime = a} :: ProductViewDetail) Prelude.. Lens.mapping Core._Time

-- | The status of the product.
--
-- -   @AVAILABLE@ - The product is ready for use.
--
-- -   @CREATING@ - Product creation has started; the product is not ready
--     for use.
--
-- -   @FAILED@ - An action failed.
productViewDetail_status :: Lens.Lens' ProductViewDetail (Prelude.Maybe RequestStatus)
productViewDetail_status = Lens.lens (\ProductViewDetail' {status} -> status) (\s@ProductViewDetail' {} a -> s {status = a} :: ProductViewDetail)

-- | Summary information about the product view.
productViewDetail_productViewSummary :: Lens.Lens' ProductViewDetail (Prelude.Maybe ProductViewSummary)
productViewDetail_productViewSummary = Lens.lens (\ProductViewDetail' {productViewSummary} -> productViewSummary) (\s@ProductViewDetail' {} a -> s {productViewSummary = a} :: ProductViewDetail)

-- | The ARN of the product.
productViewDetail_productARN :: Lens.Lens' ProductViewDetail (Prelude.Maybe Prelude.Text)
productViewDetail_productARN = Lens.lens (\ProductViewDetail' {productARN} -> productARN) (\s@ProductViewDetail' {} a -> s {productARN = a} :: ProductViewDetail)

-- | A top level @ProductViewDetail@ response containing details about the
-- product’s connection. Service Catalog returns this field for the
-- @CreateProduct@, @UpdateProduct@, @DescribeProductAsAdmin@, and
-- @SearchProductAsAdmin@ APIs. This response contains the same fields as
-- the @ConnectionParameters@ request, with the addition of the @LastSync@
-- response.
productViewDetail_sourceConnection :: Lens.Lens' ProductViewDetail (Prelude.Maybe SourceConnectionDetail)
productViewDetail_sourceConnection = Lens.lens (\ProductViewDetail' {sourceConnection} -> sourceConnection) (\s@ProductViewDetail' {} a -> s {sourceConnection = a} :: ProductViewDetail)

instance Core.FromJSON ProductViewDetail where
  parseJSON =
    Core.withObject
      "ProductViewDetail"
      ( \x ->
          ProductViewDetail'
            Prelude.<$> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ProductViewSummary")
            Prelude.<*> (x Core..:? "ProductARN")
            Prelude.<*> (x Core..:? "SourceConnection")
      )

instance Prelude.Hashable ProductViewDetail where
  hashWithSalt _salt ProductViewDetail' {..} =
    _salt `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` productViewSummary
      `Prelude.hashWithSalt` productARN
      `Prelude.hashWithSalt` sourceConnection

instance Prelude.NFData ProductViewDetail where
  rnf ProductViewDetail' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf productViewSummary
      `Prelude.seq` Prelude.rnf productARN
      `Prelude.seq` Prelude.rnf sourceConnection
