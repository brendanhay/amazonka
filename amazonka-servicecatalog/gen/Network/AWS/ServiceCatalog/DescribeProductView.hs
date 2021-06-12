{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.DescribeProductView
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified product.
module Network.AWS.ServiceCatalog.DescribeProductView
  ( -- * Creating a Request
    DescribeProductView (..),
    newDescribeProductView,

    -- * Request Lenses
    describeProductView_acceptLanguage,
    describeProductView_id,

    -- * Destructuring the Response
    DescribeProductViewResponse (..),
    newDescribeProductViewResponse,

    -- * Response Lenses
    describeProductViewResponse_provisioningArtifacts,
    describeProductViewResponse_productViewSummary,
    describeProductViewResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDescribeProductView' smart constructor.
data DescribeProductView = DescribeProductView'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The product view identifier.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProductView' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'describeProductView_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'describeProductView_id' - The product view identifier.
newDescribeProductView ::
  -- | 'id'
  Core.Text ->
  DescribeProductView
newDescribeProductView pId_ =
  DescribeProductView'
    { acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProductView_acceptLanguage :: Lens.Lens' DescribeProductView (Core.Maybe Core.Text)
describeProductView_acceptLanguage = Lens.lens (\DescribeProductView' {acceptLanguage} -> acceptLanguage) (\s@DescribeProductView' {} a -> s {acceptLanguage = a} :: DescribeProductView)

-- | The product view identifier.
describeProductView_id :: Lens.Lens' DescribeProductView Core.Text
describeProductView_id = Lens.lens (\DescribeProductView' {id} -> id) (\s@DescribeProductView' {} a -> s {id = a} :: DescribeProductView)

instance Core.AWSRequest DescribeProductView where
  type
    AWSResponse DescribeProductView =
      DescribeProductViewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProductViewResponse'
            Core.<$> ( x Core..?> "ProvisioningArtifacts"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "ProductViewSummary")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProductView

instance Core.NFData DescribeProductView

instance Core.ToHeaders DescribeProductView where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProductView" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProductView where
  toJSON DescribeProductView' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DescribeProductView where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProductView where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProductViewResponse' smart constructor.
data DescribeProductViewResponse = DescribeProductViewResponse'
  { -- | Information about the provisioning artifacts for the product.
    provisioningArtifacts :: Core.Maybe [ProvisioningArtifact],
    -- | Summary information about the product.
    productViewSummary :: Core.Maybe ProductViewSummary,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProductViewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisioningArtifacts', 'describeProductViewResponse_provisioningArtifacts' - Information about the provisioning artifacts for the product.
--
-- 'productViewSummary', 'describeProductViewResponse_productViewSummary' - Summary information about the product.
--
-- 'httpStatus', 'describeProductViewResponse_httpStatus' - The response's http status code.
newDescribeProductViewResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProductViewResponse
newDescribeProductViewResponse pHttpStatus_ =
  DescribeProductViewResponse'
    { provisioningArtifacts =
        Core.Nothing,
      productViewSummary = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the provisioning artifacts for the product.
describeProductViewResponse_provisioningArtifacts :: Lens.Lens' DescribeProductViewResponse (Core.Maybe [ProvisioningArtifact])
describeProductViewResponse_provisioningArtifacts = Lens.lens (\DescribeProductViewResponse' {provisioningArtifacts} -> provisioningArtifacts) (\s@DescribeProductViewResponse' {} a -> s {provisioningArtifacts = a} :: DescribeProductViewResponse) Core.. Lens.mapping Lens._Coerce

-- | Summary information about the product.
describeProductViewResponse_productViewSummary :: Lens.Lens' DescribeProductViewResponse (Core.Maybe ProductViewSummary)
describeProductViewResponse_productViewSummary = Lens.lens (\DescribeProductViewResponse' {productViewSummary} -> productViewSummary) (\s@DescribeProductViewResponse' {} a -> s {productViewSummary = a} :: DescribeProductViewResponse)

-- | The response's http status code.
describeProductViewResponse_httpStatus :: Lens.Lens' DescribeProductViewResponse Core.Int
describeProductViewResponse_httpStatus = Lens.lens (\DescribeProductViewResponse' {httpStatus} -> httpStatus) (\s@DescribeProductViewResponse' {} a -> s {httpStatus = a} :: DescribeProductViewResponse)

instance Core.NFData DescribeProductViewResponse
