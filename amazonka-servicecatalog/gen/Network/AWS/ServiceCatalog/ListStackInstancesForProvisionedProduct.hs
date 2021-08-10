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
-- Module      : Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated
-- with the specified @CFN_STACKSET@ type provisioned product. You can
-- filter for stack instances that are associated with a specific AWS
-- account name or region.
module Network.AWS.ServiceCatalog.ListStackInstancesForProvisionedProduct
  ( -- * Creating a Request
    ListStackInstancesForProvisionedProduct (..),
    newListStackInstancesForProvisionedProduct,

    -- * Request Lenses
    listStackInstancesForProvisionedProduct_pageSize,
    listStackInstancesForProvisionedProduct_pageToken,
    listStackInstancesForProvisionedProduct_acceptLanguage,
    listStackInstancesForProvisionedProduct_provisionedProductId,

    -- * Destructuring the Response
    ListStackInstancesForProvisionedProductResponse (..),
    newListStackInstancesForProvisionedProductResponse,

    -- * Response Lenses
    listStackInstancesForProvisionedProductResponse_nextPageToken,
    listStackInstancesForProvisionedProductResponse_stackInstances,
    listStackInstancesForProvisionedProductResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newListStackInstancesForProvisionedProduct' smart constructor.
data ListStackInstancesForProvisionedProduct = ListStackInstancesForProvisionedProduct'
  { -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioned product.
    provisionedProductId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackInstancesForProvisionedProduct' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listStackInstancesForProvisionedProduct_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'listStackInstancesForProvisionedProduct_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'acceptLanguage', 'listStackInstancesForProvisionedProduct_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'provisionedProductId', 'listStackInstancesForProvisionedProduct_provisionedProductId' - The identifier of the provisioned product.
newListStackInstancesForProvisionedProduct ::
  -- | 'provisionedProductId'
  Prelude.Text ->
  ListStackInstancesForProvisionedProduct
newListStackInstancesForProvisionedProduct
  pProvisionedProductId_ =
    ListStackInstancesForProvisionedProduct'
      { pageSize =
          Prelude.Nothing,
        pageToken = Prelude.Nothing,
        acceptLanguage = Prelude.Nothing,
        provisionedProductId =
          pProvisionedProductId_
      }

-- | The maximum number of items to return with this call.
listStackInstancesForProvisionedProduct_pageSize :: Lens.Lens' ListStackInstancesForProvisionedProduct (Prelude.Maybe Prelude.Natural)
listStackInstancesForProvisionedProduct_pageSize = Lens.lens (\ListStackInstancesForProvisionedProduct' {pageSize} -> pageSize) (\s@ListStackInstancesForProvisionedProduct' {} a -> s {pageSize = a} :: ListStackInstancesForProvisionedProduct)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
listStackInstancesForProvisionedProduct_pageToken :: Lens.Lens' ListStackInstancesForProvisionedProduct (Prelude.Maybe Prelude.Text)
listStackInstancesForProvisionedProduct_pageToken = Lens.lens (\ListStackInstancesForProvisionedProduct' {pageToken} -> pageToken) (\s@ListStackInstancesForProvisionedProduct' {} a -> s {pageToken = a} :: ListStackInstancesForProvisionedProduct)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
listStackInstancesForProvisionedProduct_acceptLanguage :: Lens.Lens' ListStackInstancesForProvisionedProduct (Prelude.Maybe Prelude.Text)
listStackInstancesForProvisionedProduct_acceptLanguage = Lens.lens (\ListStackInstancesForProvisionedProduct' {acceptLanguage} -> acceptLanguage) (\s@ListStackInstancesForProvisionedProduct' {} a -> s {acceptLanguage = a} :: ListStackInstancesForProvisionedProduct)

-- | The identifier of the provisioned product.
listStackInstancesForProvisionedProduct_provisionedProductId :: Lens.Lens' ListStackInstancesForProvisionedProduct Prelude.Text
listStackInstancesForProvisionedProduct_provisionedProductId = Lens.lens (\ListStackInstancesForProvisionedProduct' {provisionedProductId} -> provisionedProductId) (\s@ListStackInstancesForProvisionedProduct' {} a -> s {provisionedProductId = a} :: ListStackInstancesForProvisionedProduct)

instance
  Core.AWSRequest
    ListStackInstancesForProvisionedProduct
  where
  type
    AWSResponse
      ListStackInstancesForProvisionedProduct =
      ListStackInstancesForProvisionedProductResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStackInstancesForProvisionedProductResponse'
            Prelude.<$> (x Core..?> "NextPageToken")
              Prelude.<*> (x Core..?> "StackInstances" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListStackInstancesForProvisionedProduct

instance
  Prelude.NFData
    ListStackInstancesForProvisionedProduct

instance
  Core.ToHeaders
    ListStackInstancesForProvisionedProduct
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.ListStackInstancesForProvisionedProduct" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListStackInstancesForProvisionedProduct
  where
  toJSON ListStackInstancesForProvisionedProduct' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PageSize" Core..=) Prelude.<$> pageSize,
            ("PageToken" Core..=) Prelude.<$> pageToken,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just
              ( "ProvisionedProductId"
                  Core..= provisionedProductId
              )
          ]
      )

instance
  Core.ToPath
    ListStackInstancesForProvisionedProduct
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListStackInstancesForProvisionedProduct
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStackInstancesForProvisionedProductResponse' smart constructor.
data ListStackInstancesForProvisionedProductResponse = ListStackInstancesForProvisionedProductResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | List of stack instances.
    stackInstances :: Prelude.Maybe [StackInstance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStackInstancesForProvisionedProductResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'listStackInstancesForProvisionedProductResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'stackInstances', 'listStackInstancesForProvisionedProductResponse_stackInstances' - List of stack instances.
--
-- 'httpStatus', 'listStackInstancesForProvisionedProductResponse_httpStatus' - The response's http status code.
newListStackInstancesForProvisionedProductResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStackInstancesForProvisionedProductResponse
newListStackInstancesForProvisionedProductResponse
  pHttpStatus_ =
    ListStackInstancesForProvisionedProductResponse'
      { nextPageToken =
          Prelude.Nothing,
        stackInstances =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
listStackInstancesForProvisionedProductResponse_nextPageToken :: Lens.Lens' ListStackInstancesForProvisionedProductResponse (Prelude.Maybe Prelude.Text)
listStackInstancesForProvisionedProductResponse_nextPageToken = Lens.lens (\ListStackInstancesForProvisionedProductResponse' {nextPageToken} -> nextPageToken) (\s@ListStackInstancesForProvisionedProductResponse' {} a -> s {nextPageToken = a} :: ListStackInstancesForProvisionedProductResponse)

-- | List of stack instances.
listStackInstancesForProvisionedProductResponse_stackInstances :: Lens.Lens' ListStackInstancesForProvisionedProductResponse (Prelude.Maybe [StackInstance])
listStackInstancesForProvisionedProductResponse_stackInstances = Lens.lens (\ListStackInstancesForProvisionedProductResponse' {stackInstances} -> stackInstances) (\s@ListStackInstancesForProvisionedProductResponse' {} a -> s {stackInstances = a} :: ListStackInstancesForProvisionedProductResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStackInstancesForProvisionedProductResponse_httpStatus :: Lens.Lens' ListStackInstancesForProvisionedProductResponse Prelude.Int
listStackInstancesForProvisionedProductResponse_httpStatus = Lens.lens (\ListStackInstancesForProvisionedProductResponse' {httpStatus} -> httpStatus) (\s@ListStackInstancesForProvisionedProductResponse' {} a -> s {httpStatus = a} :: ListStackInstancesForProvisionedProductResponse)

instance
  Prelude.NFData
    ListStackInstancesForProvisionedProductResponse
