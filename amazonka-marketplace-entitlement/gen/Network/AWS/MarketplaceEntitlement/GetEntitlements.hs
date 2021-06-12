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
-- Module      : Network.AWS.MarketplaceEntitlement.GetEntitlements
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- GetEntitlements retrieves entitlement values for a given product. The
-- results can be filtered based on customer identifier or product
-- dimensions.
--
-- This operation returns paginated results.
module Network.AWS.MarketplaceEntitlement.GetEntitlements
  ( -- * Creating a Request
    GetEntitlements (..),
    newGetEntitlements,

    -- * Request Lenses
    getEntitlements_nextToken,
    getEntitlements_maxResults,
    getEntitlements_filter,
    getEntitlements_productCode,

    -- * Destructuring the Response
    GetEntitlementsResponse (..),
    newGetEntitlementsResponse,

    -- * Response Lenses
    getEntitlementsResponse_nextToken,
    getEntitlementsResponse_entitlements,
    getEntitlementsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceEntitlement.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GetEntitlementsRequest contains parameters for the GetEntitlements
-- operation.
--
-- /See:/ 'newGetEntitlements' smart constructor.
data GetEntitlements = GetEntitlements'
  { -- | For paginated calls to GetEntitlements, pass the NextToken from the
    -- previous GetEntitlementsResult.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to retrieve from the GetEntitlements
    -- operation. For pagination, use the NextToken field in subsequent calls
    -- to GetEntitlements.
    maxResults :: Core.Maybe Core.Int,
    -- | Filter is used to return entitlements for a specific customer or for a
    -- specific dimension. Filters are described as keys mapped to a lists of
    -- values. Filtered requests are /unioned/ for each value in the value
    -- list, and then /intersected/ for each filter key.
    filter' :: Core.Maybe (Core.HashMap GetEntitlementFilterName (Core.NonEmpty Core.Text)),
    -- | Product code is used to uniquely identify a product in AWS Marketplace.
    -- The product code will be provided by AWS Marketplace when the product
    -- listing is created.
    productCode :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEntitlements' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEntitlements_nextToken' - For paginated calls to GetEntitlements, pass the NextToken from the
-- previous GetEntitlementsResult.
--
-- 'maxResults', 'getEntitlements_maxResults' - The maximum number of items to retrieve from the GetEntitlements
-- operation. For pagination, use the NextToken field in subsequent calls
-- to GetEntitlements.
--
-- 'filter'', 'getEntitlements_filter' - Filter is used to return entitlements for a specific customer or for a
-- specific dimension. Filters are described as keys mapped to a lists of
-- values. Filtered requests are /unioned/ for each value in the value
-- list, and then /intersected/ for each filter key.
--
-- 'productCode', 'getEntitlements_productCode' - Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code will be provided by AWS Marketplace when the product
-- listing is created.
newGetEntitlements ::
  -- | 'productCode'
  Core.Text ->
  GetEntitlements
newGetEntitlements pProductCode_ =
  GetEntitlements'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      filter' = Core.Nothing,
      productCode = pProductCode_
    }

-- | For paginated calls to GetEntitlements, pass the NextToken from the
-- previous GetEntitlementsResult.
getEntitlements_nextToken :: Lens.Lens' GetEntitlements (Core.Maybe Core.Text)
getEntitlements_nextToken = Lens.lens (\GetEntitlements' {nextToken} -> nextToken) (\s@GetEntitlements' {} a -> s {nextToken = a} :: GetEntitlements)

-- | The maximum number of items to retrieve from the GetEntitlements
-- operation. For pagination, use the NextToken field in subsequent calls
-- to GetEntitlements.
getEntitlements_maxResults :: Lens.Lens' GetEntitlements (Core.Maybe Core.Int)
getEntitlements_maxResults = Lens.lens (\GetEntitlements' {maxResults} -> maxResults) (\s@GetEntitlements' {} a -> s {maxResults = a} :: GetEntitlements)

-- | Filter is used to return entitlements for a specific customer or for a
-- specific dimension. Filters are described as keys mapped to a lists of
-- values. Filtered requests are /unioned/ for each value in the value
-- list, and then /intersected/ for each filter key.
getEntitlements_filter :: Lens.Lens' GetEntitlements (Core.Maybe (Core.HashMap GetEntitlementFilterName (Core.NonEmpty Core.Text)))
getEntitlements_filter = Lens.lens (\GetEntitlements' {filter'} -> filter') (\s@GetEntitlements' {} a -> s {filter' = a} :: GetEntitlements) Core.. Lens.mapping Lens._Coerce

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code will be provided by AWS Marketplace when the product
-- listing is created.
getEntitlements_productCode :: Lens.Lens' GetEntitlements Core.Text
getEntitlements_productCode = Lens.lens (\GetEntitlements' {productCode} -> productCode) (\s@GetEntitlements' {} a -> s {productCode = a} :: GetEntitlements)

instance Core.AWSPager GetEntitlements where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getEntitlementsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getEntitlementsResponse_entitlements
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getEntitlements_nextToken
          Lens..~ rs
          Lens.^? getEntitlementsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetEntitlements where
  type
    AWSResponse GetEntitlements =
      GetEntitlementsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEntitlementsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Entitlements" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetEntitlements

instance Core.NFData GetEntitlements

instance Core.ToHeaders GetEntitlements where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMPEntitlementService.GetEntitlements" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetEntitlements where
  toJSON GetEntitlements' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filter" Core..=) Core.<$> filter',
            Core.Just ("ProductCode" Core..= productCode)
          ]
      )

instance Core.ToPath GetEntitlements where
  toPath = Core.const "/"

instance Core.ToQuery GetEntitlements where
  toQuery = Core.const Core.mempty

-- | The GetEntitlementsRequest contains results from the GetEntitlements
-- operation.
--
-- /See:/ 'newGetEntitlementsResponse' smart constructor.
data GetEntitlementsResponse = GetEntitlementsResponse'
  { -- | For paginated results, use NextToken in subsequent calls to
    -- GetEntitlements. If the result contains an empty set of entitlements,
    -- NextToken might still be present and should be used.
    nextToken :: Core.Maybe Core.Text,
    -- | The set of entitlements found through the GetEntitlements operation. If
    -- the result contains an empty set of entitlements, NextToken might still
    -- be present and should be used.
    entitlements :: Core.Maybe [Entitlement],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetEntitlementsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEntitlementsResponse_nextToken' - For paginated results, use NextToken in subsequent calls to
-- GetEntitlements. If the result contains an empty set of entitlements,
-- NextToken might still be present and should be used.
--
-- 'entitlements', 'getEntitlementsResponse_entitlements' - The set of entitlements found through the GetEntitlements operation. If
-- the result contains an empty set of entitlements, NextToken might still
-- be present and should be used.
--
-- 'httpStatus', 'getEntitlementsResponse_httpStatus' - The response's http status code.
newGetEntitlementsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetEntitlementsResponse
newGetEntitlementsResponse pHttpStatus_ =
  GetEntitlementsResponse'
    { nextToken = Core.Nothing,
      entitlements = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | For paginated results, use NextToken in subsequent calls to
-- GetEntitlements. If the result contains an empty set of entitlements,
-- NextToken might still be present and should be used.
getEntitlementsResponse_nextToken :: Lens.Lens' GetEntitlementsResponse (Core.Maybe Core.Text)
getEntitlementsResponse_nextToken = Lens.lens (\GetEntitlementsResponse' {nextToken} -> nextToken) (\s@GetEntitlementsResponse' {} a -> s {nextToken = a} :: GetEntitlementsResponse)

-- | The set of entitlements found through the GetEntitlements operation. If
-- the result contains an empty set of entitlements, NextToken might still
-- be present and should be used.
getEntitlementsResponse_entitlements :: Lens.Lens' GetEntitlementsResponse (Core.Maybe [Entitlement])
getEntitlementsResponse_entitlements = Lens.lens (\GetEntitlementsResponse' {entitlements} -> entitlements) (\s@GetEntitlementsResponse' {} a -> s {entitlements = a} :: GetEntitlementsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getEntitlementsResponse_httpStatus :: Lens.Lens' GetEntitlementsResponse Core.Int
getEntitlementsResponse_httpStatus = Lens.lens (\GetEntitlementsResponse' {httpStatus} -> httpStatus) (\s@GetEntitlementsResponse' {} a -> s {httpStatus = a} :: GetEntitlementsResponse)

instance Core.NFData GetEntitlementsResponse
