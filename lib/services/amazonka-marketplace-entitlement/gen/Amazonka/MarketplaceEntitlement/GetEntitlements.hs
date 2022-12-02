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
-- Module      : Amazonka.MarketplaceEntitlement.GetEntitlements
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.MarketplaceEntitlement.GetEntitlements
  ( -- * Creating a Request
    GetEntitlements (..),
    newGetEntitlements,

    -- * Request Lenses
    getEntitlements_nextToken,
    getEntitlements_filter,
    getEntitlements_maxResults,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MarketplaceEntitlement.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The GetEntitlementsRequest contains parameters for the GetEntitlements
-- operation.
--
-- /See:/ 'newGetEntitlements' smart constructor.
data GetEntitlements = GetEntitlements'
  { -- | For paginated calls to GetEntitlements, pass the NextToken from the
    -- previous GetEntitlementsResult.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter is used to return entitlements for a specific customer or for a
    -- specific dimension. Filters are described as keys mapped to a lists of
    -- values. Filtered requests are /unioned/ for each value in the value
    -- list, and then /intersected/ for each filter key.
    filter' :: Prelude.Maybe (Prelude.HashMap GetEntitlementFilterName (Prelude.NonEmpty Prelude.Text)),
    -- | The maximum number of items to retrieve from the GetEntitlements
    -- operation. For pagination, use the NextToken field in subsequent calls
    -- to GetEntitlements.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Product code is used to uniquely identify a product in AWS Marketplace.
    -- The product code will be provided by AWS Marketplace when the product
    -- listing is created.
    productCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'filter'', 'getEntitlements_filter' - Filter is used to return entitlements for a specific customer or for a
-- specific dimension. Filters are described as keys mapped to a lists of
-- values. Filtered requests are /unioned/ for each value in the value
-- list, and then /intersected/ for each filter key.
--
-- 'maxResults', 'getEntitlements_maxResults' - The maximum number of items to retrieve from the GetEntitlements
-- operation. For pagination, use the NextToken field in subsequent calls
-- to GetEntitlements.
--
-- 'productCode', 'getEntitlements_productCode' - Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code will be provided by AWS Marketplace when the product
-- listing is created.
newGetEntitlements ::
  -- | 'productCode'
  Prelude.Text ->
  GetEntitlements
newGetEntitlements pProductCode_ =
  GetEntitlements'
    { nextToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      productCode = pProductCode_
    }

-- | For paginated calls to GetEntitlements, pass the NextToken from the
-- previous GetEntitlementsResult.
getEntitlements_nextToken :: Lens.Lens' GetEntitlements (Prelude.Maybe Prelude.Text)
getEntitlements_nextToken = Lens.lens (\GetEntitlements' {nextToken} -> nextToken) (\s@GetEntitlements' {} a -> s {nextToken = a} :: GetEntitlements)

-- | Filter is used to return entitlements for a specific customer or for a
-- specific dimension. Filters are described as keys mapped to a lists of
-- values. Filtered requests are /unioned/ for each value in the value
-- list, and then /intersected/ for each filter key.
getEntitlements_filter :: Lens.Lens' GetEntitlements (Prelude.Maybe (Prelude.HashMap GetEntitlementFilterName (Prelude.NonEmpty Prelude.Text)))
getEntitlements_filter = Lens.lens (\GetEntitlements' {filter'} -> filter') (\s@GetEntitlements' {} a -> s {filter' = a} :: GetEntitlements) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to retrieve from the GetEntitlements
-- operation. For pagination, use the NextToken field in subsequent calls
-- to GetEntitlements.
getEntitlements_maxResults :: Lens.Lens' GetEntitlements (Prelude.Maybe Prelude.Int)
getEntitlements_maxResults = Lens.lens (\GetEntitlements' {maxResults} -> maxResults) (\s@GetEntitlements' {} a -> s {maxResults = a} :: GetEntitlements)

-- | Product code is used to uniquely identify a product in AWS Marketplace.
-- The product code will be provided by AWS Marketplace when the product
-- listing is created.
getEntitlements_productCode :: Lens.Lens' GetEntitlements Prelude.Text
getEntitlements_productCode = Lens.lens (\GetEntitlements' {productCode} -> productCode) (\s@GetEntitlements' {} a -> s {productCode = a} :: GetEntitlements)

instance Core.AWSPager GetEntitlements where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getEntitlementsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getEntitlementsResponse_entitlements
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getEntitlements_nextToken
          Lens..~ rs
          Lens.^? getEntitlementsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetEntitlements where
  type
    AWSResponse GetEntitlements =
      GetEntitlementsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEntitlementsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Entitlements" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEntitlements where
  hashWithSalt _salt GetEntitlements' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` productCode

instance Prelude.NFData GetEntitlements where
  rnf GetEntitlements' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf productCode

instance Data.ToHeaders GetEntitlements where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSMPEntitlementService.GetEntitlements" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEntitlements where
  toJSON GetEntitlements' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("ProductCode" Data..= productCode)
          ]
      )

instance Data.ToPath GetEntitlements where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEntitlements where
  toQuery = Prelude.const Prelude.mempty

-- | The GetEntitlementsRequest contains results from the GetEntitlements
-- operation.
--
-- /See:/ 'newGetEntitlementsResponse' smart constructor.
data GetEntitlementsResponse = GetEntitlementsResponse'
  { -- | For paginated results, use NextToken in subsequent calls to
    -- GetEntitlements. If the result contains an empty set of entitlements,
    -- NextToken might still be present and should be used.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The set of entitlements found through the GetEntitlements operation. If
    -- the result contains an empty set of entitlements, NextToken might still
    -- be present and should be used.
    entitlements :: Prelude.Maybe [Entitlement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetEntitlementsResponse
newGetEntitlementsResponse pHttpStatus_ =
  GetEntitlementsResponse'
    { nextToken =
        Prelude.Nothing,
      entitlements = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | For paginated results, use NextToken in subsequent calls to
-- GetEntitlements. If the result contains an empty set of entitlements,
-- NextToken might still be present and should be used.
getEntitlementsResponse_nextToken :: Lens.Lens' GetEntitlementsResponse (Prelude.Maybe Prelude.Text)
getEntitlementsResponse_nextToken = Lens.lens (\GetEntitlementsResponse' {nextToken} -> nextToken) (\s@GetEntitlementsResponse' {} a -> s {nextToken = a} :: GetEntitlementsResponse)

-- | The set of entitlements found through the GetEntitlements operation. If
-- the result contains an empty set of entitlements, NextToken might still
-- be present and should be used.
getEntitlementsResponse_entitlements :: Lens.Lens' GetEntitlementsResponse (Prelude.Maybe [Entitlement])
getEntitlementsResponse_entitlements = Lens.lens (\GetEntitlementsResponse' {entitlements} -> entitlements) (\s@GetEntitlementsResponse' {} a -> s {entitlements = a} :: GetEntitlementsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEntitlementsResponse_httpStatus :: Lens.Lens' GetEntitlementsResponse Prelude.Int
getEntitlementsResponse_httpStatus = Lens.lens (\GetEntitlementsResponse' {httpStatus} -> httpStatus) (\s@GetEntitlementsResponse' {} a -> s {httpStatus = a} :: GetEntitlementsResponse)

instance Prelude.NFData GetEntitlementsResponse where
  rnf GetEntitlementsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf httpStatus
