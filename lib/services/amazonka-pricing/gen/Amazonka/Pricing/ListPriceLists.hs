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
-- Module      : Amazonka.Pricing.ListPriceLists
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /__This feature is in preview release and is subject to change. Your use
-- of Amazon Web Services Price List API is subject to the Beta Service
-- Participation terms of the
-- <https://aws.amazon.com/service-terms/ Amazon Web Services Service Terms>
-- (Section 1.10).__/
--
-- This returns a list of Price List references that the requester if
-- authorized to view, given a @ServiceCode@, @CurrencyCode@, and an
-- @EffectiveDate@. Use without a @RegionCode@ filter to list Price List
-- references from all available Amazon Web Services Regions. Use with a
-- @RegionCode@ filter to get the Price List reference that\'s specific to
-- a specific Amazon Web Services Region. You can use the @PriceListArn@
-- from the response to get your preferred Price List files through the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_GetPriceListFileUrl.html GetPriceListFileUrl>
-- API.
--
-- This operation returns paginated results.
module Amazonka.Pricing.ListPriceLists
  ( -- * Creating a Request
    ListPriceLists (..),
    newListPriceLists,

    -- * Request Lenses
    listPriceLists_maxResults,
    listPriceLists_nextToken,
    listPriceLists_regionCode,
    listPriceLists_serviceCode,
    listPriceLists_effectiveDate,
    listPriceLists_currencyCode,

    -- * Destructuring the Response
    ListPriceListsResponse (..),
    newListPriceListsResponse,

    -- * Response Lenses
    listPriceListsResponse_nextToken,
    listPriceListsResponse_priceLists,
    listPriceListsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Pricing.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPriceLists' smart constructor.
data ListPriceLists = ListPriceLists'
  { -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that indicates the next set of results that you
    -- want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is used to filter the Price List by Amazon Web Services Region. For
    -- example, to get the price list only for the @US East (N. Virginia)@
    -- Region, use @us-east-1@. If nothing is specified, you retrieve price
    -- lists for all applicable Regions. The available @RegionCode@ list can be
    -- retrieved from
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_GetAttributeValues.html GetAttributeValues>
    -- API.
    regionCode :: Prelude.Maybe Prelude.Text,
    -- | The service code or the Savings Plan service code for the attributes
    -- that you want to retrieve. For example, to get the list of applicable
    -- Amazon EC2 price lists, use @AmazonEC2@. For a full list of service
    -- codes containing On-Demand and Reserved Instance (RI) pricing, use the
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_DescribeServices.html#awscostmanagement-pricing_DescribeServices-request-FormatVersion DescribeServices>
    -- API.
    --
    -- To retrieve the Compute Savings Plan price lists, use
    -- @ComputeSavingsPlans@. To retrieve Machine Learning Savings Plans price
    -- lists, use @MachineLearningSavingsPlans@.
    serviceCode :: Prelude.Text,
    -- | The date that the Price List file prices are effective from.
    effectiveDate :: Data.POSIX,
    -- | The three alphabetical character ISO-4217 currency code that the Price
    -- List files are denominated in.
    currencyCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPriceLists' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPriceLists_maxResults' - The maximum number of results to return in the response.
--
-- 'nextToken', 'listPriceLists_nextToken' - The pagination token that indicates the next set of results that you
-- want to retrieve.
--
-- 'regionCode', 'listPriceLists_regionCode' - This is used to filter the Price List by Amazon Web Services Region. For
-- example, to get the price list only for the @US East (N. Virginia)@
-- Region, use @us-east-1@. If nothing is specified, you retrieve price
-- lists for all applicable Regions. The available @RegionCode@ list can be
-- retrieved from
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_GetAttributeValues.html GetAttributeValues>
-- API.
--
-- 'serviceCode', 'listPriceLists_serviceCode' - The service code or the Savings Plan service code for the attributes
-- that you want to retrieve. For example, to get the list of applicable
-- Amazon EC2 price lists, use @AmazonEC2@. For a full list of service
-- codes containing On-Demand and Reserved Instance (RI) pricing, use the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_DescribeServices.html#awscostmanagement-pricing_DescribeServices-request-FormatVersion DescribeServices>
-- API.
--
-- To retrieve the Compute Savings Plan price lists, use
-- @ComputeSavingsPlans@. To retrieve Machine Learning Savings Plans price
-- lists, use @MachineLearningSavingsPlans@.
--
-- 'effectiveDate', 'listPriceLists_effectiveDate' - The date that the Price List file prices are effective from.
--
-- 'currencyCode', 'listPriceLists_currencyCode' - The three alphabetical character ISO-4217 currency code that the Price
-- List files are denominated in.
newListPriceLists ::
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'effectiveDate'
  Prelude.UTCTime ->
  -- | 'currencyCode'
  Prelude.Text ->
  ListPriceLists
newListPriceLists
  pServiceCode_
  pEffectiveDate_
  pCurrencyCode_ =
    ListPriceLists'
      { maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        regionCode = Prelude.Nothing,
        serviceCode = pServiceCode_,
        effectiveDate = Data._Time Lens.# pEffectiveDate_,
        currencyCode = pCurrencyCode_
      }

-- | The maximum number of results to return in the response.
listPriceLists_maxResults :: Lens.Lens' ListPriceLists (Prelude.Maybe Prelude.Natural)
listPriceLists_maxResults = Lens.lens (\ListPriceLists' {maxResults} -> maxResults) (\s@ListPriceLists' {} a -> s {maxResults = a} :: ListPriceLists)

-- | The pagination token that indicates the next set of results that you
-- want to retrieve.
listPriceLists_nextToken :: Lens.Lens' ListPriceLists (Prelude.Maybe Prelude.Text)
listPriceLists_nextToken = Lens.lens (\ListPriceLists' {nextToken} -> nextToken) (\s@ListPriceLists' {} a -> s {nextToken = a} :: ListPriceLists)

-- | This is used to filter the Price List by Amazon Web Services Region. For
-- example, to get the price list only for the @US East (N. Virginia)@
-- Region, use @us-east-1@. If nothing is specified, you retrieve price
-- lists for all applicable Regions. The available @RegionCode@ list can be
-- retrieved from
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_GetAttributeValues.html GetAttributeValues>
-- API.
listPriceLists_regionCode :: Lens.Lens' ListPriceLists (Prelude.Maybe Prelude.Text)
listPriceLists_regionCode = Lens.lens (\ListPriceLists' {regionCode} -> regionCode) (\s@ListPriceLists' {} a -> s {regionCode = a} :: ListPriceLists)

-- | The service code or the Savings Plan service code for the attributes
-- that you want to retrieve. For example, to get the list of applicable
-- Amazon EC2 price lists, use @AmazonEC2@. For a full list of service
-- codes containing On-Demand and Reserved Instance (RI) pricing, use the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_DescribeServices.html#awscostmanagement-pricing_DescribeServices-request-FormatVersion DescribeServices>
-- API.
--
-- To retrieve the Compute Savings Plan price lists, use
-- @ComputeSavingsPlans@. To retrieve Machine Learning Savings Plans price
-- lists, use @MachineLearningSavingsPlans@.
listPriceLists_serviceCode :: Lens.Lens' ListPriceLists Prelude.Text
listPriceLists_serviceCode = Lens.lens (\ListPriceLists' {serviceCode} -> serviceCode) (\s@ListPriceLists' {} a -> s {serviceCode = a} :: ListPriceLists)

-- | The date that the Price List file prices are effective from.
listPriceLists_effectiveDate :: Lens.Lens' ListPriceLists Prelude.UTCTime
listPriceLists_effectiveDate = Lens.lens (\ListPriceLists' {effectiveDate} -> effectiveDate) (\s@ListPriceLists' {} a -> s {effectiveDate = a} :: ListPriceLists) Prelude.. Data._Time

-- | The three alphabetical character ISO-4217 currency code that the Price
-- List files are denominated in.
listPriceLists_currencyCode :: Lens.Lens' ListPriceLists Prelude.Text
listPriceLists_currencyCode = Lens.lens (\ListPriceLists' {currencyCode} -> currencyCode) (\s@ListPriceLists' {} a -> s {currencyCode = a} :: ListPriceLists)

instance Core.AWSPager ListPriceLists where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPriceListsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPriceListsResponse_priceLists
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listPriceLists_nextToken
          Lens..~ rs
          Lens.^? listPriceListsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListPriceLists where
  type
    AWSResponse ListPriceLists =
      ListPriceListsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPriceListsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PriceLists" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPriceLists where
  hashWithSalt _salt ListPriceLists' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regionCode
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` effectiveDate
      `Prelude.hashWithSalt` currencyCode

instance Prelude.NFData ListPriceLists where
  rnf ListPriceLists' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regionCode
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf effectiveDate
      `Prelude.seq` Prelude.rnf currencyCode

instance Data.ToHeaders ListPriceLists where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPriceListService.ListPriceLists" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPriceLists where
  toJSON ListPriceLists' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("RegionCode" Data..=) Prelude.<$> regionCode,
            Prelude.Just ("ServiceCode" Data..= serviceCode),
            Prelude.Just ("EffectiveDate" Data..= effectiveDate),
            Prelude.Just ("CurrencyCode" Data..= currencyCode)
          ]
      )

instance Data.ToPath ListPriceLists where
  toPath = Prelude.const "/"

instance Data.ToQuery ListPriceLists where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPriceListsResponse' smart constructor.
data ListPriceListsResponse = ListPriceListsResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of price list references that match your request.
    priceLists :: Prelude.Maybe [PriceList],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPriceListsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPriceListsResponse_nextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- 'priceLists', 'listPriceListsResponse_priceLists' - The type of price list references that match your request.
--
-- 'httpStatus', 'listPriceListsResponse_httpStatus' - The response's http status code.
newListPriceListsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPriceListsResponse
newListPriceListsResponse pHttpStatus_ =
  ListPriceListsResponse'
    { nextToken =
        Prelude.Nothing,
      priceLists = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
listPriceListsResponse_nextToken :: Lens.Lens' ListPriceListsResponse (Prelude.Maybe Prelude.Text)
listPriceListsResponse_nextToken = Lens.lens (\ListPriceListsResponse' {nextToken} -> nextToken) (\s@ListPriceListsResponse' {} a -> s {nextToken = a} :: ListPriceListsResponse)

-- | The type of price list references that match your request.
listPriceListsResponse_priceLists :: Lens.Lens' ListPriceListsResponse (Prelude.Maybe [PriceList])
listPriceListsResponse_priceLists = Lens.lens (\ListPriceListsResponse' {priceLists} -> priceLists) (\s@ListPriceListsResponse' {} a -> s {priceLists = a} :: ListPriceListsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPriceListsResponse_httpStatus :: Lens.Lens' ListPriceListsResponse Prelude.Int
listPriceListsResponse_httpStatus = Lens.lens (\ListPriceListsResponse' {httpStatus} -> httpStatus) (\s@ListPriceListsResponse' {} a -> s {httpStatus = a} :: ListPriceListsResponse)

instance Prelude.NFData ListPriceListsResponse where
  rnf ListPriceListsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf priceLists
      `Prelude.seq` Prelude.rnf httpStatus
