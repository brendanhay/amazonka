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
-- Module      : Amazonka.SavingsPlans.DescribeSavingsPlansOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Savings Plans offerings.
module Amazonka.SavingsPlans.DescribeSavingsPlansOfferings
  ( -- * Creating a Request
    DescribeSavingsPlansOfferings (..),
    newDescribeSavingsPlansOfferings,

    -- * Request Lenses
    describeSavingsPlansOfferings_serviceCodes,
    describeSavingsPlansOfferings_productType,
    describeSavingsPlansOfferings_filters,
    describeSavingsPlansOfferings_offeringIds,
    describeSavingsPlansOfferings_currencies,
    describeSavingsPlansOfferings_nextToken,
    describeSavingsPlansOfferings_paymentOptions,
    describeSavingsPlansOfferings_descriptions,
    describeSavingsPlansOfferings_durations,
    describeSavingsPlansOfferings_planTypes,
    describeSavingsPlansOfferings_usageTypes,
    describeSavingsPlansOfferings_operations,
    describeSavingsPlansOfferings_maxResults,

    -- * Destructuring the Response
    DescribeSavingsPlansOfferingsResponse (..),
    newDescribeSavingsPlansOfferingsResponse,

    -- * Response Lenses
    describeSavingsPlansOfferingsResponse_searchResults,
    describeSavingsPlansOfferingsResponse_nextToken,
    describeSavingsPlansOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SavingsPlans.Types

-- | /See:/ 'newDescribeSavingsPlansOfferings' smart constructor.
data DescribeSavingsPlansOfferings = DescribeSavingsPlansOfferings'
  { -- | The services.
    serviceCodes :: Prelude.Maybe [Prelude.Text],
    -- | The product type.
    productType :: Prelude.Maybe SavingsPlanProductType,
    -- | The filters.
    filters :: Prelude.Maybe [SavingsPlanOfferingFilterElement],
    -- | The IDs of the offerings.
    offeringIds :: Prelude.Maybe [Prelude.Text],
    -- | The currencies.
    currencies :: Prelude.Maybe [CurrencyCode],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The payment options.
    paymentOptions :: Prelude.Maybe [SavingsPlanPaymentOption],
    -- | The descriptions.
    descriptions :: Prelude.Maybe [Prelude.Text],
    -- | The durations, in seconds.
    durations :: Prelude.Maybe [Prelude.Natural],
    -- | The plan type.
    planTypes :: Prelude.Maybe [SavingsPlanType],
    -- | The usage details of the line item in the billing report.
    usageTypes :: Prelude.Maybe [Prelude.Text],
    -- | The specific AWS operation for the line item in the billing report.
    operations :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return with a single call. To retrieve
    -- additional results, make another call with the returned token value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlansOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCodes', 'describeSavingsPlansOfferings_serviceCodes' - The services.
--
-- 'productType', 'describeSavingsPlansOfferings_productType' - The product type.
--
-- 'filters', 'describeSavingsPlansOfferings_filters' - The filters.
--
-- 'offeringIds', 'describeSavingsPlansOfferings_offeringIds' - The IDs of the offerings.
--
-- 'currencies', 'describeSavingsPlansOfferings_currencies' - The currencies.
--
-- 'nextToken', 'describeSavingsPlansOfferings_nextToken' - The token for the next page of results.
--
-- 'paymentOptions', 'describeSavingsPlansOfferings_paymentOptions' - The payment options.
--
-- 'descriptions', 'describeSavingsPlansOfferings_descriptions' - The descriptions.
--
-- 'durations', 'describeSavingsPlansOfferings_durations' - The durations, in seconds.
--
-- 'planTypes', 'describeSavingsPlansOfferings_planTypes' - The plan type.
--
-- 'usageTypes', 'describeSavingsPlansOfferings_usageTypes' - The usage details of the line item in the billing report.
--
-- 'operations', 'describeSavingsPlansOfferings_operations' - The specific AWS operation for the line item in the billing report.
--
-- 'maxResults', 'describeSavingsPlansOfferings_maxResults' - The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
newDescribeSavingsPlansOfferings ::
  DescribeSavingsPlansOfferings
newDescribeSavingsPlansOfferings =
  DescribeSavingsPlansOfferings'
    { serviceCodes =
        Prelude.Nothing,
      productType = Prelude.Nothing,
      filters = Prelude.Nothing,
      offeringIds = Prelude.Nothing,
      currencies = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      paymentOptions = Prelude.Nothing,
      descriptions = Prelude.Nothing,
      durations = Prelude.Nothing,
      planTypes = Prelude.Nothing,
      usageTypes = Prelude.Nothing,
      operations = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The services.
describeSavingsPlansOfferings_serviceCodes :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_serviceCodes = Lens.lens (\DescribeSavingsPlansOfferings' {serviceCodes} -> serviceCodes) (\s@DescribeSavingsPlansOfferings' {} a -> s {serviceCodes = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The product type.
describeSavingsPlansOfferings_productType :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe SavingsPlanProductType)
describeSavingsPlansOfferings_productType = Lens.lens (\DescribeSavingsPlansOfferings' {productType} -> productType) (\s@DescribeSavingsPlansOfferings' {} a -> s {productType = a} :: DescribeSavingsPlansOfferings)

-- | The filters.
describeSavingsPlansOfferings_filters :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [SavingsPlanOfferingFilterElement])
describeSavingsPlansOfferings_filters = Lens.lens (\DescribeSavingsPlansOfferings' {filters} -> filters) (\s@DescribeSavingsPlansOfferings' {} a -> s {filters = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the offerings.
describeSavingsPlansOfferings_offeringIds :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_offeringIds = Lens.lens (\DescribeSavingsPlansOfferings' {offeringIds} -> offeringIds) (\s@DescribeSavingsPlansOfferings' {} a -> s {offeringIds = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The currencies.
describeSavingsPlansOfferings_currencies :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [CurrencyCode])
describeSavingsPlansOfferings_currencies = Lens.lens (\DescribeSavingsPlansOfferings' {currencies} -> currencies) (\s@DescribeSavingsPlansOfferings' {} a -> s {currencies = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeSavingsPlansOfferings_nextToken :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe Prelude.Text)
describeSavingsPlansOfferings_nextToken = Lens.lens (\DescribeSavingsPlansOfferings' {nextToken} -> nextToken) (\s@DescribeSavingsPlansOfferings' {} a -> s {nextToken = a} :: DescribeSavingsPlansOfferings)

-- | The payment options.
describeSavingsPlansOfferings_paymentOptions :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [SavingsPlanPaymentOption])
describeSavingsPlansOfferings_paymentOptions = Lens.lens (\DescribeSavingsPlansOfferings' {paymentOptions} -> paymentOptions) (\s@DescribeSavingsPlansOfferings' {} a -> s {paymentOptions = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The descriptions.
describeSavingsPlansOfferings_descriptions :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_descriptions = Lens.lens (\DescribeSavingsPlansOfferings' {descriptions} -> descriptions) (\s@DescribeSavingsPlansOfferings' {} a -> s {descriptions = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The durations, in seconds.
describeSavingsPlansOfferings_durations :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Natural])
describeSavingsPlansOfferings_durations = Lens.lens (\DescribeSavingsPlansOfferings' {durations} -> durations) (\s@DescribeSavingsPlansOfferings' {} a -> s {durations = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The plan type.
describeSavingsPlansOfferings_planTypes :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [SavingsPlanType])
describeSavingsPlansOfferings_planTypes = Lens.lens (\DescribeSavingsPlansOfferings' {planTypes} -> planTypes) (\s@DescribeSavingsPlansOfferings' {} a -> s {planTypes = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The usage details of the line item in the billing report.
describeSavingsPlansOfferings_usageTypes :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_usageTypes = Lens.lens (\DescribeSavingsPlansOfferings' {usageTypes} -> usageTypes) (\s@DescribeSavingsPlansOfferings' {} a -> s {usageTypes = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The specific AWS operation for the line item in the billing report.
describeSavingsPlansOfferings_operations :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_operations = Lens.lens (\DescribeSavingsPlansOfferings' {operations} -> operations) (\s@DescribeSavingsPlansOfferings' {} a -> s {operations = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
describeSavingsPlansOfferings_maxResults :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe Prelude.Natural)
describeSavingsPlansOfferings_maxResults = Lens.lens (\DescribeSavingsPlansOfferings' {maxResults} -> maxResults) (\s@DescribeSavingsPlansOfferings' {} a -> s {maxResults = a} :: DescribeSavingsPlansOfferings)

instance
  Core.AWSRequest
    DescribeSavingsPlansOfferings
  where
  type
    AWSResponse DescribeSavingsPlansOfferings =
      DescribeSavingsPlansOfferingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSavingsPlansOfferingsResponse'
            Prelude.<$> (x Core..?> "searchResults" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSavingsPlansOfferings

instance Prelude.NFData DescribeSavingsPlansOfferings

instance Core.ToHeaders DescribeSavingsPlansOfferings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeSavingsPlansOfferings where
  toJSON DescribeSavingsPlansOfferings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("serviceCodes" Core..=) Prelude.<$> serviceCodes,
            ("productType" Core..=) Prelude.<$> productType,
            ("filters" Core..=) Prelude.<$> filters,
            ("offeringIds" Core..=) Prelude.<$> offeringIds,
            ("currencies" Core..=) Prelude.<$> currencies,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("paymentOptions" Core..=)
              Prelude.<$> paymentOptions,
            ("descriptions" Core..=) Prelude.<$> descriptions,
            ("durations" Core..=) Prelude.<$> durations,
            ("planTypes" Core..=) Prelude.<$> planTypes,
            ("usageTypes" Core..=) Prelude.<$> usageTypes,
            ("operations" Core..=) Prelude.<$> operations,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeSavingsPlansOfferings where
  toPath =
    Prelude.const "/DescribeSavingsPlansOfferings"

instance Core.ToQuery DescribeSavingsPlansOfferings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSavingsPlansOfferingsResponse' smart constructor.
data DescribeSavingsPlansOfferingsResponse = DescribeSavingsPlansOfferingsResponse'
  { -- | Information about the Savings Plans offerings.
    searchResults :: Prelude.Maybe [SavingsPlanOffering],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlansOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'searchResults', 'describeSavingsPlansOfferingsResponse_searchResults' - Information about the Savings Plans offerings.
--
-- 'nextToken', 'describeSavingsPlansOfferingsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeSavingsPlansOfferingsResponse_httpStatus' - The response's http status code.
newDescribeSavingsPlansOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSavingsPlansOfferingsResponse
newDescribeSavingsPlansOfferingsResponse pHttpStatus_ =
  DescribeSavingsPlansOfferingsResponse'
    { searchResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Savings Plans offerings.
describeSavingsPlansOfferingsResponse_searchResults :: Lens.Lens' DescribeSavingsPlansOfferingsResponse (Prelude.Maybe [SavingsPlanOffering])
describeSavingsPlansOfferingsResponse_searchResults = Lens.lens (\DescribeSavingsPlansOfferingsResponse' {searchResults} -> searchResults) (\s@DescribeSavingsPlansOfferingsResponse' {} a -> s {searchResults = a} :: DescribeSavingsPlansOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeSavingsPlansOfferingsResponse_nextToken :: Lens.Lens' DescribeSavingsPlansOfferingsResponse (Prelude.Maybe Prelude.Text)
describeSavingsPlansOfferingsResponse_nextToken = Lens.lens (\DescribeSavingsPlansOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeSavingsPlansOfferingsResponse' {} a -> s {nextToken = a} :: DescribeSavingsPlansOfferingsResponse)

-- | The response's http status code.
describeSavingsPlansOfferingsResponse_httpStatus :: Lens.Lens' DescribeSavingsPlansOfferingsResponse Prelude.Int
describeSavingsPlansOfferingsResponse_httpStatus = Lens.lens (\DescribeSavingsPlansOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeSavingsPlansOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeSavingsPlansOfferingsResponse)

instance
  Prelude.NFData
    DescribeSavingsPlansOfferingsResponse
