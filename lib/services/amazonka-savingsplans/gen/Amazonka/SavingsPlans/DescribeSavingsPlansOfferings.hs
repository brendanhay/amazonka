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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    describeSavingsPlansOfferings_nextToken,
    describeSavingsPlansOfferings_usageTypes,
    describeSavingsPlansOfferings_operations,
    describeSavingsPlansOfferings_productType,
    describeSavingsPlansOfferings_filters,
    describeSavingsPlansOfferings_descriptions,
    describeSavingsPlansOfferings_currencies,
    describeSavingsPlansOfferings_maxResults,
    describeSavingsPlansOfferings_durations,
    describeSavingsPlansOfferings_offeringIds,
    describeSavingsPlansOfferings_paymentOptions,
    describeSavingsPlansOfferings_planTypes,
    describeSavingsPlansOfferings_serviceCodes,

    -- * Destructuring the Response
    DescribeSavingsPlansOfferingsResponse (..),
    newDescribeSavingsPlansOfferingsResponse,

    -- * Response Lenses
    describeSavingsPlansOfferingsResponse_nextToken,
    describeSavingsPlansOfferingsResponse_searchResults,
    describeSavingsPlansOfferingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SavingsPlans.Types

-- | /See:/ 'newDescribeSavingsPlansOfferings' smart constructor.
data DescribeSavingsPlansOfferings = DescribeSavingsPlansOfferings'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The usage details of the line item in the billing report.
    usageTypes :: Prelude.Maybe [Prelude.Text],
    -- | The specific AWS operation for the line item in the billing report.
    operations :: Prelude.Maybe [Prelude.Text],
    -- | The product type.
    productType :: Prelude.Maybe SavingsPlanProductType,
    -- | The filters.
    filters :: Prelude.Maybe [SavingsPlanOfferingFilterElement],
    -- | The descriptions.
    descriptions :: Prelude.Maybe [Prelude.Text],
    -- | The currencies.
    currencies :: Prelude.Maybe [CurrencyCode],
    -- | The maximum number of results to return with a single call. To retrieve
    -- additional results, make another call with the returned token value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The durations, in seconds.
    durations :: Prelude.Maybe [Prelude.Natural],
    -- | The IDs of the offerings.
    offeringIds :: Prelude.Maybe [Prelude.Text],
    -- | The payment options.
    paymentOptions :: Prelude.Maybe [SavingsPlanPaymentOption],
    -- | The plan type.
    planTypes :: Prelude.Maybe [SavingsPlanType],
    -- | The services.
    serviceCodes :: Prelude.Maybe [Prelude.Text]
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
-- 'nextToken', 'describeSavingsPlansOfferings_nextToken' - The token for the next page of results.
--
-- 'usageTypes', 'describeSavingsPlansOfferings_usageTypes' - The usage details of the line item in the billing report.
--
-- 'operations', 'describeSavingsPlansOfferings_operations' - The specific AWS operation for the line item in the billing report.
--
-- 'productType', 'describeSavingsPlansOfferings_productType' - The product type.
--
-- 'filters', 'describeSavingsPlansOfferings_filters' - The filters.
--
-- 'descriptions', 'describeSavingsPlansOfferings_descriptions' - The descriptions.
--
-- 'currencies', 'describeSavingsPlansOfferings_currencies' - The currencies.
--
-- 'maxResults', 'describeSavingsPlansOfferings_maxResults' - The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
--
-- 'durations', 'describeSavingsPlansOfferings_durations' - The durations, in seconds.
--
-- 'offeringIds', 'describeSavingsPlansOfferings_offeringIds' - The IDs of the offerings.
--
-- 'paymentOptions', 'describeSavingsPlansOfferings_paymentOptions' - The payment options.
--
-- 'planTypes', 'describeSavingsPlansOfferings_planTypes' - The plan type.
--
-- 'serviceCodes', 'describeSavingsPlansOfferings_serviceCodes' - The services.
newDescribeSavingsPlansOfferings ::
  DescribeSavingsPlansOfferings
newDescribeSavingsPlansOfferings =
  DescribeSavingsPlansOfferings'
    { nextToken =
        Prelude.Nothing,
      usageTypes = Prelude.Nothing,
      operations = Prelude.Nothing,
      productType = Prelude.Nothing,
      filters = Prelude.Nothing,
      descriptions = Prelude.Nothing,
      currencies = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      durations = Prelude.Nothing,
      offeringIds = Prelude.Nothing,
      paymentOptions = Prelude.Nothing,
      planTypes = Prelude.Nothing,
      serviceCodes = Prelude.Nothing
    }

-- | The token for the next page of results.
describeSavingsPlansOfferings_nextToken :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe Prelude.Text)
describeSavingsPlansOfferings_nextToken = Lens.lens (\DescribeSavingsPlansOfferings' {nextToken} -> nextToken) (\s@DescribeSavingsPlansOfferings' {} a -> s {nextToken = a} :: DescribeSavingsPlansOfferings)

-- | The usage details of the line item in the billing report.
describeSavingsPlansOfferings_usageTypes :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_usageTypes = Lens.lens (\DescribeSavingsPlansOfferings' {usageTypes} -> usageTypes) (\s@DescribeSavingsPlansOfferings' {} a -> s {usageTypes = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The specific AWS operation for the line item in the billing report.
describeSavingsPlansOfferings_operations :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_operations = Lens.lens (\DescribeSavingsPlansOfferings' {operations} -> operations) (\s@DescribeSavingsPlansOfferings' {} a -> s {operations = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The product type.
describeSavingsPlansOfferings_productType :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe SavingsPlanProductType)
describeSavingsPlansOfferings_productType = Lens.lens (\DescribeSavingsPlansOfferings' {productType} -> productType) (\s@DescribeSavingsPlansOfferings' {} a -> s {productType = a} :: DescribeSavingsPlansOfferings)

-- | The filters.
describeSavingsPlansOfferings_filters :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [SavingsPlanOfferingFilterElement])
describeSavingsPlansOfferings_filters = Lens.lens (\DescribeSavingsPlansOfferings' {filters} -> filters) (\s@DescribeSavingsPlansOfferings' {} a -> s {filters = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The descriptions.
describeSavingsPlansOfferings_descriptions :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_descriptions = Lens.lens (\DescribeSavingsPlansOfferings' {descriptions} -> descriptions) (\s@DescribeSavingsPlansOfferings' {} a -> s {descriptions = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The currencies.
describeSavingsPlansOfferings_currencies :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [CurrencyCode])
describeSavingsPlansOfferings_currencies = Lens.lens (\DescribeSavingsPlansOfferings' {currencies} -> currencies) (\s@DescribeSavingsPlansOfferings' {} a -> s {currencies = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
describeSavingsPlansOfferings_maxResults :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe Prelude.Natural)
describeSavingsPlansOfferings_maxResults = Lens.lens (\DescribeSavingsPlansOfferings' {maxResults} -> maxResults) (\s@DescribeSavingsPlansOfferings' {} a -> s {maxResults = a} :: DescribeSavingsPlansOfferings)

-- | The durations, in seconds.
describeSavingsPlansOfferings_durations :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Natural])
describeSavingsPlansOfferings_durations = Lens.lens (\DescribeSavingsPlansOfferings' {durations} -> durations) (\s@DescribeSavingsPlansOfferings' {} a -> s {durations = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the offerings.
describeSavingsPlansOfferings_offeringIds :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_offeringIds = Lens.lens (\DescribeSavingsPlansOfferings' {offeringIds} -> offeringIds) (\s@DescribeSavingsPlansOfferings' {} a -> s {offeringIds = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The payment options.
describeSavingsPlansOfferings_paymentOptions :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [SavingsPlanPaymentOption])
describeSavingsPlansOfferings_paymentOptions = Lens.lens (\DescribeSavingsPlansOfferings' {paymentOptions} -> paymentOptions) (\s@DescribeSavingsPlansOfferings' {} a -> s {paymentOptions = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The plan type.
describeSavingsPlansOfferings_planTypes :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [SavingsPlanType])
describeSavingsPlansOfferings_planTypes = Lens.lens (\DescribeSavingsPlansOfferings' {planTypes} -> planTypes) (\s@DescribeSavingsPlansOfferings' {} a -> s {planTypes = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The services.
describeSavingsPlansOfferings_serviceCodes :: Lens.Lens' DescribeSavingsPlansOfferings (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferings_serviceCodes = Lens.lens (\DescribeSavingsPlansOfferings' {serviceCodes} -> serviceCodes) (\s@DescribeSavingsPlansOfferings' {} a -> s {serviceCodes = a} :: DescribeSavingsPlansOfferings) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    DescribeSavingsPlansOfferings
  where
  type
    AWSResponse DescribeSavingsPlansOfferings =
      DescribeSavingsPlansOfferingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSavingsPlansOfferingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "searchResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSavingsPlansOfferings
  where
  hashWithSalt _salt DescribeSavingsPlansOfferings' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` usageTypes
      `Prelude.hashWithSalt` operations
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` descriptions
      `Prelude.hashWithSalt` currencies
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` durations
      `Prelude.hashWithSalt` offeringIds
      `Prelude.hashWithSalt` paymentOptions
      `Prelude.hashWithSalt` planTypes
      `Prelude.hashWithSalt` serviceCodes

instance Prelude.NFData DescribeSavingsPlansOfferings where
  rnf DescribeSavingsPlansOfferings' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf usageTypes
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf descriptions
      `Prelude.seq` Prelude.rnf currencies
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf durations
      `Prelude.seq` Prelude.rnf offeringIds
      `Prelude.seq` Prelude.rnf paymentOptions
      `Prelude.seq` Prelude.rnf planTypes
      `Prelude.seq` Prelude.rnf serviceCodes

instance Data.ToHeaders DescribeSavingsPlansOfferings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSavingsPlansOfferings where
  toJSON DescribeSavingsPlansOfferings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("usageTypes" Data..=) Prelude.<$> usageTypes,
            ("operations" Data..=) Prelude.<$> operations,
            ("productType" Data..=) Prelude.<$> productType,
            ("filters" Data..=) Prelude.<$> filters,
            ("descriptions" Data..=) Prelude.<$> descriptions,
            ("currencies" Data..=) Prelude.<$> currencies,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("durations" Data..=) Prelude.<$> durations,
            ("offeringIds" Data..=) Prelude.<$> offeringIds,
            ("paymentOptions" Data..=)
              Prelude.<$> paymentOptions,
            ("planTypes" Data..=) Prelude.<$> planTypes,
            ("serviceCodes" Data..=) Prelude.<$> serviceCodes
          ]
      )

instance Data.ToPath DescribeSavingsPlansOfferings where
  toPath =
    Prelude.const "/DescribeSavingsPlansOfferings"

instance Data.ToQuery DescribeSavingsPlansOfferings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSavingsPlansOfferingsResponse' smart constructor.
data DescribeSavingsPlansOfferingsResponse = DescribeSavingsPlansOfferingsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Savings Plans offerings.
    searchResults :: Prelude.Maybe [SavingsPlanOffering],
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
-- 'nextToken', 'describeSavingsPlansOfferingsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'searchResults', 'describeSavingsPlansOfferingsResponse_searchResults' - Information about the Savings Plans offerings.
--
-- 'httpStatus', 'describeSavingsPlansOfferingsResponse_httpStatus' - The response's http status code.
newDescribeSavingsPlansOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSavingsPlansOfferingsResponse
newDescribeSavingsPlansOfferingsResponse pHttpStatus_ =
  DescribeSavingsPlansOfferingsResponse'
    { nextToken =
        Prelude.Nothing,
      searchResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeSavingsPlansOfferingsResponse_nextToken :: Lens.Lens' DescribeSavingsPlansOfferingsResponse (Prelude.Maybe Prelude.Text)
describeSavingsPlansOfferingsResponse_nextToken = Lens.lens (\DescribeSavingsPlansOfferingsResponse' {nextToken} -> nextToken) (\s@DescribeSavingsPlansOfferingsResponse' {} a -> s {nextToken = a} :: DescribeSavingsPlansOfferingsResponse)

-- | Information about the Savings Plans offerings.
describeSavingsPlansOfferingsResponse_searchResults :: Lens.Lens' DescribeSavingsPlansOfferingsResponse (Prelude.Maybe [SavingsPlanOffering])
describeSavingsPlansOfferingsResponse_searchResults = Lens.lens (\DescribeSavingsPlansOfferingsResponse' {searchResults} -> searchResults) (\s@DescribeSavingsPlansOfferingsResponse' {} a -> s {searchResults = a} :: DescribeSavingsPlansOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSavingsPlansOfferingsResponse_httpStatus :: Lens.Lens' DescribeSavingsPlansOfferingsResponse Prelude.Int
describeSavingsPlansOfferingsResponse_httpStatus = Lens.lens (\DescribeSavingsPlansOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeSavingsPlansOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeSavingsPlansOfferingsResponse)

instance
  Prelude.NFData
    DescribeSavingsPlansOfferingsResponse
  where
  rnf DescribeSavingsPlansOfferingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchResults
      `Prelude.seq` Prelude.rnf httpStatus
