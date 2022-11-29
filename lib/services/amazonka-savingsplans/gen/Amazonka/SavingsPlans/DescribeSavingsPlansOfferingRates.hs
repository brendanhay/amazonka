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
-- Module      : Amazonka.SavingsPlans.DescribeSavingsPlansOfferingRates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Savings Plans offering rates.
module Amazonka.SavingsPlans.DescribeSavingsPlansOfferingRates
  ( -- * Creating a Request
    DescribeSavingsPlansOfferingRates (..),
    newDescribeSavingsPlansOfferingRates,

    -- * Request Lenses
    describeSavingsPlansOfferingRates_nextToken,
    describeSavingsPlansOfferingRates_usageTypes,
    describeSavingsPlansOfferingRates_operations,
    describeSavingsPlansOfferingRates_filters,
    describeSavingsPlansOfferingRates_savingsPlanOfferingIds,
    describeSavingsPlansOfferingRates_savingsPlanPaymentOptions,
    describeSavingsPlansOfferingRates_products,
    describeSavingsPlansOfferingRates_maxResults,
    describeSavingsPlansOfferingRates_savingsPlanTypes,
    describeSavingsPlansOfferingRates_serviceCodes,

    -- * Destructuring the Response
    DescribeSavingsPlansOfferingRatesResponse (..),
    newDescribeSavingsPlansOfferingRatesResponse,

    -- * Response Lenses
    describeSavingsPlansOfferingRatesResponse_nextToken,
    describeSavingsPlansOfferingRatesResponse_searchResults,
    describeSavingsPlansOfferingRatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SavingsPlans.Types

-- | /See:/ 'newDescribeSavingsPlansOfferingRates' smart constructor.
data DescribeSavingsPlansOfferingRates = DescribeSavingsPlansOfferingRates'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The usage details of the line item in the billing report.
    usageTypes :: Prelude.Maybe [Prelude.Text],
    -- | The specific AWS operation for the line item in the billing report.
    operations :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    filters :: Prelude.Maybe [SavingsPlanOfferingRateFilterElement],
    -- | The IDs of the offerings.
    savingsPlanOfferingIds :: Prelude.Maybe [Prelude.Text],
    -- | The payment options.
    savingsPlanPaymentOptions :: Prelude.Maybe [SavingsPlanPaymentOption],
    -- | The AWS products.
    products :: Prelude.Maybe [SavingsPlanProductType],
    -- | The maximum number of results to return with a single call. To retrieve
    -- additional results, make another call with the returned token value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The plan types.
    savingsPlanTypes :: Prelude.Maybe [SavingsPlanType],
    -- | The services.
    serviceCodes :: Prelude.Maybe [SavingsPlanRateServiceCode]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlansOfferingRates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSavingsPlansOfferingRates_nextToken' - The token for the next page of results.
--
-- 'usageTypes', 'describeSavingsPlansOfferingRates_usageTypes' - The usage details of the line item in the billing report.
--
-- 'operations', 'describeSavingsPlansOfferingRates_operations' - The specific AWS operation for the line item in the billing report.
--
-- 'filters', 'describeSavingsPlansOfferingRates_filters' - The filters.
--
-- 'savingsPlanOfferingIds', 'describeSavingsPlansOfferingRates_savingsPlanOfferingIds' - The IDs of the offerings.
--
-- 'savingsPlanPaymentOptions', 'describeSavingsPlansOfferingRates_savingsPlanPaymentOptions' - The payment options.
--
-- 'products', 'describeSavingsPlansOfferingRates_products' - The AWS products.
--
-- 'maxResults', 'describeSavingsPlansOfferingRates_maxResults' - The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
--
-- 'savingsPlanTypes', 'describeSavingsPlansOfferingRates_savingsPlanTypes' - The plan types.
--
-- 'serviceCodes', 'describeSavingsPlansOfferingRates_serviceCodes' - The services.
newDescribeSavingsPlansOfferingRates ::
  DescribeSavingsPlansOfferingRates
newDescribeSavingsPlansOfferingRates =
  DescribeSavingsPlansOfferingRates'
    { nextToken =
        Prelude.Nothing,
      usageTypes = Prelude.Nothing,
      operations = Prelude.Nothing,
      filters = Prelude.Nothing,
      savingsPlanOfferingIds = Prelude.Nothing,
      savingsPlanPaymentOptions =
        Prelude.Nothing,
      products = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      savingsPlanTypes = Prelude.Nothing,
      serviceCodes = Prelude.Nothing
    }

-- | The token for the next page of results.
describeSavingsPlansOfferingRates_nextToken :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe Prelude.Text)
describeSavingsPlansOfferingRates_nextToken = Lens.lens (\DescribeSavingsPlansOfferingRates' {nextToken} -> nextToken) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {nextToken = a} :: DescribeSavingsPlansOfferingRates)

-- | The usage details of the line item in the billing report.
describeSavingsPlansOfferingRates_usageTypes :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferingRates_usageTypes = Lens.lens (\DescribeSavingsPlansOfferingRates' {usageTypes} -> usageTypes) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {usageTypes = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

-- | The specific AWS operation for the line item in the billing report.
describeSavingsPlansOfferingRates_operations :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferingRates_operations = Lens.lens (\DescribeSavingsPlansOfferingRates' {operations} -> operations) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {operations = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
describeSavingsPlansOfferingRates_filters :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [SavingsPlanOfferingRateFilterElement])
describeSavingsPlansOfferingRates_filters = Lens.lens (\DescribeSavingsPlansOfferingRates' {filters} -> filters) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {filters = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the offerings.
describeSavingsPlansOfferingRates_savingsPlanOfferingIds :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [Prelude.Text])
describeSavingsPlansOfferingRates_savingsPlanOfferingIds = Lens.lens (\DescribeSavingsPlansOfferingRates' {savingsPlanOfferingIds} -> savingsPlanOfferingIds) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {savingsPlanOfferingIds = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

-- | The payment options.
describeSavingsPlansOfferingRates_savingsPlanPaymentOptions :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [SavingsPlanPaymentOption])
describeSavingsPlansOfferingRates_savingsPlanPaymentOptions = Lens.lens (\DescribeSavingsPlansOfferingRates' {savingsPlanPaymentOptions} -> savingsPlanPaymentOptions) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {savingsPlanPaymentOptions = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

-- | The AWS products.
describeSavingsPlansOfferingRates_products :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [SavingsPlanProductType])
describeSavingsPlansOfferingRates_products = Lens.lens (\DescribeSavingsPlansOfferingRates' {products} -> products) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {products = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
describeSavingsPlansOfferingRates_maxResults :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe Prelude.Natural)
describeSavingsPlansOfferingRates_maxResults = Lens.lens (\DescribeSavingsPlansOfferingRates' {maxResults} -> maxResults) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {maxResults = a} :: DescribeSavingsPlansOfferingRates)

-- | The plan types.
describeSavingsPlansOfferingRates_savingsPlanTypes :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [SavingsPlanType])
describeSavingsPlansOfferingRates_savingsPlanTypes = Lens.lens (\DescribeSavingsPlansOfferingRates' {savingsPlanTypes} -> savingsPlanTypes) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {savingsPlanTypes = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

-- | The services.
describeSavingsPlansOfferingRates_serviceCodes :: Lens.Lens' DescribeSavingsPlansOfferingRates (Prelude.Maybe [SavingsPlanRateServiceCode])
describeSavingsPlansOfferingRates_serviceCodes = Lens.lens (\DescribeSavingsPlansOfferingRates' {serviceCodes} -> serviceCodes) (\s@DescribeSavingsPlansOfferingRates' {} a -> s {serviceCodes = a} :: DescribeSavingsPlansOfferingRates) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    DescribeSavingsPlansOfferingRates
  where
  type
    AWSResponse DescribeSavingsPlansOfferingRates =
      DescribeSavingsPlansOfferingRatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSavingsPlansOfferingRatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
              Prelude.<*> (x Core..?> "searchResults" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSavingsPlansOfferingRates
  where
  hashWithSalt
    _salt
    DescribeSavingsPlansOfferingRates' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` usageTypes
        `Prelude.hashWithSalt` operations
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` savingsPlanOfferingIds
        `Prelude.hashWithSalt` savingsPlanPaymentOptions
        `Prelude.hashWithSalt` products
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` savingsPlanTypes
        `Prelude.hashWithSalt` serviceCodes

instance
  Prelude.NFData
    DescribeSavingsPlansOfferingRates
  where
  rnf DescribeSavingsPlansOfferingRates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf usageTypes
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf savingsPlanOfferingIds
      `Prelude.seq` Prelude.rnf savingsPlanPaymentOptions
      `Prelude.seq` Prelude.rnf products
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf savingsPlanTypes
      `Prelude.seq` Prelude.rnf serviceCodes

instance
  Core.ToHeaders
    DescribeSavingsPlansOfferingRates
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeSavingsPlansOfferingRates
  where
  toJSON DescribeSavingsPlansOfferingRates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("usageTypes" Core..=) Prelude.<$> usageTypes,
            ("operations" Core..=) Prelude.<$> operations,
            ("filters" Core..=) Prelude.<$> filters,
            ("savingsPlanOfferingIds" Core..=)
              Prelude.<$> savingsPlanOfferingIds,
            ("savingsPlanPaymentOptions" Core..=)
              Prelude.<$> savingsPlanPaymentOptions,
            ("products" Core..=) Prelude.<$> products,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("savingsPlanTypes" Core..=)
              Prelude.<$> savingsPlanTypes,
            ("serviceCodes" Core..=) Prelude.<$> serviceCodes
          ]
      )

instance
  Core.ToPath
    DescribeSavingsPlansOfferingRates
  where
  toPath =
    Prelude.const "/DescribeSavingsPlansOfferingRates"

instance
  Core.ToQuery
    DescribeSavingsPlansOfferingRates
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSavingsPlansOfferingRatesResponse' smart constructor.
data DescribeSavingsPlansOfferingRatesResponse = DescribeSavingsPlansOfferingRatesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Savings Plans offering rates.
    searchResults :: Prelude.Maybe [SavingsPlanOfferingRate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlansOfferingRatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSavingsPlansOfferingRatesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'searchResults', 'describeSavingsPlansOfferingRatesResponse_searchResults' - Information about the Savings Plans offering rates.
--
-- 'httpStatus', 'describeSavingsPlansOfferingRatesResponse_httpStatus' - The response's http status code.
newDescribeSavingsPlansOfferingRatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSavingsPlansOfferingRatesResponse
newDescribeSavingsPlansOfferingRatesResponse
  pHttpStatus_ =
    DescribeSavingsPlansOfferingRatesResponse'
      { nextToken =
          Prelude.Nothing,
        searchResults = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeSavingsPlansOfferingRatesResponse_nextToken :: Lens.Lens' DescribeSavingsPlansOfferingRatesResponse (Prelude.Maybe Prelude.Text)
describeSavingsPlansOfferingRatesResponse_nextToken = Lens.lens (\DescribeSavingsPlansOfferingRatesResponse' {nextToken} -> nextToken) (\s@DescribeSavingsPlansOfferingRatesResponse' {} a -> s {nextToken = a} :: DescribeSavingsPlansOfferingRatesResponse)

-- | Information about the Savings Plans offering rates.
describeSavingsPlansOfferingRatesResponse_searchResults :: Lens.Lens' DescribeSavingsPlansOfferingRatesResponse (Prelude.Maybe [SavingsPlanOfferingRate])
describeSavingsPlansOfferingRatesResponse_searchResults = Lens.lens (\DescribeSavingsPlansOfferingRatesResponse' {searchResults} -> searchResults) (\s@DescribeSavingsPlansOfferingRatesResponse' {} a -> s {searchResults = a} :: DescribeSavingsPlansOfferingRatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSavingsPlansOfferingRatesResponse_httpStatus :: Lens.Lens' DescribeSavingsPlansOfferingRatesResponse Prelude.Int
describeSavingsPlansOfferingRatesResponse_httpStatus = Lens.lens (\DescribeSavingsPlansOfferingRatesResponse' {httpStatus} -> httpStatus) (\s@DescribeSavingsPlansOfferingRatesResponse' {} a -> s {httpStatus = a} :: DescribeSavingsPlansOfferingRatesResponse)

instance
  Prelude.NFData
    DescribeSavingsPlansOfferingRatesResponse
  where
  rnf DescribeSavingsPlansOfferingRatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf searchResults
      `Prelude.seq` Prelude.rnf httpStatus
