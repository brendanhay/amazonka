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
-- Module      : Amazonka.SavingsPlans.DescribeSavingsPlanRates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Savings Plans rates.
module Amazonka.SavingsPlans.DescribeSavingsPlanRates
  ( -- * Creating a Request
    DescribeSavingsPlanRates (..),
    newDescribeSavingsPlanRates,

    -- * Request Lenses
    describeSavingsPlanRates_filters,
    describeSavingsPlanRates_maxResults,
    describeSavingsPlanRates_nextToken,
    describeSavingsPlanRates_savingsPlanId,

    -- * Destructuring the Response
    DescribeSavingsPlanRatesResponse (..),
    newDescribeSavingsPlanRatesResponse,

    -- * Response Lenses
    describeSavingsPlanRatesResponse_nextToken,
    describeSavingsPlanRatesResponse_savingsPlanId,
    describeSavingsPlanRatesResponse_searchResults,
    describeSavingsPlanRatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SavingsPlans.Types

-- | /See:/ 'newDescribeSavingsPlanRates' smart constructor.
data DescribeSavingsPlanRates = DescribeSavingsPlanRates'
  { -- | The filters.
    filters :: Prelude.Maybe [SavingsPlanRateFilter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- additional results, make another call with the returned token value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Savings Plan.
    savingsPlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlanRates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeSavingsPlanRates_filters' - The filters.
--
-- 'maxResults', 'describeSavingsPlanRates_maxResults' - The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
--
-- 'nextToken', 'describeSavingsPlanRates_nextToken' - The token for the next page of results.
--
-- 'savingsPlanId', 'describeSavingsPlanRates_savingsPlanId' - The ID of the Savings Plan.
newDescribeSavingsPlanRates ::
  -- | 'savingsPlanId'
  Prelude.Text ->
  DescribeSavingsPlanRates
newDescribeSavingsPlanRates pSavingsPlanId_ =
  DescribeSavingsPlanRates'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      savingsPlanId = pSavingsPlanId_
    }

-- | The filters.
describeSavingsPlanRates_filters :: Lens.Lens' DescribeSavingsPlanRates (Prelude.Maybe [SavingsPlanRateFilter])
describeSavingsPlanRates_filters = Lens.lens (\DescribeSavingsPlanRates' {filters} -> filters) (\s@DescribeSavingsPlanRates' {} a -> s {filters = a} :: DescribeSavingsPlanRates) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- additional results, make another call with the returned token value.
describeSavingsPlanRates_maxResults :: Lens.Lens' DescribeSavingsPlanRates (Prelude.Maybe Prelude.Natural)
describeSavingsPlanRates_maxResults = Lens.lens (\DescribeSavingsPlanRates' {maxResults} -> maxResults) (\s@DescribeSavingsPlanRates' {} a -> s {maxResults = a} :: DescribeSavingsPlanRates)

-- | The token for the next page of results.
describeSavingsPlanRates_nextToken :: Lens.Lens' DescribeSavingsPlanRates (Prelude.Maybe Prelude.Text)
describeSavingsPlanRates_nextToken = Lens.lens (\DescribeSavingsPlanRates' {nextToken} -> nextToken) (\s@DescribeSavingsPlanRates' {} a -> s {nextToken = a} :: DescribeSavingsPlanRates)

-- | The ID of the Savings Plan.
describeSavingsPlanRates_savingsPlanId :: Lens.Lens' DescribeSavingsPlanRates Prelude.Text
describeSavingsPlanRates_savingsPlanId = Lens.lens (\DescribeSavingsPlanRates' {savingsPlanId} -> savingsPlanId) (\s@DescribeSavingsPlanRates' {} a -> s {savingsPlanId = a} :: DescribeSavingsPlanRates)

instance Core.AWSRequest DescribeSavingsPlanRates where
  type
    AWSResponse DescribeSavingsPlanRates =
      DescribeSavingsPlanRatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSavingsPlanRatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "savingsPlanId")
            Prelude.<*> (x Data..?> "searchResults" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSavingsPlanRates where
  hashWithSalt _salt DescribeSavingsPlanRates' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` savingsPlanId

instance Prelude.NFData DescribeSavingsPlanRates where
  rnf DescribeSavingsPlanRates' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf savingsPlanId

instance Data.ToHeaders DescribeSavingsPlanRates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSavingsPlanRates where
  toJSON DescribeSavingsPlanRates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("savingsPlanId" Data..= savingsPlanId)
          ]
      )

instance Data.ToPath DescribeSavingsPlanRates where
  toPath = Prelude.const "/DescribeSavingsPlanRates"

instance Data.ToQuery DescribeSavingsPlanRates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSavingsPlanRatesResponse' smart constructor.
data DescribeSavingsPlanRatesResponse = DescribeSavingsPlanRatesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Savings Plan.
    savingsPlanId :: Prelude.Maybe Prelude.Text,
    -- | Information about the Savings Plans rates.
    searchResults :: Prelude.Maybe [SavingsPlanRate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSavingsPlanRatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSavingsPlanRatesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'savingsPlanId', 'describeSavingsPlanRatesResponse_savingsPlanId' - The ID of the Savings Plan.
--
-- 'searchResults', 'describeSavingsPlanRatesResponse_searchResults' - Information about the Savings Plans rates.
--
-- 'httpStatus', 'describeSavingsPlanRatesResponse_httpStatus' - The response's http status code.
newDescribeSavingsPlanRatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSavingsPlanRatesResponse
newDescribeSavingsPlanRatesResponse pHttpStatus_ =
  DescribeSavingsPlanRatesResponse'
    { nextToken =
        Prelude.Nothing,
      savingsPlanId = Prelude.Nothing,
      searchResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeSavingsPlanRatesResponse_nextToken :: Lens.Lens' DescribeSavingsPlanRatesResponse (Prelude.Maybe Prelude.Text)
describeSavingsPlanRatesResponse_nextToken = Lens.lens (\DescribeSavingsPlanRatesResponse' {nextToken} -> nextToken) (\s@DescribeSavingsPlanRatesResponse' {} a -> s {nextToken = a} :: DescribeSavingsPlanRatesResponse)

-- | The ID of the Savings Plan.
describeSavingsPlanRatesResponse_savingsPlanId :: Lens.Lens' DescribeSavingsPlanRatesResponse (Prelude.Maybe Prelude.Text)
describeSavingsPlanRatesResponse_savingsPlanId = Lens.lens (\DescribeSavingsPlanRatesResponse' {savingsPlanId} -> savingsPlanId) (\s@DescribeSavingsPlanRatesResponse' {} a -> s {savingsPlanId = a} :: DescribeSavingsPlanRatesResponse)

-- | Information about the Savings Plans rates.
describeSavingsPlanRatesResponse_searchResults :: Lens.Lens' DescribeSavingsPlanRatesResponse (Prelude.Maybe [SavingsPlanRate])
describeSavingsPlanRatesResponse_searchResults = Lens.lens (\DescribeSavingsPlanRatesResponse' {searchResults} -> searchResults) (\s@DescribeSavingsPlanRatesResponse' {} a -> s {searchResults = a} :: DescribeSavingsPlanRatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSavingsPlanRatesResponse_httpStatus :: Lens.Lens' DescribeSavingsPlanRatesResponse Prelude.Int
describeSavingsPlanRatesResponse_httpStatus = Lens.lens (\DescribeSavingsPlanRatesResponse' {httpStatus} -> httpStatus) (\s@DescribeSavingsPlanRatesResponse' {} a -> s {httpStatus = a} :: DescribeSavingsPlanRatesResponse)

instance
  Prelude.NFData
    DescribeSavingsPlanRatesResponse
  where
  rnf DescribeSavingsPlanRatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf savingsPlanId
      `Prelude.seq` Prelude.rnf searchResults
      `Prelude.seq` Prelude.rnf httpStatus
