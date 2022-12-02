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
-- Module      : Amazonka.BillingConductor.ListBillingGroupCostReports
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A paginated call to retrieve a summary report of actual Amazon Web
-- Services charges and the calculated Amazon Web Services charges based on
-- the associated pricing plan of a billing group.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListBillingGroupCostReports
  ( -- * Creating a Request
    ListBillingGroupCostReports (..),
    newListBillingGroupCostReports,

    -- * Request Lenses
    listBillingGroupCostReports_nextToken,
    listBillingGroupCostReports_billingPeriod,
    listBillingGroupCostReports_filters,
    listBillingGroupCostReports_maxResults,

    -- * Destructuring the Response
    ListBillingGroupCostReportsResponse (..),
    newListBillingGroupCostReportsResponse,

    -- * Response Lenses
    listBillingGroupCostReportsResponse_nextToken,
    listBillingGroupCostReportsResponse_billingGroupCostReports,
    listBillingGroupCostReportsResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBillingGroupCostReports' smart constructor.
data ListBillingGroupCostReports = ListBillingGroupCostReports'
  { -- | The pagination token that\'s used on subsequent calls to get reports.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The preferred billing period for your report.
    billingPeriod :: Prelude.Maybe Prelude.Text,
    -- | A @ListBillingGroupCostReportsFilter@ to specify billing groups to
    -- retrieve reports from.
    filters :: Prelude.Maybe ListBillingGroupCostReportsFilter,
    -- | The maximum number of reports to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBillingGroupCostReports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBillingGroupCostReports_nextToken' - The pagination token that\'s used on subsequent calls to get reports.
--
-- 'billingPeriod', 'listBillingGroupCostReports_billingPeriod' - The preferred billing period for your report.
--
-- 'filters', 'listBillingGroupCostReports_filters' - A @ListBillingGroupCostReportsFilter@ to specify billing groups to
-- retrieve reports from.
--
-- 'maxResults', 'listBillingGroupCostReports_maxResults' - The maximum number of reports to retrieve.
newListBillingGroupCostReports ::
  ListBillingGroupCostReports
newListBillingGroupCostReports =
  ListBillingGroupCostReports'
    { nextToken =
        Prelude.Nothing,
      billingPeriod = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token that\'s used on subsequent calls to get reports.
listBillingGroupCostReports_nextToken :: Lens.Lens' ListBillingGroupCostReports (Prelude.Maybe Prelude.Text)
listBillingGroupCostReports_nextToken = Lens.lens (\ListBillingGroupCostReports' {nextToken} -> nextToken) (\s@ListBillingGroupCostReports' {} a -> s {nextToken = a} :: ListBillingGroupCostReports)

-- | The preferred billing period for your report.
listBillingGroupCostReports_billingPeriod :: Lens.Lens' ListBillingGroupCostReports (Prelude.Maybe Prelude.Text)
listBillingGroupCostReports_billingPeriod = Lens.lens (\ListBillingGroupCostReports' {billingPeriod} -> billingPeriod) (\s@ListBillingGroupCostReports' {} a -> s {billingPeriod = a} :: ListBillingGroupCostReports)

-- | A @ListBillingGroupCostReportsFilter@ to specify billing groups to
-- retrieve reports from.
listBillingGroupCostReports_filters :: Lens.Lens' ListBillingGroupCostReports (Prelude.Maybe ListBillingGroupCostReportsFilter)
listBillingGroupCostReports_filters = Lens.lens (\ListBillingGroupCostReports' {filters} -> filters) (\s@ListBillingGroupCostReports' {} a -> s {filters = a} :: ListBillingGroupCostReports)

-- | The maximum number of reports to retrieve.
listBillingGroupCostReports_maxResults :: Lens.Lens' ListBillingGroupCostReports (Prelude.Maybe Prelude.Natural)
listBillingGroupCostReports_maxResults = Lens.lens (\ListBillingGroupCostReports' {maxResults} -> maxResults) (\s@ListBillingGroupCostReports' {} a -> s {maxResults = a} :: ListBillingGroupCostReports)

instance Core.AWSPager ListBillingGroupCostReports where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBillingGroupCostReportsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBillingGroupCostReportsResponse_billingGroupCostReports
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBillingGroupCostReports_nextToken
          Lens..~ rs
          Lens.^? listBillingGroupCostReportsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListBillingGroupCostReports where
  type
    AWSResponse ListBillingGroupCostReports =
      ListBillingGroupCostReportsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBillingGroupCostReportsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "BillingGroupCostReports"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBillingGroupCostReports where
  hashWithSalt _salt ListBillingGroupCostReports' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` billingPeriod
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListBillingGroupCostReports where
  rnf ListBillingGroupCostReports' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf billingPeriod
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListBillingGroupCostReports where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListBillingGroupCostReports where
  toJSON ListBillingGroupCostReports' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("BillingPeriod" Data..=) Prelude.<$> billingPeriod,
            ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListBillingGroupCostReports where
  toPath =
    Prelude.const "/list-billing-group-cost-reports"

instance Data.ToQuery ListBillingGroupCostReports where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListBillingGroupCostReportsResponse' smart constructor.
data ListBillingGroupCostReportsResponse = ListBillingGroupCostReportsResponse'
  { -- | The pagination token that\'s used on subsequent calls to get reports.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @BillingGroupCostReportElement@ retrieved.
    billingGroupCostReports :: Prelude.Maybe [BillingGroupCostReportElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBillingGroupCostReportsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listBillingGroupCostReportsResponse_nextToken' - The pagination token that\'s used on subsequent calls to get reports.
--
-- 'billingGroupCostReports', 'listBillingGroupCostReportsResponse_billingGroupCostReports' - A list of @BillingGroupCostReportElement@ retrieved.
--
-- 'httpStatus', 'listBillingGroupCostReportsResponse_httpStatus' - The response's http status code.
newListBillingGroupCostReportsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBillingGroupCostReportsResponse
newListBillingGroupCostReportsResponse pHttpStatus_ =
  ListBillingGroupCostReportsResponse'
    { nextToken =
        Prelude.Nothing,
      billingGroupCostReports =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s used on subsequent calls to get reports.
listBillingGroupCostReportsResponse_nextToken :: Lens.Lens' ListBillingGroupCostReportsResponse (Prelude.Maybe Prelude.Text)
listBillingGroupCostReportsResponse_nextToken = Lens.lens (\ListBillingGroupCostReportsResponse' {nextToken} -> nextToken) (\s@ListBillingGroupCostReportsResponse' {} a -> s {nextToken = a} :: ListBillingGroupCostReportsResponse)

-- | A list of @BillingGroupCostReportElement@ retrieved.
listBillingGroupCostReportsResponse_billingGroupCostReports :: Lens.Lens' ListBillingGroupCostReportsResponse (Prelude.Maybe [BillingGroupCostReportElement])
listBillingGroupCostReportsResponse_billingGroupCostReports = Lens.lens (\ListBillingGroupCostReportsResponse' {billingGroupCostReports} -> billingGroupCostReports) (\s@ListBillingGroupCostReportsResponse' {} a -> s {billingGroupCostReports = a} :: ListBillingGroupCostReportsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listBillingGroupCostReportsResponse_httpStatus :: Lens.Lens' ListBillingGroupCostReportsResponse Prelude.Int
listBillingGroupCostReportsResponse_httpStatus = Lens.lens (\ListBillingGroupCostReportsResponse' {httpStatus} -> httpStatus) (\s@ListBillingGroupCostReportsResponse' {} a -> s {httpStatus = a} :: ListBillingGroupCostReportsResponse)

instance
  Prelude.NFData
    ListBillingGroupCostReportsResponse
  where
  rnf ListBillingGroupCostReportsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf billingGroupCostReports
      `Prelude.seq` Prelude.rnf httpStatus
