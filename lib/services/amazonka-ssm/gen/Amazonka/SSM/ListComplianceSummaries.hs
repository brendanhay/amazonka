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
-- Module      : Amazonka.SSM.ListComplianceSummaries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a summary count of compliant and non-compliant resources for a
-- compliance type. For example, this call can return State Manager
-- associations, patches, or custom compliance types according to the
-- filter criteria that you specify.
--
-- This operation returns paginated results.
module Amazonka.SSM.ListComplianceSummaries
  ( -- * Creating a Request
    ListComplianceSummaries (..),
    newListComplianceSummaries,

    -- * Request Lenses
    listComplianceSummaries_filters,
    listComplianceSummaries_maxResults,
    listComplianceSummaries_nextToken,

    -- * Destructuring the Response
    ListComplianceSummariesResponse (..),
    newListComplianceSummariesResponse,

    -- * Response Lenses
    listComplianceSummariesResponse_complianceSummaryItems,
    listComplianceSummariesResponse_nextToken,
    listComplianceSummariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newListComplianceSummaries' smart constructor.
data ListComplianceSummaries = ListComplianceSummaries'
  { -- | One or more compliance or inventory filters. Use a filter to return a
    -- more specific list of results.
    filters :: Prelude.Maybe [ComplianceStringFilter],
    -- | The maximum number of items to return for this call. Currently, you can
    -- specify null or 50. The call also returns a token that you can specify
    -- in a subsequent call to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComplianceSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listComplianceSummaries_filters' - One or more compliance or inventory filters. Use a filter to return a
-- more specific list of results.
--
-- 'maxResults', 'listComplianceSummaries_maxResults' - The maximum number of items to return for this call. Currently, you can
-- specify null or 50. The call also returns a token that you can specify
-- in a subsequent call to get the next set of results.
--
-- 'nextToken', 'listComplianceSummaries_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
newListComplianceSummaries ::
  ListComplianceSummaries
newListComplianceSummaries =
  ListComplianceSummaries'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | One or more compliance or inventory filters. Use a filter to return a
-- more specific list of results.
listComplianceSummaries_filters :: Lens.Lens' ListComplianceSummaries (Prelude.Maybe [ComplianceStringFilter])
listComplianceSummaries_filters = Lens.lens (\ListComplianceSummaries' {filters} -> filters) (\s@ListComplianceSummaries' {} a -> s {filters = a} :: ListComplianceSummaries) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this call. Currently, you can
-- specify null or 50. The call also returns a token that you can specify
-- in a subsequent call to get the next set of results.
listComplianceSummaries_maxResults :: Lens.Lens' ListComplianceSummaries (Prelude.Maybe Prelude.Natural)
listComplianceSummaries_maxResults = Lens.lens (\ListComplianceSummaries' {maxResults} -> maxResults) (\s@ListComplianceSummaries' {} a -> s {maxResults = a} :: ListComplianceSummaries)

-- | A token to start the list. Use this token to get the next set of
-- results.
listComplianceSummaries_nextToken :: Lens.Lens' ListComplianceSummaries (Prelude.Maybe Prelude.Text)
listComplianceSummaries_nextToken = Lens.lens (\ListComplianceSummaries' {nextToken} -> nextToken) (\s@ListComplianceSummaries' {} a -> s {nextToken = a} :: ListComplianceSummaries)

instance Core.AWSPager ListComplianceSummaries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComplianceSummariesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listComplianceSummariesResponse_complianceSummaryItems
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listComplianceSummaries_nextToken
          Lens..~ rs
          Lens.^? listComplianceSummariesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListComplianceSummaries where
  type
    AWSResponse ListComplianceSummaries =
      ListComplianceSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComplianceSummariesResponse'
            Prelude.<$> ( x Data..?> "ComplianceSummaryItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComplianceSummaries where
  hashWithSalt _salt ListComplianceSummaries' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListComplianceSummaries where
  rnf ListComplianceSummaries' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListComplianceSummaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.ListComplianceSummaries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListComplianceSummaries where
  toJSON ListComplianceSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListComplianceSummaries where
  toPath = Prelude.const "/"

instance Data.ToQuery ListComplianceSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComplianceSummariesResponse' smart constructor.
data ListComplianceSummariesResponse = ListComplianceSummariesResponse'
  { -- | A list of compliant and non-compliant summary counts based on compliance
    -- types. For example, this call returns State Manager associations,
    -- patches, or custom compliance types according to the filter criteria
    -- that you specified.
    complianceSummaryItems :: Prelude.Maybe [ComplianceSummaryItem],
    -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComplianceSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceSummaryItems', 'listComplianceSummariesResponse_complianceSummaryItems' - A list of compliant and non-compliant summary counts based on compliance
-- types. For example, this call returns State Manager associations,
-- patches, or custom compliance types according to the filter criteria
-- that you specified.
--
-- 'nextToken', 'listComplianceSummariesResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'httpStatus', 'listComplianceSummariesResponse_httpStatus' - The response's http status code.
newListComplianceSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComplianceSummariesResponse
newListComplianceSummariesResponse pHttpStatus_ =
  ListComplianceSummariesResponse'
    { complianceSummaryItems =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of compliant and non-compliant summary counts based on compliance
-- types. For example, this call returns State Manager associations,
-- patches, or custom compliance types according to the filter criteria
-- that you specified.
listComplianceSummariesResponse_complianceSummaryItems :: Lens.Lens' ListComplianceSummariesResponse (Prelude.Maybe [ComplianceSummaryItem])
listComplianceSummariesResponse_complianceSummaryItems = Lens.lens (\ListComplianceSummariesResponse' {complianceSummaryItems} -> complianceSummaryItems) (\s@ListComplianceSummariesResponse' {} a -> s {complianceSummaryItems = a} :: ListComplianceSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listComplianceSummariesResponse_nextToken :: Lens.Lens' ListComplianceSummariesResponse (Prelude.Maybe Prelude.Text)
listComplianceSummariesResponse_nextToken = Lens.lens (\ListComplianceSummariesResponse' {nextToken} -> nextToken) (\s@ListComplianceSummariesResponse' {} a -> s {nextToken = a} :: ListComplianceSummariesResponse)

-- | The response's http status code.
listComplianceSummariesResponse_httpStatus :: Lens.Lens' ListComplianceSummariesResponse Prelude.Int
listComplianceSummariesResponse_httpStatus = Lens.lens (\ListComplianceSummariesResponse' {httpStatus} -> httpStatus) (\s@ListComplianceSummariesResponse' {} a -> s {httpStatus = a} :: ListComplianceSummariesResponse)

instance
  Prelude.NFData
    ListComplianceSummariesResponse
  where
  rnf ListComplianceSummariesResponse' {..} =
    Prelude.rnf complianceSummaryItems
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
