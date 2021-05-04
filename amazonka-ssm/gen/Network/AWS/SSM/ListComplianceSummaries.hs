{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SSM.ListComplianceSummaries
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SSM.ListComplianceSummaries
  ( -- * Creating a Request
    ListComplianceSummaries (..),
    newListComplianceSummaries,

    -- * Request Lenses
    listComplianceSummaries_nextToken,
    listComplianceSummaries_maxResults,
    listComplianceSummaries_filters,

    -- * Destructuring the Response
    ListComplianceSummariesResponse (..),
    newListComplianceSummariesResponse,

    -- * Response Lenses
    listComplianceSummariesResponse_nextToken,
    listComplianceSummariesResponse_complianceSummaryItems,
    listComplianceSummariesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListComplianceSummaries' smart constructor.
data ListComplianceSummaries = ListComplianceSummaries'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. Currently, you can
    -- specify null or 50. The call also returns a token that you can specify
    -- in a subsequent call to get the next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more compliance or inventory filters. Use a filter to return a
    -- more specific list of results.
    filters :: Prelude.Maybe [ComplianceStringFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListComplianceSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComplianceSummaries_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listComplianceSummaries_maxResults' - The maximum number of items to return for this call. Currently, you can
-- specify null or 50. The call also returns a token that you can specify
-- in a subsequent call to get the next set of results.
--
-- 'filters', 'listComplianceSummaries_filters' - One or more compliance or inventory filters. Use a filter to return a
-- more specific list of results.
newListComplianceSummaries ::
  ListComplianceSummaries
newListComplianceSummaries =
  ListComplianceSummaries'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listComplianceSummaries_nextToken :: Lens.Lens' ListComplianceSummaries (Prelude.Maybe Prelude.Text)
listComplianceSummaries_nextToken = Lens.lens (\ListComplianceSummaries' {nextToken} -> nextToken) (\s@ListComplianceSummaries' {} a -> s {nextToken = a} :: ListComplianceSummaries)

-- | The maximum number of items to return for this call. Currently, you can
-- specify null or 50. The call also returns a token that you can specify
-- in a subsequent call to get the next set of results.
listComplianceSummaries_maxResults :: Lens.Lens' ListComplianceSummaries (Prelude.Maybe Prelude.Natural)
listComplianceSummaries_maxResults = Lens.lens (\ListComplianceSummaries' {maxResults} -> maxResults) (\s@ListComplianceSummaries' {} a -> s {maxResults = a} :: ListComplianceSummaries)

-- | One or more compliance or inventory filters. Use a filter to return a
-- more specific list of results.
listComplianceSummaries_filters :: Lens.Lens' ListComplianceSummaries (Prelude.Maybe [ComplianceStringFilter])
listComplianceSummaries_filters = Lens.lens (\ListComplianceSummaries' {filters} -> filters) (\s@ListComplianceSummaries' {} a -> s {filters = a} :: ListComplianceSummaries) Prelude.. Lens.mapping Prelude._Coerce

instance Pager.AWSPager ListComplianceSummaries where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listComplianceSummariesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listComplianceSummariesResponse_complianceSummaryItems
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listComplianceSummaries_nextToken
          Lens..~ rs
          Lens.^? listComplianceSummariesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListComplianceSummaries where
  type
    Rs ListComplianceSummaries =
      ListComplianceSummariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComplianceSummariesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ComplianceSummaryItems"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListComplianceSummaries

instance Prelude.NFData ListComplianceSummaries

instance Prelude.ToHeaders ListComplianceSummaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.ListComplianceSummaries" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListComplianceSummaries where
  toJSON ListComplianceSummaries' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("Filters" Prelude..=) Prelude.<$> filters
          ]
      )

instance Prelude.ToPath ListComplianceSummaries where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListComplianceSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComplianceSummariesResponse' smart constructor.
data ListComplianceSummariesResponse = ListComplianceSummariesResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of compliant and non-compliant summary counts based on compliance
    -- types. For example, this call returns State Manager associations,
    -- patches, or custom compliance types according to the filter criteria
    -- that you specified.
    complianceSummaryItems :: Prelude.Maybe [ComplianceSummaryItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListComplianceSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComplianceSummariesResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'complianceSummaryItems', 'listComplianceSummariesResponse_complianceSummaryItems' - A list of compliant and non-compliant summary counts based on compliance
-- types. For example, this call returns State Manager associations,
-- patches, or custom compliance types according to the filter criteria
-- that you specified.
--
-- 'httpStatus', 'listComplianceSummariesResponse_httpStatus' - The response's http status code.
newListComplianceSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComplianceSummariesResponse
newListComplianceSummariesResponse pHttpStatus_ =
  ListComplianceSummariesResponse'
    { nextToken =
        Prelude.Nothing,
      complianceSummaryItems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listComplianceSummariesResponse_nextToken :: Lens.Lens' ListComplianceSummariesResponse (Prelude.Maybe Prelude.Text)
listComplianceSummariesResponse_nextToken = Lens.lens (\ListComplianceSummariesResponse' {nextToken} -> nextToken) (\s@ListComplianceSummariesResponse' {} a -> s {nextToken = a} :: ListComplianceSummariesResponse)

-- | A list of compliant and non-compliant summary counts based on compliance
-- types. For example, this call returns State Manager associations,
-- patches, or custom compliance types according to the filter criteria
-- that you specified.
listComplianceSummariesResponse_complianceSummaryItems :: Lens.Lens' ListComplianceSummariesResponse (Prelude.Maybe [ComplianceSummaryItem])
listComplianceSummariesResponse_complianceSummaryItems = Lens.lens (\ListComplianceSummariesResponse' {complianceSummaryItems} -> complianceSummaryItems) (\s@ListComplianceSummariesResponse' {} a -> s {complianceSummaryItems = a} :: ListComplianceSummariesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listComplianceSummariesResponse_httpStatus :: Lens.Lens' ListComplianceSummariesResponse Prelude.Int
listComplianceSummariesResponse_httpStatus = Lens.lens (\ListComplianceSummariesResponse' {httpStatus} -> httpStatus) (\s@ListComplianceSummariesResponse' {} a -> s {httpStatus = a} :: ListComplianceSummariesResponse)

instance
  Prelude.NFData
    ListComplianceSummariesResponse
