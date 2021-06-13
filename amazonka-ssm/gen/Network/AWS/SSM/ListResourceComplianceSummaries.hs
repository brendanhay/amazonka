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
-- Module      : Network.AWS.SSM.ListResourceComplianceSummaries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a resource-level summary count. The summary includes information
-- about compliant and non-compliant statuses and detailed compliance-item
-- severity counts, according to the filter criteria you specify.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListResourceComplianceSummaries
  ( -- * Creating a Request
    ListResourceComplianceSummaries (..),
    newListResourceComplianceSummaries,

    -- * Request Lenses
    listResourceComplianceSummaries_nextToken,
    listResourceComplianceSummaries_maxResults,
    listResourceComplianceSummaries_filters,

    -- * Destructuring the Response
    ListResourceComplianceSummariesResponse (..),
    newListResourceComplianceSummariesResponse,

    -- * Response Lenses
    listResourceComplianceSummariesResponse_nextToken,
    listResourceComplianceSummariesResponse_resourceComplianceSummaryItems,
    listResourceComplianceSummariesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListResourceComplianceSummaries' smart constructor.
data ListResourceComplianceSummaries = ListResourceComplianceSummaries'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    filters :: Prelude.Maybe [ComplianceStringFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceComplianceSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceComplianceSummaries_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'maxResults', 'listResourceComplianceSummaries_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'filters', 'listResourceComplianceSummaries_filters' - One or more filters. Use a filter to return a more specific list of
-- results.
newListResourceComplianceSummaries ::
  ListResourceComplianceSummaries
newListResourceComplianceSummaries =
  ListResourceComplianceSummaries'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      filters = Prelude.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listResourceComplianceSummaries_nextToken :: Lens.Lens' ListResourceComplianceSummaries (Prelude.Maybe Prelude.Text)
listResourceComplianceSummaries_nextToken = Lens.lens (\ListResourceComplianceSummaries' {nextToken} -> nextToken) (\s@ListResourceComplianceSummaries' {} a -> s {nextToken = a} :: ListResourceComplianceSummaries)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listResourceComplianceSummaries_maxResults :: Lens.Lens' ListResourceComplianceSummaries (Prelude.Maybe Prelude.Natural)
listResourceComplianceSummaries_maxResults = Lens.lens (\ListResourceComplianceSummaries' {maxResults} -> maxResults) (\s@ListResourceComplianceSummaries' {} a -> s {maxResults = a} :: ListResourceComplianceSummaries)

-- | One or more filters. Use a filter to return a more specific list of
-- results.
listResourceComplianceSummaries_filters :: Lens.Lens' ListResourceComplianceSummaries (Prelude.Maybe [ComplianceStringFilter])
listResourceComplianceSummaries_filters = Lens.lens (\ListResourceComplianceSummaries' {filters} -> filters) (\s@ListResourceComplianceSummaries' {} a -> s {filters = a} :: ListResourceComplianceSummaries) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    ListResourceComplianceSummaries
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceComplianceSummariesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceComplianceSummariesResponse_resourceComplianceSummaryItems
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResourceComplianceSummaries_nextToken
          Lens..~ rs
          Lens.^? listResourceComplianceSummariesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListResourceComplianceSummaries
  where
  type
    AWSResponse ListResourceComplianceSummaries =
      ListResourceComplianceSummariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResourceComplianceSummariesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ResourceComplianceSummaryItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResourceComplianceSummaries

instance
  Prelude.NFData
    ListResourceComplianceSummaries

instance
  Core.ToHeaders
    ListResourceComplianceSummaries
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListResourceComplianceSummaries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListResourceComplianceSummaries where
  toJSON ListResourceComplianceSummaries' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("Filters" Core..=) Prelude.<$> filters
          ]
      )

instance Core.ToPath ListResourceComplianceSummaries where
  toPath = Prelude.const "/"

instance Core.ToQuery ListResourceComplianceSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResourceComplianceSummariesResponse' smart constructor.
data ListResourceComplianceSummariesResponse = ListResourceComplianceSummariesResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A summary count for specified or targeted managed instances. Summary
    -- count includes information about compliant and non-compliant State
    -- Manager associations, patch status, or custom items according to the
    -- filter criteria that you specify.
    resourceComplianceSummaryItems :: Prelude.Maybe [ResourceComplianceSummaryItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResourceComplianceSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResourceComplianceSummariesResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'resourceComplianceSummaryItems', 'listResourceComplianceSummariesResponse_resourceComplianceSummaryItems' - A summary count for specified or targeted managed instances. Summary
-- count includes information about compliant and non-compliant State
-- Manager associations, patch status, or custom items according to the
-- filter criteria that you specify.
--
-- 'httpStatus', 'listResourceComplianceSummariesResponse_httpStatus' - The response's http status code.
newListResourceComplianceSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResourceComplianceSummariesResponse
newListResourceComplianceSummariesResponse
  pHttpStatus_ =
    ListResourceComplianceSummariesResponse'
      { nextToken =
          Prelude.Nothing,
        resourceComplianceSummaryItems =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listResourceComplianceSummariesResponse_nextToken :: Lens.Lens' ListResourceComplianceSummariesResponse (Prelude.Maybe Prelude.Text)
listResourceComplianceSummariesResponse_nextToken = Lens.lens (\ListResourceComplianceSummariesResponse' {nextToken} -> nextToken) (\s@ListResourceComplianceSummariesResponse' {} a -> s {nextToken = a} :: ListResourceComplianceSummariesResponse)

-- | A summary count for specified or targeted managed instances. Summary
-- count includes information about compliant and non-compliant State
-- Manager associations, patch status, or custom items according to the
-- filter criteria that you specify.
listResourceComplianceSummariesResponse_resourceComplianceSummaryItems :: Lens.Lens' ListResourceComplianceSummariesResponse (Prelude.Maybe [ResourceComplianceSummaryItem])
listResourceComplianceSummariesResponse_resourceComplianceSummaryItems = Lens.lens (\ListResourceComplianceSummariesResponse' {resourceComplianceSummaryItems} -> resourceComplianceSummaryItems) (\s@ListResourceComplianceSummariesResponse' {} a -> s {resourceComplianceSummaryItems = a} :: ListResourceComplianceSummariesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourceComplianceSummariesResponse_httpStatus :: Lens.Lens' ListResourceComplianceSummariesResponse Prelude.Int
listResourceComplianceSummariesResponse_httpStatus = Lens.lens (\ListResourceComplianceSummariesResponse' {httpStatus} -> httpStatus) (\s@ListResourceComplianceSummariesResponse' {} a -> s {httpStatus = a} :: ListResourceComplianceSummariesResponse)

instance
  Prelude.NFData
    ListResourceComplianceSummariesResponse
