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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newListResourceComplianceSummaries' smart constructor.
data ListResourceComplianceSummaries = ListResourceComplianceSummaries'
  { -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. Use a filter to return a more specific list of
    -- results.
    filters :: Core.Maybe [ComplianceStringFilter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | A token to start the list. Use this token to get the next set of
-- results.
listResourceComplianceSummaries_nextToken :: Lens.Lens' ListResourceComplianceSummaries (Core.Maybe Core.Text)
listResourceComplianceSummaries_nextToken = Lens.lens (\ListResourceComplianceSummaries' {nextToken} -> nextToken) (\s@ListResourceComplianceSummaries' {} a -> s {nextToken = a} :: ListResourceComplianceSummaries)

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listResourceComplianceSummaries_maxResults :: Lens.Lens' ListResourceComplianceSummaries (Core.Maybe Core.Natural)
listResourceComplianceSummaries_maxResults = Lens.lens (\ListResourceComplianceSummaries' {maxResults} -> maxResults) (\s@ListResourceComplianceSummaries' {} a -> s {maxResults = a} :: ListResourceComplianceSummaries)

-- | One or more filters. Use a filter to return a more specific list of
-- results.
listResourceComplianceSummaries_filters :: Lens.Lens' ListResourceComplianceSummaries (Core.Maybe [ComplianceStringFilter])
listResourceComplianceSummaries_filters = Lens.lens (\ListResourceComplianceSummaries' {filters} -> filters) (\s@ListResourceComplianceSummaries' {} a -> s {filters = a} :: ListResourceComplianceSummaries) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    ListResourceComplianceSummaries
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResourceComplianceSummariesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listResourceComplianceSummariesResponse_resourceComplianceSummaryItems
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listResourceComplianceSummaries_nextToken
          Lens..~ rs
          Lens.^? listResourceComplianceSummariesResponse_nextToken
            Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ResourceComplianceSummaryItems"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListResourceComplianceSummaries

instance Core.NFData ListResourceComplianceSummaries

instance
  Core.ToHeaders
    ListResourceComplianceSummaries
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.ListResourceComplianceSummaries" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListResourceComplianceSummaries where
  toJSON ListResourceComplianceSummaries' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath ListResourceComplianceSummaries where
  toPath = Core.const "/"

instance Core.ToQuery ListResourceComplianceSummaries where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListResourceComplianceSummariesResponse' smart constructor.
data ListResourceComplianceSummariesResponse = ListResourceComplianceSummariesResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A summary count for specified or targeted managed instances. Summary
    -- count includes information about compliant and non-compliant State
    -- Manager associations, patch status, or custom items according to the
    -- filter criteria that you specify.
    resourceComplianceSummaryItems :: Core.Maybe [ResourceComplianceSummaryItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListResourceComplianceSummariesResponse
newListResourceComplianceSummariesResponse
  pHttpStatus_ =
    ListResourceComplianceSummariesResponse'
      { nextToken =
          Core.Nothing,
        resourceComplianceSummaryItems =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listResourceComplianceSummariesResponse_nextToken :: Lens.Lens' ListResourceComplianceSummariesResponse (Core.Maybe Core.Text)
listResourceComplianceSummariesResponse_nextToken = Lens.lens (\ListResourceComplianceSummariesResponse' {nextToken} -> nextToken) (\s@ListResourceComplianceSummariesResponse' {} a -> s {nextToken = a} :: ListResourceComplianceSummariesResponse)

-- | A summary count for specified or targeted managed instances. Summary
-- count includes information about compliant and non-compliant State
-- Manager associations, patch status, or custom items according to the
-- filter criteria that you specify.
listResourceComplianceSummariesResponse_resourceComplianceSummaryItems :: Lens.Lens' ListResourceComplianceSummariesResponse (Core.Maybe [ResourceComplianceSummaryItem])
listResourceComplianceSummariesResponse_resourceComplianceSummaryItems = Lens.lens (\ListResourceComplianceSummariesResponse' {resourceComplianceSummaryItems} -> resourceComplianceSummaryItems) (\s@ListResourceComplianceSummariesResponse' {} a -> s {resourceComplianceSummaryItems = a} :: ListResourceComplianceSummariesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listResourceComplianceSummariesResponse_httpStatus :: Lens.Lens' ListResourceComplianceSummariesResponse Core.Int
listResourceComplianceSummariesResponse_httpStatus = Lens.lens (\ListResourceComplianceSummariesResponse' {httpStatus} -> httpStatus) (\s@ListResourceComplianceSummariesResponse' {} a -> s {httpStatus = a} :: ListResourceComplianceSummariesResponse)

instance
  Core.NFData
    ListResourceComplianceSummariesResponse
