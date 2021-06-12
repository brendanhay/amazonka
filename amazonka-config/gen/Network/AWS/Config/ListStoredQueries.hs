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
-- Module      : Network.AWS.Config.ListStoredQueries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the stored queries for a single AWS account and a single AWS
-- Region. The default is 100.
module Network.AWS.Config.ListStoredQueries
  ( -- * Creating a Request
    ListStoredQueries (..),
    newListStoredQueries,

    -- * Request Lenses
    listStoredQueries_nextToken,
    listStoredQueries_maxResults,

    -- * Destructuring the Response
    ListStoredQueriesResponse (..),
    newListStoredQueriesResponse,

    -- * Response Lenses
    listStoredQueriesResponse_nextToken,
    listStoredQueriesResponse_storedQueryMetadata,
    listStoredQueriesResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStoredQueries' smart constructor.
data ListStoredQueries = ListStoredQueries'
  { -- | The nextToken string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to be returned with a single call.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStoredQueries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStoredQueries_nextToken' - The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
--
-- 'maxResults', 'listStoredQueries_maxResults' - The maximum number of results to be returned with a single call.
newListStoredQueries ::
  ListStoredQueries
newListStoredQueries =
  ListStoredQueries'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
listStoredQueries_nextToken :: Lens.Lens' ListStoredQueries (Core.Maybe Core.Text)
listStoredQueries_nextToken = Lens.lens (\ListStoredQueries' {nextToken} -> nextToken) (\s@ListStoredQueries' {} a -> s {nextToken = a} :: ListStoredQueries)

-- | The maximum number of results to be returned with a single call.
listStoredQueries_maxResults :: Lens.Lens' ListStoredQueries (Core.Maybe Core.Natural)
listStoredQueries_maxResults = Lens.lens (\ListStoredQueries' {maxResults} -> maxResults) (\s@ListStoredQueries' {} a -> s {maxResults = a} :: ListStoredQueries)

instance Core.AWSRequest ListStoredQueries where
  type
    AWSResponse ListStoredQueries =
      ListStoredQueriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStoredQueriesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "StoredQueryMetadata"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListStoredQueries

instance Core.NFData ListStoredQueries

instance Core.ToHeaders ListStoredQueries where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.ListStoredQueries" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListStoredQueries where
  toJSON ListStoredQueries' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListStoredQueries where
  toPath = Core.const "/"

instance Core.ToQuery ListStoredQueries where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListStoredQueriesResponse' smart constructor.
data ListStoredQueriesResponse = ListStoredQueriesResponse'
  { -- | If the previous paginated request didn\'t return all of the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of @StoredQueryMetadata@ objects.
    storedQueryMetadata :: Core.Maybe [StoredQueryMetadata],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListStoredQueriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStoredQueriesResponse_nextToken' - If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
--
-- 'storedQueryMetadata', 'listStoredQueriesResponse_storedQueryMetadata' - A list of @StoredQueryMetadata@ objects.
--
-- 'httpStatus', 'listStoredQueriesResponse_httpStatus' - The response's http status code.
newListStoredQueriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListStoredQueriesResponse
newListStoredQueriesResponse pHttpStatus_ =
  ListStoredQueriesResponse'
    { nextToken =
        Core.Nothing,
      storedQueryMetadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listStoredQueriesResponse_nextToken :: Lens.Lens' ListStoredQueriesResponse (Core.Maybe Core.Text)
listStoredQueriesResponse_nextToken = Lens.lens (\ListStoredQueriesResponse' {nextToken} -> nextToken) (\s@ListStoredQueriesResponse' {} a -> s {nextToken = a} :: ListStoredQueriesResponse)

-- | A list of @StoredQueryMetadata@ objects.
listStoredQueriesResponse_storedQueryMetadata :: Lens.Lens' ListStoredQueriesResponse (Core.Maybe [StoredQueryMetadata])
listStoredQueriesResponse_storedQueryMetadata = Lens.lens (\ListStoredQueriesResponse' {storedQueryMetadata} -> storedQueryMetadata) (\s@ListStoredQueriesResponse' {} a -> s {storedQueryMetadata = a} :: ListStoredQueriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listStoredQueriesResponse_httpStatus :: Lens.Lens' ListStoredQueriesResponse Core.Int
listStoredQueriesResponse_httpStatus = Lens.lens (\ListStoredQueriesResponse' {httpStatus} -> httpStatus) (\s@ListStoredQueriesResponse' {} a -> s {httpStatus = a} :: ListStoredQueriesResponse)

instance Core.NFData ListStoredQueriesResponse
