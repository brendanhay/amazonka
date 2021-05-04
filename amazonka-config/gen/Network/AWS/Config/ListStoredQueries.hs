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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListStoredQueries' smart constructor.
data ListStoredQueries = ListStoredQueries'
  { -- | The nextToken string returned in a previous request that you use to
    -- request the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned with a single call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The nextToken string returned in a previous request that you use to
-- request the next page of results in a paginated response.
listStoredQueries_nextToken :: Lens.Lens' ListStoredQueries (Prelude.Maybe Prelude.Text)
listStoredQueries_nextToken = Lens.lens (\ListStoredQueries' {nextToken} -> nextToken) (\s@ListStoredQueries' {} a -> s {nextToken = a} :: ListStoredQueries)

-- | The maximum number of results to be returned with a single call.
listStoredQueries_maxResults :: Lens.Lens' ListStoredQueries (Prelude.Maybe Prelude.Natural)
listStoredQueries_maxResults = Lens.lens (\ListStoredQueries' {maxResults} -> maxResults) (\s@ListStoredQueries' {} a -> s {maxResults = a} :: ListStoredQueries)

instance Prelude.AWSRequest ListStoredQueries where
  type Rs ListStoredQueries = ListStoredQueriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStoredQueriesResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "StoredQueryMetadata"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStoredQueries

instance Prelude.NFData ListStoredQueries

instance Prelude.ToHeaders ListStoredQueries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.ListStoredQueries" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListStoredQueries where
  toJSON ListStoredQueries' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListStoredQueries where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListStoredQueries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStoredQueriesResponse' smart constructor.
data ListStoredQueriesResponse = ListStoredQueriesResponse'
  { -- | If the previous paginated request didn\'t return all of the remaining
    -- results, the response object\'s @NextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call this action again and
    -- assign that token to the request object\'s @NextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- @NextToken@ parameter is set to @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @StoredQueryMetadata@ objects.
    storedQueryMetadata :: Prelude.Maybe [StoredQueryMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListStoredQueriesResponse
newListStoredQueriesResponse pHttpStatus_ =
  ListStoredQueriesResponse'
    { nextToken =
        Prelude.Nothing,
      storedQueryMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request didn\'t return all of the remaining
-- results, the response object\'s @NextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call this action again and
-- assign that token to the request object\'s @NextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- @NextToken@ parameter is set to @null@.
listStoredQueriesResponse_nextToken :: Lens.Lens' ListStoredQueriesResponse (Prelude.Maybe Prelude.Text)
listStoredQueriesResponse_nextToken = Lens.lens (\ListStoredQueriesResponse' {nextToken} -> nextToken) (\s@ListStoredQueriesResponse' {} a -> s {nextToken = a} :: ListStoredQueriesResponse)

-- | A list of @StoredQueryMetadata@ objects.
listStoredQueriesResponse_storedQueryMetadata :: Lens.Lens' ListStoredQueriesResponse (Prelude.Maybe [StoredQueryMetadata])
listStoredQueriesResponse_storedQueryMetadata = Lens.lens (\ListStoredQueriesResponse' {storedQueryMetadata} -> storedQueryMetadata) (\s@ListStoredQueriesResponse' {} a -> s {storedQueryMetadata = a} :: ListStoredQueriesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listStoredQueriesResponse_httpStatus :: Lens.Lens' ListStoredQueriesResponse Prelude.Int
listStoredQueriesResponse_httpStatus = Lens.lens (\ListStoredQueriesResponse' {httpStatus} -> httpStatus) (\s@ListStoredQueriesResponse' {} a -> s {httpStatus = a} :: ListStoredQueriesResponse)

instance Prelude.NFData ListStoredQueriesResponse
