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
-- Module      : Network.AWS.EKS.ListClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon EKS clusters in your AWS account in the specified
-- Region.
--
-- This operation returns paginated results.
module Network.AWS.EKS.ListClusters
  ( -- * Creating a Request
    ListClusters (..),
    newListClusters,

    -- * Request Lenses
    listClusters_nextToken,
    listClusters_maxResults,

    -- * Destructuring the Response
    ListClustersResponse (..),
    newListClustersResponse,

    -- * Response Lenses
    listClustersResponse_nextToken,
    listClustersResponse_clusters,
    listClustersResponse_httpStatus,
  )
where

import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | The @nextToken@ value returned from a previous paginated @ListClusters@
    -- request where @maxResults@ was used and the results exceeded the value
    -- of that parameter. Pagination continues from the end of the previous
    -- results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is used only
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of cluster results returned by @ListClusters@ in
    -- paginated output. When you use this parameter, @ListClusters@ returns
    -- only @maxResults@ results in a single page along with a @nextToken@
    -- response element. You can see the remaining results of the initial
    -- request by sending another @ListClusters@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If you don\'t
    -- use this parameter, @ListClusters@ returns up to 100 results and a
    -- @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listClusters_nextToken' - The @nextToken@ value returned from a previous paginated @ListClusters@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'listClusters_maxResults' - The maximum number of cluster results returned by @ListClusters@ in
-- paginated output. When you use this parameter, @ListClusters@ returns
-- only @maxResults@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListClusters@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If you don\'t
-- use this parameter, @ListClusters@ returns up to 100 results and a
-- @nextToken@ value if applicable.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The @nextToken@ value returned from a previous paginated @ListClusters@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is used only
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listClusters_nextToken :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Text)
listClusters_nextToken = Lens.lens (\ListClusters' {nextToken} -> nextToken) (\s@ListClusters' {} a -> s {nextToken = a} :: ListClusters)

-- | The maximum number of cluster results returned by @ListClusters@ in
-- paginated output. When you use this parameter, @ListClusters@ returns
-- only @maxResults@ results in a single page along with a @nextToken@
-- response element. You can see the remaining results of the initial
-- request by sending another @ListClusters@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If you don\'t
-- use this parameter, @ListClusters@ returns up to 100 results and a
-- @nextToken@ value if applicable.
listClusters_maxResults :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Natural)
listClusters_maxResults = Lens.lens (\ListClusters' {maxResults} -> maxResults) (\s@ListClusters' {} a -> s {maxResults = a} :: ListClusters)

instance Pager.AWSPager ListClusters where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listClustersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listClustersResponse_clusters Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listClusters_nextToken
          Lens..~ rs
          Lens.^? listClustersResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListClusters where
  type Rs ListClusters = ListClustersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "clusters" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusters

instance Prelude.NFData ListClusters

instance Prelude.ToHeaders ListClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListClusters where
  toPath = Prelude.const "/clusters"

instance Prelude.ToQuery ListClusters where
  toQuery ListClusters' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "maxResults" Prelude.=: maxResults
      ]

-- | /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | The @nextToken@ value to include in a future @ListClusters@ request.
    -- When the results of a @ListClusters@ request exceed @maxResults@, you
    -- can use this value to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of all of the clusters for your account in the specified Region.
    clusters :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listClustersResponse_nextToken' - The @nextToken@ value to include in a future @ListClusters@ request.
-- When the results of a @ListClusters@ request exceed @maxResults@, you
-- can use this value to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'clusters', 'listClustersResponse_clusters' - A list of all of the clusters for your account in the specified Region.
--
-- 'httpStatus', 'listClustersResponse_httpStatus' - The response's http status code.
newListClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { nextToken = Prelude.Nothing,
      clusters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListClusters@ request.
-- When the results of a @ListClusters@ request exceed @maxResults@, you
-- can use this value to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listClustersResponse_nextToken :: Lens.Lens' ListClustersResponse (Prelude.Maybe Prelude.Text)
listClustersResponse_nextToken = Lens.lens (\ListClustersResponse' {nextToken} -> nextToken) (\s@ListClustersResponse' {} a -> s {nextToken = a} :: ListClustersResponse)

-- | A list of all of the clusters for your account in the specified Region.
listClustersResponse_clusters :: Lens.Lens' ListClustersResponse (Prelude.Maybe [Prelude.Text])
listClustersResponse_clusters = Lens.lens (\ListClustersResponse' {clusters} -> clusters) (\s@ListClustersResponse' {} a -> s {clusters = a} :: ListClustersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Prelude.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Prelude.NFData ListClustersResponse
