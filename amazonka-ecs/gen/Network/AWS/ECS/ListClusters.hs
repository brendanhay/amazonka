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
-- Module      : Network.AWS.ECS.ListClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing clusters.
--
-- This operation returns paginated results.
module Network.AWS.ECS.ListClusters
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
    listClustersResponse_clusterArns,
    listClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | The @nextToken@ value returned from a @ListClusters@ request indicating
    -- that more results are available to fulfill the request and further calls
    -- will be needed. If @maxResults@ was provided, it is possible the number
    -- of results to be fewer than @maxResults@.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of cluster results returned by @ListClusters@ in
    -- paginated output. When this parameter is used, @ListClusters@ only
    -- returns @maxResults@ results in a single page along with a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @ListClusters@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter is not used, then @ListClusters@ returns up to 100 results and
    -- a @nextToken@ value if applicable.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listClusters_nextToken' - The @nextToken@ value returned from a @ListClusters@ request indicating
-- that more results are available to fulfill the request and further calls
-- will be needed. If @maxResults@ was provided, it is possible the number
-- of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'listClusters_maxResults' - The maximum number of cluster results returned by @ListClusters@ in
-- paginated output. When this parameter is used, @ListClusters@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListClusters@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListClusters@ returns up to 100 results and
-- a @nextToken@ value if applicable.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The @nextToken@ value returned from a @ListClusters@ request indicating
-- that more results are available to fulfill the request and further calls
-- will be needed. If @maxResults@ was provided, it is possible the number
-- of results to be fewer than @maxResults@.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listClusters_nextToken :: Lens.Lens' ListClusters (Core.Maybe Core.Text)
listClusters_nextToken = Lens.lens (\ListClusters' {nextToken} -> nextToken) (\s@ListClusters' {} a -> s {nextToken = a} :: ListClusters)

-- | The maximum number of cluster results returned by @ListClusters@ in
-- paginated output. When this parameter is used, @ListClusters@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @ListClusters@ request with the returned
-- @nextToken@ value. This value can be between 1 and 100. If this
-- parameter is not used, then @ListClusters@ returns up to 100 results and
-- a @nextToken@ value if applicable.
listClusters_maxResults :: Lens.Lens' ListClusters (Core.Maybe Core.Int)
listClusters_maxResults = Lens.lens (\ListClusters' {maxResults} -> maxResults) (\s@ListClusters' {} a -> s {maxResults = a} :: ListClusters)

instance Core.AWSPager ListClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_clusterArns Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listClusters_nextToken
          Lens..~ rs
          Lens.^? listClustersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListClusters where
  type AWSResponse ListClusters = ListClustersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "clusterArns" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListClusters

instance Core.NFData ListClusters

instance Core.ToHeaders ListClusters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.ListClusters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListClusters where
  toJSON ListClusters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListClusters where
  toPath = Core.const "/"

instance Core.ToQuery ListClusters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | The @nextToken@ value to include in a future @ListClusters@ request.
    -- When the results of a @ListClusters@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of full Amazon Resource Name (ARN) entries for each cluster
    -- associated with your account.
    clusterArns :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listClustersResponse_nextToken' - The @nextToken@ value to include in a future @ListClusters@ request.
-- When the results of a @ListClusters@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'clusterArns', 'listClustersResponse_clusterArns' - The list of full Amazon Resource Name (ARN) entries for each cluster
-- associated with your account.
--
-- 'httpStatus', 'listClustersResponse_httpStatus' - The response's http status code.
newListClustersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { nextToken = Core.Nothing,
      clusterArns = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListClusters@ request.
-- When the results of a @ListClusters@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listClustersResponse_nextToken :: Lens.Lens' ListClustersResponse (Core.Maybe Core.Text)
listClustersResponse_nextToken = Lens.lens (\ListClustersResponse' {nextToken} -> nextToken) (\s@ListClustersResponse' {} a -> s {nextToken = a} :: ListClustersResponse)

-- | The list of full Amazon Resource Name (ARN) entries for each cluster
-- associated with your account.
listClustersResponse_clusterArns :: Lens.Lens' ListClustersResponse (Core.Maybe [Core.Text])
listClustersResponse_clusterArns = Lens.lens (\ListClustersResponse' {clusterArns} -> clusterArns) (\s@ListClustersResponse' {} a -> s {clusterArns = a} :: ListClustersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Core.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Core.NFData ListClustersResponse
