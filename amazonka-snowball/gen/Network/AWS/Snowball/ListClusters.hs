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
-- Module      : Network.AWS.Snowball.ListClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterListEntry@ objects of the specified length.
-- Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
-- ID, and other important status information.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListClusters
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
    listClustersResponse_clusterListEntries,
    listClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of @ClusterListEntry@ objects, you have the option of
    -- specifying @NextToken@ as the starting point for your returned list.
    nextToken :: Core.Maybe Core.Text,
    -- | The number of @ClusterListEntry@ objects to return.
    maxResults :: Core.Maybe Core.Natural
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
-- 'nextToken', 'listClusters_nextToken' - HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ClusterListEntry@ objects, you have the option of
-- specifying @NextToken@ as the starting point for your returned list.
--
-- 'maxResults', 'listClusters_maxResults' - The number of @ClusterListEntry@ objects to return.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ClusterListEntry@ objects, you have the option of
-- specifying @NextToken@ as the starting point for your returned list.
listClusters_nextToken :: Lens.Lens' ListClusters (Core.Maybe Core.Text)
listClusters_nextToken = Lens.lens (\ListClusters' {nextToken} -> nextToken) (\s@ListClusters' {} a -> s {nextToken = a} :: ListClusters)

-- | The number of @ClusterListEntry@ objects to return.
listClusters_maxResults :: Lens.Lens' ListClusters (Core.Maybe Core.Natural)
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
            Lens.^? listClustersResponse_clusterListEntries
              Core.. Lens._Just
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ClusterListEntries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListClusters

instance Core.NFData ListClusters

instance Core.ToHeaders ListClusters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIESnowballJobManagementService.ListClusters" ::
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
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListClusters where
  toPath = Core.const "/"

instance Core.ToQuery ListClusters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | HTTP requests are stateless. If you use the automatically generated
    -- @NextToken@ value in your next @ClusterListEntry@ call, your list of
    -- returned clusters will start from this point in the array.
    nextToken :: Core.Maybe Core.Text,
    -- | Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
    -- ID, and other important status information.
    clusterListEntries :: Core.Maybe [ClusterListEntry],
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
-- 'nextToken', 'listClustersResponse_nextToken' - HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @ClusterListEntry@ call, your list of
-- returned clusters will start from this point in the array.
--
-- 'clusterListEntries', 'listClustersResponse_clusterListEntries' - Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
-- ID, and other important status information.
--
-- 'httpStatus', 'listClustersResponse_httpStatus' - The response's http status code.
newListClustersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { nextToken = Core.Nothing,
      clusterListEntries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @ClusterListEntry@ call, your list of
-- returned clusters will start from this point in the array.
listClustersResponse_nextToken :: Lens.Lens' ListClustersResponse (Core.Maybe Core.Text)
listClustersResponse_nextToken = Lens.lens (\ListClustersResponse' {nextToken} -> nextToken) (\s@ListClustersResponse' {} a -> s {nextToken = a} :: ListClustersResponse)

-- | Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
-- ID, and other important status information.
listClustersResponse_clusterListEntries :: Lens.Lens' ListClustersResponse (Core.Maybe [ClusterListEntry])
listClustersResponse_clusterListEntries = Lens.lens (\ListClustersResponse' {clusterListEntries} -> clusterListEntries) (\s@ListClustersResponse' {} a -> s {clusterListEntries = a} :: ListClustersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Core.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Core.NFData ListClustersResponse
