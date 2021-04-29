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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | HTTP requests are stateless. To identify what object comes \"next\" in
    -- the list of @ClusterListEntry@ objects, you have the option of
    -- specifying @NextToken@ as the starting point for your returned list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of @ClusterListEntry@ objects to return.
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
-- 'nextToken', 'listClusters_nextToken' - HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ClusterListEntry@ objects, you have the option of
-- specifying @NextToken@ as the starting point for your returned list.
--
-- 'maxResults', 'listClusters_maxResults' - The number of @ClusterListEntry@ objects to return.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes \"next\" in
-- the list of @ClusterListEntry@ objects, you have the option of
-- specifying @NextToken@ as the starting point for your returned list.
listClusters_nextToken :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Text)
listClusters_nextToken = Lens.lens (\ListClusters' {nextToken} -> nextToken) (\s@ListClusters' {} a -> s {nextToken = a} :: ListClusters)

-- | The number of @ClusterListEntry@ objects to return.
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
            Lens.^? listClustersResponse_clusterListEntries
              Prelude.. Lens._Just
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ClusterListEntries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusters

instance Prelude.NFData ListClusters

instance Prelude.ToHeaders ListClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.ListClusters" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListClusters where
  toJSON ListClusters' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults
          ]
      )

instance Prelude.ToPath ListClusters where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | HTTP requests are stateless. If you use the automatically generated
    -- @NextToken@ value in your next @ClusterListEntry@ call, your list of
    -- returned clusters will start from this point in the array.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
    -- ID, and other important status information.
    clusterListEntries :: Prelude.Maybe [ClusterListEntry],
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
  Prelude.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { nextToken = Prelude.Nothing,
      clusterListEntries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | HTTP requests are stateless. If you use the automatically generated
-- @NextToken@ value in your next @ClusterListEntry@ call, your list of
-- returned clusters will start from this point in the array.
listClustersResponse_nextToken :: Lens.Lens' ListClustersResponse (Prelude.Maybe Prelude.Text)
listClustersResponse_nextToken = Lens.lens (\ListClustersResponse' {nextToken} -> nextToken) (\s@ListClustersResponse' {} a -> s {nextToken = a} :: ListClustersResponse)

-- | Each @ClusterListEntry@ object contains a cluster\'s state, a cluster\'s
-- ID, and other important status information.
listClustersResponse_clusterListEntries :: Lens.Lens' ListClustersResponse (Prelude.Maybe [ClusterListEntry])
listClustersResponse_clusterListEntries = Lens.lens (\ListClustersResponse' {clusterListEntries} -> clusterListEntries) (\s@ListClustersResponse' {} a -> s {clusterListEntries = a} :: ListClustersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Prelude.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Prelude.NFData ListClustersResponse
