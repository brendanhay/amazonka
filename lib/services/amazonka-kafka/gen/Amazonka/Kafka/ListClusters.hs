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
-- Module      : Amazonka.Kafka.ListClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the MSK clusters in the current Region.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListClusters
  ( -- * Creating a Request
    ListClusters (..),
    newListClusters,

    -- * Request Lenses
    listClusters_clusterNameFilter,
    listClusters_maxResults,
    listClusters_nextToken,

    -- * Destructuring the Response
    ListClustersResponse (..),
    newListClustersResponse,

    -- * Response Lenses
    listClustersResponse_clusterInfoList,
    listClustersResponse_nextToken,
    listClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | Specify a prefix of the name of the clusters that you want to list. The
    -- service lists all the clusters whose names start with this prefix.
    clusterNameFilter :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response. If there are
    -- more results, the response includes a NextToken parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The paginated results marker. When the result of the operation is
    -- truncated, the call returns NextToken in the response. To get the next
    -- batch, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterNameFilter', 'listClusters_clusterNameFilter' - Specify a prefix of the name of the clusters that you want to list. The
-- service lists all the clusters whose names start with this prefix.
--
-- 'maxResults', 'listClusters_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
--
-- 'nextToken', 'listClusters_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
newListClusters ::
  ListClusters
newListClusters =
  ListClusters'
    { clusterNameFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specify a prefix of the name of the clusters that you want to list. The
-- service lists all the clusters whose names start with this prefix.
listClusters_clusterNameFilter :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Text)
listClusters_clusterNameFilter = Lens.lens (\ListClusters' {clusterNameFilter} -> clusterNameFilter) (\s@ListClusters' {} a -> s {clusterNameFilter = a} :: ListClusters)

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listClusters_maxResults :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Natural)
listClusters_maxResults = Lens.lens (\ListClusters' {maxResults} -> maxResults) (\s@ListClusters' {} a -> s {maxResults = a} :: ListClusters)

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listClusters_nextToken :: Lens.Lens' ListClusters (Prelude.Maybe Prelude.Text)
listClusters_nextToken = Lens.lens (\ListClusters' {nextToken} -> nextToken) (\s@ListClusters' {} a -> s {nextToken = a} :: ListClusters)

instance Core.AWSPager ListClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClustersResponse_clusterInfoList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listClusters_nextToken
              Lens..~ rs
              Lens.^? listClustersResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListClusters where
  type AWSResponse ListClusters = ListClustersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Prelude.<$> ( x
                            Data..?> "clusterInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClusters where
  hashWithSalt _salt ListClusters' {..} =
    _salt
      `Prelude.hashWithSalt` clusterNameFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListClusters where
  rnf ListClusters' {..} =
    Prelude.rnf clusterNameFilter `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListClusters where
  toPath = Prelude.const "/v1/clusters"

instance Data.ToQuery ListClusters where
  toQuery ListClusters' {..} =
    Prelude.mconcat
      [ "clusterNameFilter" Data.=: clusterNameFilter,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | Information on each of the MSK clusters in the response.
    clusterInfoList :: Prelude.Maybe [ClusterInfo],
    -- | The paginated results marker. When the result of a ListClusters
    -- operation is truncated, the call returns NextToken in the response. To
    -- get another batch of clusters, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterInfoList', 'listClustersResponse_clusterInfoList' - Information on each of the MSK clusters in the response.
--
-- 'nextToken', 'listClustersResponse_nextToken' - The paginated results marker. When the result of a ListClusters
-- operation is truncated, the call returns NextToken in the response. To
-- get another batch of clusters, provide this token in your next request.
--
-- 'httpStatus', 'listClustersResponse_httpStatus' - The response's http status code.
newListClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClustersResponse
newListClustersResponse pHttpStatus_ =
  ListClustersResponse'
    { clusterInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on each of the MSK clusters in the response.
listClustersResponse_clusterInfoList :: Lens.Lens' ListClustersResponse (Prelude.Maybe [ClusterInfo])
listClustersResponse_clusterInfoList = Lens.lens (\ListClustersResponse' {clusterInfoList} -> clusterInfoList) (\s@ListClustersResponse' {} a -> s {clusterInfoList = a} :: ListClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The paginated results marker. When the result of a ListClusters
-- operation is truncated, the call returns NextToken in the response. To
-- get another batch of clusters, provide this token in your next request.
listClustersResponse_nextToken :: Lens.Lens' ListClustersResponse (Prelude.Maybe Prelude.Text)
listClustersResponse_nextToken = Lens.lens (\ListClustersResponse' {nextToken} -> nextToken) (\s@ListClustersResponse' {} a -> s {nextToken = a} :: ListClustersResponse)

-- | The response's http status code.
listClustersResponse_httpStatus :: Lens.Lens' ListClustersResponse Prelude.Int
listClustersResponse_httpStatus = Lens.lens (\ListClustersResponse' {httpStatus} -> httpStatus) (\s@ListClustersResponse' {} a -> s {httpStatus = a} :: ListClustersResponse)

instance Prelude.NFData ListClustersResponse where
  rnf ListClustersResponse' {..} =
    Prelude.rnf clusterInfoList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
