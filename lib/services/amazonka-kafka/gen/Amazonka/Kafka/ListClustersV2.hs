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
-- Module      : Amazonka.Kafka.ListClustersV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the MSK clusters in the current Region.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListClustersV2
  ( -- * Creating a Request
    ListClustersV2 (..),
    newListClustersV2,

    -- * Request Lenses
    listClustersV2_clusterNameFilter,
    listClustersV2_clusterTypeFilter,
    listClustersV2_maxResults,
    listClustersV2_nextToken,

    -- * Destructuring the Response
    ListClustersV2Response (..),
    newListClustersV2Response,

    -- * Response Lenses
    listClustersV2Response_clusterInfoList,
    listClustersV2Response_nextToken,
    listClustersV2Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListClustersV2' smart constructor.
data ListClustersV2 = ListClustersV2'
  { -- | Specify a prefix of the names of the clusters that you want to list. The
    -- service lists all the clusters whose names start with this prefix.
    clusterNameFilter :: Prelude.Maybe Prelude.Text,
    -- | Specify either PROVISIONED or SERVERLESS.
    clusterTypeFilter :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ListClustersV2' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterNameFilter', 'listClustersV2_clusterNameFilter' - Specify a prefix of the names of the clusters that you want to list. The
-- service lists all the clusters whose names start with this prefix.
--
-- 'clusterTypeFilter', 'listClustersV2_clusterTypeFilter' - Specify either PROVISIONED or SERVERLESS.
--
-- 'maxResults', 'listClustersV2_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
--
-- 'nextToken', 'listClustersV2_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
newListClustersV2 ::
  ListClustersV2
newListClustersV2 =
  ListClustersV2'
    { clusterNameFilter =
        Prelude.Nothing,
      clusterTypeFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specify a prefix of the names of the clusters that you want to list. The
-- service lists all the clusters whose names start with this prefix.
listClustersV2_clusterNameFilter :: Lens.Lens' ListClustersV2 (Prelude.Maybe Prelude.Text)
listClustersV2_clusterNameFilter = Lens.lens (\ListClustersV2' {clusterNameFilter} -> clusterNameFilter) (\s@ListClustersV2' {} a -> s {clusterNameFilter = a} :: ListClustersV2)

-- | Specify either PROVISIONED or SERVERLESS.
listClustersV2_clusterTypeFilter :: Lens.Lens' ListClustersV2 (Prelude.Maybe Prelude.Text)
listClustersV2_clusterTypeFilter = Lens.lens (\ListClustersV2' {clusterTypeFilter} -> clusterTypeFilter) (\s@ListClustersV2' {} a -> s {clusterTypeFilter = a} :: ListClustersV2)

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listClustersV2_maxResults :: Lens.Lens' ListClustersV2 (Prelude.Maybe Prelude.Natural)
listClustersV2_maxResults = Lens.lens (\ListClustersV2' {maxResults} -> maxResults) (\s@ListClustersV2' {} a -> s {maxResults = a} :: ListClustersV2)

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listClustersV2_nextToken :: Lens.Lens' ListClustersV2 (Prelude.Maybe Prelude.Text)
listClustersV2_nextToken = Lens.lens (\ListClustersV2' {nextToken} -> nextToken) (\s@ListClustersV2' {} a -> s {nextToken = a} :: ListClustersV2)

instance Core.AWSPager ListClustersV2 where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClustersV2Response_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClustersV2Response_clusterInfoList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listClustersV2_nextToken
              Lens..~ rs
              Lens.^? listClustersV2Response_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListClustersV2 where
  type
    AWSResponse ListClustersV2 =
      ListClustersV2Response
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersV2Response'
            Prelude.<$> ( x
                            Data..?> "clusterInfoList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClustersV2 where
  hashWithSalt _salt ListClustersV2' {..} =
    _salt
      `Prelude.hashWithSalt` clusterNameFilter
      `Prelude.hashWithSalt` clusterTypeFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListClustersV2 where
  rnf ListClustersV2' {..} =
    Prelude.rnf clusterNameFilter `Prelude.seq`
      Prelude.rnf clusterTypeFilter `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken

instance Data.ToHeaders ListClustersV2 where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListClustersV2 where
  toPath = Prelude.const "/api/v2/clusters"

instance Data.ToQuery ListClustersV2 where
  toQuery ListClustersV2' {..} =
    Prelude.mconcat
      [ "clusterNameFilter" Data.=: clusterNameFilter,
        "clusterTypeFilter" Data.=: clusterTypeFilter,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListClustersV2Response' smart constructor.
data ListClustersV2Response = ListClustersV2Response'
  { -- | Information on each of the MSK clusters in the response.
    clusterInfoList :: Prelude.Maybe [Cluster],
    -- | The paginated results marker. When the result of a ListClusters
    -- operation is truncated, the call returns NextToken in the response. To
    -- get another batch of clusters, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClustersV2Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterInfoList', 'listClustersV2Response_clusterInfoList' - Information on each of the MSK clusters in the response.
--
-- 'nextToken', 'listClustersV2Response_nextToken' - The paginated results marker. When the result of a ListClusters
-- operation is truncated, the call returns NextToken in the response. To
-- get another batch of clusters, provide this token in your next request.
--
-- 'httpStatus', 'listClustersV2Response_httpStatus' - The response's http status code.
newListClustersV2Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClustersV2Response
newListClustersV2Response pHttpStatus_ =
  ListClustersV2Response'
    { clusterInfoList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on each of the MSK clusters in the response.
listClustersV2Response_clusterInfoList :: Lens.Lens' ListClustersV2Response (Prelude.Maybe [Cluster])
listClustersV2Response_clusterInfoList = Lens.lens (\ListClustersV2Response' {clusterInfoList} -> clusterInfoList) (\s@ListClustersV2Response' {} a -> s {clusterInfoList = a} :: ListClustersV2Response) Prelude.. Lens.mapping Lens.coerced

-- | The paginated results marker. When the result of a ListClusters
-- operation is truncated, the call returns NextToken in the response. To
-- get another batch of clusters, provide this token in your next request.
listClustersV2Response_nextToken :: Lens.Lens' ListClustersV2Response (Prelude.Maybe Prelude.Text)
listClustersV2Response_nextToken = Lens.lens (\ListClustersV2Response' {nextToken} -> nextToken) (\s@ListClustersV2Response' {} a -> s {nextToken = a} :: ListClustersV2Response)

-- | The response's http status code.
listClustersV2Response_httpStatus :: Lens.Lens' ListClustersV2Response Prelude.Int
listClustersV2Response_httpStatus = Lens.lens (\ListClustersV2Response' {httpStatus} -> httpStatus) (\s@ListClustersV2Response' {} a -> s {httpStatus = a} :: ListClustersV2Response)

instance Prelude.NFData ListClustersV2Response where
  rnf ListClustersV2Response' {..} =
    Prelude.rnf clusterInfoList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
