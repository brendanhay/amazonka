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
-- Module      : Amazonka.MemoryDb.DescribeClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned clusters if no cluster
-- identifier is specified, or about a specific cluster if a cluster name
-- is supplied.
--
-- This operation returns paginated results.
module Amazonka.MemoryDb.DescribeClusters
  ( -- * Creating a Request
    DescribeClusters (..),
    newDescribeClusters,

    -- * Request Lenses
    describeClusters_clusterName,
    describeClusters_maxResults,
    describeClusters_nextToken,
    describeClusters_showShardDetails,

    -- * Destructuring the Response
    DescribeClustersResponse (..),
    newDescribeClustersResponse,

    -- * Response Lenses
    describeClustersResponse_clusters,
    describeClustersResponse_nextToken,
    describeClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | The name of the cluster
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional flag that can be included in the request to retrieve
    -- information about the individual shard(s).
    showShardDetails :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'describeClusters_clusterName' - The name of the cluster
--
-- 'maxResults', 'describeClusters_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'describeClusters_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'showShardDetails', 'describeClusters_showShardDetails' - An optional flag that can be included in the request to retrieve
-- information about the individual shard(s).
newDescribeClusters ::
  DescribeClusters
newDescribeClusters =
  DescribeClusters'
    { clusterName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      showShardDetails = Prelude.Nothing
    }

-- | The name of the cluster
describeClusters_clusterName :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_clusterName = Lens.lens (\DescribeClusters' {clusterName} -> clusterName) (\s@DescribeClusters' {} a -> s {clusterName = a} :: DescribeClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeClusters_maxResults :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Int)
describeClusters_maxResults = Lens.lens (\DescribeClusters' {maxResults} -> maxResults) (\s@DescribeClusters' {} a -> s {maxResults = a} :: DescribeClusters)

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeClusters_nextToken :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_nextToken = Lens.lens (\DescribeClusters' {nextToken} -> nextToken) (\s@DescribeClusters' {} a -> s {nextToken = a} :: DescribeClusters)

-- | An optional flag that can be included in the request to retrieve
-- information about the individual shard(s).
describeClusters_showShardDetails :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Bool)
describeClusters_showShardDetails = Lens.lens (\DescribeClusters' {showShardDetails} -> showShardDetails) (\s@DescribeClusters' {} a -> s {showShardDetails = a} :: DescribeClusters)

instance Core.AWSPager DescribeClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClustersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClustersResponse_clusters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeClusters_nextToken
          Lens..~ rs
          Lens.^? describeClustersResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeClusters where
  type
    AWSResponse DescribeClusters =
      DescribeClustersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Prelude.<$> (x Data..?> "Clusters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusters where
  hashWithSalt _salt DescribeClusters' {..} =
    _salt
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` showShardDetails

instance Prelude.NFData DescribeClusters where
  rnf DescribeClusters' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf showShardDetails

instance Data.ToHeaders DescribeClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonMemoryDB.DescribeClusters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterName" Data..=) Prelude.<$> clusterName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ShowShardDetails" Data..=)
              Prelude.<$> showShardDetails
          ]
      )

instance Data.ToPath DescribeClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | A list of clusters
    clusters :: Prelude.Maybe [Cluster],
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusters', 'describeClustersResponse_clusters' - A list of clusters
--
-- 'nextToken', 'describeClustersResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'httpStatus', 'describeClustersResponse_httpStatus' - The response's http status code.
newDescribeClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClustersResponse
newDescribeClustersResponse pHttpStatus_ =
  DescribeClustersResponse'
    { clusters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of clusters
describeClustersResponse_clusters :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Cluster])
describeClustersResponse_clusters = Lens.lens (\DescribeClustersResponse' {clusters} -> clusters) (\s@DescribeClustersResponse' {} a -> s {clusters = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeClustersResponse_nextToken :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe Prelude.Text)
describeClustersResponse_nextToken = Lens.lens (\DescribeClustersResponse' {nextToken} -> nextToken) (\s@DescribeClustersResponse' {} a -> s {nextToken = a} :: DescribeClustersResponse)

-- | The response's http status code.
describeClustersResponse_httpStatus :: Lens.Lens' DescribeClustersResponse Prelude.Int
describeClustersResponse_httpStatus = Lens.lens (\DescribeClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeClustersResponse' {} a -> s {httpStatus = a} :: DescribeClustersResponse)

instance Prelude.NFData DescribeClustersResponse where
  rnf DescribeClustersResponse' {..} =
    Prelude.rnf clusters
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
