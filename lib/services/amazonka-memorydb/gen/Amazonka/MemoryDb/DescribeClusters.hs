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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned clusters if no cluster
-- identifier is specified, or about a specific cluster if a cluster name
-- is supplied.
module Amazonka.MemoryDb.DescribeClusters
  ( -- * Creating a Request
    DescribeClusters (..),
    newDescribeClusters,

    -- * Request Lenses
    describeClusters_nextToken,
    describeClusters_maxResults,
    describeClusters_showShardDetails,
    describeClusters_clusterName,

    -- * Destructuring the Response
    DescribeClustersResponse (..),
    newDescribeClustersResponse,

    -- * Response Lenses
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | An optional flag that can be included in the request to retrieve
    -- information about the individual shard(s).
    showShardDetails :: Prelude.Maybe Prelude.Bool,
    -- | The name of the cluster
    clusterName :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'describeClusters_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'maxResults', 'describeClusters_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'showShardDetails', 'describeClusters_showShardDetails' - An optional flag that can be included in the request to retrieve
-- information about the individual shard(s).
--
-- 'clusterName', 'describeClusters_clusterName' - The name of the cluster
newDescribeClusters ::
  DescribeClusters
newDescribeClusters =
  DescribeClusters'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      showShardDetails = Prelude.Nothing,
      clusterName = Prelude.Nothing
    }

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeClusters_nextToken :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_nextToken = Lens.lens (\DescribeClusters' {nextToken} -> nextToken) (\s@DescribeClusters' {} a -> s {nextToken = a} :: DescribeClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeClusters_maxResults :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Int)
describeClusters_maxResults = Lens.lens (\DescribeClusters' {maxResults} -> maxResults) (\s@DescribeClusters' {} a -> s {maxResults = a} :: DescribeClusters)

-- | An optional flag that can be included in the request to retrieve
-- information about the individual shard(s).
describeClusters_showShardDetails :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Bool)
describeClusters_showShardDetails = Lens.lens (\DescribeClusters' {showShardDetails} -> showShardDetails) (\s@DescribeClusters' {} a -> s {showShardDetails = a} :: DescribeClusters)

-- | The name of the cluster
describeClusters_clusterName :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_clusterName = Lens.lens (\DescribeClusters' {clusterName} -> clusterName) (\s@DescribeClusters' {} a -> s {clusterName = a} :: DescribeClusters)

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Clusters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusters where
  hashWithSalt _salt DescribeClusters' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` showShardDetails
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData DescribeClusters where
  rnf DescribeClusters' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf showShardDetails
      `Prelude.seq` Prelude.rnf clusterName

instance Core.ToHeaders DescribeClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.DescribeClusters" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ShowShardDetails" Core..=)
              Prelude.<$> showShardDetails,
            ("ClusterName" Core..=) Prelude.<$> clusterName
          ]
      )

instance Core.ToPath DescribeClusters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of clusters
    clusters :: Prelude.Maybe [Cluster],
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
-- 'nextToken', 'describeClustersResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'clusters', 'describeClustersResponse_clusters' - A list of clusters
--
-- 'httpStatus', 'describeClustersResponse_httpStatus' - The response's http status code.
newDescribeClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClustersResponse
newDescribeClustersResponse pHttpStatus_ =
  DescribeClustersResponse'
    { nextToken =
        Prelude.Nothing,
      clusters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeClustersResponse_nextToken :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe Prelude.Text)
describeClustersResponse_nextToken = Lens.lens (\DescribeClustersResponse' {nextToken} -> nextToken) (\s@DescribeClustersResponse' {} a -> s {nextToken = a} :: DescribeClustersResponse)

-- | A list of clusters
describeClustersResponse_clusters :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Cluster])
describeClustersResponse_clusters = Lens.lens (\DescribeClustersResponse' {clusters} -> clusters) (\s@DescribeClustersResponse' {} a -> s {clusters = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeClustersResponse_httpStatus :: Lens.Lens' DescribeClustersResponse Prelude.Int
describeClustersResponse_httpStatus = Lens.lens (\DescribeClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeClustersResponse' {} a -> s {httpStatus = a} :: DescribeClustersResponse)

instance Prelude.NFData DescribeClustersResponse where
  rnf DescribeClustersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusters
      `Prelude.seq` Prelude.rnf httpStatus
