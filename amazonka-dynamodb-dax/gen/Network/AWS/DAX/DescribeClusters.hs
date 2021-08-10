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
-- Module      : Network.AWS.DAX.DescribeClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned DAX clusters if no cluster
-- identifier is specified, or about a specific DAX cluster if a cluster
-- identifier is supplied.
--
-- If the cluster is in the CREATING state, only cluster level information
-- will be displayed until all of the nodes are successfully provisioned.
--
-- If the cluster is in the DELETING state, only cluster level information
-- will be displayed.
--
-- If nodes are currently being added to the DAX cluster, node endpoint
-- information and creation time for the additional nodes will not be
-- displayed until they are completely provisioned. When the DAX cluster
-- state is /available/, the cluster is ready for use.
--
-- If nodes are currently being removed from the DAX cluster, no endpoint
-- information for the removed nodes is displayed.
--
-- This operation returns paginated results.
module Network.AWS.DAX.DescribeClusters
  ( -- * Creating a Request
    DescribeClusters (..),
    newDescribeClusters,

    -- * Request Lenses
    describeClusters_nextToken,
    describeClusters_maxResults,
    describeClusters_clusterNames,

    -- * Destructuring the Response
    DescribeClustersResponse (..),
    newDescribeClustersResponse,

    -- * Response Lenses
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DAX.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    --
    -- The value for @MaxResults@ must be between 20 and 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The names of the DAX clusters being described.
    clusterNames :: Prelude.Maybe [Prelude.Text]
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
-- 'nextToken', 'describeClusters_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'maxResults', 'describeClusters_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
--
-- 'clusterNames', 'describeClusters_clusterNames' - The names of the DAX clusters being described.
newDescribeClusters ::
  DescribeClusters
newDescribeClusters =
  DescribeClusters'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      clusterNames = Prelude.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
describeClusters_nextToken :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_nextToken = Lens.lens (\DescribeClusters' {nextToken} -> nextToken) (\s@DescribeClusters' {} a -> s {nextToken = a} :: DescribeClusters)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- The value for @MaxResults@ must be between 20 and 100.
describeClusters_maxResults :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Int)
describeClusters_maxResults = Lens.lens (\DescribeClusters' {maxResults} -> maxResults) (\s@DescribeClusters' {} a -> s {maxResults = a} :: DescribeClusters)

-- | The names of the DAX clusters being described.
describeClusters_clusterNames :: Lens.Lens' DescribeClusters (Prelude.Maybe [Prelude.Text])
describeClusters_clusterNames = Lens.lens (\DescribeClusters' {clusterNames} -> clusterNames) (\s@DescribeClusters' {} a -> s {clusterNames = a} :: DescribeClusters) Prelude.. Lens.mapping Lens._Coerce

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
      Prelude.Just Prelude.$
        rq
          Prelude.& describeClusters_nextToken
          Lens..~ rs
          Lens.^? describeClustersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeClusters where
  type
    AWSResponse DescribeClusters =
      DescribeClustersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Clusters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClusters

instance Prelude.NFData DescribeClusters

instance Core.ToHeaders DescribeClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDAXV3.DescribeClusters" ::
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
            ("ClusterNames" Core..=) Prelude.<$> clusterNames
          ]
      )

instance Core.ToPath DescribeClusters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The descriptions of your DAX clusters, in response to a
    -- /DescribeClusters/ request.
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
-- 'nextToken', 'describeClustersResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'clusters', 'describeClustersResponse_clusters' - The descriptions of your DAX clusters, in response to a
-- /DescribeClusters/ request.
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

-- | Provides an identifier to allow retrieval of paginated results.
describeClustersResponse_nextToken :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe Prelude.Text)
describeClustersResponse_nextToken = Lens.lens (\DescribeClustersResponse' {nextToken} -> nextToken) (\s@DescribeClustersResponse' {} a -> s {nextToken = a} :: DescribeClustersResponse)

-- | The descriptions of your DAX clusters, in response to a
-- /DescribeClusters/ request.
describeClustersResponse_clusters :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe [Cluster])
describeClustersResponse_clusters = Lens.lens (\DescribeClustersResponse' {clusters} -> clusters) (\s@DescribeClustersResponse' {} a -> s {clusters = a} :: DescribeClustersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClustersResponse_httpStatus :: Lens.Lens' DescribeClustersResponse Prelude.Int
describeClustersResponse_httpStatus = Lens.lens (\DescribeClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeClustersResponse' {} a -> s {httpStatus = a} :: DescribeClustersResponse)

instance Prelude.NFData DescribeClustersResponse
