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
-- Module      : Amazonka.CloudHSMV2.DescribeClusters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about AWS CloudHSM clusters.
--
-- This is a paginated operation, which means that each response might
-- contain only a subset of all the clusters. When the response contains
-- only a subset of clusters, it includes a @NextToken@ value. Use this
-- value in a subsequent @DescribeClusters@ request to get more clusters.
-- When you receive a response with no @NextToken@ (or an empty or null
-- value), that means there are no more clusters to get.
--
-- This operation returns paginated results.
module Amazonka.CloudHSMV2.DescribeClusters
  ( -- * Creating a Request
    DescribeClusters (..),
    newDescribeClusters,

    -- * Request Lenses
    describeClusters_nextToken,
    describeClusters_filters,
    describeClusters_maxResults,

    -- * Destructuring the Response
    DescribeClustersResponse (..),
    newDescribeClustersResponse,

    -- * Response Lenses
    describeClustersResponse_nextToken,
    describeClustersResponse_clusters,
    describeClustersResponse_httpStatus,
  )
where

import Amazonka.CloudHSMV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | The @NextToken@ value that you received in the previous response. Use
    -- this value to get more clusters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more filters to limit the items returned in the response.
    --
    -- Use the @clusterIds@ filter to return only the specified clusters.
    -- Specify clusters by their cluster identifier (ID).
    --
    -- Use the @vpcIds@ filter to return only the clusters in the specified
    -- virtual private clouds (VPCs). Specify VPCs by their VPC identifier
    -- (ID).
    --
    -- Use the @states@ filter to return only clusters that match the specified
    -- state.
    filters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The maximum number of clusters to return in the response. When there are
    -- more clusters than the number you specify, the response contains a
    -- @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'describeClusters_nextToken' - The @NextToken@ value that you received in the previous response. Use
-- this value to get more clusters.
--
-- 'filters', 'describeClusters_filters' - One or more filters to limit the items returned in the response.
--
-- Use the @clusterIds@ filter to return only the specified clusters.
-- Specify clusters by their cluster identifier (ID).
--
-- Use the @vpcIds@ filter to return only the clusters in the specified
-- virtual private clouds (VPCs). Specify VPCs by their VPC identifier
-- (ID).
--
-- Use the @states@ filter to return only clusters that match the specified
-- state.
--
-- 'maxResults', 'describeClusters_maxResults' - The maximum number of clusters to return in the response. When there are
-- more clusters than the number you specify, the response contains a
-- @NextToken@ value.
newDescribeClusters ::
  DescribeClusters
newDescribeClusters =
  DescribeClusters'
    { nextToken = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The @NextToken@ value that you received in the previous response. Use
-- this value to get more clusters.
describeClusters_nextToken :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Text)
describeClusters_nextToken = Lens.lens (\DescribeClusters' {nextToken} -> nextToken) (\s@DescribeClusters' {} a -> s {nextToken = a} :: DescribeClusters)

-- | One or more filters to limit the items returned in the response.
--
-- Use the @clusterIds@ filter to return only the specified clusters.
-- Specify clusters by their cluster identifier (ID).
--
-- Use the @vpcIds@ filter to return only the clusters in the specified
-- virtual private clouds (VPCs). Specify VPCs by their VPC identifier
-- (ID).
--
-- Use the @states@ filter to return only clusters that match the specified
-- state.
describeClusters_filters :: Lens.Lens' DescribeClusters (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
describeClusters_filters = Lens.lens (\DescribeClusters' {filters} -> filters) (\s@DescribeClusters' {} a -> s {filters = a} :: DescribeClusters) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of clusters to return in the response. When there are
-- more clusters than the number you specify, the response contains a
-- @NextToken@ value.
describeClusters_maxResults :: Lens.Lens' DescribeClusters (Prelude.Maybe Prelude.Natural)
describeClusters_maxResults = Lens.lens (\DescribeClusters' {maxResults} -> maxResults) (\s@DescribeClusters' {} a -> s {maxResults = a} :: DescribeClusters)

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
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeClusters where
  rnf DescribeClusters' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeClusters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BaldrApiService.DescribeClusters" ::
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
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeClusters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeClusters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | An opaque string that indicates that the response contains only a subset
    -- of clusters. Use this value in a subsequent @DescribeClusters@ request
    -- to get more clusters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of clusters.
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
-- 'nextToken', 'describeClustersResponse_nextToken' - An opaque string that indicates that the response contains only a subset
-- of clusters. Use this value in a subsequent @DescribeClusters@ request
-- to get more clusters.
--
-- 'clusters', 'describeClustersResponse_clusters' - A list of clusters.
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

-- | An opaque string that indicates that the response contains only a subset
-- of clusters. Use this value in a subsequent @DescribeClusters@ request
-- to get more clusters.
describeClustersResponse_nextToken :: Lens.Lens' DescribeClustersResponse (Prelude.Maybe Prelude.Text)
describeClustersResponse_nextToken = Lens.lens (\DescribeClustersResponse' {nextToken} -> nextToken) (\s@DescribeClustersResponse' {} a -> s {nextToken = a} :: DescribeClustersResponse)

-- | A list of clusters.
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
