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
-- Module      : Amazonka.ElastiCache.DescribeCacheClusters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all provisioned clusters if no cluster
-- identifier is specified, or about a specific cache cluster if a cluster
-- identifier is supplied.
--
-- By default, abbreviated information about the clusters is returned. You
-- can use the optional /ShowCacheNodeInfo/ flag to retrieve detailed
-- information about the cache nodes associated with the clusters. These
-- details include the DNS address and port for the cache node endpoint.
--
-- If the cluster is in the /creating/ state, only cluster-level
-- information is displayed until all of the nodes are successfully
-- provisioned.
--
-- If the cluster is in the /deleting/ state, only cluster-level
-- information is displayed.
--
-- If cache nodes are currently being added to the cluster, node endpoint
-- information and creation time for the additional nodes are not displayed
-- until they are completely provisioned. When the cluster state is
-- /available/, the cluster is ready for use.
--
-- If cache nodes are currently being removed from the cluster, no endpoint
-- information for the removed nodes is displayed.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeCacheClusters
  ( -- * Creating a Request
    DescribeCacheClusters (..),
    newDescribeCacheClusters,

    -- * Request Lenses
    describeCacheClusters_cacheClusterId,
    describeCacheClusters_marker,
    describeCacheClusters_maxRecords,
    describeCacheClusters_showCacheClustersNotInReplicationGroups,
    describeCacheClusters_showCacheNodeInfo,

    -- * Destructuring the Response
    DescribeCacheClustersResponse (..),
    newDescribeCacheClustersResponse,

    -- * Response Lenses
    describeCacheClustersResponse_cacheClusters,
    describeCacheClustersResponse_marker,
    describeCacheClustersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeCacheClusters@ operation.
--
-- /See:/ 'newDescribeCacheClusters' smart constructor.
data DescribeCacheClusters = DescribeCacheClusters'
  { -- | The user-supplied cluster identifier. If this parameter is specified,
    -- only information about that specific cluster is returned. This parameter
    -- isn\'t case sensitive.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional flag that can be included in the @DescribeCacheCluster@
    -- request to show only nodes (API\/CLI: clusters) that are not members of
    -- a replication group. In practice, this mean Memcached and single node
    -- Redis clusters.
    showCacheClustersNotInReplicationGroups :: Prelude.Maybe Prelude.Bool,
    -- | An optional flag that can be included in the @DescribeCacheCluster@
    -- request to retrieve information about the individual cache nodes.
    showCacheNodeInfo :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusterId', 'describeCacheClusters_cacheClusterId' - The user-supplied cluster identifier. If this parameter is specified,
-- only information about that specific cluster is returned. This parameter
-- isn\'t case sensitive.
--
-- 'marker', 'describeCacheClusters_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheClusters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
--
-- 'showCacheClustersNotInReplicationGroups', 'describeCacheClusters_showCacheClustersNotInReplicationGroups' - An optional flag that can be included in the @DescribeCacheCluster@
-- request to show only nodes (API\/CLI: clusters) that are not members of
-- a replication group. In practice, this mean Memcached and single node
-- Redis clusters.
--
-- 'showCacheNodeInfo', 'describeCacheClusters_showCacheNodeInfo' - An optional flag that can be included in the @DescribeCacheCluster@
-- request to retrieve information about the individual cache nodes.
newDescribeCacheClusters ::
  DescribeCacheClusters
newDescribeCacheClusters =
  DescribeCacheClusters'
    { cacheClusterId =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      showCacheClustersNotInReplicationGroups =
        Prelude.Nothing,
      showCacheNodeInfo = Prelude.Nothing
    }

-- | The user-supplied cluster identifier. If this parameter is specified,
-- only information about that specific cluster is returned. This parameter
-- isn\'t case sensitive.
describeCacheClusters_cacheClusterId :: Lens.Lens' DescribeCacheClusters (Prelude.Maybe Prelude.Text)
describeCacheClusters_cacheClusterId = Lens.lens (\DescribeCacheClusters' {cacheClusterId} -> cacheClusterId) (\s@DescribeCacheClusters' {} a -> s {cacheClusterId = a} :: DescribeCacheClusters)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheClusters_marker :: Lens.Lens' DescribeCacheClusters (Prelude.Maybe Prelude.Text)
describeCacheClusters_marker = Lens.lens (\DescribeCacheClusters' {marker} -> marker) (\s@DescribeCacheClusters' {} a -> s {marker = a} :: DescribeCacheClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheClusters_maxRecords :: Lens.Lens' DescribeCacheClusters (Prelude.Maybe Prelude.Int)
describeCacheClusters_maxRecords = Lens.lens (\DescribeCacheClusters' {maxRecords} -> maxRecords) (\s@DescribeCacheClusters' {} a -> s {maxRecords = a} :: DescribeCacheClusters)

-- | An optional flag that can be included in the @DescribeCacheCluster@
-- request to show only nodes (API\/CLI: clusters) that are not members of
-- a replication group. In practice, this mean Memcached and single node
-- Redis clusters.
describeCacheClusters_showCacheClustersNotInReplicationGroups :: Lens.Lens' DescribeCacheClusters (Prelude.Maybe Prelude.Bool)
describeCacheClusters_showCacheClustersNotInReplicationGroups = Lens.lens (\DescribeCacheClusters' {showCacheClustersNotInReplicationGroups} -> showCacheClustersNotInReplicationGroups) (\s@DescribeCacheClusters' {} a -> s {showCacheClustersNotInReplicationGroups = a} :: DescribeCacheClusters)

-- | An optional flag that can be included in the @DescribeCacheCluster@
-- request to retrieve information about the individual cache nodes.
describeCacheClusters_showCacheNodeInfo :: Lens.Lens' DescribeCacheClusters (Prelude.Maybe Prelude.Bool)
describeCacheClusters_showCacheNodeInfo = Lens.lens (\DescribeCacheClusters' {showCacheNodeInfo} -> showCacheNodeInfo) (\s@DescribeCacheClusters' {} a -> s {showCacheNodeInfo = a} :: DescribeCacheClusters)

instance Core.AWSPager DescribeCacheClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheClustersResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheClustersResponse_cacheClusters
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeCacheClusters_marker
          Lens..~ rs
          Lens.^? describeCacheClustersResponse_marker
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeCacheClusters where
  type
    AWSResponse DescribeCacheClusters =
      DescribeCacheClustersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCacheClustersResult"
      ( \s h x ->
          DescribeCacheClustersResponse'
            Prelude.<$> ( x
                            Data..@? "CacheClusters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "CacheCluster")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCacheClusters where
  hashWithSalt _salt DescribeCacheClusters' {..} =
    _salt
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` showCacheClustersNotInReplicationGroups
      `Prelude.hashWithSalt` showCacheNodeInfo

instance Prelude.NFData DescribeCacheClusters where
  rnf DescribeCacheClusters' {..} =
    Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf showCacheClustersNotInReplicationGroups
      `Prelude.seq` Prelude.rnf showCacheNodeInfo

instance Data.ToHeaders DescribeCacheClusters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCacheClusters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCacheClusters where
  toQuery DescribeCacheClusters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeCacheClusters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheClusterId" Data.=: cacheClusterId,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ShowCacheClustersNotInReplicationGroups"
          Data.=: showCacheClustersNotInReplicationGroups,
        "ShowCacheNodeInfo" Data.=: showCacheNodeInfo
      ]

-- | Represents the output of a @DescribeCacheClusters@ operation.
--
-- /See:/ 'newDescribeCacheClustersResponse' smart constructor.
data DescribeCacheClustersResponse = DescribeCacheClustersResponse'
  { -- | A list of clusters. Each item in the list contains detailed information
    -- about one cluster.
    cacheClusters :: Prelude.Maybe [CacheCluster],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheClusters', 'describeCacheClustersResponse_cacheClusters' - A list of clusters. Each item in the list contains detailed information
-- about one cluster.
--
-- 'marker', 'describeCacheClustersResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeCacheClustersResponse_httpStatus' - The response's http status code.
newDescribeCacheClustersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCacheClustersResponse
newDescribeCacheClustersResponse pHttpStatus_ =
  DescribeCacheClustersResponse'
    { cacheClusters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of clusters. Each item in the list contains detailed information
-- about one cluster.
describeCacheClustersResponse_cacheClusters :: Lens.Lens' DescribeCacheClustersResponse (Prelude.Maybe [CacheCluster])
describeCacheClustersResponse_cacheClusters = Lens.lens (\DescribeCacheClustersResponse' {cacheClusters} -> cacheClusters) (\s@DescribeCacheClustersResponse' {} a -> s {cacheClusters = a} :: DescribeCacheClustersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheClustersResponse_marker :: Lens.Lens' DescribeCacheClustersResponse (Prelude.Maybe Prelude.Text)
describeCacheClustersResponse_marker = Lens.lens (\DescribeCacheClustersResponse' {marker} -> marker) (\s@DescribeCacheClustersResponse' {} a -> s {marker = a} :: DescribeCacheClustersResponse)

-- | The response's http status code.
describeCacheClustersResponse_httpStatus :: Lens.Lens' DescribeCacheClustersResponse Prelude.Int
describeCacheClustersResponse_httpStatus = Lens.lens (\DescribeCacheClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheClustersResponse' {} a -> s {httpStatus = a} :: DescribeCacheClustersResponse)

instance Prelude.NFData DescribeCacheClustersResponse where
  rnf DescribeCacheClustersResponse' {..} =
    Prelude.rnf cacheClusters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
