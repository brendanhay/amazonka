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
-- Module      : Network.AWS.RDS.DescribeGlobalClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about Aurora global database clusters. This API
-- supports pagination.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeGlobalClusters
  ( -- * Creating a Request
    DescribeGlobalClusters (..),
    newDescribeGlobalClusters,

    -- * Request Lenses
    describeGlobalClusters_filters,
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,

    -- * Destructuring the Response
    DescribeGlobalClustersResponse (..),
    newDescribeGlobalClustersResponse,

    -- * Response Lenses
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeGlobalClusters' smart constructor.
data DescribeGlobalClusters = DescribeGlobalClusters'
  { -- | A filter that specifies one or more global DB clusters to describe.
    --
    -- Supported filters:
    --
    -- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
    --     Amazon Resource Names (ARNs). The results list will only include
    --     information about the DB clusters identified by these ARNs.
    filters :: Core.Maybe [Filter],
    -- | The user-supplied DB cluster identifier. If this parameter is specified,
    -- information from only the specific DB cluster is returned. This
    -- parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   If supplied, must match an existing DBClusterIdentifier.
    globalClusterIdentifier :: Core.Maybe Core.Text,
    -- | An optional pagination token provided by a previous
    -- @DescribeGlobalClusters@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGlobalClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeGlobalClusters_filters' - A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB clusters identified by these ARNs.
--
-- 'globalClusterIdentifier', 'describeGlobalClusters_globalClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified,
-- information from only the specific DB cluster is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBClusterIdentifier.
--
-- 'marker', 'describeGlobalClusters_marker' - An optional pagination token provided by a previous
-- @DescribeGlobalClusters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeGlobalClusters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeGlobalClusters ::
  DescribeGlobalClusters
newDescribeGlobalClusters =
  DescribeGlobalClusters'
    { filters = Core.Nothing,
      globalClusterIdentifier = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | A filter that specifies one or more global DB clusters to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB clusters identified by these ARNs.
describeGlobalClusters_filters :: Lens.Lens' DescribeGlobalClusters (Core.Maybe [Filter])
describeGlobalClusters_filters = Lens.lens (\DescribeGlobalClusters' {filters} -> filters) (\s@DescribeGlobalClusters' {} a -> s {filters = a} :: DescribeGlobalClusters) Core.. Lens.mapping Lens._Coerce

-- | The user-supplied DB cluster identifier. If this parameter is specified,
-- information from only the specific DB cluster is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBClusterIdentifier.
describeGlobalClusters_globalClusterIdentifier :: Lens.Lens' DescribeGlobalClusters (Core.Maybe Core.Text)
describeGlobalClusters_globalClusterIdentifier = Lens.lens (\DescribeGlobalClusters' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@DescribeGlobalClusters' {} a -> s {globalClusterIdentifier = a} :: DescribeGlobalClusters)

-- | An optional pagination token provided by a previous
-- @DescribeGlobalClusters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeGlobalClusters_marker :: Lens.Lens' DescribeGlobalClusters (Core.Maybe Core.Text)
describeGlobalClusters_marker = Lens.lens (\DescribeGlobalClusters' {marker} -> marker) (\s@DescribeGlobalClusters' {} a -> s {marker = a} :: DescribeGlobalClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeGlobalClusters_maxRecords :: Lens.Lens' DescribeGlobalClusters (Core.Maybe Core.Int)
describeGlobalClusters_maxRecords = Lens.lens (\DescribeGlobalClusters' {maxRecords} -> maxRecords) (\s@DescribeGlobalClusters' {} a -> s {maxRecords = a} :: DescribeGlobalClusters)

instance Core.AWSPager DescribeGlobalClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGlobalClustersResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGlobalClustersResponse_globalClusters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeGlobalClusters_marker
          Lens..~ rs
          Lens.^? describeGlobalClustersResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeGlobalClusters where
  type
    AWSResponse DescribeGlobalClusters =
      DescribeGlobalClustersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalClustersResult"
      ( \s h x ->
          DescribeGlobalClustersResponse'
            Core.<$> ( x Core..@? "GlobalClusters" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "GlobalClusterMember")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeGlobalClusters

instance Core.NFData DescribeGlobalClusters

instance Core.ToHeaders DescribeGlobalClusters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeGlobalClusters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeGlobalClusters where
  toQuery DescribeGlobalClusters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeGlobalClusters" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "GlobalClusterIdentifier"
          Core.=: globalClusterIdentifier,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeGlobalClustersResponse' smart constructor.
data DescribeGlobalClustersResponse = DescribeGlobalClustersResponse'
  { -- | The list of global clusters returned by this request.
    globalClusters :: Core.Maybe [GlobalCluster],
    -- | An optional pagination token provided by a previous
    -- @DescribeGlobalClusters@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGlobalClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalClusters', 'describeGlobalClustersResponse_globalClusters' - The list of global clusters returned by this request.
--
-- 'marker', 'describeGlobalClustersResponse_marker' - An optional pagination token provided by a previous
-- @DescribeGlobalClusters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'httpStatus', 'describeGlobalClustersResponse_httpStatus' - The response's http status code.
newDescribeGlobalClustersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeGlobalClustersResponse
newDescribeGlobalClustersResponse pHttpStatus_ =
  DescribeGlobalClustersResponse'
    { globalClusters =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of global clusters returned by this request.
describeGlobalClustersResponse_globalClusters :: Lens.Lens' DescribeGlobalClustersResponse (Core.Maybe [GlobalCluster])
describeGlobalClustersResponse_globalClusters = Lens.lens (\DescribeGlobalClustersResponse' {globalClusters} -> globalClusters) (\s@DescribeGlobalClustersResponse' {} a -> s {globalClusters = a} :: DescribeGlobalClustersResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeGlobalClusters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeGlobalClustersResponse_marker :: Lens.Lens' DescribeGlobalClustersResponse (Core.Maybe Core.Text)
describeGlobalClustersResponse_marker = Lens.lens (\DescribeGlobalClustersResponse' {marker} -> marker) (\s@DescribeGlobalClustersResponse' {} a -> s {marker = a} :: DescribeGlobalClustersResponse)

-- | The response's http status code.
describeGlobalClustersResponse_httpStatus :: Lens.Lens' DescribeGlobalClustersResponse Core.Int
describeGlobalClustersResponse_httpStatus = Lens.lens (\DescribeGlobalClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalClustersResponse' {} a -> s {httpStatus = a} :: DescribeGlobalClustersResponse)

instance Core.NFData DescribeGlobalClustersResponse
