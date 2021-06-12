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
-- Module      : Network.AWS.RDS.DescribeDBClusters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about provisioned Aurora DB clusters. This API
-- supports pagination.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This operation can also return information for Amazon Neptune DB
-- instances and Amazon DocumentDB instances.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusters
  ( -- * Creating a Request
    DescribeDBClusters (..),
    newDescribeDBClusters,

    -- * Request Lenses
    describeDBClusters_includeShared,
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_filters,
    describeDBClusters_marker,
    describeDBClusters_maxRecords,

    -- * Destructuring the Response
    DescribeDBClustersResponse (..),
    newDescribeDBClustersResponse,

    -- * Response Lenses
    describeDBClustersResponse_dbClusters,
    describeDBClustersResponse_marker,
    describeDBClustersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusters' smart constructor.
data DescribeDBClusters = DescribeDBClusters'
  { -- | Optional Boolean parameter that specifies whether the output includes
    -- information about clusters shared from other AWS accounts.
    includeShared :: Core.Maybe Core.Bool,
    -- | The user-supplied DB cluster identifier. If this parameter is specified,
    -- information from only the specific DB cluster is returned. This
    -- parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   If supplied, must match an existing DBClusterIdentifier.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | A filter that specifies one or more DB clusters to describe.
    --
    -- Supported filters:
    --
    -- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
    --     Amazon Resource Names (ARNs). The results list will only include
    --     information about the DB clusters identified by these ARNs.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous @DescribeDBClusters@
    -- request. If this parameter is specified, the response includes only
    -- records beyond the marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeShared', 'describeDBClusters_includeShared' - Optional Boolean parameter that specifies whether the output includes
-- information about clusters shared from other AWS accounts.
--
-- 'dbClusterIdentifier', 'describeDBClusters_dbClusterIdentifier' - The user-supplied DB cluster identifier. If this parameter is specified,
-- information from only the specific DB cluster is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBClusterIdentifier.
--
-- 'filters', 'describeDBClusters_filters' - A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB clusters identified by these ARNs.
--
-- 'marker', 'describeDBClusters_marker' - An optional pagination token provided by a previous @DescribeDBClusters@
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBClusters ::
  DescribeDBClusters
newDescribeDBClusters =
  DescribeDBClusters'
    { includeShared = Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Optional Boolean parameter that specifies whether the output includes
-- information about clusters shared from other AWS accounts.
describeDBClusters_includeShared :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Bool)
describeDBClusters_includeShared = Lens.lens (\DescribeDBClusters' {includeShared} -> includeShared) (\s@DescribeDBClusters' {} a -> s {includeShared = a} :: DescribeDBClusters)

-- | The user-supplied DB cluster identifier. If this parameter is specified,
-- information from only the specific DB cluster is returned. This
-- parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   If supplied, must match an existing DBClusterIdentifier.
describeDBClusters_dbClusterIdentifier :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Text)
describeDBClusters_dbClusterIdentifier = Lens.lens (\DescribeDBClusters' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusters' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusters)

-- | A filter that specifies one or more DB clusters to describe.
--
-- Supported filters:
--
-- -   @db-cluster-id@ - Accepts DB cluster identifiers and DB cluster
--     Amazon Resource Names (ARNs). The results list will only include
--     information about the DB clusters identified by these ARNs.
describeDBClusters_filters :: Lens.Lens' DescribeDBClusters (Core.Maybe [Filter])
describeDBClusters_filters = Lens.lens (\DescribeDBClusters' {filters} -> filters) (\s@DescribeDBClusters' {} a -> s {filters = a} :: DescribeDBClusters) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous @DescribeDBClusters@
-- request. If this parameter is specified, the response includes only
-- records beyond the marker, up to the value specified by @MaxRecords@.
describeDBClusters_marker :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Text)
describeDBClusters_marker = Lens.lens (\DescribeDBClusters' {marker} -> marker) (\s@DescribeDBClusters' {} a -> s {marker = a} :: DescribeDBClusters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusters_maxRecords :: Lens.Lens' DescribeDBClusters (Core.Maybe Core.Int)
describeDBClusters_maxRecords = Lens.lens (\DescribeDBClusters' {maxRecords} -> maxRecords) (\s@DescribeDBClusters' {} a -> s {maxRecords = a} :: DescribeDBClusters)

instance Core.AWSPager DescribeDBClusters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClustersResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClustersResponse_dbClusters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBClusters_marker
          Lens..~ rs
          Lens.^? describeDBClustersResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeDBClusters where
  type
    AWSResponse DescribeDBClusters =
      DescribeDBClustersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClustersResult"
      ( \s h x ->
          DescribeDBClustersResponse'
            Core.<$> ( x Core..@? "DBClusters" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "DBCluster")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBClusters

instance Core.NFData DescribeDBClusters

instance Core.ToHeaders DescribeDBClusters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBClusters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBClusters where
  toQuery DescribeDBClusters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBClusters" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "IncludeShared" Core.=: includeShared,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBClusters@ action.
--
-- /See:/ 'newDescribeDBClustersResponse' smart constructor.
data DescribeDBClustersResponse = DescribeDBClustersResponse'
  { -- | Contains a list of DB clusters for the user.
    dbClusters :: Core.Maybe [DBCluster],
    -- | A pagination token that can be used in a later DescribeDBClusters
    -- request.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClustersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusters', 'describeDBClustersResponse_dbClusters' - Contains a list of DB clusters for the user.
--
-- 'marker', 'describeDBClustersResponse_marker' - A pagination token that can be used in a later DescribeDBClusters
-- request.
--
-- 'httpStatus', 'describeDBClustersResponse_httpStatus' - The response's http status code.
newDescribeDBClustersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBClustersResponse
newDescribeDBClustersResponse pHttpStatus_ =
  DescribeDBClustersResponse'
    { dbClusters =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains a list of DB clusters for the user.
describeDBClustersResponse_dbClusters :: Lens.Lens' DescribeDBClustersResponse (Core.Maybe [DBCluster])
describeDBClustersResponse_dbClusters = Lens.lens (\DescribeDBClustersResponse' {dbClusters} -> dbClusters) (\s@DescribeDBClustersResponse' {} a -> s {dbClusters = a} :: DescribeDBClustersResponse) Core.. Lens.mapping Lens._Coerce

-- | A pagination token that can be used in a later DescribeDBClusters
-- request.
describeDBClustersResponse_marker :: Lens.Lens' DescribeDBClustersResponse (Core.Maybe Core.Text)
describeDBClustersResponse_marker = Lens.lens (\DescribeDBClustersResponse' {marker} -> marker) (\s@DescribeDBClustersResponse' {} a -> s {marker = a} :: DescribeDBClustersResponse)

-- | The response's http status code.
describeDBClustersResponse_httpStatus :: Lens.Lens' DescribeDBClustersResponse Core.Int
describeDBClustersResponse_httpStatus = Lens.lens (\DescribeDBClustersResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClustersResponse' {} a -> s {httpStatus = a} :: DescribeDBClustersResponse)

instance Core.NFData DescribeDBClustersResponse
