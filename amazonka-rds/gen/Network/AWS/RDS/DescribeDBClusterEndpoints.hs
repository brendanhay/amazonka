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
-- Module      : Network.AWS.RDS.DescribeDBClusterEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about endpoints for an Amazon Aurora DB cluster.
--
-- This action only applies to Aurora DB clusters.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterEndpoints
  ( -- * Creating a Request
    DescribeDBClusterEndpoints (..),
    newDescribeDBClusterEndpoints,

    -- * Request Lenses
    describeDBClusterEndpoints_dbClusterEndpointIdentifier,
    describeDBClusterEndpoints_dbClusterIdentifier,
    describeDBClusterEndpoints_filters,
    describeDBClusterEndpoints_marker,
    describeDBClusterEndpoints_maxRecords,

    -- * Destructuring the Response
    DescribeDBClusterEndpointsResponse (..),
    newDescribeDBClusterEndpointsResponse,

    -- * Response Lenses
    describeDBClusterEndpointsResponse_dbClusterEndpoints,
    describeDBClusterEndpointsResponse_marker,
    describeDBClusterEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBClusterEndpoints' smart constructor.
data DescribeDBClusterEndpoints = DescribeDBClusterEndpoints'
  { -- | The identifier of the endpoint to describe. This parameter is stored as
    -- a lowercase string.
    dbClusterEndpointIdentifier :: Core.Maybe Core.Text,
    -- | The DB cluster identifier of the DB cluster associated with the
    -- endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | A set of name-value pairs that define which endpoints to include in the
    -- output. The filters are specified as name-value pairs, in the format
    -- @Name=endpoint_type,Values=endpoint_type1,endpoint_type2,...@. @Name@
    -- can be one of: @db-cluster-endpoint-type@,
    -- @db-cluster-endpoint-custom-type@, @db-cluster-endpoint-id@,
    -- @db-cluster-endpoint-status@. @Values@ for the
    -- @ db-cluster-endpoint-type@ filter can be one or more of: @reader@,
    -- @writer@, @custom@. @Values@ for the @db-cluster-endpoint-custom-type@
    -- filter can be one or more of: @reader@, @any@. @Values@ for the
    -- @db-cluster-endpoint-status@ filter can be one or more of: @available@,
    -- @creating@, @deleting@, @inactive@, @modifying@.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
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
-- Create a value of 'DescribeDBClusterEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterEndpointIdentifier', 'describeDBClusterEndpoints_dbClusterEndpointIdentifier' - The identifier of the endpoint to describe. This parameter is stored as
-- a lowercase string.
--
-- 'dbClusterIdentifier', 'describeDBClusterEndpoints_dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
--
-- 'filters', 'describeDBClusterEndpoints_filters' - A set of name-value pairs that define which endpoints to include in the
-- output. The filters are specified as name-value pairs, in the format
-- @Name=endpoint_type,Values=endpoint_type1,endpoint_type2,...@. @Name@
-- can be one of: @db-cluster-endpoint-type@,
-- @db-cluster-endpoint-custom-type@, @db-cluster-endpoint-id@,
-- @db-cluster-endpoint-status@. @Values@ for the
-- @ db-cluster-endpoint-type@ filter can be one or more of: @reader@,
-- @writer@, @custom@. @Values@ for the @db-cluster-endpoint-custom-type@
-- filter can be one or more of: @reader@, @any@. @Values@ for the
-- @db-cluster-endpoint-status@ filter can be one or more of: @available@,
-- @creating@, @deleting@, @inactive@, @modifying@.
--
-- 'marker', 'describeDBClusterEndpoints_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterEndpoints_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeDBClusterEndpoints ::
  DescribeDBClusterEndpoints
newDescribeDBClusterEndpoints =
  DescribeDBClusterEndpoints'
    { dbClusterEndpointIdentifier =
        Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The identifier of the endpoint to describe. This parameter is stored as
-- a lowercase string.
describeDBClusterEndpoints_dbClusterEndpointIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Core.Maybe Core.Text)
describeDBClusterEndpoints_dbClusterEndpointIdentifier = Lens.lens (\DescribeDBClusterEndpoints' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@DescribeDBClusterEndpoints' {} a -> s {dbClusterEndpointIdentifier = a} :: DescribeDBClusterEndpoints)

-- | The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
describeDBClusterEndpoints_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Core.Maybe Core.Text)
describeDBClusterEndpoints_dbClusterIdentifier = Lens.lens (\DescribeDBClusterEndpoints' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusterEndpoints' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusterEndpoints)

-- | A set of name-value pairs that define which endpoints to include in the
-- output. The filters are specified as name-value pairs, in the format
-- @Name=endpoint_type,Values=endpoint_type1,endpoint_type2,...@. @Name@
-- can be one of: @db-cluster-endpoint-type@,
-- @db-cluster-endpoint-custom-type@, @db-cluster-endpoint-id@,
-- @db-cluster-endpoint-status@. @Values@ for the
-- @ db-cluster-endpoint-type@ filter can be one or more of: @reader@,
-- @writer@, @custom@. @Values@ for the @db-cluster-endpoint-custom-type@
-- filter can be one or more of: @reader@, @any@. @Values@ for the
-- @db-cluster-endpoint-status@ filter can be one or more of: @available@,
-- @creating@, @deleting@, @inactive@, @modifying@.
describeDBClusterEndpoints_filters :: Lens.Lens' DescribeDBClusterEndpoints (Core.Maybe [Filter])
describeDBClusterEndpoints_filters = Lens.lens (\DescribeDBClusterEndpoints' {filters} -> filters) (\s@DescribeDBClusterEndpoints' {} a -> s {filters = a} :: DescribeDBClusterEndpoints) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterEndpoints_marker :: Lens.Lens' DescribeDBClusterEndpoints (Core.Maybe Core.Text)
describeDBClusterEndpoints_marker = Lens.lens (\DescribeDBClusterEndpoints' {marker} -> marker) (\s@DescribeDBClusterEndpoints' {} a -> s {marker = a} :: DescribeDBClusterEndpoints)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterEndpoints_maxRecords :: Lens.Lens' DescribeDBClusterEndpoints (Core.Maybe Core.Int)
describeDBClusterEndpoints_maxRecords = Lens.lens (\DescribeDBClusterEndpoints' {maxRecords} -> maxRecords) (\s@DescribeDBClusterEndpoints' {} a -> s {maxRecords = a} :: DescribeDBClusterEndpoints)

instance Core.AWSPager DescribeDBClusterEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterEndpointsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterEndpointsResponse_dbClusterEndpoints
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBClusterEndpoints_marker
          Lens..~ rs
          Lens.^? describeDBClusterEndpointsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeDBClusterEndpoints where
  type
    AWSResponse DescribeDBClusterEndpoints =
      DescribeDBClusterEndpointsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterEndpointsResult"
      ( \s h x ->
          DescribeDBClusterEndpointsResponse'
            Core.<$> ( x Core..@? "DBClusterEndpoints" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "DBClusterEndpointList")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBClusterEndpoints

instance Core.NFData DescribeDBClusterEndpoints

instance Core.ToHeaders DescribeDBClusterEndpoints where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBClusterEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBClusterEndpoints where
  toQuery DescribeDBClusterEndpoints' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBClusterEndpoints" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "DBClusterEndpointIdentifier"
          Core.=: dbClusterEndpointIdentifier,
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeDBClusterEndpointsResponse' smart constructor.
data DescribeDBClusterEndpointsResponse = DescribeDBClusterEndpointsResponse'
  { -- | Contains the details of the endpoints associated with the cluster and
    -- matching any filter conditions.
    dbClusterEndpoints :: Core.Maybe [DBClusterEndpoint],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusterEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbClusterEndpoints', 'describeDBClusterEndpointsResponse_dbClusterEndpoints' - Contains the details of the endpoints associated with the cluster and
-- matching any filter conditions.
--
-- 'marker', 'describeDBClusterEndpointsResponse_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBClusterEndpointsResponse_httpStatus' - The response's http status code.
newDescribeDBClusterEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBClusterEndpointsResponse
newDescribeDBClusterEndpointsResponse pHttpStatus_ =
  DescribeDBClusterEndpointsResponse'
    { dbClusterEndpoints =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the endpoints associated with the cluster and
-- matching any filter conditions.
describeDBClusterEndpointsResponse_dbClusterEndpoints :: Lens.Lens' DescribeDBClusterEndpointsResponse (Core.Maybe [DBClusterEndpoint])
describeDBClusterEndpointsResponse_dbClusterEndpoints = Lens.lens (\DescribeDBClusterEndpointsResponse' {dbClusterEndpoints} -> dbClusterEndpoints) (\s@DescribeDBClusterEndpointsResponse' {} a -> s {dbClusterEndpoints = a} :: DescribeDBClusterEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterEndpointsResponse_marker :: Lens.Lens' DescribeDBClusterEndpointsResponse (Core.Maybe Core.Text)
describeDBClusterEndpointsResponse_marker = Lens.lens (\DescribeDBClusterEndpointsResponse' {marker} -> marker) (\s@DescribeDBClusterEndpointsResponse' {} a -> s {marker = a} :: DescribeDBClusterEndpointsResponse)

-- | The response's http status code.
describeDBClusterEndpointsResponse_httpStatus :: Lens.Lens' DescribeDBClusterEndpointsResponse Core.Int
describeDBClusterEndpointsResponse_httpStatus = Lens.lens (\DescribeDBClusterEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterEndpointsResponse)

instance
  Core.NFData
    DescribeDBClusterEndpointsResponse
