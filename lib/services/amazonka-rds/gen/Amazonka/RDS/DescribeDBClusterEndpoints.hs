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
-- Module      : Amazonka.RDS.DescribeDBClusterEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.RDS.DescribeDBClusterEndpoints
  ( -- * Creating a Request
    DescribeDBClusterEndpoints (..),
    newDescribeDBClusterEndpoints,

    -- * Request Lenses
    describeDBClusterEndpoints_marker,
    describeDBClusterEndpoints_filters,
    describeDBClusterEndpoints_dbClusterIdentifier,
    describeDBClusterEndpoints_maxRecords,
    describeDBClusterEndpoints_dbClusterEndpointIdentifier,

    -- * Destructuring the Response
    DescribeDBClusterEndpointsResponse (..),
    newDescribeDBClusterEndpointsResponse,

    -- * Response Lenses
    describeDBClusterEndpointsResponse_dbClusterEndpoints,
    describeDBClusterEndpointsResponse_marker,
    describeDBClusterEndpointsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBClusterEndpoints' smart constructor.
data DescribeDBClusterEndpoints = DescribeDBClusterEndpoints'
  { -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
    filters :: Prelude.Maybe [Filter],
    -- | The DB cluster identifier of the DB cluster associated with the
    -- endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the endpoint to describe. This parameter is stored as
    -- a lowercase string.
    dbClusterEndpointIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBClusterEndpoints_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
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
-- 'dbClusterIdentifier', 'describeDBClusterEndpoints_dbClusterIdentifier' - The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
--
-- 'maxRecords', 'describeDBClusterEndpoints_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbClusterEndpointIdentifier', 'describeDBClusterEndpoints_dbClusterEndpointIdentifier' - The identifier of the endpoint to describe. This parameter is stored as
-- a lowercase string.
newDescribeDBClusterEndpoints ::
  DescribeDBClusterEndpoints
newDescribeDBClusterEndpoints =
  DescribeDBClusterEndpoints'
    { marker =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      dbClusterEndpointIdentifier = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterEndpoints_marker :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe Prelude.Text)
describeDBClusterEndpoints_marker = Lens.lens (\DescribeDBClusterEndpoints' {marker} -> marker) (\s@DescribeDBClusterEndpoints' {} a -> s {marker = a} :: DescribeDBClusterEndpoints)

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
describeDBClusterEndpoints_filters :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe [Filter])
describeDBClusterEndpoints_filters = Lens.lens (\DescribeDBClusterEndpoints' {filters} -> filters) (\s@DescribeDBClusterEndpoints' {} a -> s {filters = a} :: DescribeDBClusterEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
describeDBClusterEndpoints_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe Prelude.Text)
describeDBClusterEndpoints_dbClusterIdentifier = Lens.lens (\DescribeDBClusterEndpoints' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DescribeDBClusterEndpoints' {} a -> s {dbClusterIdentifier = a} :: DescribeDBClusterEndpoints)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterEndpoints_maxRecords :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe Prelude.Int)
describeDBClusterEndpoints_maxRecords = Lens.lens (\DescribeDBClusterEndpoints' {maxRecords} -> maxRecords) (\s@DescribeDBClusterEndpoints' {} a -> s {maxRecords = a} :: DescribeDBClusterEndpoints)

-- | The identifier of the endpoint to describe. This parameter is stored as
-- a lowercase string.
describeDBClusterEndpoints_dbClusterEndpointIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe Prelude.Text)
describeDBClusterEndpoints_dbClusterEndpointIdentifier = Lens.lens (\DescribeDBClusterEndpoints' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@DescribeDBClusterEndpoints' {} a -> s {dbClusterEndpointIdentifier = a} :: DescribeDBClusterEndpoints)

instance Core.AWSPager DescribeDBClusterEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterEndpointsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterEndpointsResponse_dbClusterEndpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBClusterEndpoints_marker
          Lens..~ rs
          Lens.^? describeDBClusterEndpointsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBClusterEndpoints where
  type
    AWSResponse DescribeDBClusterEndpoints =
      DescribeDBClusterEndpointsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterEndpointsResult"
      ( \s h x ->
          DescribeDBClusterEndpointsResponse'
            Prelude.<$> ( x Core..@? "DBClusterEndpoints"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "DBClusterEndpointList")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterEndpoints where
  hashWithSalt _salt DescribeDBClusterEndpoints' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` dbClusterEndpointIdentifier

instance Prelude.NFData DescribeDBClusterEndpoints where
  rnf DescribeDBClusterEndpoints' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf dbClusterEndpointIdentifier

instance Core.ToHeaders DescribeDBClusterEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBClusterEndpoints where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDBClusterEndpoints where
  toQuery DescribeDBClusterEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeDBClusterEndpoints" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "MaxRecords" Core.=: maxRecords,
        "DBClusterEndpointIdentifier"
          Core.=: dbClusterEndpointIdentifier
      ]

-- | /See:/ 'newDescribeDBClusterEndpointsResponse' smart constructor.
data DescribeDBClusterEndpointsResponse = DescribeDBClusterEndpointsResponse'
  { -- | Contains the details of the endpoints associated with the cluster and
    -- matching any filter conditions.
    dbClusterEndpoints :: Prelude.Maybe [DBClusterEndpoint],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDBClusterEndpointsResponse
newDescribeDBClusterEndpointsResponse pHttpStatus_ =
  DescribeDBClusterEndpointsResponse'
    { dbClusterEndpoints =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the endpoints associated with the cluster and
-- matching any filter conditions.
describeDBClusterEndpointsResponse_dbClusterEndpoints :: Lens.Lens' DescribeDBClusterEndpointsResponse (Prelude.Maybe [DBClusterEndpoint])
describeDBClusterEndpointsResponse_dbClusterEndpoints = Lens.lens (\DescribeDBClusterEndpointsResponse' {dbClusterEndpoints} -> dbClusterEndpoints) (\s@DescribeDBClusterEndpointsResponse' {} a -> s {dbClusterEndpoints = a} :: DescribeDBClusterEndpointsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterEndpointsResponse_marker :: Lens.Lens' DescribeDBClusterEndpointsResponse (Prelude.Maybe Prelude.Text)
describeDBClusterEndpointsResponse_marker = Lens.lens (\DescribeDBClusterEndpointsResponse' {marker} -> marker) (\s@DescribeDBClusterEndpointsResponse' {} a -> s {marker = a} :: DescribeDBClusterEndpointsResponse)

-- | The response's http status code.
describeDBClusterEndpointsResponse_httpStatus :: Lens.Lens' DescribeDBClusterEndpointsResponse Prelude.Int
describeDBClusterEndpointsResponse_httpStatus = Lens.lens (\DescribeDBClusterEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterEndpointsResponse)

instance
  Prelude.NFData
    DescribeDBClusterEndpointsResponse
  where
  rnf DescribeDBClusterEndpointsResponse' {..} =
    Prelude.rnf dbClusterEndpoints
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
