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
-- Module      : Amazonka.Neptune.DescribeDBClusterEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about endpoints for an Amazon Neptune DB cluster.
--
-- This operation can also return information for Amazon RDS clusters and
-- Amazon DocDB clusters.
--
-- This operation returns paginated results.
module Amazonka.Neptune.DescribeDBClusterEndpoints
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBClusterEndpoints' smart constructor.
data DescribeDBClusterEndpoints = DescribeDBClusterEndpoints'
  { -- | The identifier of the endpoint to describe. This parameter is stored as
    -- a lowercase string.
    dbClusterEndpointIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The DB cluster identifier of the DB cluster associated with the
    -- endpoint. This parameter is stored as a lowercase string.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
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
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
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
        Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The identifier of the endpoint to describe. This parameter is stored as
-- a lowercase string.
describeDBClusterEndpoints_dbClusterEndpointIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe Prelude.Text)
describeDBClusterEndpoints_dbClusterEndpointIdentifier = Lens.lens (\DescribeDBClusterEndpoints' {dbClusterEndpointIdentifier} -> dbClusterEndpointIdentifier) (\s@DescribeDBClusterEndpoints' {} a -> s {dbClusterEndpointIdentifier = a} :: DescribeDBClusterEndpoints)

-- | The DB cluster identifier of the DB cluster associated with the
-- endpoint. This parameter is stored as a lowercase string.
describeDBClusterEndpoints_dbClusterIdentifier :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe Prelude.Text)
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
describeDBClusterEndpoints_filters :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe [Filter])
describeDBClusterEndpoints_filters = Lens.lens (\DescribeDBClusterEndpoints' {filters} -> filters) (\s@DescribeDBClusterEndpoints' {} a -> s {filters = a} :: DescribeDBClusterEndpoints) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterEndpoints@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterEndpoints_marker :: Lens.Lens' DescribeDBClusterEndpoints (Prelude.Maybe Prelude.Text)
describeDBClusterEndpoints_marker = Lens.lens (\DescribeDBClusterEndpoints' {marker} -> marker) (\s@DescribeDBClusterEndpoints' {} a -> s {marker = a} :: DescribeDBClusterEndpoints)

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
            Prelude.<$> ( x Data..@? "DBClusterEndpoints"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "DBClusterEndpointList")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterEndpoints where
  hashWithSalt _salt DescribeDBClusterEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` dbClusterEndpointIdentifier
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeDBClusterEndpoints where
  rnf DescribeDBClusterEndpoints' {..} =
    Prelude.rnf dbClusterEndpointIdentifier
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeDBClusterEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBClusterEndpoints where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBClusterEndpoints where
  toQuery DescribeDBClusterEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBClusterEndpoints" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "DBClusterEndpointIdentifier"
          Data.=: dbClusterEndpointIdentifier,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
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
