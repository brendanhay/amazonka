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
-- Module      : Network.AWS.RDS.DescribeDBProxyEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about DB proxy endpoints.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBProxyEndpoints
  ( -- * Creating a Request
    DescribeDBProxyEndpoints (..),
    newDescribeDBProxyEndpoints,

    -- * Request Lenses
    describeDBProxyEndpoints_dbProxyEndpointName,
    describeDBProxyEndpoints_filters,
    describeDBProxyEndpoints_dbProxyName,
    describeDBProxyEndpoints_maxRecords,
    describeDBProxyEndpoints_marker,

    -- * Destructuring the Response
    DescribeDBProxyEndpointsResponse (..),
    newDescribeDBProxyEndpointsResponse,

    -- * Response Lenses
    describeDBProxyEndpointsResponse_dbProxyEndpoints,
    describeDBProxyEndpointsResponse_marker,
    describeDBProxyEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBProxyEndpoints' smart constructor.
data DescribeDBProxyEndpoints = DescribeDBProxyEndpoints'
  { -- | The name of a DB proxy endpoint to describe. If you omit this parameter,
    -- the output includes information about all DB proxy endpoints associated
    -- with the specified proxy.
    dbProxyEndpointName :: Prelude.Maybe Prelude.Text,
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The name of the DB proxy whose endpoints you want to describe. If you
    -- omit this parameter, the output includes information about all DB proxy
    -- endpoints associated with all your DB proxies.
    dbProxyName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Natural,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBProxyEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyEndpointName', 'describeDBProxyEndpoints_dbProxyEndpointName' - The name of a DB proxy endpoint to describe. If you omit this parameter,
-- the output includes information about all DB proxy endpoints associated
-- with the specified proxy.
--
-- 'filters', 'describeDBProxyEndpoints_filters' - This parameter is not currently supported.
--
-- 'dbProxyName', 'describeDBProxyEndpoints_dbProxyName' - The name of the DB proxy whose endpoints you want to describe. If you
-- omit this parameter, the output includes information about all DB proxy
-- endpoints associated with all your DB proxies.
--
-- 'maxRecords', 'describeDBProxyEndpoints_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'marker', 'describeDBProxyEndpoints_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
newDescribeDBProxyEndpoints ::
  DescribeDBProxyEndpoints
newDescribeDBProxyEndpoints =
  DescribeDBProxyEndpoints'
    { dbProxyEndpointName =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      dbProxyName = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The name of a DB proxy endpoint to describe. If you omit this parameter,
-- the output includes information about all DB proxy endpoints associated
-- with the specified proxy.
describeDBProxyEndpoints_dbProxyEndpointName :: Lens.Lens' DescribeDBProxyEndpoints (Prelude.Maybe Prelude.Text)
describeDBProxyEndpoints_dbProxyEndpointName = Lens.lens (\DescribeDBProxyEndpoints' {dbProxyEndpointName} -> dbProxyEndpointName) (\s@DescribeDBProxyEndpoints' {} a -> s {dbProxyEndpointName = a} :: DescribeDBProxyEndpoints)

-- | This parameter is not currently supported.
describeDBProxyEndpoints_filters :: Lens.Lens' DescribeDBProxyEndpoints (Prelude.Maybe [Filter])
describeDBProxyEndpoints_filters = Lens.lens (\DescribeDBProxyEndpoints' {filters} -> filters) (\s@DescribeDBProxyEndpoints' {} a -> s {filters = a} :: DescribeDBProxyEndpoints) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the DB proxy whose endpoints you want to describe. If you
-- omit this parameter, the output includes information about all DB proxy
-- endpoints associated with all your DB proxies.
describeDBProxyEndpoints_dbProxyName :: Lens.Lens' DescribeDBProxyEndpoints (Prelude.Maybe Prelude.Text)
describeDBProxyEndpoints_dbProxyName = Lens.lens (\DescribeDBProxyEndpoints' {dbProxyName} -> dbProxyName) (\s@DescribeDBProxyEndpoints' {} a -> s {dbProxyName = a} :: DescribeDBProxyEndpoints)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBProxyEndpoints_maxRecords :: Lens.Lens' DescribeDBProxyEndpoints (Prelude.Maybe Prelude.Natural)
describeDBProxyEndpoints_maxRecords = Lens.lens (\DescribeDBProxyEndpoints' {maxRecords} -> maxRecords) (\s@DescribeDBProxyEndpoints' {} a -> s {maxRecords = a} :: DescribeDBProxyEndpoints)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyEndpoints_marker :: Lens.Lens' DescribeDBProxyEndpoints (Prelude.Maybe Prelude.Text)
describeDBProxyEndpoints_marker = Lens.lens (\DescribeDBProxyEndpoints' {marker} -> marker) (\s@DescribeDBProxyEndpoints' {} a -> s {marker = a} :: DescribeDBProxyEndpoints)

instance Core.AWSPager DescribeDBProxyEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBProxyEndpointsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBProxyEndpointsResponse_dbProxyEndpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBProxyEndpoints_marker
          Lens..~ rs
          Lens.^? describeDBProxyEndpointsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBProxyEndpoints where
  type
    AWSResponse DescribeDBProxyEndpoints =
      DescribeDBProxyEndpointsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBProxyEndpointsResult"
      ( \s h x ->
          DescribeDBProxyEndpointsResponse'
            Prelude.<$> ( x Core..@? "DBProxyEndpoints"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBProxyEndpoints

instance Prelude.NFData DescribeDBProxyEndpoints

instance Core.ToHeaders DescribeDBProxyEndpoints where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDBProxyEndpoints where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDBProxyEndpoints where
  toQuery DescribeDBProxyEndpoints' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeDBProxyEndpoints" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DBProxyEndpointName" Core.=: dbProxyEndpointName,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "DBProxyName" Core.=: dbProxyName,
        "MaxRecords" Core.=: maxRecords,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeDBProxyEndpointsResponse' smart constructor.
data DescribeDBProxyEndpointsResponse = DescribeDBProxyEndpointsResponse'
  { -- | The list of @ProxyEndpoint@ objects returned by the API operation.
    dbProxyEndpoints :: Prelude.Maybe [DBProxyEndpoint],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBProxyEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbProxyEndpoints', 'describeDBProxyEndpointsResponse_dbProxyEndpoints' - The list of @ProxyEndpoint@ objects returned by the API operation.
--
-- 'marker', 'describeDBProxyEndpointsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBProxyEndpointsResponse_httpStatus' - The response's http status code.
newDescribeDBProxyEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBProxyEndpointsResponse
newDescribeDBProxyEndpointsResponse pHttpStatus_ =
  DescribeDBProxyEndpointsResponse'
    { dbProxyEndpoints =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of @ProxyEndpoint@ objects returned by the API operation.
describeDBProxyEndpointsResponse_dbProxyEndpoints :: Lens.Lens' DescribeDBProxyEndpointsResponse (Prelude.Maybe [DBProxyEndpoint])
describeDBProxyEndpointsResponse_dbProxyEndpoints = Lens.lens (\DescribeDBProxyEndpointsResponse' {dbProxyEndpoints} -> dbProxyEndpoints) (\s@DescribeDBProxyEndpointsResponse' {} a -> s {dbProxyEndpoints = a} :: DescribeDBProxyEndpointsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBProxyEndpointsResponse_marker :: Lens.Lens' DescribeDBProxyEndpointsResponse (Prelude.Maybe Prelude.Text)
describeDBProxyEndpointsResponse_marker = Lens.lens (\DescribeDBProxyEndpointsResponse' {marker} -> marker) (\s@DescribeDBProxyEndpointsResponse' {} a -> s {marker = a} :: DescribeDBProxyEndpointsResponse)

-- | The response's http status code.
describeDBProxyEndpointsResponse_httpStatus :: Lens.Lens' DescribeDBProxyEndpointsResponse Prelude.Int
describeDBProxyEndpointsResponse_httpStatus = Lens.lens (\DescribeDBProxyEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeDBProxyEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeDBProxyEndpointsResponse)

instance
  Prelude.NFData
    DescribeDBProxyEndpointsResponse
