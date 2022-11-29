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
-- Module      : Amazonka.DMS.DescribeConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the connections that have been made between the
-- replication instance and an endpoint. Connections are created when you
-- test an endpoint.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeConnections
  ( -- * Creating a Request
    DescribeConnections (..),
    newDescribeConnections,

    -- * Request Lenses
    describeConnections_marker,
    describeConnections_filters,
    describeConnections_maxRecords,

    -- * Destructuring the Response
    DescribeConnectionsResponse (..),
    newDescribeConnectionsResponse,

    -- * Response Lenses
    describeConnectionsResponse_marker,
    describeConnectionsResponse_connections,
    describeConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeConnections' smart constructor.
data DescribeConnections = DescribeConnections'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The filters applied to the connection.
    --
    -- Valid filter names: endpoint-arn | replication-instance-arn
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeConnections_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'filters', 'describeConnections_filters' - The filters applied to the connection.
--
-- Valid filter names: endpoint-arn | replication-instance-arn
--
-- 'maxRecords', 'describeConnections_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeConnections ::
  DescribeConnections
newDescribeConnections =
  DescribeConnections'
    { marker = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeConnections_marker :: Lens.Lens' DescribeConnections (Prelude.Maybe Prelude.Text)
describeConnections_marker = Lens.lens (\DescribeConnections' {marker} -> marker) (\s@DescribeConnections' {} a -> s {marker = a} :: DescribeConnections)

-- | The filters applied to the connection.
--
-- Valid filter names: endpoint-arn | replication-instance-arn
describeConnections_filters :: Lens.Lens' DescribeConnections (Prelude.Maybe [Filter])
describeConnections_filters = Lens.lens (\DescribeConnections' {filters} -> filters) (\s@DescribeConnections' {} a -> s {filters = a} :: DescribeConnections) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeConnections_maxRecords :: Lens.Lens' DescribeConnections (Prelude.Maybe Prelude.Int)
describeConnections_maxRecords = Lens.lens (\DescribeConnections' {maxRecords} -> maxRecords) (\s@DescribeConnections' {} a -> s {maxRecords = a} :: DescribeConnections)

instance Core.AWSPager DescribeConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeConnectionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeConnectionsResponse_connections
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeConnections_marker
          Lens..~ rs
          Lens.^? describeConnectionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeConnections where
  type
    AWSResponse DescribeConnections =
      DescribeConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConnectionsResponse'
            Prelude.<$> (x Core..?> "Marker")
            Prelude.<*> (x Core..?> "Connections" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConnections where
  hashWithSalt _salt DescribeConnections' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeConnections where
  rnf DescribeConnections' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords

instance Core.ToHeaders DescribeConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeConnections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeConnections where
  toJSON DescribeConnections' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeConnections where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConnections where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeConnectionsResponse' smart constructor.
data DescribeConnectionsResponse = DescribeConnectionsResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A description of the connections.
    connections :: Prelude.Maybe [Connection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeConnectionsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'connections', 'describeConnectionsResponse_connections' - A description of the connections.
--
-- 'httpStatus', 'describeConnectionsResponse_httpStatus' - The response's http status code.
newDescribeConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConnectionsResponse
newDescribeConnectionsResponse pHttpStatus_ =
  DescribeConnectionsResponse'
    { marker =
        Prelude.Nothing,
      connections = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeConnectionsResponse_marker :: Lens.Lens' DescribeConnectionsResponse (Prelude.Maybe Prelude.Text)
describeConnectionsResponse_marker = Lens.lens (\DescribeConnectionsResponse' {marker} -> marker) (\s@DescribeConnectionsResponse' {} a -> s {marker = a} :: DescribeConnectionsResponse)

-- | A description of the connections.
describeConnectionsResponse_connections :: Lens.Lens' DescribeConnectionsResponse (Prelude.Maybe [Connection])
describeConnectionsResponse_connections = Lens.lens (\DescribeConnectionsResponse' {connections} -> connections) (\s@DescribeConnectionsResponse' {} a -> s {connections = a} :: DescribeConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeConnectionsResponse_httpStatus :: Lens.Lens' DescribeConnectionsResponse Prelude.Int
describeConnectionsResponse_httpStatus = Lens.lens (\DescribeConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeConnectionsResponse)

instance Prelude.NFData DescribeConnectionsResponse where
  rnf DescribeConnectionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf httpStatus
