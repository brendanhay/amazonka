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
-- Module      : Network.AWS.DMS.DescribeEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the endpoints for your account in the current
-- region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEndpoints
  ( -- * Creating a Request
    DescribeEndpoints (..),
    newDescribeEndpoints,

    -- * Request Lenses
    describeEndpoints_filters,
    describeEndpoints_marker,
    describeEndpoints_maxRecords,

    -- * Destructuring the Response
    DescribeEndpointsResponse (..),
    newDescribeEndpointsResponse,

    -- * Response Lenses
    describeEndpointsResponse_endpoints,
    describeEndpointsResponse_marker,
    describeEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEndpoints' smart constructor.
data DescribeEndpoints = DescribeEndpoints'
  { -- | Filters applied to the endpoints.
    --
    -- Valid filter names: endpoint-arn | endpoint-type | endpoint-id |
    -- engine-name
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeEndpoints_filters' - Filters applied to the endpoints.
--
-- Valid filter names: endpoint-arn | endpoint-type | endpoint-id |
-- engine-name
--
-- 'marker', 'describeEndpoints_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEndpoints_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeEndpoints ::
  DescribeEndpoints
newDescribeEndpoints =
  DescribeEndpoints'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters applied to the endpoints.
--
-- Valid filter names: endpoint-arn | endpoint-type | endpoint-id |
-- engine-name
describeEndpoints_filters :: Lens.Lens' DescribeEndpoints (Core.Maybe [Filter])
describeEndpoints_filters = Lens.lens (\DescribeEndpoints' {filters} -> filters) (\s@DescribeEndpoints' {} a -> s {filters = a} :: DescribeEndpoints) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpoints_marker :: Lens.Lens' DescribeEndpoints (Core.Maybe Core.Text)
describeEndpoints_marker = Lens.lens (\DescribeEndpoints' {marker} -> marker) (\s@DescribeEndpoints' {} a -> s {marker = a} :: DescribeEndpoints)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEndpoints_maxRecords :: Lens.Lens' DescribeEndpoints (Core.Maybe Core.Int)
describeEndpoints_maxRecords = Lens.lens (\DescribeEndpoints' {maxRecords} -> maxRecords) (\s@DescribeEndpoints' {} a -> s {maxRecords = a} :: DescribeEndpoints)

instance Core.AWSPager DescribeEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointsResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointsResponse_endpoints
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEndpoints_marker
          Lens..~ rs
          Lens.^? describeEndpointsResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeEndpoints where
  type
    AWSResponse DescribeEndpoints =
      DescribeEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Core.<$> (x Core..?> "Endpoints" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEndpoints

instance Core.NFData DescribeEndpoints

instance Core.ToHeaders DescribeEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeEndpoints" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEndpoints where
  toJSON DescribeEndpoints' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEndpoints where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | Endpoint description.
    endpoints :: Core.Maybe [Endpoint],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'describeEndpointsResponse_endpoints' - Endpoint description.
--
-- 'marker', 'describeEndpointsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeEndpointsResponse_httpStatus' - The response's http status code.
newDescribeEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEndpointsResponse
newDescribeEndpointsResponse pHttpStatus_ =
  DescribeEndpointsResponse'
    { endpoints =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Endpoint description.
describeEndpointsResponse_endpoints :: Lens.Lens' DescribeEndpointsResponse (Core.Maybe [Endpoint])
describeEndpointsResponse_endpoints = Lens.lens (\DescribeEndpointsResponse' {endpoints} -> endpoints) (\s@DescribeEndpointsResponse' {} a -> s {endpoints = a} :: DescribeEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointsResponse_marker :: Lens.Lens' DescribeEndpointsResponse (Core.Maybe Core.Text)
describeEndpointsResponse_marker = Lens.lens (\DescribeEndpointsResponse' {marker} -> marker) (\s@DescribeEndpointsResponse' {} a -> s {marker = a} :: DescribeEndpointsResponse)

-- | The response's http status code.
describeEndpointsResponse_httpStatus :: Lens.Lens' DescribeEndpointsResponse Core.Int
describeEndpointsResponse_httpStatus = Lens.lens (\DescribeEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeEndpointsResponse)

instance Core.NFData DescribeEndpointsResponse
