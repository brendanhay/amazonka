{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to the endpoints.
--
-- Valid filter names: endpoint-arn | endpoint-type | endpoint-id |
-- engine-name
describeEndpoints_filters :: Lens.Lens' DescribeEndpoints (Prelude.Maybe [Filter])
describeEndpoints_filters = Lens.lens (\DescribeEndpoints' {filters} -> filters) (\s@DescribeEndpoints' {} a -> s {filters = a} :: DescribeEndpoints) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpoints_marker :: Lens.Lens' DescribeEndpoints (Prelude.Maybe Prelude.Text)
describeEndpoints_marker = Lens.lens (\DescribeEndpoints' {marker} -> marker) (\s@DescribeEndpoints' {} a -> s {marker = a} :: DescribeEndpoints)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEndpoints_maxRecords :: Lens.Lens' DescribeEndpoints (Prelude.Maybe Prelude.Int)
describeEndpoints_maxRecords = Lens.lens (\DescribeEndpoints' {maxRecords} -> maxRecords) (\s@DescribeEndpoints' {} a -> s {maxRecords = a} :: DescribeEndpoints)

instance Pager.AWSPager DescribeEndpoints where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeEndpointsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeEndpointsResponse_endpoints
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeEndpoints_marker
          Lens..~ rs
          Lens.^? describeEndpointsResponse_marker Prelude.. Lens._Just

instance Prelude.AWSRequest DescribeEndpoints where
  type Rs DescribeEndpoints = DescribeEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointsResponse'
            Prelude.<$> ( x Prelude..?> "Endpoints"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpoints

instance Prelude.NFData DescribeEndpoints

instance Prelude.ToHeaders DescribeEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.DescribeEndpoints" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEndpoints where
  toJSON DescribeEndpoints' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Filters" Prelude..=) Prelude.<$> filters,
            ("Marker" Prelude..=) Prelude.<$> marker,
            ("MaxRecords" Prelude..=) Prelude.<$> maxRecords
          ]
      )

instance Prelude.ToPath DescribeEndpoints where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeEndpointsResponse' smart constructor.
data DescribeEndpointsResponse = DescribeEndpointsResponse'
  { -- | Endpoint description.
    endpoints :: Prelude.Maybe [Endpoint],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEndpointsResponse
newDescribeEndpointsResponse pHttpStatus_ =
  DescribeEndpointsResponse'
    { endpoints =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Endpoint description.
describeEndpointsResponse_endpoints :: Lens.Lens' DescribeEndpointsResponse (Prelude.Maybe [Endpoint])
describeEndpointsResponse_endpoints = Lens.lens (\DescribeEndpointsResponse' {endpoints} -> endpoints) (\s@DescribeEndpointsResponse' {} a -> s {endpoints = a} :: DescribeEndpointsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointsResponse_marker :: Lens.Lens' DescribeEndpointsResponse (Prelude.Maybe Prelude.Text)
describeEndpointsResponse_marker = Lens.lens (\DescribeEndpointsResponse' {marker} -> marker) (\s@DescribeEndpointsResponse' {} a -> s {marker = a} :: DescribeEndpointsResponse)

-- | The response's http status code.
describeEndpointsResponse_httpStatus :: Lens.Lens' DescribeEndpointsResponse Prelude.Int
describeEndpointsResponse_httpStatus = Lens.lens (\DescribeEndpointsResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointsResponse' {} a -> s {httpStatus = a} :: DescribeEndpointsResponse)

instance Prelude.NFData DescribeEndpointsResponse
