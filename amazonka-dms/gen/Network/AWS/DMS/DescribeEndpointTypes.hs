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
-- Module      : Network.AWS.DMS.DescribeEndpointTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the type of endpoints available.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeEndpointTypes
  ( -- * Creating a Request
    DescribeEndpointTypes (..),
    newDescribeEndpointTypes,

    -- * Request Lenses
    describeEndpointTypes_filters,
    describeEndpointTypes_marker,
    describeEndpointTypes_maxRecords,

    -- * Destructuring the Response
    DescribeEndpointTypesResponse (..),
    newDescribeEndpointTypesResponse,

    -- * Response Lenses
    describeEndpointTypesResponse_supportedEndpointTypes,
    describeEndpointTypesResponse_marker,
    describeEndpointTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEndpointTypes' smart constructor.
data DescribeEndpointTypes = DescribeEndpointTypes'
  { -- | Filters applied to the endpoint types.
    --
    -- Valid filter names: engine-name | endpoint-type
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
-- Create a value of 'DescribeEndpointTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeEndpointTypes_filters' - Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
--
-- 'marker', 'describeEndpointTypes_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEndpointTypes_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeEndpointTypes ::
  DescribeEndpointTypes
newDescribeEndpointTypes =
  DescribeEndpointTypes'
    { filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
describeEndpointTypes_filters :: Lens.Lens' DescribeEndpointTypes (Core.Maybe [Filter])
describeEndpointTypes_filters = Lens.lens (\DescribeEndpointTypes' {filters} -> filters) (\s@DescribeEndpointTypes' {} a -> s {filters = a} :: DescribeEndpointTypes) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointTypes_marker :: Lens.Lens' DescribeEndpointTypes (Core.Maybe Core.Text)
describeEndpointTypes_marker = Lens.lens (\DescribeEndpointTypes' {marker} -> marker) (\s@DescribeEndpointTypes' {} a -> s {marker = a} :: DescribeEndpointTypes)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEndpointTypes_maxRecords :: Lens.Lens' DescribeEndpointTypes (Core.Maybe Core.Int)
describeEndpointTypes_maxRecords = Lens.lens (\DescribeEndpointTypes' {maxRecords} -> maxRecords) (\s@DescribeEndpointTypes' {} a -> s {maxRecords = a} :: DescribeEndpointTypes)

instance Core.AWSPager DescribeEndpointTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointTypesResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointTypesResponse_supportedEndpointTypes
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEndpointTypes_marker
          Lens..~ rs
          Lens.^? describeEndpointTypesResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeEndpointTypes where
  type
    AWSResponse DescribeEndpointTypes =
      DescribeEndpointTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointTypesResponse'
            Core.<$> ( x Core..?> "SupportedEndpointTypes"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEndpointTypes

instance Core.NFData DescribeEndpointTypes

instance Core.ToHeaders DescribeEndpointTypes where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeEndpointTypes" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEndpointTypes where
  toJSON DescribeEndpointTypes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeEndpointTypes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEndpointTypes where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeEndpointTypesResponse' smart constructor.
data DescribeEndpointTypesResponse = DescribeEndpointTypesResponse'
  { -- | The types of endpoints that are supported.
    supportedEndpointTypes :: Core.Maybe [SupportedEndpointType],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEndpointTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'supportedEndpointTypes', 'describeEndpointTypesResponse_supportedEndpointTypes' - The types of endpoints that are supported.
--
-- 'marker', 'describeEndpointTypesResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeEndpointTypesResponse_httpStatus' - The response's http status code.
newDescribeEndpointTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEndpointTypesResponse
newDescribeEndpointTypesResponse pHttpStatus_ =
  DescribeEndpointTypesResponse'
    { supportedEndpointTypes =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The types of endpoints that are supported.
describeEndpointTypesResponse_supportedEndpointTypes :: Lens.Lens' DescribeEndpointTypesResponse (Core.Maybe [SupportedEndpointType])
describeEndpointTypesResponse_supportedEndpointTypes = Lens.lens (\DescribeEndpointTypesResponse' {supportedEndpointTypes} -> supportedEndpointTypes) (\s@DescribeEndpointTypesResponse' {} a -> s {supportedEndpointTypes = a} :: DescribeEndpointTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointTypesResponse_marker :: Lens.Lens' DescribeEndpointTypesResponse (Core.Maybe Core.Text)
describeEndpointTypesResponse_marker = Lens.lens (\DescribeEndpointTypesResponse' {marker} -> marker) (\s@DescribeEndpointTypesResponse' {} a -> s {marker = a} :: DescribeEndpointTypesResponse)

-- | The response's http status code.
describeEndpointTypesResponse_httpStatus :: Lens.Lens' DescribeEndpointTypesResponse Core.Int
describeEndpointTypesResponse_httpStatus = Lens.lens (\DescribeEndpointTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointTypesResponse' {} a -> s {httpStatus = a} :: DescribeEndpointTypesResponse)

instance Core.NFData DescribeEndpointTypesResponse
