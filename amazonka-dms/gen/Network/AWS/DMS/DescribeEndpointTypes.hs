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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEndpointTypes' smart constructor.
data DescribeEndpointTypes = DescribeEndpointTypes'
  { -- | Filters applied to the endpoint types.
    --
    -- Valid filter names: engine-name | endpoint-type
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to the endpoint types.
--
-- Valid filter names: engine-name | endpoint-type
describeEndpointTypes_filters :: Lens.Lens' DescribeEndpointTypes (Prelude.Maybe [Filter])
describeEndpointTypes_filters = Lens.lens (\DescribeEndpointTypes' {filters} -> filters) (\s@DescribeEndpointTypes' {} a -> s {filters = a} :: DescribeEndpointTypes) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointTypes_marker :: Lens.Lens' DescribeEndpointTypes (Prelude.Maybe Prelude.Text)
describeEndpointTypes_marker = Lens.lens (\DescribeEndpointTypes' {marker} -> marker) (\s@DescribeEndpointTypes' {} a -> s {marker = a} :: DescribeEndpointTypes)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEndpointTypes_maxRecords :: Lens.Lens' DescribeEndpointTypes (Prelude.Maybe Prelude.Int)
describeEndpointTypes_maxRecords = Lens.lens (\DescribeEndpointTypes' {maxRecords} -> maxRecords) (\s@DescribeEndpointTypes' {} a -> s {maxRecords = a} :: DescribeEndpointTypes)

instance Core.AWSPager DescribeEndpointTypes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEndpointTypesResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEndpointTypesResponse_supportedEndpointTypes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEndpointTypes_marker
          Lens..~ rs
          Lens.^? describeEndpointTypesResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeEndpointTypes where
  type
    AWSResponse DescribeEndpointTypes =
      DescribeEndpointTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointTypesResponse'
            Prelude.<$> ( x Core..?> "SupportedEndpointTypes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpointTypes

instance Prelude.NFData DescribeEndpointTypes

instance Core.ToHeaders DescribeEndpointTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeEndpointTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEndpointTypes where
  toJSON DescribeEndpointTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("Marker" Core..=) Prelude.<$> marker,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeEndpointTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEndpointTypes where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeEndpointTypesResponse' smart constructor.
data DescribeEndpointTypesResponse = DescribeEndpointTypesResponse'
  { -- | The types of endpoints that are supported.
    supportedEndpointTypes :: Prelude.Maybe [SupportedEndpointType],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEndpointTypesResponse
newDescribeEndpointTypesResponse pHttpStatus_ =
  DescribeEndpointTypesResponse'
    { supportedEndpointTypes =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The types of endpoints that are supported.
describeEndpointTypesResponse_supportedEndpointTypes :: Lens.Lens' DescribeEndpointTypesResponse (Prelude.Maybe [SupportedEndpointType])
describeEndpointTypesResponse_supportedEndpointTypes = Lens.lens (\DescribeEndpointTypesResponse' {supportedEndpointTypes} -> supportedEndpointTypes) (\s@DescribeEndpointTypesResponse' {} a -> s {supportedEndpointTypes = a} :: DescribeEndpointTypesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeEndpointTypesResponse_marker :: Lens.Lens' DescribeEndpointTypesResponse (Prelude.Maybe Prelude.Text)
describeEndpointTypesResponse_marker = Lens.lens (\DescribeEndpointTypesResponse' {marker} -> marker) (\s@DescribeEndpointTypesResponse' {} a -> s {marker = a} :: DescribeEndpointTypesResponse)

-- | The response's http status code.
describeEndpointTypesResponse_httpStatus :: Lens.Lens' DescribeEndpointTypesResponse Prelude.Int
describeEndpointTypesResponse_httpStatus = Lens.lens (\DescribeEndpointTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointTypesResponse' {} a -> s {httpStatus = a} :: DescribeEndpointTypesResponse)

instance Prelude.NFData DescribeEndpointTypesResponse
