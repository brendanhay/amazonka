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
-- Module      : Network.AWS.RDS.DescribeDBParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB parameter group.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBParameters
  ( -- * Creating a Request
    DescribeDBParameters (..),
    newDescribeDBParameters,

    -- * Request Lenses
    describeDBParameters_source,
    describeDBParameters_filters,
    describeDBParameters_marker,
    describeDBParameters_maxRecords,
    describeDBParameters_dbParameterGroupName,

    -- * Destructuring the Response
    DescribeDBParametersResponse (..),
    newDescribeDBParametersResponse,

    -- * Response Lenses
    describeDBParametersResponse_parameters,
    describeDBParametersResponse_marker,
    describeDBParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDBParameters' smart constructor.
data DescribeDBParameters = DescribeDBParameters'
  { -- | The parameter types to return.
    --
    -- Default: All parameter types returned
    --
    -- Valid Values: @user | system | engine-default@
    source :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBParameters@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of a specific DB parameter group to return details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing DBParameterGroup.
    dbParameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'describeDBParameters_source' - The parameter types to return.
--
-- Default: All parameter types returned
--
-- Valid Values: @user | system | engine-default@
--
-- 'filters', 'describeDBParameters_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeDBParameters_marker' - An optional pagination token provided by a previous
-- @DescribeDBParameters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbParameterGroupName', 'describeDBParameters_dbParameterGroupName' - The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DBParameterGroup.
newDescribeDBParameters ::
  -- | 'dbParameterGroupName'
  Core.Text ->
  DescribeDBParameters
newDescribeDBParameters pDBParameterGroupName_ =
  DescribeDBParameters'
    { source = Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing,
      dbParameterGroupName = pDBParameterGroupName_
    }

-- | The parameter types to return.
--
-- Default: All parameter types returned
--
-- Valid Values: @user | system | engine-default@
describeDBParameters_source :: Lens.Lens' DescribeDBParameters (Core.Maybe Core.Text)
describeDBParameters_source = Lens.lens (\DescribeDBParameters' {source} -> source) (\s@DescribeDBParameters' {} a -> s {source = a} :: DescribeDBParameters)

-- | This parameter isn\'t currently supported.
describeDBParameters_filters :: Lens.Lens' DescribeDBParameters (Core.Maybe [Filter])
describeDBParameters_filters = Lens.lens (\DescribeDBParameters' {filters} -> filters) (\s@DescribeDBParameters' {} a -> s {filters = a} :: DescribeDBParameters) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBParameters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBParameters_marker :: Lens.Lens' DescribeDBParameters (Core.Maybe Core.Text)
describeDBParameters_marker = Lens.lens (\DescribeDBParameters' {marker} -> marker) (\s@DescribeDBParameters' {} a -> s {marker = a} :: DescribeDBParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBParameters_maxRecords :: Lens.Lens' DescribeDBParameters (Core.Maybe Core.Int)
describeDBParameters_maxRecords = Lens.lens (\DescribeDBParameters' {maxRecords} -> maxRecords) (\s@DescribeDBParameters' {} a -> s {maxRecords = a} :: DescribeDBParameters)

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DBParameterGroup.
describeDBParameters_dbParameterGroupName :: Lens.Lens' DescribeDBParameters Core.Text
describeDBParameters_dbParameterGroupName = Lens.lens (\DescribeDBParameters' {dbParameterGroupName} -> dbParameterGroupName) (\s@DescribeDBParameters' {} a -> s {dbParameterGroupName = a} :: DescribeDBParameters)

instance Core.AWSPager DescribeDBParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBParametersResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBParametersResponse_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBParameters_marker
          Lens..~ rs
          Lens.^? describeDBParametersResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeDBParameters where
  type
    AWSResponse DescribeDBParameters =
      DescribeDBParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBParametersResult"
      ( \s h x ->
          DescribeDBParametersResponse'
            Core.<$> ( x Core..@? "Parameters" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Parameter")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBParameters

instance Core.NFData DescribeDBParameters

instance Core.ToHeaders DescribeDBParameters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBParameters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBParameters where
  toQuery DescribeDBParameters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBParameters" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Source" Core.=: source,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBParameterGroupName" Core.=: dbParameterGroupName
      ]

-- | Contains the result of a successful invocation of the
-- @DescribeDBParameters@ action.
--
-- /See:/ 'newDescribeDBParametersResponse' smart constructor.
data DescribeDBParametersResponse = DescribeDBParametersResponse'
  { -- | A list of @Parameter@ values.
    parameters :: Core.Maybe [Parameter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'describeDBParametersResponse_parameters' - A list of @Parameter@ values.
--
-- 'marker', 'describeDBParametersResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeDBParametersResponse_httpStatus' - The response's http status code.
newDescribeDBParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBParametersResponse
newDescribeDBParametersResponse pHttpStatus_ =
  DescribeDBParametersResponse'
    { parameters =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @Parameter@ values.
describeDBParametersResponse_parameters :: Lens.Lens' DescribeDBParametersResponse (Core.Maybe [Parameter])
describeDBParametersResponse_parameters = Lens.lens (\DescribeDBParametersResponse' {parameters} -> parameters) (\s@DescribeDBParametersResponse' {} a -> s {parameters = a} :: DescribeDBParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBParametersResponse_marker :: Lens.Lens' DescribeDBParametersResponse (Core.Maybe Core.Text)
describeDBParametersResponse_marker = Lens.lens (\DescribeDBParametersResponse' {marker} -> marker) (\s@DescribeDBParametersResponse' {} a -> s {marker = a} :: DescribeDBParametersResponse)

-- | The response's http status code.
describeDBParametersResponse_httpStatus :: Lens.Lens' DescribeDBParametersResponse Core.Int
describeDBParametersResponse_httpStatus = Lens.lens (\DescribeDBParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeDBParametersResponse' {} a -> s {httpStatus = a} :: DescribeDBParametersResponse)

instance Core.NFData DescribeDBParametersResponse
