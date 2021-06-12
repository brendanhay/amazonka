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
-- Module      : Network.AWS.RDS.DescribeDBClusterParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB cluster
-- parameter group.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeDBClusterParameters
  ( -- * Creating a Request
    DescribeDBClusterParameters (..),
    newDescribeDBClusterParameters,

    -- * Request Lenses
    describeDBClusterParameters_source,
    describeDBClusterParameters_filters,
    describeDBClusterParameters_marker,
    describeDBClusterParameters_maxRecords,
    describeDBClusterParameters_dbClusterParameterGroupName,

    -- * Destructuring the Response
    DescribeDBClusterParametersResponse (..),
    newDescribeDBClusterParametersResponse,

    -- * Response Lenses
    describeDBClusterParametersResponse_parameters,
    describeDBClusterParametersResponse_marker,
    describeDBClusterParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusterParameters' smart constructor.
data DescribeDBClusterParameters = DescribeDBClusterParameters'
  { -- | A value that indicates to return only parameters for a specific source.
    -- Parameter sources can be @engine@, @service@, or @customer@.
    source :: Core.Maybe Core.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterParameters@ request. If this parameter is specified,
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
    maxRecords :: Core.Maybe Core.Int,
    -- | The name of a specific DB cluster parameter group to return parameter
    -- details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'source', 'describeDBClusterParameters_source' - A value that indicates to return only parameters for a specific source.
-- Parameter sources can be @engine@, @service@, or @customer@.
--
-- 'filters', 'describeDBClusterParameters_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeDBClusterParameters_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterParameters@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbClusterParameterGroupName', 'describeDBClusterParameters_dbClusterParameterGroupName' - The name of a specific DB cluster parameter group to return parameter
-- details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
newDescribeDBClusterParameters ::
  -- | 'dbClusterParameterGroupName'
  Core.Text ->
  DescribeDBClusterParameters
newDescribeDBClusterParameters
  pDBClusterParameterGroupName_ =
    DescribeDBClusterParameters'
      { source = Core.Nothing,
        filters = Core.Nothing,
        marker = Core.Nothing,
        maxRecords = Core.Nothing,
        dbClusterParameterGroupName =
          pDBClusterParameterGroupName_
      }

-- | A value that indicates to return only parameters for a specific source.
-- Parameter sources can be @engine@, @service@, or @customer@.
describeDBClusterParameters_source :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Core.Text)
describeDBClusterParameters_source = Lens.lens (\DescribeDBClusterParameters' {source} -> source) (\s@DescribeDBClusterParameters' {} a -> s {source = a} :: DescribeDBClusterParameters)

-- | This parameter isn\'t currently supported.
describeDBClusterParameters_filters :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe [Filter])
describeDBClusterParameters_filters = Lens.lens (\DescribeDBClusterParameters' {filters} -> filters) (\s@DescribeDBClusterParameters' {} a -> s {filters = a} :: DescribeDBClusterParameters) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterParameters@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterParameters_marker :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Core.Text)
describeDBClusterParameters_marker = Lens.lens (\DescribeDBClusterParameters' {marker} -> marker) (\s@DescribeDBClusterParameters' {} a -> s {marker = a} :: DescribeDBClusterParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterParameters_maxRecords :: Lens.Lens' DescribeDBClusterParameters (Core.Maybe Core.Int)
describeDBClusterParameters_maxRecords = Lens.lens (\DescribeDBClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeDBClusterParameters' {} a -> s {maxRecords = a} :: DescribeDBClusterParameters)

-- | The name of a specific DB cluster parameter group to return parameter
-- details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
describeDBClusterParameters_dbClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameters Core.Text
describeDBClusterParameters_dbClusterParameterGroupName = Lens.lens (\DescribeDBClusterParameters' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DescribeDBClusterParameters' {} a -> s {dbClusterParameterGroupName = a} :: DescribeDBClusterParameters)

instance Core.AWSPager DescribeDBClusterParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterParametersResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterParametersResponse_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDBClusterParameters_marker
          Lens..~ rs
          Lens.^? describeDBClusterParametersResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeDBClusterParameters where
  type
    AWSResponse DescribeDBClusterParameters =
      DescribeDBClusterParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterParametersResult"
      ( \s h x ->
          DescribeDBClusterParametersResponse'
            Core.<$> ( x Core..@? "Parameters" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "Parameter")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDBClusterParameters

instance Core.NFData DescribeDBClusterParameters

instance Core.ToHeaders DescribeDBClusterParameters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeDBClusterParameters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDBClusterParameters where
  toQuery DescribeDBClusterParameters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeDBClusterParameters" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Source" Core.=: source,
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName
      ]

-- | Provides details about a DB cluster parameter group including the
-- parameters in the DB cluster parameter group.
--
-- /See:/ 'newDescribeDBClusterParametersResponse' smart constructor.
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
  { -- | Provides a list of parameters for the DB cluster parameter group.
    parameters :: Core.Maybe [Parameter],
    -- | An optional pagination token provided by a previous
    -- DescribeDBClusterParameters request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@ .
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDBClusterParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'describeDBClusterParametersResponse_parameters' - Provides a list of parameters for the DB cluster parameter group.
--
-- 'marker', 'describeDBClusterParametersResponse_marker' - An optional pagination token provided by a previous
-- DescribeDBClusterParameters request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
--
-- 'httpStatus', 'describeDBClusterParametersResponse_httpStatus' - The response's http status code.
newDescribeDBClusterParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDBClusterParametersResponse
newDescribeDBClusterParametersResponse pHttpStatus_ =
  DescribeDBClusterParametersResponse'
    { parameters =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides a list of parameters for the DB cluster parameter group.
describeDBClusterParametersResponse_parameters :: Lens.Lens' DescribeDBClusterParametersResponse (Core.Maybe [Parameter])
describeDBClusterParametersResponse_parameters = Lens.lens (\DescribeDBClusterParametersResponse' {parameters} -> parameters) (\s@DescribeDBClusterParametersResponse' {} a -> s {parameters = a} :: DescribeDBClusterParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- DescribeDBClusterParameters request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
describeDBClusterParametersResponse_marker :: Lens.Lens' DescribeDBClusterParametersResponse (Core.Maybe Core.Text)
describeDBClusterParametersResponse_marker = Lens.lens (\DescribeDBClusterParametersResponse' {marker} -> marker) (\s@DescribeDBClusterParametersResponse' {} a -> s {marker = a} :: DescribeDBClusterParametersResponse)

-- | The response's http status code.
describeDBClusterParametersResponse_httpStatus :: Lens.Lens' DescribeDBClusterParametersResponse Core.Int
describeDBClusterParametersResponse_httpStatus = Lens.lens (\DescribeDBClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterParametersResponse)

instance
  Core.NFData
    DescribeDBClusterParametersResponse
