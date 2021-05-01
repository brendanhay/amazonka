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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeDBClusterParameters' smart constructor.
data DescribeDBClusterParameters = DescribeDBClusterParameters'
  { -- | A value that indicates to return only parameters for a specific source.
    -- Parameter sources can be @engine@, @service@, or @customer@.
    source :: Prelude.Maybe Prelude.Text,
    -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterParameters@ request. If this parameter is specified,
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
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of a specific DB cluster parameter group to return parameter
    -- details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeDBClusterParameters
newDescribeDBClusterParameters
  pDBClusterParameterGroupName_ =
    DescribeDBClusterParameters'
      { source =
          Prelude.Nothing,
        filters = Prelude.Nothing,
        marker = Prelude.Nothing,
        maxRecords = Prelude.Nothing,
        dbClusterParameterGroupName =
          pDBClusterParameterGroupName_
      }

-- | A value that indicates to return only parameters for a specific source.
-- Parameter sources can be @engine@, @service@, or @customer@.
describeDBClusterParameters_source :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe Prelude.Text)
describeDBClusterParameters_source = Lens.lens (\DescribeDBClusterParameters' {source} -> source) (\s@DescribeDBClusterParameters' {} a -> s {source = a} :: DescribeDBClusterParameters)

-- | This parameter isn\'t currently supported.
describeDBClusterParameters_filters :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe [Filter])
describeDBClusterParameters_filters = Lens.lens (\DescribeDBClusterParameters' {filters} -> filters) (\s@DescribeDBClusterParameters' {} a -> s {filters = a} :: DescribeDBClusterParameters) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterParameters@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterParameters_marker :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe Prelude.Text)
describeDBClusterParameters_marker = Lens.lens (\DescribeDBClusterParameters' {marker} -> marker) (\s@DescribeDBClusterParameters' {} a -> s {marker = a} :: DescribeDBClusterParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterParameters_maxRecords :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe Prelude.Int)
describeDBClusterParameters_maxRecords = Lens.lens (\DescribeDBClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeDBClusterParameters' {} a -> s {maxRecords = a} :: DescribeDBClusterParameters)

-- | The name of a specific DB cluster parameter group to return parameter
-- details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
describeDBClusterParameters_dbClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameters Prelude.Text
describeDBClusterParameters_dbClusterParameterGroupName = Lens.lens (\DescribeDBClusterParameters' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DescribeDBClusterParameters' {} a -> s {dbClusterParameterGroupName = a} :: DescribeDBClusterParameters)

instance Pager.AWSPager DescribeDBClusterParameters where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeDBClusterParametersResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeDBClusterParametersResponse_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeDBClusterParameters_marker
          Lens..~ rs
          Lens.^? describeDBClusterParametersResponse_marker
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeDBClusterParameters
  where
  type
    Rs DescribeDBClusterParameters =
      DescribeDBClusterParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterParametersResult"
      ( \s h x ->
          DescribeDBClusterParametersResponse'
            Prelude.<$> ( x Prelude..@? "Parameters"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "Parameter")
                        )
            Prelude.<*> (x Prelude..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterParameters

instance Prelude.NFData DescribeDBClusterParameters

instance
  Prelude.ToHeaders
    DescribeDBClusterParameters
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeDBClusterParameters where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeDBClusterParameters where
  toQuery DescribeDBClusterParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeDBClusterParameters" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "Source" Prelude.=: source,
        "Filters"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords,
        "DBClusterParameterGroupName"
          Prelude.=: dbClusterParameterGroupName
      ]

-- | Provides details about a DB cluster parameter group including the
-- parameters in the DB cluster parameter group.
--
-- /See:/ 'newDescribeDBClusterParametersResponse' smart constructor.
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
  { -- | Provides a list of parameters for the DB cluster parameter group.
    parameters :: Prelude.Maybe [Parameter],
    -- | An optional pagination token provided by a previous
    -- DescribeDBClusterParameters request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@ .
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDBClusterParametersResponse
newDescribeDBClusterParametersResponse pHttpStatus_ =
  DescribeDBClusterParametersResponse'
    { parameters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides a list of parameters for the DB cluster parameter group.
describeDBClusterParametersResponse_parameters :: Lens.Lens' DescribeDBClusterParametersResponse (Prelude.Maybe [Parameter])
describeDBClusterParametersResponse_parameters = Lens.lens (\DescribeDBClusterParametersResponse' {parameters} -> parameters) (\s@DescribeDBClusterParametersResponse' {} a -> s {parameters = a} :: DescribeDBClusterParametersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous
-- DescribeDBClusterParameters request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
describeDBClusterParametersResponse_marker :: Lens.Lens' DescribeDBClusterParametersResponse (Prelude.Maybe Prelude.Text)
describeDBClusterParametersResponse_marker = Lens.lens (\DescribeDBClusterParametersResponse' {marker} -> marker) (\s@DescribeDBClusterParametersResponse' {} a -> s {marker = a} :: DescribeDBClusterParametersResponse)

-- | The response's http status code.
describeDBClusterParametersResponse_httpStatus :: Lens.Lens' DescribeDBClusterParametersResponse Prelude.Int
describeDBClusterParametersResponse_httpStatus = Lens.lens (\DescribeDBClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterParametersResponse)

instance
  Prelude.NFData
    DescribeDBClusterParametersResponse
