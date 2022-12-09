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
-- Module      : Amazonka.Neptune.DescribeDBClusterParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB cluster
-- parameter group.
--
-- This operation returns paginated results.
module Amazonka.Neptune.DescribeDBClusterParameters
  ( -- * Creating a Request
    DescribeDBClusterParameters (..),
    newDescribeDBClusterParameters,

    -- * Request Lenses
    describeDBClusterParameters_filters,
    describeDBClusterParameters_marker,
    describeDBClusterParameters_maxRecords,
    describeDBClusterParameters_source,
    describeDBClusterParameters_dbClusterParameterGroupName,

    -- * Destructuring the Response
    DescribeDBClusterParametersResponse (..),
    newDescribeDBClusterParametersResponse,

    -- * Response Lenses
    describeDBClusterParametersResponse_marker,
    describeDBClusterParametersResponse_parameters,
    describeDBClusterParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBClusterParameters' smart constructor.
data DescribeDBClusterParameters = DescribeDBClusterParameters'
  { -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeDBClusterParameters@ request. If this parameter is specified,
    -- the response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates to return only parameters for a specific source.
    -- Parameter sources can be @engine@, @service@, or @customer@.
    source :: Prelude.Maybe Prelude.Text,
    -- | The name of a specific DB cluster parameter group to return parameter
    -- details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeDBClusterParameters_filters' - This parameter is not currently supported.
--
-- 'marker', 'describeDBClusterParameters_marker' - An optional pagination token provided by a previous
-- @DescribeDBClusterParameters@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'maxRecords', 'describeDBClusterParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'source', 'describeDBClusterParameters_source' - A value that indicates to return only parameters for a specific source.
-- Parameter sources can be @engine@, @service@, or @customer@.
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
      { filters =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        maxRecords = Prelude.Nothing,
        source = Prelude.Nothing,
        dbClusterParameterGroupName =
          pDBClusterParameterGroupName_
      }

-- | This parameter is not currently supported.
describeDBClusterParameters_filters :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe [Filter])
describeDBClusterParameters_filters = Lens.lens (\DescribeDBClusterParameters' {filters} -> filters) (\s@DescribeDBClusterParameters' {} a -> s {filters = a} :: DescribeDBClusterParameters) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeDBClusterParameters@ request. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBClusterParameters_marker :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe Prelude.Text)
describeDBClusterParameters_marker = Lens.lens (\DescribeDBClusterParameters' {marker} -> marker) (\s@DescribeDBClusterParameters' {} a -> s {marker = a} :: DescribeDBClusterParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBClusterParameters_maxRecords :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe Prelude.Int)
describeDBClusterParameters_maxRecords = Lens.lens (\DescribeDBClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeDBClusterParameters' {} a -> s {maxRecords = a} :: DescribeDBClusterParameters)

-- | A value that indicates to return only parameters for a specific source.
-- Parameter sources can be @engine@, @service@, or @customer@.
describeDBClusterParameters_source :: Lens.Lens' DescribeDBClusterParameters (Prelude.Maybe Prelude.Text)
describeDBClusterParameters_source = Lens.lens (\DescribeDBClusterParameters' {source} -> source) (\s@DescribeDBClusterParameters' {} a -> s {source = a} :: DescribeDBClusterParameters)

-- | The name of a specific DB cluster parameter group to return parameter
-- details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
describeDBClusterParameters_dbClusterParameterGroupName :: Lens.Lens' DescribeDBClusterParameters Prelude.Text
describeDBClusterParameters_dbClusterParameterGroupName = Lens.lens (\DescribeDBClusterParameters' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@DescribeDBClusterParameters' {} a -> s {dbClusterParameterGroupName = a} :: DescribeDBClusterParameters)

instance Core.AWSPager DescribeDBClusterParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBClusterParametersResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBClusterParametersResponse_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBClusterParameters_marker
          Lens..~ rs
          Lens.^? describeDBClusterParametersResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBClusterParameters where
  type
    AWSResponse DescribeDBClusterParameters =
      DescribeDBClusterParametersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBClusterParametersResult"
      ( \s h x ->
          DescribeDBClusterParametersResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "Parameters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Parameter")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBClusterParameters where
  hashWithSalt _salt DescribeDBClusterParameters' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` dbClusterParameterGroupName

instance Prelude.NFData DescribeDBClusterParameters where
  rnf DescribeDBClusterParameters' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName

instance Data.ToHeaders DescribeDBClusterParameters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBClusterParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBClusterParameters where
  toQuery DescribeDBClusterParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDBClusterParameters" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "Source" Data.=: source,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName
      ]

-- | /See:/ 'newDescribeDBClusterParametersResponse' smart constructor.
data DescribeDBClusterParametersResponse = DescribeDBClusterParametersResponse'
  { -- | An optional pagination token provided by a previous
    -- DescribeDBClusterParameters request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@ .
    marker :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of parameters for the DB cluster parameter group.
    parameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBClusterParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBClusterParametersResponse_marker' - An optional pagination token provided by a previous
-- DescribeDBClusterParameters request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
--
-- 'parameters', 'describeDBClusterParametersResponse_parameters' - Provides a list of parameters for the DB cluster parameter group.
--
-- 'httpStatus', 'describeDBClusterParametersResponse_httpStatus' - The response's http status code.
newDescribeDBClusterParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBClusterParametersResponse
newDescribeDBClusterParametersResponse pHttpStatus_ =
  DescribeDBClusterParametersResponse'
    { marker =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous
-- DescribeDBClusterParameters request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@ .
describeDBClusterParametersResponse_marker :: Lens.Lens' DescribeDBClusterParametersResponse (Prelude.Maybe Prelude.Text)
describeDBClusterParametersResponse_marker = Lens.lens (\DescribeDBClusterParametersResponse' {marker} -> marker) (\s@DescribeDBClusterParametersResponse' {} a -> s {marker = a} :: DescribeDBClusterParametersResponse)

-- | Provides a list of parameters for the DB cluster parameter group.
describeDBClusterParametersResponse_parameters :: Lens.Lens' DescribeDBClusterParametersResponse (Prelude.Maybe [Parameter])
describeDBClusterParametersResponse_parameters = Lens.lens (\DescribeDBClusterParametersResponse' {parameters} -> parameters) (\s@DescribeDBClusterParametersResponse' {} a -> s {parameters = a} :: DescribeDBClusterParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBClusterParametersResponse_httpStatus :: Lens.Lens' DescribeDBClusterParametersResponse Prelude.Int
describeDBClusterParametersResponse_httpStatus = Lens.lens (\DescribeDBClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeDBClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeDBClusterParametersResponse)

instance
  Prelude.NFData
    DescribeDBClusterParametersResponse
  where
  rnf DescribeDBClusterParametersResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
