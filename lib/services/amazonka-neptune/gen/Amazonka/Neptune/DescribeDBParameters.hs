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
-- Module      : Amazonka.Neptune.DescribeDBParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular DB parameter group.
--
-- This operation returns paginated results.
module Amazonka.Neptune.DescribeDBParameters
  ( -- * Creating a Request
    DescribeDBParameters (..),
    newDescribeDBParameters,

    -- * Request Lenses
    describeDBParameters_marker,
    describeDBParameters_filters,
    describeDBParameters_maxRecords,
    describeDBParameters_source,
    describeDBParameters_dbParameterGroupName,

    -- * Destructuring the Response
    DescribeDBParametersResponse (..),
    newDescribeDBParametersResponse,

    -- * Response Lenses
    describeDBParametersResponse_marker,
    describeDBParametersResponse_parameters,
    describeDBParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDBParameters' smart constructor.
data DescribeDBParameters = DescribeDBParameters'
  { -- | An optional pagination token provided by a previous
    -- @DescribeDBParameters@ request. If this parameter is specified, the
    -- response includes only records beyond the marker, up to the value
    -- specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | This parameter is not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The parameter types to return.
    --
    -- Default: All parameter types returned
    --
    -- Valid Values: @user | system | engine-default@
    source :: Prelude.Maybe Prelude.Text,
    -- | The name of a specific DB parameter group to return details for.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing DBParameterGroup.
    dbParameterGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBParameters_marker' - An optional pagination token provided by a previous
-- @DescribeDBParameters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
--
-- 'filters', 'describeDBParameters_filters' - This parameter is not currently supported.
--
-- 'maxRecords', 'describeDBParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'source', 'describeDBParameters_source' - The parameter types to return.
--
-- Default: All parameter types returned
--
-- Valid Values: @user | system | engine-default@
--
-- 'dbParameterGroupName', 'describeDBParameters_dbParameterGroupName' - The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DBParameterGroup.
newDescribeDBParameters ::
  -- | 'dbParameterGroupName'
  Prelude.Text ->
  DescribeDBParameters
newDescribeDBParameters pDBParameterGroupName_ =
  DescribeDBParameters'
    { marker = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      source = Prelude.Nothing,
      dbParameterGroupName = pDBParameterGroupName_
    }

-- | An optional pagination token provided by a previous
-- @DescribeDBParameters@ request. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value
-- specified by @MaxRecords@.
describeDBParameters_marker :: Lens.Lens' DescribeDBParameters (Prelude.Maybe Prelude.Text)
describeDBParameters_marker = Lens.lens (\DescribeDBParameters' {marker} -> marker) (\s@DescribeDBParameters' {} a -> s {marker = a} :: DescribeDBParameters)

-- | This parameter is not currently supported.
describeDBParameters_filters :: Lens.Lens' DescribeDBParameters (Prelude.Maybe [Filter])
describeDBParameters_filters = Lens.lens (\DescribeDBParameters' {filters} -> filters) (\s@DescribeDBParameters' {} a -> s {filters = a} :: DescribeDBParameters) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeDBParameters_maxRecords :: Lens.Lens' DescribeDBParameters (Prelude.Maybe Prelude.Int)
describeDBParameters_maxRecords = Lens.lens (\DescribeDBParameters' {maxRecords} -> maxRecords) (\s@DescribeDBParameters' {} a -> s {maxRecords = a} :: DescribeDBParameters)

-- | The parameter types to return.
--
-- Default: All parameter types returned
--
-- Valid Values: @user | system | engine-default@
describeDBParameters_source :: Lens.Lens' DescribeDBParameters (Prelude.Maybe Prelude.Text)
describeDBParameters_source = Lens.lens (\DescribeDBParameters' {source} -> source) (\s@DescribeDBParameters' {} a -> s {source = a} :: DescribeDBParameters)

-- | The name of a specific DB parameter group to return details for.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DBParameterGroup.
describeDBParameters_dbParameterGroupName :: Lens.Lens' DescribeDBParameters Prelude.Text
describeDBParameters_dbParameterGroupName = Lens.lens (\DescribeDBParameters' {dbParameterGroupName} -> dbParameterGroupName) (\s@DescribeDBParameters' {} a -> s {dbParameterGroupName = a} :: DescribeDBParameters)

instance Core.AWSPager DescribeDBParameters where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDBParametersResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDBParametersResponse_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDBParameters_marker
          Lens..~ rs
          Lens.^? describeDBParametersResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDBParameters where
  type
    AWSResponse DescribeDBParameters =
      DescribeDBParametersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDBParametersResult"
      ( \s h x ->
          DescribeDBParametersResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "Parameters" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "Parameter")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDBParameters where
  hashWithSalt _salt DescribeDBParameters' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` dbParameterGroupName

instance Prelude.NFData DescribeDBParameters where
  rnf DescribeDBParameters' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf dbParameterGroupName

instance Data.ToHeaders DescribeDBParameters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDBParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDBParameters where
  toQuery DescribeDBParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeDBParameters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Data.=: maxRecords,
        "Source" Data.=: source,
        "DBParameterGroupName" Data.=: dbParameterGroupName
      ]

-- | /See:/ 'newDescribeDBParametersResponse' smart constructor.
data DescribeDBParametersResponse = DescribeDBParametersResponse'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | A list of Parameter values.
    parameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDBParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDBParametersResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'parameters', 'describeDBParametersResponse_parameters' - A list of Parameter values.
--
-- 'httpStatus', 'describeDBParametersResponse_httpStatus' - The response's http status code.
newDescribeDBParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDBParametersResponse
newDescribeDBParametersResponse pHttpStatus_ =
  DescribeDBParametersResponse'
    { marker =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeDBParametersResponse_marker :: Lens.Lens' DescribeDBParametersResponse (Prelude.Maybe Prelude.Text)
describeDBParametersResponse_marker = Lens.lens (\DescribeDBParametersResponse' {marker} -> marker) (\s@DescribeDBParametersResponse' {} a -> s {marker = a} :: DescribeDBParametersResponse)

-- | A list of Parameter values.
describeDBParametersResponse_parameters :: Lens.Lens' DescribeDBParametersResponse (Prelude.Maybe [Parameter])
describeDBParametersResponse_parameters = Lens.lens (\DescribeDBParametersResponse' {parameters} -> parameters) (\s@DescribeDBParametersResponse' {} a -> s {parameters = a} :: DescribeDBParametersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDBParametersResponse_httpStatus :: Lens.Lens' DescribeDBParametersResponse Prelude.Int
describeDBParametersResponse_httpStatus = Lens.lens (\DescribeDBParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeDBParametersResponse' {} a -> s {httpStatus = a} :: DescribeDBParametersResponse)

instance Prelude.NFData DescribeDBParametersResponse where
  rnf DescribeDBParametersResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf httpStatus
