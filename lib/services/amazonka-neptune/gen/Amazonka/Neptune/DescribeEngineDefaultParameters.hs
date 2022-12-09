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
-- Module      : Amazonka.Neptune.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the
-- specified database engine.
--
-- This operation returns paginated results.
module Amazonka.Neptune.DescribeEngineDefaultParameters
  ( -- * Creating a Request
    DescribeEngineDefaultParameters (..),
    newDescribeEngineDefaultParameters,

    -- * Request Lenses
    describeEngineDefaultParameters_filters,
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_dbParameterGroupFamily,

    -- * Destructuring the Response
    DescribeEngineDefaultParametersResponse (..),
    newDescribeEngineDefaultParametersResponse,

    -- * Response Lenses
    describeEngineDefaultParametersResponse_engineDefaults,
    describeEngineDefaultParametersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { -- | Not currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeEngineDefaultParameters@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
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
    -- | The name of the DB parameter group family.
    dbParameterGroupFamily :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEngineDefaultParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeEngineDefaultParameters_filters' - Not currently supported.
--
-- 'marker', 'describeEngineDefaultParameters_marker' - An optional pagination token provided by a previous
-- @DescribeEngineDefaultParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEngineDefaultParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbParameterGroupFamily', 'describeEngineDefaultParameters_dbParameterGroupFamily' - The name of the DB parameter group family.
newDescribeEngineDefaultParameters ::
  -- | 'dbParameterGroupFamily'
  Prelude.Text ->
  DescribeEngineDefaultParameters
newDescribeEngineDefaultParameters
  pDBParameterGroupFamily_ =
    DescribeEngineDefaultParameters'
      { filters =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        maxRecords = Prelude.Nothing,
        dbParameterGroupFamily =
          pDBParameterGroupFamily_
      }

-- | Not currently supported.
describeEngineDefaultParameters_filters :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe [Filter])
describeEngineDefaultParameters_filters = Lens.lens (\DescribeEngineDefaultParameters' {filters} -> filters) (\s@DescribeEngineDefaultParameters' {} a -> s {filters = a} :: DescribeEngineDefaultParameters) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous
-- @DescribeEngineDefaultParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeEngineDefaultParameters_marker :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe Prelude.Text)
describeEngineDefaultParameters_marker = Lens.lens (\DescribeEngineDefaultParameters' {marker} -> marker) (\s@DescribeEngineDefaultParameters' {} a -> s {marker = a} :: DescribeEngineDefaultParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEngineDefaultParameters_maxRecords :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe Prelude.Int)
describeEngineDefaultParameters_maxRecords = Lens.lens (\DescribeEngineDefaultParameters' {maxRecords} -> maxRecords) (\s@DescribeEngineDefaultParameters' {} a -> s {maxRecords = a} :: DescribeEngineDefaultParameters)

-- | The name of the DB parameter group family.
describeEngineDefaultParameters_dbParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultParameters Prelude.Text
describeEngineDefaultParameters_dbParameterGroupFamily = Lens.lens (\DescribeEngineDefaultParameters' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DescribeEngineDefaultParameters' {} a -> s {dbParameterGroupFamily = a} :: DescribeEngineDefaultParameters)

instance
  Core.AWSPager
    DescribeEngineDefaultParameters
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEngineDefaultParametersResponse_engineDefaults
              Prelude.. Lens._Just
              Prelude.. engineDefaults_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEngineDefaultParametersResponse_engineDefaults
              Prelude.. Lens._Just
              Prelude.. engineDefaults_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeEngineDefaultParameters_marker
          Lens..~ rs
          Lens.^? describeEngineDefaultParametersResponse_engineDefaults
            Prelude.. Lens._Just
            Prelude.. engineDefaults_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEngineDefaultParameters
  where
  type
    AWSResponse DescribeEngineDefaultParameters =
      DescribeEngineDefaultParametersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEngineDefaultParametersResult"
      ( \s h x ->
          DescribeEngineDefaultParametersResponse'
            Prelude.<$> (x Data..@? "EngineDefaults")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEngineDefaultParameters
  where
  hashWithSalt
    _salt
    DescribeEngineDefaultParameters' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` dbParameterGroupFamily

instance
  Prelude.NFData
    DescribeEngineDefaultParameters
  where
  rnf DescribeEngineDefaultParameters' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf dbParameterGroupFamily

instance
  Data.ToHeaders
    DescribeEngineDefaultParameters
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeEngineDefaultParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEngineDefaultParameters where
  toQuery DescribeEngineDefaultParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeEngineDefaultParameters" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "DBParameterGroupFamily"
          Data.=: dbParameterGroupFamily
      ]

-- | /See:/ 'newDescribeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { engineDefaults :: Prelude.Maybe EngineDefaults,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEngineDefaultParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineDefaults', 'describeEngineDefaultParametersResponse_engineDefaults' - Undocumented member.
--
-- 'httpStatus', 'describeEngineDefaultParametersResponse_httpStatus' - The response's http status code.
newDescribeEngineDefaultParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEngineDefaultParametersResponse
newDescribeEngineDefaultParametersResponse
  pHttpStatus_ =
    DescribeEngineDefaultParametersResponse'
      { engineDefaults =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeEngineDefaultParametersResponse_engineDefaults :: Lens.Lens' DescribeEngineDefaultParametersResponse (Prelude.Maybe EngineDefaults)
describeEngineDefaultParametersResponse_engineDefaults = Lens.lens (\DescribeEngineDefaultParametersResponse' {engineDefaults} -> engineDefaults) (\s@DescribeEngineDefaultParametersResponse' {} a -> s {engineDefaults = a} :: DescribeEngineDefaultParametersResponse)

-- | The response's http status code.
describeEngineDefaultParametersResponse_httpStatus :: Lens.Lens' DescribeEngineDefaultParametersResponse Prelude.Int
describeEngineDefaultParametersResponse_httpStatus = Lens.lens (\DescribeEngineDefaultParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeEngineDefaultParametersResponse' {} a -> s {httpStatus = a} :: DescribeEngineDefaultParametersResponse)

instance
  Prelude.NFData
    DescribeEngineDefaultParametersResponse
  where
  rnf DescribeEngineDefaultParametersResponse' {..} =
    Prelude.rnf engineDefaults
      `Prelude.seq` Prelude.rnf httpStatus
