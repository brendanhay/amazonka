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
-- Module      : Network.AWS.RDS.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the
-- specified database engine.
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultParameters
  ( -- * Creating a Request
    DescribeEngineDefaultParameters (..),
    newDescribeEngineDefaultParameters,

    -- * Request Lenses
    describeEngineDefaultParameters_filters,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_dbParameterGroupFamily,

    -- * Destructuring the Response
    DescribeEngineDefaultParametersResponse (..),
    newDescribeEngineDefaultParametersResponse,

    -- * Response Lenses
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so you can retrieve the
    -- remaining results.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional pagination token provided by a previous
    -- @DescribeEngineDefaultParameters@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
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
-- 'filters', 'describeEngineDefaultParameters_filters' - This parameter isn\'t currently supported.
--
-- 'maxRecords', 'describeEngineDefaultParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'marker', 'describeEngineDefaultParameters_marker' - An optional pagination token provided by a previous
-- @DescribeEngineDefaultParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
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
        maxRecords = Prelude.Nothing,
        marker = Prelude.Nothing,
        dbParameterGroupFamily =
          pDBParameterGroupFamily_
      }

-- | This parameter isn\'t currently supported.
describeEngineDefaultParameters_filters :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe [Filter])
describeEngineDefaultParameters_filters = Lens.lens (\DescribeEngineDefaultParameters' {filters} -> filters) (\s@DescribeEngineDefaultParameters' {} a -> s {filters = a} :: DescribeEngineDefaultParameters) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEngineDefaultParameters_maxRecords :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe Prelude.Int)
describeEngineDefaultParameters_maxRecords = Lens.lens (\DescribeEngineDefaultParameters' {maxRecords} -> maxRecords) (\s@DescribeEngineDefaultParameters' {} a -> s {maxRecords = a} :: DescribeEngineDefaultParameters)

-- | An optional pagination token provided by a previous
-- @DescribeEngineDefaultParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeEngineDefaultParameters_marker :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe Prelude.Text)
describeEngineDefaultParameters_marker = Lens.lens (\DescribeEngineDefaultParameters' {marker} -> marker) (\s@DescribeEngineDefaultParameters' {} a -> s {marker = a} :: DescribeEngineDefaultParameters)

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
              Prelude.. engineDefaults_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEngineDefaultParametersResponse_engineDefaults
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
            Prelude.. engineDefaults_marker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeEngineDefaultParameters
  where
  type
    AWSResponse DescribeEngineDefaultParameters =
      DescribeEngineDefaultParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEngineDefaultParametersResult"
      ( \s h x ->
          DescribeEngineDefaultParametersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "EngineDefaults")
      )

instance
  Prelude.Hashable
    DescribeEngineDefaultParameters

instance
  Prelude.NFData
    DescribeEngineDefaultParameters

instance
  Core.ToHeaders
    DescribeEngineDefaultParameters
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeEngineDefaultParameters where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEngineDefaultParameters where
  toQuery DescribeEngineDefaultParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeEngineDefaultParameters" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
        "MaxRecords" Core.=: maxRecords,
        "Marker" Core.=: marker,
        "DBParameterGroupFamily"
          Core.=: dbParameterGroupFamily
      ]

-- | /See:/ 'newDescribeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    engineDefaults :: EngineDefaults
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
-- 'httpStatus', 'describeEngineDefaultParametersResponse_httpStatus' - The response's http status code.
--
-- 'engineDefaults', 'describeEngineDefaultParametersResponse_engineDefaults' - Undocumented member.
newDescribeEngineDefaultParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'engineDefaults'
  EngineDefaults ->
  DescribeEngineDefaultParametersResponse
newDescribeEngineDefaultParametersResponse
  pHttpStatus_
  pEngineDefaults_ =
    DescribeEngineDefaultParametersResponse'
      { httpStatus =
          pHttpStatus_,
        engineDefaults = pEngineDefaults_
      }

-- | The response's http status code.
describeEngineDefaultParametersResponse_httpStatus :: Lens.Lens' DescribeEngineDefaultParametersResponse Prelude.Int
describeEngineDefaultParametersResponse_httpStatus = Lens.lens (\DescribeEngineDefaultParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeEngineDefaultParametersResponse' {} a -> s {httpStatus = a} :: DescribeEngineDefaultParametersResponse)

-- | Undocumented member.
describeEngineDefaultParametersResponse_engineDefaults :: Lens.Lens' DescribeEngineDefaultParametersResponse EngineDefaults
describeEngineDefaultParametersResponse_engineDefaults = Lens.lens (\DescribeEngineDefaultParametersResponse' {engineDefaults} -> engineDefaults) (\s@DescribeEngineDefaultParametersResponse' {} a -> s {engineDefaults = a} :: DescribeEngineDefaultParametersResponse)

instance
  Prelude.NFData
    DescribeEngineDefaultParametersResponse
