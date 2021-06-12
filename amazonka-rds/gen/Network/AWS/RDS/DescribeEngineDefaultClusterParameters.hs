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
-- Module      : Network.AWS.RDS.DescribeEngineDefaultClusterParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the
-- cluster database engine.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeEngineDefaultClusterParameters
  ( -- * Creating a Request
    DescribeEngineDefaultClusterParameters (..),
    newDescribeEngineDefaultClusterParameters,

    -- * Request Lenses
    describeEngineDefaultClusterParameters_filters,
    describeEngineDefaultClusterParameters_marker,
    describeEngineDefaultClusterParameters_maxRecords,
    describeEngineDefaultClusterParameters_dbParameterGroupFamily,

    -- * Destructuring the Response
    DescribeEngineDefaultClusterParametersResponse (..),
    newDescribeEngineDefaultClusterParametersResponse,

    -- * Response Lenses
    describeEngineDefaultClusterParametersResponse_engineDefaults,
    describeEngineDefaultClusterParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEngineDefaultClusterParameters' smart constructor.
data DescribeEngineDefaultClusterParameters = DescribeEngineDefaultClusterParameters'
  { -- | This parameter isn\'t currently supported.
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous
    -- @DescribeEngineDefaultClusterParameters@ request. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
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
    -- | The name of the DB cluster parameter group family to return engine
    -- parameter information for.
    dbParameterGroupFamily :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEngineDefaultClusterParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeEngineDefaultClusterParameters_filters' - This parameter isn\'t currently supported.
--
-- 'marker', 'describeEngineDefaultClusterParameters_marker' - An optional pagination token provided by a previous
-- @DescribeEngineDefaultClusterParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEngineDefaultClusterParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'dbParameterGroupFamily', 'describeEngineDefaultClusterParameters_dbParameterGroupFamily' - The name of the DB cluster parameter group family to return engine
-- parameter information for.
newDescribeEngineDefaultClusterParameters ::
  -- | 'dbParameterGroupFamily'
  Core.Text ->
  DescribeEngineDefaultClusterParameters
newDescribeEngineDefaultClusterParameters
  pDBParameterGroupFamily_ =
    DescribeEngineDefaultClusterParameters'
      { filters =
          Core.Nothing,
        marker = Core.Nothing,
        maxRecords = Core.Nothing,
        dbParameterGroupFamily =
          pDBParameterGroupFamily_
      }

-- | This parameter isn\'t currently supported.
describeEngineDefaultClusterParameters_filters :: Lens.Lens' DescribeEngineDefaultClusterParameters (Core.Maybe [Filter])
describeEngineDefaultClusterParameters_filters = Lens.lens (\DescribeEngineDefaultClusterParameters' {filters} -> filters) (\s@DescribeEngineDefaultClusterParameters' {} a -> s {filters = a} :: DescribeEngineDefaultClusterParameters) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous
-- @DescribeEngineDefaultClusterParameters@ request. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeEngineDefaultClusterParameters_marker :: Lens.Lens' DescribeEngineDefaultClusterParameters (Core.Maybe Core.Text)
describeEngineDefaultClusterParameters_marker = Lens.lens (\DescribeEngineDefaultClusterParameters' {marker} -> marker) (\s@DescribeEngineDefaultClusterParameters' {} a -> s {marker = a} :: DescribeEngineDefaultClusterParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so you can retrieve the
-- remaining results.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeEngineDefaultClusterParameters_maxRecords :: Lens.Lens' DescribeEngineDefaultClusterParameters (Core.Maybe Core.Int)
describeEngineDefaultClusterParameters_maxRecords = Lens.lens (\DescribeEngineDefaultClusterParameters' {maxRecords} -> maxRecords) (\s@DescribeEngineDefaultClusterParameters' {} a -> s {maxRecords = a} :: DescribeEngineDefaultClusterParameters)

-- | The name of the DB cluster parameter group family to return engine
-- parameter information for.
describeEngineDefaultClusterParameters_dbParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultClusterParameters Core.Text
describeEngineDefaultClusterParameters_dbParameterGroupFamily = Lens.lens (\DescribeEngineDefaultClusterParameters' {dbParameterGroupFamily} -> dbParameterGroupFamily) (\s@DescribeEngineDefaultClusterParameters' {} a -> s {dbParameterGroupFamily = a} :: DescribeEngineDefaultClusterParameters)

instance
  Core.AWSPager
    DescribeEngineDefaultClusterParameters
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEngineDefaultClusterParametersResponse_engineDefaults
              Core.. Lens._Just
              Core.. engineDefaults_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEngineDefaultClusterParametersResponse_engineDefaults
              Core.. Lens._Just
              Core.. engineDefaults_parameters
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEngineDefaultClusterParameters_marker
          Lens..~ rs
          Lens.^? describeEngineDefaultClusterParametersResponse_engineDefaults
            Core.. Lens._Just
            Core.. engineDefaults_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeEngineDefaultClusterParameters
  where
  type
    AWSResponse
      DescribeEngineDefaultClusterParameters =
      DescribeEngineDefaultClusterParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEngineDefaultClusterParametersResult"
      ( \s h x ->
          DescribeEngineDefaultClusterParametersResponse'
            Core.<$> (x Core..@? "EngineDefaults")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeEngineDefaultClusterParameters

instance
  Core.NFData
    DescribeEngineDefaultClusterParameters

instance
  Core.ToHeaders
    DescribeEngineDefaultClusterParameters
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeEngineDefaultClusterParameters
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeEngineDefaultClusterParameters
  where
  toQuery DescribeEngineDefaultClusterParameters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeEngineDefaultClusterParameters" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "Filters"
          Core.=: Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "DBParameterGroupFamily"
          Core.=: dbParameterGroupFamily
      ]

-- | /See:/ 'newDescribeEngineDefaultClusterParametersResponse' smart constructor.
data DescribeEngineDefaultClusterParametersResponse = DescribeEngineDefaultClusterParametersResponse'
  { engineDefaults :: Core.Maybe EngineDefaults,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEngineDefaultClusterParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineDefaults', 'describeEngineDefaultClusterParametersResponse_engineDefaults' - Undocumented member.
--
-- 'httpStatus', 'describeEngineDefaultClusterParametersResponse_httpStatus' - The response's http status code.
newDescribeEngineDefaultClusterParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEngineDefaultClusterParametersResponse
newDescribeEngineDefaultClusterParametersResponse
  pHttpStatus_ =
    DescribeEngineDefaultClusterParametersResponse'
      { engineDefaults =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describeEngineDefaultClusterParametersResponse_engineDefaults :: Lens.Lens' DescribeEngineDefaultClusterParametersResponse (Core.Maybe EngineDefaults)
describeEngineDefaultClusterParametersResponse_engineDefaults = Lens.lens (\DescribeEngineDefaultClusterParametersResponse' {engineDefaults} -> engineDefaults) (\s@DescribeEngineDefaultClusterParametersResponse' {} a -> s {engineDefaults = a} :: DescribeEngineDefaultClusterParametersResponse)

-- | The response's http status code.
describeEngineDefaultClusterParametersResponse_httpStatus :: Lens.Lens' DescribeEngineDefaultClusterParametersResponse Core.Int
describeEngineDefaultClusterParametersResponse_httpStatus = Lens.lens (\DescribeEngineDefaultClusterParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeEngineDefaultClusterParametersResponse' {} a -> s {httpStatus = a} :: DescribeEngineDefaultClusterParametersResponse)

instance
  Core.NFData
    DescribeEngineDefaultClusterParametersResponse
