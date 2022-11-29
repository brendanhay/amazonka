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
-- Module      : Amazonka.ElastiCache.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the
-- specified cache engine.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeEngineDefaultParameters
  ( -- * Creating a Request
    DescribeEngineDefaultParameters (..),
    newDescribeEngineDefaultParameters,

    -- * Request Lenses
    describeEngineDefaultParameters_marker,
    describeEngineDefaultParameters_maxRecords,
    describeEngineDefaultParameters_cacheParameterGroupFamily,

    -- * Destructuring the Response
    DescribeEngineDefaultParametersResponse (..),
    newDescribeEngineDefaultParametersResponse,

    -- * Response Lenses
    describeEngineDefaultParametersResponse_httpStatus,
    describeEngineDefaultParametersResponse_engineDefaults,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeEngineDefaultParameters@ operation.
--
-- /See:/ 'newDescribeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The name of the cache parameter group family.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@ | @redis6.2@
    cacheParameterGroupFamily :: Prelude.Text
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
-- 'marker', 'describeEngineDefaultParameters_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeEngineDefaultParameters_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
--
-- 'cacheParameterGroupFamily', 'describeEngineDefaultParameters_cacheParameterGroupFamily' - The name of the cache parameter group family.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ | @redis6.2@
newDescribeEngineDefaultParameters ::
  -- | 'cacheParameterGroupFamily'
  Prelude.Text ->
  DescribeEngineDefaultParameters
newDescribeEngineDefaultParameters
  pCacheParameterGroupFamily_ =
    DescribeEngineDefaultParameters'
      { marker =
          Prelude.Nothing,
        maxRecords = Prelude.Nothing,
        cacheParameterGroupFamily =
          pCacheParameterGroupFamily_
      }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeEngineDefaultParameters_marker :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe Prelude.Text)
describeEngineDefaultParameters_marker = Lens.lens (\DescribeEngineDefaultParameters' {marker} -> marker) (\s@DescribeEngineDefaultParameters' {} a -> s {marker = a} :: DescribeEngineDefaultParameters)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeEngineDefaultParameters_maxRecords :: Lens.Lens' DescribeEngineDefaultParameters (Prelude.Maybe Prelude.Int)
describeEngineDefaultParameters_maxRecords = Lens.lens (\DescribeEngineDefaultParameters' {maxRecords} -> maxRecords) (\s@DescribeEngineDefaultParameters' {} a -> s {maxRecords = a} :: DescribeEngineDefaultParameters)

-- | The name of the cache parameter group family.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ | @redis6.2@
describeEngineDefaultParameters_cacheParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultParameters Prelude.Text
describeEngineDefaultParameters_cacheParameterGroupFamily = Lens.lens (\DescribeEngineDefaultParameters' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@DescribeEngineDefaultParameters' {} a -> s {cacheParameterGroupFamily = a} :: DescribeEngineDefaultParameters)

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
  request overrides =
    Request.postQuery (overrides defaultService)
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
  where
  hashWithSalt
    _salt
    DescribeEngineDefaultParameters' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` cacheParameterGroupFamily

instance
  Prelude.NFData
    DescribeEngineDefaultParameters
  where
  rnf DescribeEngineDefaultParameters' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf cacheParameterGroupFamily

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
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords,
        "CacheParameterGroupFamily"
          Core.=: cacheParameterGroupFamily
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
  where
  rnf DescribeEngineDefaultParametersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf engineDefaults
