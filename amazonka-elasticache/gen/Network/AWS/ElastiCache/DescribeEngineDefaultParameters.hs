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
-- Module      : Network.AWS.ElastiCache.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the
-- specified cache engine.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
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

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    -- @redis6.x@ |
    cacheParameterGroupFamily :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- @redis6.x@ |
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
-- @redis6.x@ |
describeEngineDefaultParameters_cacheParameterGroupFamily :: Lens.Lens' DescribeEngineDefaultParameters Prelude.Text
describeEngineDefaultParameters_cacheParameterGroupFamily = Lens.lens (\DescribeEngineDefaultParameters' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@DescribeEngineDefaultParameters' {} a -> s {cacheParameterGroupFamily = a} :: DescribeEngineDefaultParameters)

instance
  Pager.AWSPager
    DescribeEngineDefaultParameters
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeEngineDefaultParametersResponse_engineDefaults
              Prelude.. engineDefaults_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeEngineDefaultParametersResponse_engineDefaults
              Prelude.. engineDefaults_parameters
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeEngineDefaultParameters_marker
          Lens..~ rs
          Lens.^? describeEngineDefaultParametersResponse_engineDefaults
            Prelude.. engineDefaults_marker
            Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeEngineDefaultParameters
  where
  type
    Rs DescribeEngineDefaultParameters =
      DescribeEngineDefaultParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEngineDefaultParametersResult"
      ( \s h x ->
          DescribeEngineDefaultParametersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "EngineDefaults")
      )

instance
  Prelude.Hashable
    DescribeEngineDefaultParameters

instance
  Prelude.NFData
    DescribeEngineDefaultParameters

instance
  Prelude.ToHeaders
    DescribeEngineDefaultParameters
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DescribeEngineDefaultParameters
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeEngineDefaultParameters
  where
  toQuery DescribeEngineDefaultParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DescribeEngineDefaultParameters" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "Marker" Prelude.=: marker,
        "MaxRecords" Prelude.=: maxRecords,
        "CacheParameterGroupFamily"
          Prelude.=: cacheParameterGroupFamily
      ]

-- | /See:/ 'newDescribeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    engineDefaults :: EngineDefaults
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
