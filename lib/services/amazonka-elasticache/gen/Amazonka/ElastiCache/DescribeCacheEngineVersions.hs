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
-- Module      : Amazonka.ElastiCache.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available cache engines and their versions.
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeCacheEngineVersions
  ( -- * Creating a Request
    DescribeCacheEngineVersions (..),
    newDescribeCacheEngineVersions,

    -- * Request Lenses
    describeCacheEngineVersions_cacheParameterGroupFamily,
    describeCacheEngineVersions_defaultOnly,
    describeCacheEngineVersions_engine,
    describeCacheEngineVersions_engineVersion,
    describeCacheEngineVersions_marker,
    describeCacheEngineVersions_maxRecords,

    -- * Destructuring the Response
    DescribeCacheEngineVersionsResponse (..),
    newDescribeCacheEngineVersionsResponse,

    -- * Response Lenses
    describeCacheEngineVersionsResponse_cacheEngineVersions,
    describeCacheEngineVersionsResponse_marker,
    describeCacheEngineVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DescribeCacheEngineVersions@ operation.
--
-- /See:/ 'newDescribeCacheEngineVersions' smart constructor.
data DescribeCacheEngineVersions = DescribeCacheEngineVersions'
  { -- | The name of a specific cache parameter group family to return details
    -- for.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@ | @redis6.2@
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    cacheParameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | If @true@, specifies that only the default version of the specified
    -- engine or engine and major version combination is to be returned.
    defaultOnly :: Prelude.Maybe Prelude.Bool,
    -- | The cache engine to return. Valid values: @memcached@ | @redis@
    engine :: Prelude.Maybe Prelude.Text,
    -- | The cache engine version to return.
    --
    -- Example: @1.4.14@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | An optional marker returned from a prior request. Use this marker for
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheEngineVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroupFamily', 'describeCacheEngineVersions_cacheParameterGroupFamily' - The name of a specific cache parameter group family to return details
-- for.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ | @redis6.2@
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- 'defaultOnly', 'describeCacheEngineVersions_defaultOnly' - If @true@, specifies that only the default version of the specified
-- engine or engine and major version combination is to be returned.
--
-- 'engine', 'describeCacheEngineVersions_engine' - The cache engine to return. Valid values: @memcached@ | @redis@
--
-- 'engineVersion', 'describeCacheEngineVersions_engineVersion' - The cache engine version to return.
--
-- Example: @1.4.14@
--
-- 'marker', 'describeCacheEngineVersions_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheEngineVersions_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeCacheEngineVersions ::
  DescribeCacheEngineVersions
newDescribeCacheEngineVersions =
  DescribeCacheEngineVersions'
    { cacheParameterGroupFamily =
        Prelude.Nothing,
      defaultOnly = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The name of a specific cache parameter group family to return details
-- for.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ | @redis6.2@
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
describeCacheEngineVersions_cacheParameterGroupFamily :: Lens.Lens' DescribeCacheEngineVersions (Prelude.Maybe Prelude.Text)
describeCacheEngineVersions_cacheParameterGroupFamily = Lens.lens (\DescribeCacheEngineVersions' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@DescribeCacheEngineVersions' {} a -> s {cacheParameterGroupFamily = a} :: DescribeCacheEngineVersions)

-- | If @true@, specifies that only the default version of the specified
-- engine or engine and major version combination is to be returned.
describeCacheEngineVersions_defaultOnly :: Lens.Lens' DescribeCacheEngineVersions (Prelude.Maybe Prelude.Bool)
describeCacheEngineVersions_defaultOnly = Lens.lens (\DescribeCacheEngineVersions' {defaultOnly} -> defaultOnly) (\s@DescribeCacheEngineVersions' {} a -> s {defaultOnly = a} :: DescribeCacheEngineVersions)

-- | The cache engine to return. Valid values: @memcached@ | @redis@
describeCacheEngineVersions_engine :: Lens.Lens' DescribeCacheEngineVersions (Prelude.Maybe Prelude.Text)
describeCacheEngineVersions_engine = Lens.lens (\DescribeCacheEngineVersions' {engine} -> engine) (\s@DescribeCacheEngineVersions' {} a -> s {engine = a} :: DescribeCacheEngineVersions)

-- | The cache engine version to return.
--
-- Example: @1.4.14@
describeCacheEngineVersions_engineVersion :: Lens.Lens' DescribeCacheEngineVersions (Prelude.Maybe Prelude.Text)
describeCacheEngineVersions_engineVersion = Lens.lens (\DescribeCacheEngineVersions' {engineVersion} -> engineVersion) (\s@DescribeCacheEngineVersions' {} a -> s {engineVersion = a} :: DescribeCacheEngineVersions)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheEngineVersions_marker :: Lens.Lens' DescribeCacheEngineVersions (Prelude.Maybe Prelude.Text)
describeCacheEngineVersions_marker = Lens.lens (\DescribeCacheEngineVersions' {marker} -> marker) (\s@DescribeCacheEngineVersions' {} a -> s {marker = a} :: DescribeCacheEngineVersions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheEngineVersions_maxRecords :: Lens.Lens' DescribeCacheEngineVersions (Prelude.Maybe Prelude.Int)
describeCacheEngineVersions_maxRecords = Lens.lens (\DescribeCacheEngineVersions' {maxRecords} -> maxRecords) (\s@DescribeCacheEngineVersions' {} a -> s {maxRecords = a} :: DescribeCacheEngineVersions)

instance Core.AWSPager DescribeCacheEngineVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheEngineVersionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheEngineVersionsResponse_cacheEngineVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeCacheEngineVersions_marker
          Lens..~ rs
          Lens.^? describeCacheEngineVersionsResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeCacheEngineVersions where
  type
    AWSResponse DescribeCacheEngineVersions =
      DescribeCacheEngineVersionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeCacheEngineVersionsResult"
      ( \s h x ->
          DescribeCacheEngineVersionsResponse'
            Prelude.<$> ( x Data..@? "CacheEngineVersions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "CacheEngineVersion")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCacheEngineVersions where
  hashWithSalt _salt DescribeCacheEngineVersions' {..} =
    _salt
      `Prelude.hashWithSalt` cacheParameterGroupFamily
      `Prelude.hashWithSalt` defaultOnly
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords

instance Prelude.NFData DescribeCacheEngineVersions where
  rnf DescribeCacheEngineVersions' {..} =
    Prelude.rnf cacheParameterGroupFamily
      `Prelude.seq` Prelude.rnf defaultOnly
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance Data.ToHeaders DescribeCacheEngineVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCacheEngineVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCacheEngineVersions where
  toQuery DescribeCacheEngineVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeCacheEngineVersions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheParameterGroupFamily"
          Data.=: cacheParameterGroupFamily,
        "DefaultOnly" Data.=: defaultOnly,
        "Engine" Data.=: engine,
        "EngineVersion" Data.=: engineVersion,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords
      ]

-- | Represents the output of a DescribeCacheEngineVersions operation.
--
-- /See:/ 'newDescribeCacheEngineVersionsResponse' smart constructor.
data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse'
  { -- | A list of cache engine version details. Each element in the list
    -- contains detailed information about one cache engine version.
    cacheEngineVersions :: Prelude.Maybe [CacheEngineVersion],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCacheEngineVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheEngineVersions', 'describeCacheEngineVersionsResponse_cacheEngineVersions' - A list of cache engine version details. Each element in the list
-- contains detailed information about one cache engine version.
--
-- 'marker', 'describeCacheEngineVersionsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeCacheEngineVersionsResponse_httpStatus' - The response's http status code.
newDescribeCacheEngineVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCacheEngineVersionsResponse
newDescribeCacheEngineVersionsResponse pHttpStatus_ =
  DescribeCacheEngineVersionsResponse'
    { cacheEngineVersions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache engine version details. Each element in the list
-- contains detailed information about one cache engine version.
describeCacheEngineVersionsResponse_cacheEngineVersions :: Lens.Lens' DescribeCacheEngineVersionsResponse (Prelude.Maybe [CacheEngineVersion])
describeCacheEngineVersionsResponse_cacheEngineVersions = Lens.lens (\DescribeCacheEngineVersionsResponse' {cacheEngineVersions} -> cacheEngineVersions) (\s@DescribeCacheEngineVersionsResponse' {} a -> s {cacheEngineVersions = a} :: DescribeCacheEngineVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheEngineVersionsResponse_marker :: Lens.Lens' DescribeCacheEngineVersionsResponse (Prelude.Maybe Prelude.Text)
describeCacheEngineVersionsResponse_marker = Lens.lens (\DescribeCacheEngineVersionsResponse' {marker} -> marker) (\s@DescribeCacheEngineVersionsResponse' {} a -> s {marker = a} :: DescribeCacheEngineVersionsResponse)

-- | The response's http status code.
describeCacheEngineVersionsResponse_httpStatus :: Lens.Lens' DescribeCacheEngineVersionsResponse Prelude.Int
describeCacheEngineVersionsResponse_httpStatus = Lens.lens (\DescribeCacheEngineVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheEngineVersionsResponse' {} a -> s {httpStatus = a} :: DescribeCacheEngineVersionsResponse)

instance
  Prelude.NFData
    DescribeCacheEngineVersionsResponse
  where
  rnf DescribeCacheEngineVersionsResponse' {..} =
    Prelude.rnf cacheEngineVersions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
