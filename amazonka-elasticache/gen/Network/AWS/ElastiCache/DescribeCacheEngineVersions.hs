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
-- Module      : Network.AWS.ElastiCache.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available cache engines and their versions.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheEngineVersions
  ( -- * Creating a Request
    DescribeCacheEngineVersions (..),
    newDescribeCacheEngineVersions,

    -- * Request Lenses
    describeCacheEngineVersions_defaultOnly,
    describeCacheEngineVersions_engineVersion,
    describeCacheEngineVersions_cacheParameterGroupFamily,
    describeCacheEngineVersions_engine,
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheEngineVersions@ operation.
--
-- /See:/ 'newDescribeCacheEngineVersions' smart constructor.
data DescribeCacheEngineVersions = DescribeCacheEngineVersions'
  { -- | If @true@, specifies that only the default version of the specified
    -- engine or engine and major version combination is to be returned.
    defaultOnly :: Core.Maybe Core.Bool,
    -- | The cache engine version to return.
    --
    -- Example: @1.4.14@
    engineVersion :: Core.Maybe Core.Text,
    -- | The name of a specific cache parameter group family to return details
    -- for.
    --
    -- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
    -- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
    -- @redis6.x@ |
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 alphanumeric characters
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    cacheParameterGroupFamily :: Core.Maybe Core.Text,
    -- | The cache engine to return. Valid values: @memcached@ | @redis@
    engine :: Core.Maybe Core.Text,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a marker is
    -- included in the response so that the remaining results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: minimum 20; maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCacheEngineVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultOnly', 'describeCacheEngineVersions_defaultOnly' - If @true@, specifies that only the default version of the specified
-- engine or engine and major version combination is to be returned.
--
-- 'engineVersion', 'describeCacheEngineVersions_engineVersion' - The cache engine version to return.
--
-- Example: @1.4.14@
--
-- 'cacheParameterGroupFamily', 'describeCacheEngineVersions_cacheParameterGroupFamily' - The name of a specific cache parameter group family to return details
-- for.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- 'engine', 'describeCacheEngineVersions_engine' - The cache engine to return. Valid values: @memcached@ | @redis@
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
    { defaultOnly =
        Core.Nothing,
      engineVersion = Core.Nothing,
      cacheParameterGroupFamily = Core.Nothing,
      engine = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | If @true@, specifies that only the default version of the specified
-- engine or engine and major version combination is to be returned.
describeCacheEngineVersions_defaultOnly :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Bool)
describeCacheEngineVersions_defaultOnly = Lens.lens (\DescribeCacheEngineVersions' {defaultOnly} -> defaultOnly) (\s@DescribeCacheEngineVersions' {} a -> s {defaultOnly = a} :: DescribeCacheEngineVersions)

-- | The cache engine version to return.
--
-- Example: @1.4.14@
describeCacheEngineVersions_engineVersion :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Text)
describeCacheEngineVersions_engineVersion = Lens.lens (\DescribeCacheEngineVersions' {engineVersion} -> engineVersion) (\s@DescribeCacheEngineVersions' {} a -> s {engineVersion = a} :: DescribeCacheEngineVersions)

-- | The name of a specific cache parameter group family to return details
-- for.
--
-- Valid values are: @memcached1.4@ | @memcached1.5@ | @memcached1.6@ |
-- @redis2.6@ | @redis2.8@ | @redis3.2@ | @redis4.0@ | @redis5.0@ |
-- @redis6.x@ |
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
describeCacheEngineVersions_cacheParameterGroupFamily :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Text)
describeCacheEngineVersions_cacheParameterGroupFamily = Lens.lens (\DescribeCacheEngineVersions' {cacheParameterGroupFamily} -> cacheParameterGroupFamily) (\s@DescribeCacheEngineVersions' {} a -> s {cacheParameterGroupFamily = a} :: DescribeCacheEngineVersions)

-- | The cache engine to return. Valid values: @memcached@ | @redis@
describeCacheEngineVersions_engine :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Text)
describeCacheEngineVersions_engine = Lens.lens (\DescribeCacheEngineVersions' {engine} -> engine) (\s@DescribeCacheEngineVersions' {} a -> s {engine = a} :: DescribeCacheEngineVersions)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheEngineVersions_marker :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Text)
describeCacheEngineVersions_marker = Lens.lens (\DescribeCacheEngineVersions' {marker} -> marker) (\s@DescribeCacheEngineVersions' {} a -> s {marker = a} :: DescribeCacheEngineVersions)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheEngineVersions_maxRecords :: Lens.Lens' DescribeCacheEngineVersions (Core.Maybe Core.Int)
describeCacheEngineVersions_maxRecords = Lens.lens (\DescribeCacheEngineVersions' {maxRecords} -> maxRecords) (\s@DescribeCacheEngineVersions' {} a -> s {maxRecords = a} :: DescribeCacheEngineVersions)

instance Core.AWSPager DescribeCacheEngineVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheEngineVersionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheEngineVersionsResponse_cacheEngineVersions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCacheEngineVersions_marker
          Lens..~ rs
          Lens.^? describeCacheEngineVersionsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeCacheEngineVersions where
  type
    AWSResponse DescribeCacheEngineVersions =
      DescribeCacheEngineVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeCacheEngineVersionsResult"
      ( \s h x ->
          DescribeCacheEngineVersionsResponse'
            Core.<$> ( x Core..@? "CacheEngineVersions"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "CacheEngineVersion")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCacheEngineVersions

instance Core.NFData DescribeCacheEngineVersions

instance Core.ToHeaders DescribeCacheEngineVersions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCacheEngineVersions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCacheEngineVersions where
  toQuery DescribeCacheEngineVersions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeCacheEngineVersions" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "DefaultOnly" Core.=: defaultOnly,
        "EngineVersion" Core.=: engineVersion,
        "CacheParameterGroupFamily"
          Core.=: cacheParameterGroupFamily,
        "Engine" Core.=: engine,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of a DescribeCacheEngineVersions operation.
--
-- /See:/ 'newDescribeCacheEngineVersionsResponse' smart constructor.
data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse'
  { -- | A list of cache engine version details. Each element in the list
    -- contains detailed information about one cache engine version.
    cacheEngineVersions :: Core.Maybe [CacheEngineVersion],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeCacheEngineVersionsResponse
newDescribeCacheEngineVersionsResponse pHttpStatus_ =
  DescribeCacheEngineVersionsResponse'
    { cacheEngineVersions =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache engine version details. Each element in the list
-- contains detailed information about one cache engine version.
describeCacheEngineVersionsResponse_cacheEngineVersions :: Lens.Lens' DescribeCacheEngineVersionsResponse (Core.Maybe [CacheEngineVersion])
describeCacheEngineVersionsResponse_cacheEngineVersions = Lens.lens (\DescribeCacheEngineVersionsResponse' {cacheEngineVersions} -> cacheEngineVersions) (\s@DescribeCacheEngineVersionsResponse' {} a -> s {cacheEngineVersions = a} :: DescribeCacheEngineVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheEngineVersionsResponse_marker :: Lens.Lens' DescribeCacheEngineVersionsResponse (Core.Maybe Core.Text)
describeCacheEngineVersionsResponse_marker = Lens.lens (\DescribeCacheEngineVersionsResponse' {marker} -> marker) (\s@DescribeCacheEngineVersionsResponse' {} a -> s {marker = a} :: DescribeCacheEngineVersionsResponse)

-- | The response's http status code.
describeCacheEngineVersionsResponse_httpStatus :: Lens.Lens' DescribeCacheEngineVersionsResponse Core.Int
describeCacheEngineVersionsResponse_httpStatus = Lens.lens (\DescribeCacheEngineVersionsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheEngineVersionsResponse' {} a -> s {httpStatus = a} :: DescribeCacheEngineVersionsResponse)

instance
  Core.NFData
    DescribeCacheEngineVersionsResponse
