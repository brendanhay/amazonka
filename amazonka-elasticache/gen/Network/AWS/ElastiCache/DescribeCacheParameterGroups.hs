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
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache parameter group descriptions. If a cache
-- parameter group name is specified, the list contains only the
-- descriptions for that group.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameterGroups
  ( -- * Creating a Request
    DescribeCacheParameterGroups (..),
    newDescribeCacheParameterGroups,

    -- * Request Lenses
    describeCacheParameterGroups_cacheParameterGroupName,
    describeCacheParameterGroups_marker,
    describeCacheParameterGroups_maxRecords,

    -- * Destructuring the Response
    DescribeCacheParameterGroupsResponse (..),
    newDescribeCacheParameterGroupsResponse,

    -- * Response Lenses
    describeCacheParameterGroupsResponse_cacheParameterGroups,
    describeCacheParameterGroupsResponse_marker,
    describeCacheParameterGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'newDescribeCacheParameterGroups' smart constructor.
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
  { -- | The name of a specific cache parameter group to return details for.
    cacheParameterGroupName :: Core.Maybe Core.Text,
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
-- Create a value of 'DescribeCacheParameterGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroupName', 'describeCacheParameterGroups_cacheParameterGroupName' - The name of a specific cache parameter group to return details for.
--
-- 'marker', 'describeCacheParameterGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheParameterGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeCacheParameterGroups ::
  DescribeCacheParameterGroups
newDescribeCacheParameterGroups =
  DescribeCacheParameterGroups'
    { cacheParameterGroupName =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of a specific cache parameter group to return details for.
describeCacheParameterGroups_cacheParameterGroupName :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Core.Text)
describeCacheParameterGroups_cacheParameterGroupName = Lens.lens (\DescribeCacheParameterGroups' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@DescribeCacheParameterGroups' {} a -> s {cacheParameterGroupName = a} :: DescribeCacheParameterGroups)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheParameterGroups_marker :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Core.Text)
describeCacheParameterGroups_marker = Lens.lens (\DescribeCacheParameterGroups' {marker} -> marker) (\s@DescribeCacheParameterGroups' {} a -> s {marker = a} :: DescribeCacheParameterGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheParameterGroups_maxRecords :: Lens.Lens' DescribeCacheParameterGroups (Core.Maybe Core.Int)
describeCacheParameterGroups_maxRecords = Lens.lens (\DescribeCacheParameterGroups' {maxRecords} -> maxRecords) (\s@DescribeCacheParameterGroups' {} a -> s {maxRecords = a} :: DescribeCacheParameterGroups)

instance Core.AWSPager DescribeCacheParameterGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheParameterGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheParameterGroupsResponse_cacheParameterGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCacheParameterGroups_marker
          Lens..~ rs
          Lens.^? describeCacheParameterGroupsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeCacheParameterGroups where
  type
    AWSResponse DescribeCacheParameterGroups =
      DescribeCacheParameterGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeCacheParameterGroupsResult"
      ( \s h x ->
          DescribeCacheParameterGroupsResponse'
            Core.<$> ( x Core..@? "CacheParameterGroups"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "CacheParameterGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCacheParameterGroups

instance Core.NFData DescribeCacheParameterGroups

instance Core.ToHeaders DescribeCacheParameterGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCacheParameterGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCacheParameterGroups where
  toQuery DescribeCacheParameterGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeCacheParameterGroups" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheParameterGroups@ operation.
--
-- /See:/ 'newDescribeCacheParameterGroupsResponse' smart constructor.
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
  { -- | A list of cache parameter groups. Each element in the list contains
    -- detailed information about one cache parameter group.
    cacheParameterGroups :: Core.Maybe [CacheParameterGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCacheParameterGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheParameterGroups', 'describeCacheParameterGroupsResponse_cacheParameterGroups' - A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
--
-- 'marker', 'describeCacheParameterGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeCacheParameterGroupsResponse_httpStatus' - The response's http status code.
newDescribeCacheParameterGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCacheParameterGroupsResponse
newDescribeCacheParameterGroupsResponse pHttpStatus_ =
  DescribeCacheParameterGroupsResponse'
    { cacheParameterGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache parameter groups. Each element in the list contains
-- detailed information about one cache parameter group.
describeCacheParameterGroupsResponse_cacheParameterGroups :: Lens.Lens' DescribeCacheParameterGroupsResponse (Core.Maybe [CacheParameterGroup])
describeCacheParameterGroupsResponse_cacheParameterGroups = Lens.lens (\DescribeCacheParameterGroupsResponse' {cacheParameterGroups} -> cacheParameterGroups) (\s@DescribeCacheParameterGroupsResponse' {} a -> s {cacheParameterGroups = a} :: DescribeCacheParameterGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheParameterGroupsResponse_marker :: Lens.Lens' DescribeCacheParameterGroupsResponse (Core.Maybe Core.Text)
describeCacheParameterGroupsResponse_marker = Lens.lens (\DescribeCacheParameterGroupsResponse' {marker} -> marker) (\s@DescribeCacheParameterGroupsResponse' {} a -> s {marker = a} :: DescribeCacheParameterGroupsResponse)

-- | The response's http status code.
describeCacheParameterGroupsResponse_httpStatus :: Lens.Lens' DescribeCacheParameterGroupsResponse Core.Int
describeCacheParameterGroupsResponse_httpStatus = Lens.lens (\DescribeCacheParameterGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheParameterGroupsResponse' {} a -> s {httpStatus = a} :: DescribeCacheParameterGroupsResponse)

instance
  Core.NFData
    DescribeCacheParameterGroupsResponse
