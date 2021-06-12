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
-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache subnet group descriptions. If a subnet group
-- name is specified, the list contains only the description of that group.
-- This is applicable only when you have ElastiCache in VPC setup. All
-- ElastiCache clusters now launch in VPC by default.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
  ( -- * Creating a Request
    DescribeCacheSubnetGroups (..),
    newDescribeCacheSubnetGroups,

    -- * Request Lenses
    describeCacheSubnetGroups_cacheSubnetGroupName,
    describeCacheSubnetGroups_marker,
    describeCacheSubnetGroups_maxRecords,

    -- * Destructuring the Response
    DescribeCacheSubnetGroupsResponse (..),
    newDescribeCacheSubnetGroupsResponse,

    -- * Response Lenses
    describeCacheSubnetGroupsResponse_cacheSubnetGroups,
    describeCacheSubnetGroupsResponse_marker,
    describeCacheSubnetGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'newDescribeCacheSubnetGroups' smart constructor.
data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups'
  { -- | The name of the cache subnet group to return details for.
    cacheSubnetGroupName :: Core.Maybe Core.Text,
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
-- Create a value of 'DescribeCacheSubnetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSubnetGroupName', 'describeCacheSubnetGroups_cacheSubnetGroupName' - The name of the cache subnet group to return details for.
--
-- 'marker', 'describeCacheSubnetGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeCacheSubnetGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
newDescribeCacheSubnetGroups ::
  DescribeCacheSubnetGroups
newDescribeCacheSubnetGroups =
  DescribeCacheSubnetGroups'
    { cacheSubnetGroupName =
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the cache subnet group to return details for.
describeCacheSubnetGroups_cacheSubnetGroupName :: Lens.Lens' DescribeCacheSubnetGroups (Core.Maybe Core.Text)
describeCacheSubnetGroups_cacheSubnetGroupName = Lens.lens (\DescribeCacheSubnetGroups' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@DescribeCacheSubnetGroups' {} a -> s {cacheSubnetGroupName = a} :: DescribeCacheSubnetGroups)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeCacheSubnetGroups_marker :: Lens.Lens' DescribeCacheSubnetGroups (Core.Maybe Core.Text)
describeCacheSubnetGroups_marker = Lens.lens (\DescribeCacheSubnetGroups' {marker} -> marker) (\s@DescribeCacheSubnetGroups' {} a -> s {marker = a} :: DescribeCacheSubnetGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
describeCacheSubnetGroups_maxRecords :: Lens.Lens' DescribeCacheSubnetGroups (Core.Maybe Core.Int)
describeCacheSubnetGroups_maxRecords = Lens.lens (\DescribeCacheSubnetGroups' {maxRecords} -> maxRecords) (\s@DescribeCacheSubnetGroups' {} a -> s {maxRecords = a} :: DescribeCacheSubnetGroups)

instance Core.AWSPager DescribeCacheSubnetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCacheSubnetGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCacheSubnetGroupsResponse_cacheSubnetGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCacheSubnetGroups_marker
          Lens..~ rs
          Lens.^? describeCacheSubnetGroupsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeCacheSubnetGroups where
  type
    AWSResponse DescribeCacheSubnetGroups =
      DescribeCacheSubnetGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeCacheSubnetGroupsResult"
      ( \s h x ->
          DescribeCacheSubnetGroupsResponse'
            Core.<$> ( x Core..@? "CacheSubnetGroups" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "CacheSubnetGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCacheSubnetGroups

instance Core.NFData DescribeCacheSubnetGroups

instance Core.ToHeaders DescribeCacheSubnetGroups where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCacheSubnetGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCacheSubnetGroups where
  toQuery DescribeCacheSubnetGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeCacheSubnetGroups" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "CacheSubnetGroupName" Core.=: cacheSubnetGroupName,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | Represents the output of a @DescribeCacheSubnetGroups@ operation.
--
-- /See:/ 'newDescribeCacheSubnetGroupsResponse' smart constructor.
data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse'
  { -- | A list of cache subnet groups. Each element in the list contains
    -- detailed information about one group.
    cacheSubnetGroups :: Core.Maybe [CacheSubnetGroup],
    -- | Provides an identifier to allow retrieval of paginated results.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCacheSubnetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheSubnetGroups', 'describeCacheSubnetGroupsResponse_cacheSubnetGroups' - A list of cache subnet groups. Each element in the list contains
-- detailed information about one group.
--
-- 'marker', 'describeCacheSubnetGroupsResponse_marker' - Provides an identifier to allow retrieval of paginated results.
--
-- 'httpStatus', 'describeCacheSubnetGroupsResponse_httpStatus' - The response's http status code.
newDescribeCacheSubnetGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCacheSubnetGroupsResponse
newDescribeCacheSubnetGroupsResponse pHttpStatus_ =
  DescribeCacheSubnetGroupsResponse'
    { cacheSubnetGroups =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of cache subnet groups. Each element in the list contains
-- detailed information about one group.
describeCacheSubnetGroupsResponse_cacheSubnetGroups :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Core.Maybe [CacheSubnetGroup])
describeCacheSubnetGroupsResponse_cacheSubnetGroups = Lens.lens (\DescribeCacheSubnetGroupsResponse' {cacheSubnetGroups} -> cacheSubnetGroups) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {cacheSubnetGroups = a} :: DescribeCacheSubnetGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | Provides an identifier to allow retrieval of paginated results.
describeCacheSubnetGroupsResponse_marker :: Lens.Lens' DescribeCacheSubnetGroupsResponse (Core.Maybe Core.Text)
describeCacheSubnetGroupsResponse_marker = Lens.lens (\DescribeCacheSubnetGroupsResponse' {marker} -> marker) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {marker = a} :: DescribeCacheSubnetGroupsResponse)

-- | The response's http status code.
describeCacheSubnetGroupsResponse_httpStatus :: Lens.Lens' DescribeCacheSubnetGroupsResponse Core.Int
describeCacheSubnetGroupsResponse_httpStatus = Lens.lens (\DescribeCacheSubnetGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeCacheSubnetGroupsResponse' {} a -> s {httpStatus = a} :: DescribeCacheSubnetGroupsResponse)

instance
  Core.NFData
    DescribeCacheSubnetGroupsResponse
