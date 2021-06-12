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
-- Module      : Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a particular global replication group. If no
-- identifier is specified, returns information about all Global
-- Datastores.
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
  ( -- * Creating a Request
    DescribeGlobalReplicationGroups (..),
    newDescribeGlobalReplicationGroups,

    -- * Request Lenses
    describeGlobalReplicationGroups_globalReplicationGroupId,
    describeGlobalReplicationGroups_showMemberInfo,
    describeGlobalReplicationGroups_marker,
    describeGlobalReplicationGroups_maxRecords,

    -- * Destructuring the Response
    DescribeGlobalReplicationGroupsResponse (..),
    newDescribeGlobalReplicationGroupsResponse,

    -- * Response Lenses
    describeGlobalReplicationGroupsResponse_globalReplicationGroups,
    describeGlobalReplicationGroupsResponse_marker,
    describeGlobalReplicationGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeGlobalReplicationGroups' smart constructor.
data DescribeGlobalReplicationGroups = DescribeGlobalReplicationGroups'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Maybe Core.Text,
    -- | Returns the list of members that comprise the Global Datastore.
    showMemberInfo :: Core.Maybe Core.Bool,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxRecords value, a marker is included
    -- in the response so that the remaining results can be retrieved.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGlobalReplicationGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroupId', 'describeGlobalReplicationGroups_globalReplicationGroupId' - The name of the Global Datastore
--
-- 'showMemberInfo', 'describeGlobalReplicationGroups_showMemberInfo' - Returns the list of members that comprise the Global Datastore.
--
-- 'marker', 'describeGlobalReplicationGroups_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeGlobalReplicationGroups_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
newDescribeGlobalReplicationGroups ::
  DescribeGlobalReplicationGroups
newDescribeGlobalReplicationGroups =
  DescribeGlobalReplicationGroups'
    { globalReplicationGroupId =
        Core.Nothing,
      showMemberInfo = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The name of the Global Datastore
describeGlobalReplicationGroups_globalReplicationGroupId :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Text)
describeGlobalReplicationGroups_globalReplicationGroupId = Lens.lens (\DescribeGlobalReplicationGroups' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@DescribeGlobalReplicationGroups' {} a -> s {globalReplicationGroupId = a} :: DescribeGlobalReplicationGroups)

-- | Returns the list of members that comprise the Global Datastore.
describeGlobalReplicationGroups_showMemberInfo :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Bool)
describeGlobalReplicationGroups_showMemberInfo = Lens.lens (\DescribeGlobalReplicationGroups' {showMemberInfo} -> showMemberInfo) (\s@DescribeGlobalReplicationGroups' {} a -> s {showMemberInfo = a} :: DescribeGlobalReplicationGroups)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeGlobalReplicationGroups_marker :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Text)
describeGlobalReplicationGroups_marker = Lens.lens (\DescribeGlobalReplicationGroups' {marker} -> marker) (\s@DescribeGlobalReplicationGroups' {} a -> s {marker = a} :: DescribeGlobalReplicationGroups)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxRecords value, a marker is included
-- in the response so that the remaining results can be retrieved.
describeGlobalReplicationGroups_maxRecords :: Lens.Lens' DescribeGlobalReplicationGroups (Core.Maybe Core.Int)
describeGlobalReplicationGroups_maxRecords = Lens.lens (\DescribeGlobalReplicationGroups' {maxRecords} -> maxRecords) (\s@DescribeGlobalReplicationGroups' {} a -> s {maxRecords = a} :: DescribeGlobalReplicationGroups)

instance
  Core.AWSPager
    DescribeGlobalReplicationGroups
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGlobalReplicationGroupsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGlobalReplicationGroupsResponse_globalReplicationGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeGlobalReplicationGroups_marker
          Lens..~ rs
          Lens.^? describeGlobalReplicationGroupsResponse_marker
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeGlobalReplicationGroups
  where
  type
    AWSResponse DescribeGlobalReplicationGroups =
      DescribeGlobalReplicationGroupsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeGlobalReplicationGroupsResult"
      ( \s h x ->
          DescribeGlobalReplicationGroupsResponse'
            Core.<$> ( x Core..@? "GlobalReplicationGroups"
                         Core..!@ Core.mempty
                         Core.>>= Core.may
                           (Core.parseXMLList "GlobalReplicationGroup")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeGlobalReplicationGroups

instance Core.NFData DescribeGlobalReplicationGroups

instance
  Core.ToHeaders
    DescribeGlobalReplicationGroups
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeGlobalReplicationGroups where
  toPath = Core.const "/"

instance Core.ToQuery DescribeGlobalReplicationGroups where
  toQuery DescribeGlobalReplicationGroups' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeGlobalReplicationGroups" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId,
        "ShowMemberInfo" Core.=: showMemberInfo,
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeGlobalReplicationGroupsResponse' smart constructor.
data DescribeGlobalReplicationGroupsResponse = DescribeGlobalReplicationGroupsResponse'
  { -- | Indicates the slot configuration and global identifier for each slice
    -- group.
    globalReplicationGroups :: Core.Maybe [GlobalReplicationGroup],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by MaxRecords. >
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGlobalReplicationGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalReplicationGroups', 'describeGlobalReplicationGroupsResponse_globalReplicationGroups' - Indicates the slot configuration and global identifier for each slice
-- group.
--
-- 'marker', 'describeGlobalReplicationGroupsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
--
-- 'httpStatus', 'describeGlobalReplicationGroupsResponse_httpStatus' - The response's http status code.
newDescribeGlobalReplicationGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeGlobalReplicationGroupsResponse
newDescribeGlobalReplicationGroupsResponse
  pHttpStatus_ =
    DescribeGlobalReplicationGroupsResponse'
      { globalReplicationGroups =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates the slot configuration and global identifier for each slice
-- group.
describeGlobalReplicationGroupsResponse_globalReplicationGroups :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Core.Maybe [GlobalReplicationGroup])
describeGlobalReplicationGroupsResponse_globalReplicationGroups = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {globalReplicationGroups} -> globalReplicationGroups) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {globalReplicationGroups = a} :: DescribeGlobalReplicationGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords. >
describeGlobalReplicationGroupsResponse_marker :: Lens.Lens' DescribeGlobalReplicationGroupsResponse (Core.Maybe Core.Text)
describeGlobalReplicationGroupsResponse_marker = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {marker} -> marker) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {marker = a} :: DescribeGlobalReplicationGroupsResponse)

-- | The response's http status code.
describeGlobalReplicationGroupsResponse_httpStatus :: Lens.Lens' DescribeGlobalReplicationGroupsResponse Core.Int
describeGlobalReplicationGroupsResponse_httpStatus = Lens.lens (\DescribeGlobalReplicationGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalReplicationGroupsResponse' {} a -> s {httpStatus = a} :: DescribeGlobalReplicationGroupsResponse)

instance
  Core.NFData
    DescribeGlobalReplicationGroupsResponse
