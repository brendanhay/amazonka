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
-- Module      : Network.AWS.ElastiCache.DescribeUpdateActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the update actions
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeUpdateActions
  ( -- * Creating a Request
    DescribeUpdateActions (..),
    newDescribeUpdateActions,

    -- * Request Lenses
    describeUpdateActions_updateActionStatus,
    describeUpdateActions_showNodeLevelUpdateStatus,
    describeUpdateActions_serviceUpdateStatus,
    describeUpdateActions_engine,
    describeUpdateActions_serviceUpdateTimeRange,
    describeUpdateActions_serviceUpdateName,
    describeUpdateActions_cacheClusterIds,
    describeUpdateActions_replicationGroupIds,
    describeUpdateActions_marker,
    describeUpdateActions_maxRecords,

    -- * Destructuring the Response
    DescribeUpdateActionsResponse (..),
    newDescribeUpdateActionsResponse,

    -- * Response Lenses
    describeUpdateActionsResponse_updateActions,
    describeUpdateActionsResponse_marker,
    describeUpdateActionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUpdateActions' smart constructor.
data DescribeUpdateActions = DescribeUpdateActions'
  { -- | The status of the update action.
    updateActionStatus :: Core.Maybe [UpdateActionStatus],
    -- | Dictates whether to include node level update status in the response
    showNodeLevelUpdateStatus :: Core.Maybe Core.Bool,
    -- | The status of the service update
    serviceUpdateStatus :: Core.Maybe [ServiceUpdateStatus],
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Core.Maybe Core.Text,
    -- | The range of time specified to search for service updates that are in
    -- available status
    serviceUpdateTimeRange :: Core.Maybe TimeRangeFilter,
    -- | The unique ID of the service update
    serviceUpdateName :: Core.Maybe Core.Text,
    -- | The cache cluster IDs
    cacheClusterIds :: Core.Maybe [Core.Text],
    -- | The replication group IDs
    replicationGroupIds :: Core.Maybe [Core.Text],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUpdateActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateActionStatus', 'describeUpdateActions_updateActionStatus' - The status of the update action.
--
-- 'showNodeLevelUpdateStatus', 'describeUpdateActions_showNodeLevelUpdateStatus' - Dictates whether to include node level update status in the response
--
-- 'serviceUpdateStatus', 'describeUpdateActions_serviceUpdateStatus' - The status of the service update
--
-- 'engine', 'describeUpdateActions_engine' - The Elasticache engine to which the update applies. Either Redis or
-- Memcached
--
-- 'serviceUpdateTimeRange', 'describeUpdateActions_serviceUpdateTimeRange' - The range of time specified to search for service updates that are in
-- available status
--
-- 'serviceUpdateName', 'describeUpdateActions_serviceUpdateName' - The unique ID of the service update
--
-- 'cacheClusterIds', 'describeUpdateActions_cacheClusterIds' - The cache cluster IDs
--
-- 'replicationGroupIds', 'describeUpdateActions_replicationGroupIds' - The replication group IDs
--
-- 'marker', 'describeUpdateActions_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeUpdateActions_maxRecords' - The maximum number of records to include in the response
newDescribeUpdateActions ::
  DescribeUpdateActions
newDescribeUpdateActions =
  DescribeUpdateActions'
    { updateActionStatus =
        Core.Nothing,
      showNodeLevelUpdateStatus = Core.Nothing,
      serviceUpdateStatus = Core.Nothing,
      engine = Core.Nothing,
      serviceUpdateTimeRange = Core.Nothing,
      serviceUpdateName = Core.Nothing,
      cacheClusterIds = Core.Nothing,
      replicationGroupIds = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | The status of the update action.
describeUpdateActions_updateActionStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe [UpdateActionStatus])
describeUpdateActions_updateActionStatus = Lens.lens (\DescribeUpdateActions' {updateActionStatus} -> updateActionStatus) (\s@DescribeUpdateActions' {} a -> s {updateActionStatus = a} :: DescribeUpdateActions) Core.. Lens.mapping Lens._Coerce

-- | Dictates whether to include node level update status in the response
describeUpdateActions_showNodeLevelUpdateStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Bool)
describeUpdateActions_showNodeLevelUpdateStatus = Lens.lens (\DescribeUpdateActions' {showNodeLevelUpdateStatus} -> showNodeLevelUpdateStatus) (\s@DescribeUpdateActions' {} a -> s {showNodeLevelUpdateStatus = a} :: DescribeUpdateActions)

-- | The status of the service update
describeUpdateActions_serviceUpdateStatus :: Lens.Lens' DescribeUpdateActions (Core.Maybe [ServiceUpdateStatus])
describeUpdateActions_serviceUpdateStatus = Lens.lens (\DescribeUpdateActions' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateStatus = a} :: DescribeUpdateActions) Core.. Lens.mapping Lens._Coerce

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
describeUpdateActions_engine :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Text)
describeUpdateActions_engine = Lens.lens (\DescribeUpdateActions' {engine} -> engine) (\s@DescribeUpdateActions' {} a -> s {engine = a} :: DescribeUpdateActions)

-- | The range of time specified to search for service updates that are in
-- available status
describeUpdateActions_serviceUpdateTimeRange :: Lens.Lens' DescribeUpdateActions (Core.Maybe TimeRangeFilter)
describeUpdateActions_serviceUpdateTimeRange = Lens.lens (\DescribeUpdateActions' {serviceUpdateTimeRange} -> serviceUpdateTimeRange) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateTimeRange = a} :: DescribeUpdateActions)

-- | The unique ID of the service update
describeUpdateActions_serviceUpdateName :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Text)
describeUpdateActions_serviceUpdateName = Lens.lens (\DescribeUpdateActions' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateName = a} :: DescribeUpdateActions)

-- | The cache cluster IDs
describeUpdateActions_cacheClusterIds :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Core.Text])
describeUpdateActions_cacheClusterIds = Lens.lens (\DescribeUpdateActions' {cacheClusterIds} -> cacheClusterIds) (\s@DescribeUpdateActions' {} a -> s {cacheClusterIds = a} :: DescribeUpdateActions) Core.. Lens.mapping Lens._Coerce

-- | The replication group IDs
describeUpdateActions_replicationGroupIds :: Lens.Lens' DescribeUpdateActions (Core.Maybe [Core.Text])
describeUpdateActions_replicationGroupIds = Lens.lens (\DescribeUpdateActions' {replicationGroupIds} -> replicationGroupIds) (\s@DescribeUpdateActions' {} a -> s {replicationGroupIds = a} :: DescribeUpdateActions) Core.. Lens.mapping Lens._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeUpdateActions_marker :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Text)
describeUpdateActions_marker = Lens.lens (\DescribeUpdateActions' {marker} -> marker) (\s@DescribeUpdateActions' {} a -> s {marker = a} :: DescribeUpdateActions)

-- | The maximum number of records to include in the response
describeUpdateActions_maxRecords :: Lens.Lens' DescribeUpdateActions (Core.Maybe Core.Int)
describeUpdateActions_maxRecords = Lens.lens (\DescribeUpdateActions' {maxRecords} -> maxRecords) (\s@DescribeUpdateActions' {} a -> s {maxRecords = a} :: DescribeUpdateActions)

instance Core.AWSPager DescribeUpdateActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUpdateActionsResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUpdateActionsResponse_updateActions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeUpdateActions_marker
          Lens..~ rs
          Lens.^? describeUpdateActionsResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeUpdateActions where
  type
    AWSResponse DescribeUpdateActions =
      DescribeUpdateActionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeUpdateActionsResult"
      ( \s h x ->
          DescribeUpdateActionsResponse'
            Core.<$> ( x Core..@? "UpdateActions" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "UpdateAction")
                     )
            Core.<*> (x Core..@? "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUpdateActions

instance Core.NFData DescribeUpdateActions

instance Core.ToHeaders DescribeUpdateActions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeUpdateActions where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUpdateActions where
  toQuery DescribeUpdateActions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeUpdateActions" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "UpdateActionStatus"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> updateActionStatus
            ),
        "ShowNodeLevelUpdateStatus"
          Core.=: showNodeLevelUpdateStatus,
        "ServiceUpdateStatus"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> serviceUpdateStatus
            ),
        "Engine" Core.=: engine,
        "ServiceUpdateTimeRange"
          Core.=: serviceUpdateTimeRange,
        "ServiceUpdateName" Core.=: serviceUpdateName,
        "CacheClusterIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> cacheClusterIds),
        "ReplicationGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Core.<$> replicationGroupIds
            ),
        "Marker" Core.=: marker,
        "MaxRecords" Core.=: maxRecords
      ]

-- | /See:/ 'newDescribeUpdateActionsResponse' smart constructor.
data DescribeUpdateActionsResponse = DescribeUpdateActionsResponse'
  { -- | Returns a list of update actions
    updateActions :: Core.Maybe [UpdateAction],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUpdateActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateActions', 'describeUpdateActionsResponse_updateActions' - Returns a list of update actions
--
-- 'marker', 'describeUpdateActionsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeUpdateActionsResponse_httpStatus' - The response's http status code.
newDescribeUpdateActionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUpdateActionsResponse
newDescribeUpdateActionsResponse pHttpStatus_ =
  DescribeUpdateActionsResponse'
    { updateActions =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of update actions
describeUpdateActionsResponse_updateActions :: Lens.Lens' DescribeUpdateActionsResponse (Core.Maybe [UpdateAction])
describeUpdateActionsResponse_updateActions = Lens.lens (\DescribeUpdateActionsResponse' {updateActions} -> updateActions) (\s@DescribeUpdateActionsResponse' {} a -> s {updateActions = a} :: DescribeUpdateActionsResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeUpdateActionsResponse_marker :: Lens.Lens' DescribeUpdateActionsResponse (Core.Maybe Core.Text)
describeUpdateActionsResponse_marker = Lens.lens (\DescribeUpdateActionsResponse' {marker} -> marker) (\s@DescribeUpdateActionsResponse' {} a -> s {marker = a} :: DescribeUpdateActionsResponse)

-- | The response's http status code.
describeUpdateActionsResponse_httpStatus :: Lens.Lens' DescribeUpdateActionsResponse Core.Int
describeUpdateActionsResponse_httpStatus = Lens.lens (\DescribeUpdateActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeUpdateActionsResponse' {} a -> s {httpStatus = a} :: DescribeUpdateActionsResponse)

instance Core.NFData DescribeUpdateActionsResponse
