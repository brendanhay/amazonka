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
    describeUpdateActions_serviceUpdateStatus,
    describeUpdateActions_showNodeLevelUpdateStatus,
    describeUpdateActions_engine,
    describeUpdateActions_serviceUpdateTimeRange,
    describeUpdateActions_serviceUpdateName,
    describeUpdateActions_cacheClusterIds,
    describeUpdateActions_replicationGroupIds,
    describeUpdateActions_maxRecords,
    describeUpdateActions_marker,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUpdateActions' smart constructor.
data DescribeUpdateActions = DescribeUpdateActions'
  { -- | The status of the update action.
    updateActionStatus :: Prelude.Maybe [UpdateActionStatus],
    -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe [ServiceUpdateStatus],
    -- | Dictates whether to include node level update status in the response
    showNodeLevelUpdateStatus :: Prelude.Maybe Prelude.Bool,
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Prelude.Maybe Prelude.Text,
    -- | The range of time specified to search for service updates that are in
    -- available status
    serviceUpdateTimeRange :: Prelude.Maybe TimeRangeFilter,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The cache cluster IDs
    cacheClusterIds :: Prelude.Maybe [Prelude.Text],
    -- | The replication group IDs
    replicationGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of records to include in the response
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'serviceUpdateStatus', 'describeUpdateActions_serviceUpdateStatus' - The status of the service update
--
-- 'showNodeLevelUpdateStatus', 'describeUpdateActions_showNodeLevelUpdateStatus' - Dictates whether to include node level update status in the response
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
-- 'maxRecords', 'describeUpdateActions_maxRecords' - The maximum number of records to include in the response
--
-- 'marker', 'describeUpdateActions_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
newDescribeUpdateActions ::
  DescribeUpdateActions
newDescribeUpdateActions =
  DescribeUpdateActions'
    { updateActionStatus =
        Prelude.Nothing,
      serviceUpdateStatus = Prelude.Nothing,
      showNodeLevelUpdateStatus = Prelude.Nothing,
      engine = Prelude.Nothing,
      serviceUpdateTimeRange = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      cacheClusterIds = Prelude.Nothing,
      replicationGroupIds = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The status of the update action.
describeUpdateActions_updateActionStatus :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [UpdateActionStatus])
describeUpdateActions_updateActionStatus = Lens.lens (\DescribeUpdateActions' {updateActionStatus} -> updateActionStatus) (\s@DescribeUpdateActions' {} a -> s {updateActionStatus = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens._Coerce

-- | The status of the service update
describeUpdateActions_serviceUpdateStatus :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [ServiceUpdateStatus])
describeUpdateActions_serviceUpdateStatus = Lens.lens (\DescribeUpdateActions' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateStatus = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens._Coerce

-- | Dictates whether to include node level update status in the response
describeUpdateActions_showNodeLevelUpdateStatus :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Bool)
describeUpdateActions_showNodeLevelUpdateStatus = Lens.lens (\DescribeUpdateActions' {showNodeLevelUpdateStatus} -> showNodeLevelUpdateStatus) (\s@DescribeUpdateActions' {} a -> s {showNodeLevelUpdateStatus = a} :: DescribeUpdateActions)

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
describeUpdateActions_engine :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Text)
describeUpdateActions_engine = Lens.lens (\DescribeUpdateActions' {engine} -> engine) (\s@DescribeUpdateActions' {} a -> s {engine = a} :: DescribeUpdateActions)

-- | The range of time specified to search for service updates that are in
-- available status
describeUpdateActions_serviceUpdateTimeRange :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe TimeRangeFilter)
describeUpdateActions_serviceUpdateTimeRange = Lens.lens (\DescribeUpdateActions' {serviceUpdateTimeRange} -> serviceUpdateTimeRange) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateTimeRange = a} :: DescribeUpdateActions)

-- | The unique ID of the service update
describeUpdateActions_serviceUpdateName :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Text)
describeUpdateActions_serviceUpdateName = Lens.lens (\DescribeUpdateActions' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateName = a} :: DescribeUpdateActions)

-- | The cache cluster IDs
describeUpdateActions_cacheClusterIds :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [Prelude.Text])
describeUpdateActions_cacheClusterIds = Lens.lens (\DescribeUpdateActions' {cacheClusterIds} -> cacheClusterIds) (\s@DescribeUpdateActions' {} a -> s {cacheClusterIds = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens._Coerce

-- | The replication group IDs
describeUpdateActions_replicationGroupIds :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [Prelude.Text])
describeUpdateActions_replicationGroupIds = Lens.lens (\DescribeUpdateActions' {replicationGroupIds} -> replicationGroupIds) (\s@DescribeUpdateActions' {} a -> s {replicationGroupIds = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens._Coerce

-- | The maximum number of records to include in the response
describeUpdateActions_maxRecords :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Int)
describeUpdateActions_maxRecords = Lens.lens (\DescribeUpdateActions' {maxRecords} -> maxRecords) (\s@DescribeUpdateActions' {} a -> s {maxRecords = a} :: DescribeUpdateActions)

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeUpdateActions_marker :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Text)
describeUpdateActions_marker = Lens.lens (\DescribeUpdateActions' {marker} -> marker) (\s@DescribeUpdateActions' {} a -> s {marker = a} :: DescribeUpdateActions)

instance Core.AWSPager DescribeUpdateActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeUpdateActionsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeUpdateActionsResponse_updateActions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeUpdateActions_marker
          Lens..~ rs
          Lens.^? describeUpdateActionsResponse_marker
            Prelude.. Lens._Just

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
            Prelude.<$> ( x Core..@? "UpdateActions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "UpdateAction")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUpdateActions

instance Prelude.NFData DescribeUpdateActions

instance Core.ToHeaders DescribeUpdateActions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeUpdateActions where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUpdateActions where
  toQuery DescribeUpdateActions' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeUpdateActions" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "UpdateActionStatus"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> updateActionStatus
            ),
        "ServiceUpdateStatus"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> serviceUpdateStatus
            ),
        "ShowNodeLevelUpdateStatus"
          Core.=: showNodeLevelUpdateStatus,
        "Engine" Core.=: engine,
        "ServiceUpdateTimeRange"
          Core.=: serviceUpdateTimeRange,
        "ServiceUpdateName" Core.=: serviceUpdateName,
        "CacheClusterIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> cacheClusterIds
            ),
        "ReplicationGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> replicationGroupIds
            ),
        "MaxRecords" Core.=: maxRecords,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeUpdateActionsResponse' smart constructor.
data DescribeUpdateActionsResponse = DescribeUpdateActionsResponse'
  { -- | Returns a list of update actions
    updateActions :: Prelude.Maybe [UpdateAction],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeUpdateActionsResponse
newDescribeUpdateActionsResponse pHttpStatus_ =
  DescribeUpdateActionsResponse'
    { updateActions =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a list of update actions
describeUpdateActionsResponse_updateActions :: Lens.Lens' DescribeUpdateActionsResponse (Prelude.Maybe [UpdateAction])
describeUpdateActionsResponse_updateActions = Lens.lens (\DescribeUpdateActionsResponse' {updateActions} -> updateActions) (\s@DescribeUpdateActionsResponse' {} a -> s {updateActions = a} :: DescribeUpdateActionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeUpdateActionsResponse_marker :: Lens.Lens' DescribeUpdateActionsResponse (Prelude.Maybe Prelude.Text)
describeUpdateActionsResponse_marker = Lens.lens (\DescribeUpdateActionsResponse' {marker} -> marker) (\s@DescribeUpdateActionsResponse' {} a -> s {marker = a} :: DescribeUpdateActionsResponse)

-- | The response's http status code.
describeUpdateActionsResponse_httpStatus :: Lens.Lens' DescribeUpdateActionsResponse Prelude.Int
describeUpdateActionsResponse_httpStatus = Lens.lens (\DescribeUpdateActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeUpdateActionsResponse' {} a -> s {httpStatus = a} :: DescribeUpdateActionsResponse)

instance Prelude.NFData DescribeUpdateActionsResponse
