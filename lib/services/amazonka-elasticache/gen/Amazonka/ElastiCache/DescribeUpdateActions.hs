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
-- Module      : Amazonka.ElastiCache.DescribeUpdateActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the update actions
--
-- This operation returns paginated results.
module Amazonka.ElastiCache.DescribeUpdateActions
  ( -- * Creating a Request
    DescribeUpdateActions (..),
    newDescribeUpdateActions,

    -- * Request Lenses
    describeUpdateActions_cacheClusterIds,
    describeUpdateActions_replicationGroupIds,
    describeUpdateActions_marker,
    describeUpdateActions_updateActionStatus,
    describeUpdateActions_showNodeLevelUpdateStatus,
    describeUpdateActions_maxRecords,
    describeUpdateActions_serviceUpdateName,
    describeUpdateActions_serviceUpdateStatus,
    describeUpdateActions_engine,
    describeUpdateActions_serviceUpdateTimeRange,

    -- * Destructuring the Response
    DescribeUpdateActionsResponse (..),
    newDescribeUpdateActionsResponse,

    -- * Response Lenses
    describeUpdateActionsResponse_marker,
    describeUpdateActionsResponse_updateActions,
    describeUpdateActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUpdateActions' smart constructor.
data DescribeUpdateActions = DescribeUpdateActions'
  { -- | The cache cluster IDs
    cacheClusterIds :: Prelude.Maybe [Prelude.Text],
    -- | The replication group IDs
    replicationGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The status of the update action.
    updateActionStatus :: Prelude.Maybe [UpdateActionStatus],
    -- | Dictates whether to include node level update status in the response
    showNodeLevelUpdateStatus :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of records to include in the response
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The unique ID of the service update
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The status of the service update
    serviceUpdateStatus :: Prelude.Maybe [ServiceUpdateStatus],
    -- | The Elasticache engine to which the update applies. Either Redis or
    -- Memcached
    engine :: Prelude.Maybe Prelude.Text,
    -- | The range of time specified to search for service updates that are in
    -- available status
    serviceUpdateTimeRange :: Prelude.Maybe TimeRangeFilter
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
-- 'cacheClusterIds', 'describeUpdateActions_cacheClusterIds' - The cache cluster IDs
--
-- 'replicationGroupIds', 'describeUpdateActions_replicationGroupIds' - The replication group IDs
--
-- 'marker', 'describeUpdateActions_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'updateActionStatus', 'describeUpdateActions_updateActionStatus' - The status of the update action.
--
-- 'showNodeLevelUpdateStatus', 'describeUpdateActions_showNodeLevelUpdateStatus' - Dictates whether to include node level update status in the response
--
-- 'maxRecords', 'describeUpdateActions_maxRecords' - The maximum number of records to include in the response
--
-- 'serviceUpdateName', 'describeUpdateActions_serviceUpdateName' - The unique ID of the service update
--
-- 'serviceUpdateStatus', 'describeUpdateActions_serviceUpdateStatus' - The status of the service update
--
-- 'engine', 'describeUpdateActions_engine' - The Elasticache engine to which the update applies. Either Redis or
-- Memcached
--
-- 'serviceUpdateTimeRange', 'describeUpdateActions_serviceUpdateTimeRange' - The range of time specified to search for service updates that are in
-- available status
newDescribeUpdateActions ::
  DescribeUpdateActions
newDescribeUpdateActions =
  DescribeUpdateActions'
    { cacheClusterIds =
        Prelude.Nothing,
      replicationGroupIds = Prelude.Nothing,
      marker = Prelude.Nothing,
      updateActionStatus = Prelude.Nothing,
      showNodeLevelUpdateStatus = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      serviceUpdateStatus = Prelude.Nothing,
      engine = Prelude.Nothing,
      serviceUpdateTimeRange = Prelude.Nothing
    }

-- | The cache cluster IDs
describeUpdateActions_cacheClusterIds :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [Prelude.Text])
describeUpdateActions_cacheClusterIds = Lens.lens (\DescribeUpdateActions' {cacheClusterIds} -> cacheClusterIds) (\s@DescribeUpdateActions' {} a -> s {cacheClusterIds = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens.coerced

-- | The replication group IDs
describeUpdateActions_replicationGroupIds :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [Prelude.Text])
describeUpdateActions_replicationGroupIds = Lens.lens (\DescribeUpdateActions' {replicationGroupIds} -> replicationGroupIds) (\s@DescribeUpdateActions' {} a -> s {replicationGroupIds = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens.coerced

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeUpdateActions_marker :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Text)
describeUpdateActions_marker = Lens.lens (\DescribeUpdateActions' {marker} -> marker) (\s@DescribeUpdateActions' {} a -> s {marker = a} :: DescribeUpdateActions)

-- | The status of the update action.
describeUpdateActions_updateActionStatus :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [UpdateActionStatus])
describeUpdateActions_updateActionStatus = Lens.lens (\DescribeUpdateActions' {updateActionStatus} -> updateActionStatus) (\s@DescribeUpdateActions' {} a -> s {updateActionStatus = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens.coerced

-- | Dictates whether to include node level update status in the response
describeUpdateActions_showNodeLevelUpdateStatus :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Bool)
describeUpdateActions_showNodeLevelUpdateStatus = Lens.lens (\DescribeUpdateActions' {showNodeLevelUpdateStatus} -> showNodeLevelUpdateStatus) (\s@DescribeUpdateActions' {} a -> s {showNodeLevelUpdateStatus = a} :: DescribeUpdateActions)

-- | The maximum number of records to include in the response
describeUpdateActions_maxRecords :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Int)
describeUpdateActions_maxRecords = Lens.lens (\DescribeUpdateActions' {maxRecords} -> maxRecords) (\s@DescribeUpdateActions' {} a -> s {maxRecords = a} :: DescribeUpdateActions)

-- | The unique ID of the service update
describeUpdateActions_serviceUpdateName :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Text)
describeUpdateActions_serviceUpdateName = Lens.lens (\DescribeUpdateActions' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateName = a} :: DescribeUpdateActions)

-- | The status of the service update
describeUpdateActions_serviceUpdateStatus :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe [ServiceUpdateStatus])
describeUpdateActions_serviceUpdateStatus = Lens.lens (\DescribeUpdateActions' {serviceUpdateStatus} -> serviceUpdateStatus) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateStatus = a} :: DescribeUpdateActions) Prelude.. Lens.mapping Lens.coerced

-- | The Elasticache engine to which the update applies. Either Redis or
-- Memcached
describeUpdateActions_engine :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe Prelude.Text)
describeUpdateActions_engine = Lens.lens (\DescribeUpdateActions' {engine} -> engine) (\s@DescribeUpdateActions' {} a -> s {engine = a} :: DescribeUpdateActions)

-- | The range of time specified to search for service updates that are in
-- available status
describeUpdateActions_serviceUpdateTimeRange :: Lens.Lens' DescribeUpdateActions (Prelude.Maybe TimeRangeFilter)
describeUpdateActions_serviceUpdateTimeRange = Lens.lens (\DescribeUpdateActions' {serviceUpdateTimeRange} -> serviceUpdateTimeRange) (\s@DescribeUpdateActions' {} a -> s {serviceUpdateTimeRange = a} :: DescribeUpdateActions)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeUpdateActionsResult"
      ( \s h x ->
          DescribeUpdateActionsResponse'
            Prelude.<$> (x Data..@? "Marker")
            Prelude.<*> ( x Data..@? "UpdateActions" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "UpdateAction")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUpdateActions where
  hashWithSalt _salt DescribeUpdateActions' {..} =
    _salt `Prelude.hashWithSalt` cacheClusterIds
      `Prelude.hashWithSalt` replicationGroupIds
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` updateActionStatus
      `Prelude.hashWithSalt` showNodeLevelUpdateStatus
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` serviceUpdateStatus
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` serviceUpdateTimeRange

instance Prelude.NFData DescribeUpdateActions where
  rnf DescribeUpdateActions' {..} =
    Prelude.rnf cacheClusterIds
      `Prelude.seq` Prelude.rnf replicationGroupIds
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf updateActionStatus
      `Prelude.seq` Prelude.rnf showNodeLevelUpdateStatus
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf serviceUpdateStatus
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf serviceUpdateTimeRange

instance Data.ToHeaders DescribeUpdateActions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeUpdateActions where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUpdateActions where
  toQuery DescribeUpdateActions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeUpdateActions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "CacheClusterIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> cacheClusterIds
            ),
        "ReplicationGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> replicationGroupIds
            ),
        "Marker" Data.=: marker,
        "UpdateActionStatus"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> updateActionStatus
            ),
        "ShowNodeLevelUpdateStatus"
          Data.=: showNodeLevelUpdateStatus,
        "MaxRecords" Data.=: maxRecords,
        "ServiceUpdateName" Data.=: serviceUpdateName,
        "ServiceUpdateStatus"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> serviceUpdateStatus
            ),
        "Engine" Data.=: engine,
        "ServiceUpdateTimeRange"
          Data.=: serviceUpdateTimeRange
      ]

-- | /See:/ 'newDescribeUpdateActionsResponse' smart constructor.
data DescribeUpdateActionsResponse = DescribeUpdateActionsResponse'
  { -- | An optional marker returned from a prior request. Use this marker for
    -- pagination of results from this operation. If this parameter is
    -- specified, the response includes only records beyond the marker, up to
    -- the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Returns a list of update actions
    updateActions :: Prelude.Maybe [UpdateAction],
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
-- 'marker', 'describeUpdateActionsResponse_marker' - An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
--
-- 'updateActions', 'describeUpdateActionsResponse_updateActions' - Returns a list of update actions
--
-- 'httpStatus', 'describeUpdateActionsResponse_httpStatus' - The response's http status code.
newDescribeUpdateActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUpdateActionsResponse
newDescribeUpdateActionsResponse pHttpStatus_ =
  DescribeUpdateActionsResponse'
    { marker =
        Prelude.Nothing,
      updateActions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by @MaxRecords@.
describeUpdateActionsResponse_marker :: Lens.Lens' DescribeUpdateActionsResponse (Prelude.Maybe Prelude.Text)
describeUpdateActionsResponse_marker = Lens.lens (\DescribeUpdateActionsResponse' {marker} -> marker) (\s@DescribeUpdateActionsResponse' {} a -> s {marker = a} :: DescribeUpdateActionsResponse)

-- | Returns a list of update actions
describeUpdateActionsResponse_updateActions :: Lens.Lens' DescribeUpdateActionsResponse (Prelude.Maybe [UpdateAction])
describeUpdateActionsResponse_updateActions = Lens.lens (\DescribeUpdateActionsResponse' {updateActions} -> updateActions) (\s@DescribeUpdateActionsResponse' {} a -> s {updateActions = a} :: DescribeUpdateActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeUpdateActionsResponse_httpStatus :: Lens.Lens' DescribeUpdateActionsResponse Prelude.Int
describeUpdateActionsResponse_httpStatus = Lens.lens (\DescribeUpdateActionsResponse' {httpStatus} -> httpStatus) (\s@DescribeUpdateActionsResponse' {} a -> s {httpStatus = a} :: DescribeUpdateActionsResponse)

instance Prelude.NFData DescribeUpdateActionsResponse where
  rnf DescribeUpdateActionsResponse' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf updateActions
      `Prelude.seq` Prelude.rnf httpStatus
