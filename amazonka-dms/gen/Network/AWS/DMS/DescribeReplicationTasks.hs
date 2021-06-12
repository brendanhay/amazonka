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
-- Module      : Network.AWS.DMS.DescribeReplicationTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about replication tasks for your account in the
-- current region.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationTasks
  ( -- * Creating a Request
    DescribeReplicationTasks (..),
    newDescribeReplicationTasks,

    -- * Request Lenses
    describeReplicationTasks_withoutSettings,
    describeReplicationTasks_filters,
    describeReplicationTasks_marker,
    describeReplicationTasks_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationTasksResponse (..),
    newDescribeReplicationTasksResponse,

    -- * Response Lenses
    describeReplicationTasksResponse_replicationTasks,
    describeReplicationTasksResponse_marker,
    describeReplicationTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReplicationTasks' smart constructor.
data DescribeReplicationTasks = DescribeReplicationTasks'
  { -- | An option to set to avoid returning information about settings. Use this
    -- to reduce overhead when setting information is too large. To use this
    -- option, choose @true@; otherwise, choose @false@ (the default).
    withoutSettings :: Core.Maybe Core.Bool,
    -- | Filters applied to replication tasks.
    --
    -- Valid filter names: replication-task-arn | replication-task-id |
    -- migration-type | endpoint-arn | replication-instance-arn
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReplicationTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'withoutSettings', 'describeReplicationTasks_withoutSettings' - An option to set to avoid returning information about settings. Use this
-- to reduce overhead when setting information is too large. To use this
-- option, choose @true@; otherwise, choose @false@ (the default).
--
-- 'filters', 'describeReplicationTasks_filters' - Filters applied to replication tasks.
--
-- Valid filter names: replication-task-arn | replication-task-id |
-- migration-type | endpoint-arn | replication-instance-arn
--
-- 'marker', 'describeReplicationTasks_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationTasks_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
newDescribeReplicationTasks ::
  DescribeReplicationTasks
newDescribeReplicationTasks =
  DescribeReplicationTasks'
    { withoutSettings =
        Core.Nothing,
      filters = Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | An option to set to avoid returning information about settings. Use this
-- to reduce overhead when setting information is too large. To use this
-- option, choose @true@; otherwise, choose @false@ (the default).
describeReplicationTasks_withoutSettings :: Lens.Lens' DescribeReplicationTasks (Core.Maybe Core.Bool)
describeReplicationTasks_withoutSettings = Lens.lens (\DescribeReplicationTasks' {withoutSettings} -> withoutSettings) (\s@DescribeReplicationTasks' {} a -> s {withoutSettings = a} :: DescribeReplicationTasks)

-- | Filters applied to replication tasks.
--
-- Valid filter names: replication-task-arn | replication-task-id |
-- migration-type | endpoint-arn | replication-instance-arn
describeReplicationTasks_filters :: Lens.Lens' DescribeReplicationTasks (Core.Maybe [Filter])
describeReplicationTasks_filters = Lens.lens (\DescribeReplicationTasks' {filters} -> filters) (\s@DescribeReplicationTasks' {} a -> s {filters = a} :: DescribeReplicationTasks) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTasks_marker :: Lens.Lens' DescribeReplicationTasks (Core.Maybe Core.Text)
describeReplicationTasks_marker = Lens.lens (\DescribeReplicationTasks' {marker} -> marker) (\s@DescribeReplicationTasks' {} a -> s {marker = a} :: DescribeReplicationTasks)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationTasks_maxRecords :: Lens.Lens' DescribeReplicationTasks (Core.Maybe Core.Int)
describeReplicationTasks_maxRecords = Lens.lens (\DescribeReplicationTasks' {maxRecords} -> maxRecords) (\s@DescribeReplicationTasks' {} a -> s {maxRecords = a} :: DescribeReplicationTasks)

instance Core.AWSPager DescribeReplicationTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationTasksResponse_marker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationTasksResponse_replicationTasks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeReplicationTasks_marker
          Lens..~ rs
          Lens.^? describeReplicationTasksResponse_marker
            Core.. Lens._Just

instance Core.AWSRequest DescribeReplicationTasks where
  type
    AWSResponse DescribeReplicationTasks =
      DescribeReplicationTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTasksResponse'
            Core.<$> (x Core..?> "ReplicationTasks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeReplicationTasks

instance Core.NFData DescribeReplicationTasks

instance Core.ToHeaders DescribeReplicationTasks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeReplicationTasks where
  toJSON DescribeReplicationTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("WithoutSettings" Core..=)
              Core.<$> withoutSettings,
            ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeReplicationTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReplicationTasks where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTasksResponse' smart constructor.
data DescribeReplicationTasksResponse = DescribeReplicationTasksResponse'
  { -- | A description of the replication tasks.
    replicationTasks :: Core.Maybe [ReplicationTask],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReplicationTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTasks', 'describeReplicationTasksResponse_replicationTasks' - A description of the replication tasks.
--
-- 'marker', 'describeReplicationTasksResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeReplicationTasksResponse_httpStatus' - The response's http status code.
newDescribeReplicationTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReplicationTasksResponse
newDescribeReplicationTasksResponse pHttpStatus_ =
  DescribeReplicationTasksResponse'
    { replicationTasks =
        Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the replication tasks.
describeReplicationTasksResponse_replicationTasks :: Lens.Lens' DescribeReplicationTasksResponse (Core.Maybe [ReplicationTask])
describeReplicationTasksResponse_replicationTasks = Lens.lens (\DescribeReplicationTasksResponse' {replicationTasks} -> replicationTasks) (\s@DescribeReplicationTasksResponse' {} a -> s {replicationTasks = a} :: DescribeReplicationTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTasksResponse_marker :: Lens.Lens' DescribeReplicationTasksResponse (Core.Maybe Core.Text)
describeReplicationTasksResponse_marker = Lens.lens (\DescribeReplicationTasksResponse' {marker} -> marker) (\s@DescribeReplicationTasksResponse' {} a -> s {marker = a} :: DescribeReplicationTasksResponse)

-- | The response's http status code.
describeReplicationTasksResponse_httpStatus :: Lens.Lens' DescribeReplicationTasksResponse Core.Int
describeReplicationTasksResponse_httpStatus = Lens.lens (\DescribeReplicationTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTasksResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTasksResponse)

instance Core.NFData DescribeReplicationTasksResponse
