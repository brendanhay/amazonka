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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReplicationTasks' smart constructor.
data DescribeReplicationTasks = DescribeReplicationTasks'
  { -- | An option to set to avoid returning information about settings. Use this
    -- to reduce overhead when setting information is too large. To use this
    -- option, choose @true@; otherwise, choose @false@ (the default).
    withoutSettings :: Prelude.Maybe Prelude.Bool,
    -- | Filters applied to replication tasks.
    --
    -- Valid filter names: replication-task-arn | replication-task-id |
    -- migration-type | endpoint-arn | replication-instance-arn
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      filters = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | An option to set to avoid returning information about settings. Use this
-- to reduce overhead when setting information is too large. To use this
-- option, choose @true@; otherwise, choose @false@ (the default).
describeReplicationTasks_withoutSettings :: Lens.Lens' DescribeReplicationTasks (Prelude.Maybe Prelude.Bool)
describeReplicationTasks_withoutSettings = Lens.lens (\DescribeReplicationTasks' {withoutSettings} -> withoutSettings) (\s@DescribeReplicationTasks' {} a -> s {withoutSettings = a} :: DescribeReplicationTasks)

-- | Filters applied to replication tasks.
--
-- Valid filter names: replication-task-arn | replication-task-id |
-- migration-type | endpoint-arn | replication-instance-arn
describeReplicationTasks_filters :: Lens.Lens' DescribeReplicationTasks (Prelude.Maybe [Filter])
describeReplicationTasks_filters = Lens.lens (\DescribeReplicationTasks' {filters} -> filters) (\s@DescribeReplicationTasks' {} a -> s {filters = a} :: DescribeReplicationTasks) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTasks_marker :: Lens.Lens' DescribeReplicationTasks (Prelude.Maybe Prelude.Text)
describeReplicationTasks_marker = Lens.lens (\DescribeReplicationTasks' {marker} -> marker) (\s@DescribeReplicationTasks' {} a -> s {marker = a} :: DescribeReplicationTasks)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationTasks_maxRecords :: Lens.Lens' DescribeReplicationTasks (Prelude.Maybe Prelude.Int)
describeReplicationTasks_maxRecords = Lens.lens (\DescribeReplicationTasks' {maxRecords} -> maxRecords) (\s@DescribeReplicationTasks' {} a -> s {maxRecords = a} :: DescribeReplicationTasks)

instance Core.AWSPager DescribeReplicationTasks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationTasksResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationTasksResponse_replicationTasks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReplicationTasks_marker
          Lens..~ rs
          Lens.^? describeReplicationTasksResponse_marker
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeReplicationTasks where
  type
    AWSResponse DescribeReplicationTasks =
      DescribeReplicationTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTasksResponse'
            Prelude.<$> ( x Core..?> "ReplicationTasks"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReplicationTasks

instance Prelude.NFData DescribeReplicationTasks

instance Core.ToHeaders DescribeReplicationTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationTasks" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeReplicationTasks where
  toJSON DescribeReplicationTasks' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WithoutSettings" Core..=)
              Prelude.<$> withoutSettings,
            ("Filters" Core..=) Prelude.<$> filters,
            ("Marker" Core..=) Prelude.<$> marker,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance Core.ToPath DescribeReplicationTasks where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeReplicationTasks where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTasksResponse' smart constructor.
data DescribeReplicationTasksResponse = DescribeReplicationTasksResponse'
  { -- | A description of the replication tasks.
    replicationTasks :: Prelude.Maybe [ReplicationTask],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeReplicationTasksResponse
newDescribeReplicationTasksResponse pHttpStatus_ =
  DescribeReplicationTasksResponse'
    { replicationTasks =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the replication tasks.
describeReplicationTasksResponse_replicationTasks :: Lens.Lens' DescribeReplicationTasksResponse (Prelude.Maybe [ReplicationTask])
describeReplicationTasksResponse_replicationTasks = Lens.lens (\DescribeReplicationTasksResponse' {replicationTasks} -> replicationTasks) (\s@DescribeReplicationTasksResponse' {} a -> s {replicationTasks = a} :: DescribeReplicationTasksResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTasksResponse_marker :: Lens.Lens' DescribeReplicationTasksResponse (Prelude.Maybe Prelude.Text)
describeReplicationTasksResponse_marker = Lens.lens (\DescribeReplicationTasksResponse' {marker} -> marker) (\s@DescribeReplicationTasksResponse' {} a -> s {marker = a} :: DescribeReplicationTasksResponse)

-- | The response's http status code.
describeReplicationTasksResponse_httpStatus :: Lens.Lens' DescribeReplicationTasksResponse Prelude.Int
describeReplicationTasksResponse_httpStatus = Lens.lens (\DescribeReplicationTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTasksResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTasksResponse)

instance
  Prelude.NFData
    DescribeReplicationTasksResponse
