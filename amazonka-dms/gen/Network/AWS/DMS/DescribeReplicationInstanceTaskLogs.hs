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
-- Module      : Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the task logs for the specified task.
module Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
  ( -- * Creating a Request
    DescribeReplicationInstanceTaskLogs (..),
    newDescribeReplicationInstanceTaskLogs,

    -- * Request Lenses
    describeReplicationInstanceTaskLogs_marker,
    describeReplicationInstanceTaskLogs_maxRecords,
    describeReplicationInstanceTaskLogs_replicationInstanceArn,

    -- * Destructuring the Response
    DescribeReplicationInstanceTaskLogsResponse (..),
    newDescribeReplicationInstanceTaskLogsResponse,

    -- * Response Lenses
    describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs,
    describeReplicationInstanceTaskLogsResponse_replicationInstanceArn,
    describeReplicationInstanceTaskLogsResponse_marker,
    describeReplicationInstanceTaskLogsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeReplicationInstanceTaskLogs' smart constructor.
data DescribeReplicationInstanceTaskLogs = DescribeReplicationInstanceTaskLogs'
  { -- | An optional pagination token provided by a previous request. If this
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
    maxRecords :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReplicationInstanceTaskLogs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReplicationInstanceTaskLogs_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationInstanceTaskLogs_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'replicationInstanceArn', 'describeReplicationInstanceTaskLogs_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
newDescribeReplicationInstanceTaskLogs ::
  -- | 'replicationInstanceArn'
  Core.Text ->
  DescribeReplicationInstanceTaskLogs
newDescribeReplicationInstanceTaskLogs
  pReplicationInstanceArn_ =
    DescribeReplicationInstanceTaskLogs'
      { marker =
          Core.Nothing,
        maxRecords = Core.Nothing,
        replicationInstanceArn =
          pReplicationInstanceArn_
      }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstanceTaskLogs_marker :: Lens.Lens' DescribeReplicationInstanceTaskLogs (Core.Maybe Core.Text)
describeReplicationInstanceTaskLogs_marker = Lens.lens (\DescribeReplicationInstanceTaskLogs' {marker} -> marker) (\s@DescribeReplicationInstanceTaskLogs' {} a -> s {marker = a} :: DescribeReplicationInstanceTaskLogs)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationInstanceTaskLogs_maxRecords :: Lens.Lens' DescribeReplicationInstanceTaskLogs (Core.Maybe Core.Int)
describeReplicationInstanceTaskLogs_maxRecords = Lens.lens (\DescribeReplicationInstanceTaskLogs' {maxRecords} -> maxRecords) (\s@DescribeReplicationInstanceTaskLogs' {} a -> s {maxRecords = a} :: DescribeReplicationInstanceTaskLogs)

-- | The Amazon Resource Name (ARN) of the replication instance.
describeReplicationInstanceTaskLogs_replicationInstanceArn :: Lens.Lens' DescribeReplicationInstanceTaskLogs Core.Text
describeReplicationInstanceTaskLogs_replicationInstanceArn = Lens.lens (\DescribeReplicationInstanceTaskLogs' {replicationInstanceArn} -> replicationInstanceArn) (\s@DescribeReplicationInstanceTaskLogs' {} a -> s {replicationInstanceArn = a} :: DescribeReplicationInstanceTaskLogs)

instance
  Core.AWSRequest
    DescribeReplicationInstanceTaskLogs
  where
  type
    AWSResponse DescribeReplicationInstanceTaskLogs =
      DescribeReplicationInstanceTaskLogsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationInstanceTaskLogsResponse'
            Core.<$> ( x Core..?> "ReplicationInstanceTaskLogs"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "ReplicationInstanceArn")
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeReplicationInstanceTaskLogs

instance
  Core.NFData
    DescribeReplicationInstanceTaskLogs

instance
  Core.ToHeaders
    DescribeReplicationInstanceTaskLogs
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationInstanceTaskLogs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeReplicationInstanceTaskLogs
  where
  toJSON DescribeReplicationInstanceTaskLogs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords,
            Core.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              )
          ]
      )

instance
  Core.ToPath
    DescribeReplicationInstanceTaskLogs
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeReplicationInstanceTaskLogs
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeReplicationInstanceTaskLogsResponse' smart constructor.
data DescribeReplicationInstanceTaskLogsResponse = DescribeReplicationInstanceTaskLogsResponse'
  { -- | An array of replication task log metadata. Each member of the array
    -- contains the replication task name, ARN, and task log size (in bytes).
    replicationInstanceTaskLogs :: Core.Maybe [ReplicationInstanceTaskLog],
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Maybe Core.Text,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReplicationInstanceTaskLogsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationInstanceTaskLogs', 'describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs' - An array of replication task log metadata. Each member of the array
-- contains the replication task name, ARN, and task log size (in bytes).
--
-- 'replicationInstanceArn', 'describeReplicationInstanceTaskLogsResponse_replicationInstanceArn' - The Amazon Resource Name (ARN) of the replication instance.
--
-- 'marker', 'describeReplicationInstanceTaskLogsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeReplicationInstanceTaskLogsResponse_httpStatus' - The response's http status code.
newDescribeReplicationInstanceTaskLogsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReplicationInstanceTaskLogsResponse
newDescribeReplicationInstanceTaskLogsResponse
  pHttpStatus_ =
    DescribeReplicationInstanceTaskLogsResponse'
      { replicationInstanceTaskLogs =
          Core.Nothing,
        replicationInstanceArn =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of replication task log metadata. Each member of the array
-- contains the replication task name, ARN, and task log size (in bytes).
describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Core.Maybe [ReplicationInstanceTaskLog])
describeReplicationInstanceTaskLogsResponse_replicationInstanceTaskLogs = Lens.lens (\DescribeReplicationInstanceTaskLogsResponse' {replicationInstanceTaskLogs} -> replicationInstanceTaskLogs) (\s@DescribeReplicationInstanceTaskLogsResponse' {} a -> s {replicationInstanceTaskLogs = a} :: DescribeReplicationInstanceTaskLogsResponse) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the replication instance.
describeReplicationInstanceTaskLogsResponse_replicationInstanceArn :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Core.Maybe Core.Text)
describeReplicationInstanceTaskLogsResponse_replicationInstanceArn = Lens.lens (\DescribeReplicationInstanceTaskLogsResponse' {replicationInstanceArn} -> replicationInstanceArn) (\s@DescribeReplicationInstanceTaskLogsResponse' {} a -> s {replicationInstanceArn = a} :: DescribeReplicationInstanceTaskLogsResponse)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationInstanceTaskLogsResponse_marker :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Core.Maybe Core.Text)
describeReplicationInstanceTaskLogsResponse_marker = Lens.lens (\DescribeReplicationInstanceTaskLogsResponse' {marker} -> marker) (\s@DescribeReplicationInstanceTaskLogsResponse' {} a -> s {marker = a} :: DescribeReplicationInstanceTaskLogsResponse)

-- | The response's http status code.
describeReplicationInstanceTaskLogsResponse_httpStatus :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse Core.Int
describeReplicationInstanceTaskLogsResponse_httpStatus = Lens.lens (\DescribeReplicationInstanceTaskLogsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationInstanceTaskLogsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationInstanceTaskLogsResponse)

instance
  Core.NFData
    DescribeReplicationInstanceTaskLogsResponse
