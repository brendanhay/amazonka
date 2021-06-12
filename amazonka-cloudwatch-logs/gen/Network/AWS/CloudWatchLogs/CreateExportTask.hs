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
-- Module      : Network.AWS.CloudWatchLogs.CreateExportTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an export task, which allows you to efficiently export data from
-- a log group to an Amazon S3 bucket. When you perform a
-- @CreateExportTask@ operation, you must use credentials that have
-- permission to write to the S3 bucket that you specify as the
-- destination.
--
-- This is an asynchronous call. If all the required information is
-- provided, this operation initiates an export task and responds with the
-- ID of the task. After the task has started, you can use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeExportTasks.html DescribeExportTasks>
-- to get the status of the export task. Each account can only have one
-- active (@RUNNING@ or @PENDING@) export task at a time. To cancel an
-- export task, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_CancelExportTask.html CancelExportTask>.
--
-- You can export logs from multiple log groups or multiple time ranges to
-- the same S3 bucket. To separate out log data for each export task, you
-- can specify a prefix to be used as the Amazon S3 key prefix for all
-- exported objects.
--
-- Exporting to S3 buckets that are encrypted with AES-256 is supported.
-- Exporting to S3 buckets encrypted with SSE-KMS is not supported.
module Network.AWS.CloudWatchLogs.CreateExportTask
  ( -- * Creating a Request
    CreateExportTask (..),
    newCreateExportTask,

    -- * Request Lenses
    createExportTask_logStreamNamePrefix,
    createExportTask_taskName,
    createExportTask_destinationPrefix,
    createExportTask_logGroupName,
    createExportTask_from,
    createExportTask_to,
    createExportTask_destination,

    -- * Destructuring the Response
    CreateExportTaskResponse (..),
    newCreateExportTaskResponse,

    -- * Response Lenses
    createExportTaskResponse_taskId,
    createExportTaskResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateExportTask' smart constructor.
data CreateExportTask = CreateExportTask'
  { -- | Export only log streams that match the provided prefix. If you don\'t
    -- specify a value, no prefix filter is applied.
    logStreamNamePrefix :: Core.Maybe Core.Text,
    -- | The name of the export task.
    taskName :: Core.Maybe Core.Text,
    -- | The prefix used as the start of the key for every object exported. If
    -- you don\'t specify a value, the default is @exportedlogs@.
    destinationPrefix :: Core.Maybe Core.Text,
    -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The start time of the range for the request, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp
    -- earlier than this time are not exported.
    from :: Core.Natural,
    -- | The end time of the range for the request, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp
    -- later than this time are not exported.
    to :: Core.Natural,
    -- | The name of S3 bucket for the exported log data. The bucket must be in
    -- the same AWS region.
    destination :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreamNamePrefix', 'createExportTask_logStreamNamePrefix' - Export only log streams that match the provided prefix. If you don\'t
-- specify a value, no prefix filter is applied.
--
-- 'taskName', 'createExportTask_taskName' - The name of the export task.
--
-- 'destinationPrefix', 'createExportTask_destinationPrefix' - The prefix used as the start of the key for every object exported. If
-- you don\'t specify a value, the default is @exportedlogs@.
--
-- 'logGroupName', 'createExportTask_logGroupName' - The name of the log group.
--
-- 'from', 'createExportTask_from' - The start time of the range for the request, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp
-- earlier than this time are not exported.
--
-- 'to', 'createExportTask_to' - The end time of the range for the request, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp
-- later than this time are not exported.
--
-- 'destination', 'createExportTask_destination' - The name of S3 bucket for the exported log data. The bucket must be in
-- the same AWS region.
newCreateExportTask ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'from'
  Core.Natural ->
  -- | 'to'
  Core.Natural ->
  -- | 'destination'
  Core.Text ->
  CreateExportTask
newCreateExportTask
  pLogGroupName_
  pFrom_
  pTo_
  pDestination_ =
    CreateExportTask'
      { logStreamNamePrefix =
          Core.Nothing,
        taskName = Core.Nothing,
        destinationPrefix = Core.Nothing,
        logGroupName = pLogGroupName_,
        from = pFrom_,
        to = pTo_,
        destination = pDestination_
      }

-- | Export only log streams that match the provided prefix. If you don\'t
-- specify a value, no prefix filter is applied.
createExportTask_logStreamNamePrefix :: Lens.Lens' CreateExportTask (Core.Maybe Core.Text)
createExportTask_logStreamNamePrefix = Lens.lens (\CreateExportTask' {logStreamNamePrefix} -> logStreamNamePrefix) (\s@CreateExportTask' {} a -> s {logStreamNamePrefix = a} :: CreateExportTask)

-- | The name of the export task.
createExportTask_taskName :: Lens.Lens' CreateExportTask (Core.Maybe Core.Text)
createExportTask_taskName = Lens.lens (\CreateExportTask' {taskName} -> taskName) (\s@CreateExportTask' {} a -> s {taskName = a} :: CreateExportTask)

-- | The prefix used as the start of the key for every object exported. If
-- you don\'t specify a value, the default is @exportedlogs@.
createExportTask_destinationPrefix :: Lens.Lens' CreateExportTask (Core.Maybe Core.Text)
createExportTask_destinationPrefix = Lens.lens (\CreateExportTask' {destinationPrefix} -> destinationPrefix) (\s@CreateExportTask' {} a -> s {destinationPrefix = a} :: CreateExportTask)

-- | The name of the log group.
createExportTask_logGroupName :: Lens.Lens' CreateExportTask Core.Text
createExportTask_logGroupName = Lens.lens (\CreateExportTask' {logGroupName} -> logGroupName) (\s@CreateExportTask' {} a -> s {logGroupName = a} :: CreateExportTask)

-- | The start time of the range for the request, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp
-- earlier than this time are not exported.
createExportTask_from :: Lens.Lens' CreateExportTask Core.Natural
createExportTask_from = Lens.lens (\CreateExportTask' {from} -> from) (\s@CreateExportTask' {} a -> s {from = a} :: CreateExportTask)

-- | The end time of the range for the request, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp
-- later than this time are not exported.
createExportTask_to :: Lens.Lens' CreateExportTask Core.Natural
createExportTask_to = Lens.lens (\CreateExportTask' {to} -> to) (\s@CreateExportTask' {} a -> s {to = a} :: CreateExportTask)

-- | The name of S3 bucket for the exported log data. The bucket must be in
-- the same AWS region.
createExportTask_destination :: Lens.Lens' CreateExportTask Core.Text
createExportTask_destination = Lens.lens (\CreateExportTask' {destination} -> destination) (\s@CreateExportTask' {} a -> s {destination = a} :: CreateExportTask)

instance Core.AWSRequest CreateExportTask where
  type
    AWSResponse CreateExportTask =
      CreateExportTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExportTaskResponse'
            Core.<$> (x Core..?> "taskId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateExportTask

instance Core.NFData CreateExportTask

instance Core.ToHeaders CreateExportTask where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.CreateExportTask" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateExportTask where
  toJSON CreateExportTask' {..} =
    Core.object
      ( Core.catMaybes
          [ ("logStreamNamePrefix" Core..=)
              Core.<$> logStreamNamePrefix,
            ("taskName" Core..=) Core.<$> taskName,
            ("destinationPrefix" Core..=)
              Core.<$> destinationPrefix,
            Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("from" Core..= from),
            Core.Just ("to" Core..= to),
            Core.Just ("destination" Core..= destination)
          ]
      )

instance Core.ToPath CreateExportTask where
  toPath = Core.const "/"

instance Core.ToQuery CreateExportTask where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateExportTaskResponse' smart constructor.
data CreateExportTaskResponse = CreateExportTaskResponse'
  { -- | The ID of the export task.
    taskId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateExportTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskId', 'createExportTaskResponse_taskId' - The ID of the export task.
--
-- 'httpStatus', 'createExportTaskResponse_httpStatus' - The response's http status code.
newCreateExportTaskResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateExportTaskResponse
newCreateExportTaskResponse pHttpStatus_ =
  CreateExportTaskResponse'
    { taskId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the export task.
createExportTaskResponse_taskId :: Lens.Lens' CreateExportTaskResponse (Core.Maybe Core.Text)
createExportTaskResponse_taskId = Lens.lens (\CreateExportTaskResponse' {taskId} -> taskId) (\s@CreateExportTaskResponse' {} a -> s {taskId = a} :: CreateExportTaskResponse)

-- | The response's http status code.
createExportTaskResponse_httpStatus :: Lens.Lens' CreateExportTaskResponse Core.Int
createExportTaskResponse_httpStatus = Lens.lens (\CreateExportTaskResponse' {httpStatus} -> httpStatus) (\s@CreateExportTaskResponse' {} a -> s {httpStatus = a} :: CreateExportTaskResponse)

instance Core.NFData CreateExportTaskResponse
