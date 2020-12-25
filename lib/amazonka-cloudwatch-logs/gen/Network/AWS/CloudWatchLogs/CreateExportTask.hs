{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CreateExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an export task, which allows you to efficiently export data from a log group to an Amazon S3 bucket. When you perform a @CreateExportTask@ operation, you must use credentials that have permission to write to the S3 bucket that you specify as the destination.
--
-- This is an asynchronous call. If all the required information is provided, this operation initiates an export task and responds with the ID of the task. After the task has started, you can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeExportTasks.html DescribeExportTasks> to get the status of the export task. Each account can only have one active (@RUNNING@ or @PENDING@ ) export task at a time. To cancel an export task, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_CancelExportTask.html CancelExportTask> .
-- You can export logs from multiple log groups or multiple time ranges to the same S3 bucket. To separate out log data for each export task, you can specify a prefix to be used as the Amazon S3 key prefix for all exported objects.
-- Exporting to S3 buckets that are encrypted with AES-256 is supported. Exporting to S3 buckets encrypted with SSE-KMS is not supported.
module Network.AWS.CloudWatchLogs.CreateExportTask
  ( -- * Creating a request
    CreateExportTask (..),
    mkCreateExportTask,

    -- ** Request lenses
    cetLogGroupName,
    cetFrom,
    cetTo,
    cetDestination,
    cetDestinationPrefix,
    cetLogStreamNamePrefix,
    cetTaskName,

    -- * Destructuring the response
    CreateExportTaskResponse (..),
    mkCreateExportTaskResponse,

    -- ** Response lenses
    cetrrsTaskId,
    cetrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateExportTask' smart constructor.
data CreateExportTask = CreateExportTask'
  { -- | The name of the log group.
    logGroupName :: Types.LogGroupName,
    -- | The start time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp earlier than this time are not exported.
    from :: Core.Natural,
    -- | The end time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
    to :: Core.Natural,
    -- | The name of S3 bucket for the exported log data. The bucket must be in the same AWS region.
    destination :: Types.ExportDestinationBucket,
    -- | The prefix used as the start of the key for every object exported. If you don't specify a value, the default is @exportedlogs@ .
    destinationPrefix :: Core.Maybe Types.DestinationPrefix,
    -- | Export only log streams that match the provided prefix. If you don't specify a value, no prefix filter is applied.
    logStreamNamePrefix :: Core.Maybe Types.LogStreamNamePrefix,
    -- | The name of the export task.
    taskName :: Core.Maybe Types.TaskName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExportTask' value with any optional fields omitted.
mkCreateExportTask ::
  -- | 'logGroupName'
  Types.LogGroupName ->
  -- | 'from'
  Core.Natural ->
  -- | 'to'
  Core.Natural ->
  -- | 'destination'
  Types.ExportDestinationBucket ->
  CreateExportTask
mkCreateExportTask logGroupName from to destination =
  CreateExportTask'
    { logGroupName,
      from,
      to,
      destination,
      destinationPrefix = Core.Nothing,
      logStreamNamePrefix = Core.Nothing,
      taskName = Core.Nothing
    }

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetLogGroupName :: Lens.Lens' CreateExportTask Types.LogGroupName
cetLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED cetLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The start time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp earlier than this time are not exported.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetFrom :: Lens.Lens' CreateExportTask Core.Natural
cetFrom = Lens.field @"from"
{-# DEPRECATED cetFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The end time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTo :: Lens.Lens' CreateExportTask Core.Natural
cetTo = Lens.field @"to"
{-# DEPRECATED cetTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The name of S3 bucket for the exported log data. The bucket must be in the same AWS region.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetDestination :: Lens.Lens' CreateExportTask Types.ExportDestinationBucket
cetDestination = Lens.field @"destination"
{-# DEPRECATED cetDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The prefix used as the start of the key for every object exported. If you don't specify a value, the default is @exportedlogs@ .
--
-- /Note:/ Consider using 'destinationPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetDestinationPrefix :: Lens.Lens' CreateExportTask (Core.Maybe Types.DestinationPrefix)
cetDestinationPrefix = Lens.field @"destinationPrefix"
{-# DEPRECATED cetDestinationPrefix "Use generic-lens or generic-optics with 'destinationPrefix' instead." #-}

-- | Export only log streams that match the provided prefix. If you don't specify a value, no prefix filter is applied.
--
-- /Note:/ Consider using 'logStreamNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetLogStreamNamePrefix :: Lens.Lens' CreateExportTask (Core.Maybe Types.LogStreamNamePrefix)
cetLogStreamNamePrefix = Lens.field @"logStreamNamePrefix"
{-# DEPRECATED cetLogStreamNamePrefix "Use generic-lens or generic-optics with 'logStreamNamePrefix' instead." #-}

-- | The name of the export task.
--
-- /Note:/ Consider using 'taskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTaskName :: Lens.Lens' CreateExportTask (Core.Maybe Types.TaskName)
cetTaskName = Lens.field @"taskName"
{-# DEPRECATED cetTaskName "Use generic-lens or generic-optics with 'taskName' instead." #-}

instance Core.FromJSON CreateExportTask where
  toJSON CreateExportTask {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("from" Core..= from),
            Core.Just ("to" Core..= to),
            Core.Just ("destination" Core..= destination),
            ("destinationPrefix" Core..=) Core.<$> destinationPrefix,
            ("logStreamNamePrefix" Core..=) Core.<$> logStreamNamePrefix,
            ("taskName" Core..=) Core.<$> taskName
          ]
      )

instance Core.AWSRequest CreateExportTask where
  type Rs CreateExportTask = CreateExportTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Logs_20140328.CreateExportTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExportTaskResponse'
            Core.<$> (x Core..:? "taskId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateExportTaskResponse' smart constructor.
data CreateExportTaskResponse = CreateExportTaskResponse'
  { -- | The ID of the export task.
    taskId :: Core.Maybe Types.ExportTaskId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateExportTaskResponse' value with any optional fields omitted.
mkCreateExportTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateExportTaskResponse
mkCreateExportTaskResponse responseStatus =
  CreateExportTaskResponse' {taskId = Core.Nothing, responseStatus}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrrsTaskId :: Lens.Lens' CreateExportTaskResponse (Core.Maybe Types.ExportTaskId)
cetrrsTaskId = Lens.field @"taskId"
{-# DEPRECATED cetrrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrrsResponseStatus :: Lens.Lens' CreateExportTaskResponse Core.Int
cetrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cetrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
