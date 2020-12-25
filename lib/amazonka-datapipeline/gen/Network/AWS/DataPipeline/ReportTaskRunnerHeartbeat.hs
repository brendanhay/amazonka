{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskRunnerHeartbeat@ every 15 minutes to indicate that they are operational. If the AWS Data Pipeline Task Runner is launched on a resource managed by AWS Data Pipeline, the web service can use this call to detect when the task runner application has failed and restart a new instance.
module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
  ( -- * Creating a request
    ReportTaskRunnerHeartbeat (..),
    mkReportTaskRunnerHeartbeat,

    -- ** Request lenses
    rtrhTaskrunnerId,
    rtrhHostname,
    rtrhWorkerGroup,

    -- * Destructuring the response
    ReportTaskRunnerHeartbeatResponse (..),
    mkReportTaskRunnerHeartbeatResponse,

    -- ** Response lenses
    rtrhrrsTerminate,
    rtrhrrsResponseStatus,
  )
where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ReportTaskRunnerHeartbeat.
--
-- /See:/ 'mkReportTaskRunnerHeartbeat' smart constructor.
data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat'
  { -- | The ID of the task runner. This value should be unique across your AWS account. In the case of AWS Data Pipeline Task Runner launched on a resource managed by AWS Data Pipeline, the web service provides a unique identifier when it launches the application. If you have written a custom task runner, you should assign a unique identifier for the task runner.
    taskrunnerId :: Types.Id,
    -- | The public DNS name of the task runner.
    hostname :: Core.Maybe Types.Id,
    -- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
    workerGroup :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportTaskRunnerHeartbeat' value with any optional fields omitted.
mkReportTaskRunnerHeartbeat ::
  -- | 'taskrunnerId'
  Types.Id ->
  ReportTaskRunnerHeartbeat
mkReportTaskRunnerHeartbeat taskrunnerId =
  ReportTaskRunnerHeartbeat'
    { taskrunnerId,
      hostname = Core.Nothing,
      workerGroup = Core.Nothing
    }

-- | The ID of the task runner. This value should be unique across your AWS account. In the case of AWS Data Pipeline Task Runner launched on a resource managed by AWS Data Pipeline, the web service provides a unique identifier when it launches the application. If you have written a custom task runner, you should assign a unique identifier for the task runner.
--
-- /Note:/ Consider using 'taskrunnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhTaskrunnerId :: Lens.Lens' ReportTaskRunnerHeartbeat Types.Id
rtrhTaskrunnerId = Lens.field @"taskrunnerId"
{-# DEPRECATED rtrhTaskrunnerId "Use generic-lens or generic-optics with 'taskrunnerId' instead." #-}

-- | The public DNS name of the task runner.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhHostname :: Lens.Lens' ReportTaskRunnerHeartbeat (Core.Maybe Types.Id)
rtrhHostname = Lens.field @"hostname"
{-# DEPRECATED rtrhHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
--
-- /Note:/ Consider using 'workerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhWorkerGroup :: Lens.Lens' ReportTaskRunnerHeartbeat (Core.Maybe Types.String)
rtrhWorkerGroup = Lens.field @"workerGroup"
{-# DEPRECATED rtrhWorkerGroup "Use generic-lens or generic-optics with 'workerGroup' instead." #-}

instance Core.FromJSON ReportTaskRunnerHeartbeat where
  toJSON ReportTaskRunnerHeartbeat {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("taskrunnerId" Core..= taskrunnerId),
            ("hostname" Core..=) Core.<$> hostname,
            ("workerGroup" Core..=) Core.<$> workerGroup
          ]
      )

instance Core.AWSRequest ReportTaskRunnerHeartbeat where
  type
    Rs ReportTaskRunnerHeartbeat =
      ReportTaskRunnerHeartbeatResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DataPipeline.ReportTaskRunnerHeartbeat")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ReportTaskRunnerHeartbeatResponse'
            Core.<$> (x Core..: "terminate") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of ReportTaskRunnerHeartbeat.
--
-- /See:/ 'mkReportTaskRunnerHeartbeatResponse' smart constructor.
data ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse'
  { -- | Indicates whether the calling task runner should terminate.
    terminate :: Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportTaskRunnerHeartbeatResponse' value with any optional fields omitted.
mkReportTaskRunnerHeartbeatResponse ::
  -- | 'terminate'
  Core.Bool ->
  -- | 'responseStatus'
  Core.Int ->
  ReportTaskRunnerHeartbeatResponse
mkReportTaskRunnerHeartbeatResponse terminate responseStatus =
  ReportTaskRunnerHeartbeatResponse' {terminate, responseStatus}

-- | Indicates whether the calling task runner should terminate.
--
-- /Note:/ Consider using 'terminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhrrsTerminate :: Lens.Lens' ReportTaskRunnerHeartbeatResponse Core.Bool
rtrhrrsTerminate = Lens.field @"terminate"
{-# DEPRECATED rtrhrrsTerminate "Use generic-lens or generic-optics with 'terminate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhrrsResponseStatus :: Lens.Lens' ReportTaskRunnerHeartbeatResponse Core.Int
rtrhrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rtrhrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
