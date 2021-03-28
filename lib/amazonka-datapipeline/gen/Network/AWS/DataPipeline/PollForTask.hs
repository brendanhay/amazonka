{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.PollForTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @PollForTask@ to receive a task to perform from AWS Data Pipeline. The task runner specifies which tasks it can perform by setting a value for the @workerGroup@ parameter. The task returned can come from any of the pipelines that match the @workerGroup@ value passed in by the task runner and that was launched using the IAM user credentials specified by the task runner.
--
-- If tasks are ready in the work queue, @PollForTask@ returns a response immediately. If no tasks are available in the queue, @PollForTask@ uses long-polling and holds on to a poll connection for up to a 90 seconds, during which time the first newly scheduled task is handed to the task runner. To accomodate this, set the socket timeout in your task runner to 90 seconds. The task runner should not call @PollForTask@ again on the same @workerGroup@ until it receives a response, and this can take up to 90 seconds. 
module Network.AWS.DataPipeline.PollForTask
    (
    -- * Creating a request
      PollForTask (..)
    , mkPollForTask
    -- ** Request lenses
    , pftWorkerGroup
    , pftHostname
    , pftInstanceIdentity

    -- * Destructuring the response
    , PollForTaskResponse (..)
    , mkPollForTaskResponse
    -- ** Response lenses
    , pftrrsTaskObject
    , pftrrsResponseStatus
    ) where

import qualified Network.AWS.DataPipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for PollForTask.
--
-- /See:/ 'mkPollForTask' smart constructor.
data PollForTask = PollForTask'
  { workerGroup :: Core.Text
    -- ^ The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ in the call to @PollForTask@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
  , hostname :: Core.Maybe Types.Hostname
    -- ^ The public DNS name of the calling task runner.
  , instanceIdentity :: Core.Maybe Types.InstanceIdentity
    -- ^ Identity information for the EC2 instance that is hosting the task runner. You can get this value from the instance using @http://169.254.169.254/latest/meta-data/instance-id@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForTask' value with any optional fields omitted.
mkPollForTask
    :: Core.Text -- ^ 'workerGroup'
    -> PollForTask
mkPollForTask workerGroup
  = PollForTask'{workerGroup, hostname = Core.Nothing,
                 instanceIdentity = Core.Nothing}

-- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ in the call to @PollForTask@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
--
-- /Note:/ Consider using 'workerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftWorkerGroup :: Lens.Lens' PollForTask Core.Text
pftWorkerGroup = Lens.field @"workerGroup"
{-# INLINEABLE pftWorkerGroup #-}
{-# DEPRECATED workerGroup "Use generic-lens or generic-optics with 'workerGroup' instead"  #-}

-- | The public DNS name of the calling task runner.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftHostname :: Lens.Lens' PollForTask (Core.Maybe Types.Hostname)
pftHostname = Lens.field @"hostname"
{-# INLINEABLE pftHostname #-}
{-# DEPRECATED hostname "Use generic-lens or generic-optics with 'hostname' instead"  #-}

-- | Identity information for the EC2 instance that is hosting the task runner. You can get this value from the instance using @http://169.254.169.254/latest/meta-data/instance-id@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
--
-- /Note:/ Consider using 'instanceIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftInstanceIdentity :: Lens.Lens' PollForTask (Core.Maybe Types.InstanceIdentity)
pftInstanceIdentity = Lens.field @"instanceIdentity"
{-# INLINEABLE pftInstanceIdentity #-}
{-# DEPRECATED instanceIdentity "Use generic-lens or generic-optics with 'instanceIdentity' instead"  #-}

instance Core.ToQuery PollForTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PollForTask where
        toHeaders PollForTask{..}
          = Core.pure ("X-Amz-Target", "DataPipeline.PollForTask") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PollForTask where
        toJSON PollForTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("workerGroup" Core..= workerGroup),
                  ("hostname" Core..=) Core.<$> hostname,
                  ("instanceIdentity" Core..=) Core.<$> instanceIdentity])

instance Core.AWSRequest PollForTask where
        type Rs PollForTask = PollForTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PollForTaskResponse' Core.<$>
                   (x Core..:? "taskObject") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of PollForTask.
--
-- /See:/ 'mkPollForTaskResponse' smart constructor.
data PollForTaskResponse = PollForTaskResponse'
  { taskObject :: Core.Maybe Types.TaskObject
    -- ^ The information needed to complete the task that is being assigned to the task runner. One of the fields returned in this object is @taskId@ , which contains an identifier for the task being assigned. The calling task runner uses @taskId@ in subsequent calls to 'ReportTaskProgress' and 'SetTaskStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PollForTaskResponse' value with any optional fields omitted.
mkPollForTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PollForTaskResponse
mkPollForTaskResponse responseStatus
  = PollForTaskResponse'{taskObject = Core.Nothing, responseStatus}

-- | The information needed to complete the task that is being assigned to the task runner. One of the fields returned in this object is @taskId@ , which contains an identifier for the task being assigned. The calling task runner uses @taskId@ in subsequent calls to 'ReportTaskProgress' and 'SetTaskStatus' .
--
-- /Note:/ Consider using 'taskObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftrrsTaskObject :: Lens.Lens' PollForTaskResponse (Core.Maybe Types.TaskObject)
pftrrsTaskObject = Lens.field @"taskObject"
{-# INLINEABLE pftrrsTaskObject #-}
{-# DEPRECATED taskObject "Use generic-lens or generic-optics with 'taskObject' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftrrsResponseStatus :: Lens.Lens' PollForTaskResponse Core.Int
pftrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pftrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
