{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindowTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a task assigned to a maintenance window. You can't change the task type, but you can change the following values:
--
--
--     * TaskARN. For example, you can change a RUN_COMMAND task from AWS-RunPowerShellScript to AWS-RunShellScript.
--
--
--     * ServiceRoleArn
--
--
--     * TaskInvocationParameters
--
--
--     * Priority
--
--
--     * MaxConcurrency
--
--
--     * MaxErrors
--
--
-- If the value for a parameter in @UpdateMaintenanceWindowTask@ is null, then the corresponding field is not modified. If you set @Replace@ to true, then all fields required by the 'RegisterTaskWithMaintenanceWindow' action are required for this request. Optional fields that aren't specified are set to null.
-- /Important:/ When you update a maintenance window task that has options specified in @TaskInvocationParameters@ , you must provide again all the @TaskInvocationParameters@ values that you want to retain. The values you do not specify again are removed. For example, suppose that when you registered a Run Command task, you specified @TaskInvocationParameters@ values for @Comment@ , @NotificationConfig@ , and @OutputS3BucketName@ . If you update the maintenance window task and specify only a different @OutputS3BucketName@ value, the values for @Comment@ and @NotificationConfig@ are removed.
module Network.AWS.SSM.UpdateMaintenanceWindowTask
    (
    -- * Creating a request
      UpdateMaintenanceWindowTask (..)
    , mkUpdateMaintenanceWindowTask
    -- ** Request lenses
    , umwtWindowId
    , umwtWindowTaskId
    , umwtDescription
    , umwtLoggingInfo
    , umwtMaxConcurrency
    , umwtMaxErrors
    , umwtName
    , umwtPriority
    , umwtReplace
    , umwtServiceRoleArn
    , umwtTargets
    , umwtTaskArn
    , umwtTaskInvocationParameters
    , umwtTaskParameters

    -- * Destructuring the response
    , UpdateMaintenanceWindowTaskResponse (..)
    , mkUpdateMaintenanceWindowTaskResponse
    -- ** Response lenses
    , umwtrrsDescription
    , umwtrrsLoggingInfo
    , umwtrrsMaxConcurrency
    , umwtrrsMaxErrors
    , umwtrrsName
    , umwtrrsPriority
    , umwtrrsServiceRoleArn
    , umwtrrsTargets
    , umwtrrsTaskArn
    , umwtrrsTaskInvocationParameters
    , umwtrrsTaskParameters
    , umwtrrsWindowId
    , umwtrrsWindowTaskId
    , umwtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateMaintenanceWindowTask' smart constructor.
data UpdateMaintenanceWindowTask = UpdateMaintenanceWindowTask'
  { windowId :: Types.MaintenanceWindowId
    -- ^ The maintenance window ID that contains the task to modify.
  , windowTaskId :: Types.MaintenanceWindowTaskId
    -- ^ The task ID to modify.
  , description :: Core.Maybe Types.Description
    -- ^ The new task description to specify.
  , loggingInfo :: Core.Maybe Types.LoggingInfo
    -- ^ The new logging location in Amazon S3 to specify.
  , maxConcurrency :: Core.Maybe Types.MaxConcurrency
    -- ^ The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is the number of targets that are allowed to run this task in parallel.
  , maxErrors :: Core.Maybe Types.MaxErrors
    -- ^ The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number of errors that are allowed before the task stops being scheduled.
  , name :: Core.Maybe Types.MaintenanceWindowName
    -- ^ The new task name to specify.
  , priority :: Core.Maybe Core.Natural
    -- ^ The new task priority to specify. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
  , replace :: Core.Maybe Core.Bool
    -- ^ If True, then all fields that are required by the RegisterTaskWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
  , serviceRoleArn :: Core.Maybe Types.ServiceRoleArn
    -- ^ The ARN of the IAM service role for Systems Manager to assume when running a maintenance window task. If you do not specify a service role ARN, Systems Manager uses your account's service-linked role. If no service-linked role for Systems Manager exists in your account, it is created when you run @RegisterTaskWithMaintenanceWindow@ .
--
-- For more information, see the following topics in the in the /AWS Systems Manager User Guide/ :
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager> 
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks? > 
--
--
  , targets :: Core.Maybe [Types.Target]
    -- ^ The targets (either instances or tags) to modify. Instances are specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags are specified using Key=tag_name,Values=tag_value. 
  , taskArn :: Core.Maybe Types.TaskArn
    -- ^ The task ARN to modify.
  , taskInvocationParameters :: Core.Maybe Types.MaintenanceWindowTaskInvocationParameters
    -- ^ The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- /Important:/ When you update a maintenance window task that has options specified in @TaskInvocationParameters@ , you must provide again all the @TaskInvocationParameters@ values that you want to retain. The values you do not specify again are removed. For example, suppose that when you registered a Run Command task, you specified @TaskInvocationParameters@ values for @Comment@ , @NotificationConfig@ , and @OutputS3BucketName@ . If you update the maintenance window task and specify only a different @OutputS3BucketName@ value, the values for @Comment@ and @NotificationConfig@ are removed.
  , taskParameters :: Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression)
    -- ^ The parameters to modify.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMaintenanceWindowTask' value with any optional fields omitted.
mkUpdateMaintenanceWindowTask
    :: Types.MaintenanceWindowId -- ^ 'windowId'
    -> Types.MaintenanceWindowTaskId -- ^ 'windowTaskId'
    -> UpdateMaintenanceWindowTask
mkUpdateMaintenanceWindowTask windowId windowTaskId
  = UpdateMaintenanceWindowTask'{windowId, windowTaskId,
                                 description = Core.Nothing, loggingInfo = Core.Nothing,
                                 maxConcurrency = Core.Nothing, maxErrors = Core.Nothing,
                                 name = Core.Nothing, priority = Core.Nothing,
                                 replace = Core.Nothing, serviceRoleArn = Core.Nothing,
                                 targets = Core.Nothing, taskArn = Core.Nothing,
                                 taskInvocationParameters = Core.Nothing,
                                 taskParameters = Core.Nothing}

-- | The maintenance window ID that contains the task to modify.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtWindowId :: Lens.Lens' UpdateMaintenanceWindowTask Types.MaintenanceWindowId
umwtWindowId = Lens.field @"windowId"
{-# INLINEABLE umwtWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

-- | The task ID to modify.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtWindowTaskId :: Lens.Lens' UpdateMaintenanceWindowTask Types.MaintenanceWindowTaskId
umwtWindowTaskId = Lens.field @"windowTaskId"
{-# INLINEABLE umwtWindowTaskId #-}
{-# DEPRECATED windowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead"  #-}

-- | The new task description to specify.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtDescription :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.Description)
umwtDescription = Lens.field @"description"
{-# INLINEABLE umwtDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The new logging location in Amazon S3 to specify.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtLoggingInfo :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.LoggingInfo)
umwtLoggingInfo = Lens.field @"loggingInfo"
{-# INLINEABLE umwtLoggingInfo #-}
{-# DEPRECATED loggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead"  #-}

-- | The new @MaxConcurrency@ value you want to specify. @MaxConcurrency@ is the number of targets that are allowed to run this task in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtMaxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.MaxConcurrency)
umwtMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE umwtMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The new @MaxErrors@ value to specify. @MaxErrors@ is the maximum number of errors that are allowed before the task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtMaxErrors :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.MaxErrors)
umwtMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE umwtMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

-- | The new task name to specify.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtName :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.MaintenanceWindowName)
umwtName = Lens.field @"name"
{-# INLINEABLE umwtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The new task priority to specify. The lower the number, the higher the priority. Tasks that have the same priority are scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtPriority :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Natural)
umwtPriority = Lens.field @"priority"
{-# INLINEABLE umwtPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | If True, then all fields that are required by the RegisterTaskWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtReplace :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Core.Bool)
umwtReplace = Lens.field @"replace"
{-# INLINEABLE umwtReplace #-}
{-# DEPRECATED replace "Use generic-lens or generic-optics with 'replace' instead"  #-}

-- | The ARN of the IAM service role for Systems Manager to assume when running a maintenance window task. If you do not specify a service role ARN, Systems Manager uses your account's service-linked role. If no service-linked role for Systems Manager exists in your account, it is created when you run @RegisterTaskWithMaintenanceWindow@ .
--
-- For more information, see the following topics in the in the /AWS Systems Manager User Guide/ :
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/using-service-linked-roles.html#slr-permissions Using service-linked roles for Systems Manager> 
--
--
--     * <https://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-maintenance-permissions.html#maintenance-window-tasks-service-role Should I use a service-linked role or a custom service role to run maintenance window tasks? > 
--
--
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtServiceRoleArn :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.ServiceRoleArn)
umwtServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE umwtServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | The targets (either instances or tags) to modify. Instances are specified using Key=instanceids,Values=instanceID_1,instanceID_2. Tags are specified using Key=tag_name,Values=tag_value. 
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTargets :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe [Types.Target])
umwtTargets = Lens.field @"targets"
{-# INLINEABLE umwtTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The task ARN to modify.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTaskArn :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.TaskArn)
umwtTaskArn = Lens.field @"taskArn"
{-# INLINEABLE umwtTaskArn #-}
{-# DEPRECATED taskArn "Use generic-lens or generic-optics with 'taskArn' instead"  #-}

-- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty.
--
-- /Important:/ When you update a maintenance window task that has options specified in @TaskInvocationParameters@ , you must provide again all the @TaskInvocationParameters@ values that you want to retain. The values you do not specify again are removed. For example, suppose that when you registered a Run Command task, you specified @TaskInvocationParameters@ values for @Comment@ , @NotificationConfig@ , and @OutputS3BucketName@ . If you update the maintenance window task and specify only a different @OutputS3BucketName@ value, the values for @Comment@ and @NotificationConfig@ are removed.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTaskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe Types.MaintenanceWindowTaskInvocationParameters)
umwtTaskInvocationParameters = Lens.field @"taskInvocationParameters"
{-# INLINEABLE umwtTaskInvocationParameters #-}
{-# DEPRECATED taskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead"  #-}

-- | The parameters to modify.
--
-- The map has the following format:
-- Key: string, between 1 and 255 characters
-- Value: an array of strings, each string is between 1 and 255 characters
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtTaskParameters :: Lens.Lens' UpdateMaintenanceWindowTask (Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression))
umwtTaskParameters = Lens.field @"taskParameters"
{-# INLINEABLE umwtTaskParameters #-}
{-# DEPRECATED taskParameters "Use generic-lens or generic-optics with 'taskParameters' instead"  #-}

instance Core.ToQuery UpdateMaintenanceWindowTask where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateMaintenanceWindowTask where
        toHeaders UpdateMaintenanceWindowTask{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.UpdateMaintenanceWindowTask")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateMaintenanceWindowTask where
        toJSON UpdateMaintenanceWindowTask{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WindowId" Core..= windowId),
                  Core.Just ("WindowTaskId" Core..= windowTaskId),
                  ("Description" Core..=) Core.<$> description,
                  ("LoggingInfo" Core..=) Core.<$> loggingInfo,
                  ("MaxConcurrency" Core..=) Core.<$> maxConcurrency,
                  ("MaxErrors" Core..=) Core.<$> maxErrors,
                  ("Name" Core..=) Core.<$> name,
                  ("Priority" Core..=) Core.<$> priority,
                  ("Replace" Core..=) Core.<$> replace,
                  ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
                  ("Targets" Core..=) Core.<$> targets,
                  ("TaskArn" Core..=) Core.<$> taskArn,
                  ("TaskInvocationParameters" Core..=) Core.<$>
                    taskInvocationParameters,
                  ("TaskParameters" Core..=) Core.<$> taskParameters])

instance Core.AWSRequest UpdateMaintenanceWindowTask where
        type Rs UpdateMaintenanceWindowTask =
             UpdateMaintenanceWindowTaskResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateMaintenanceWindowTaskResponse' Core.<$>
                   (x Core..:? "Description") Core.<*> x Core..:? "LoggingInfo"
                     Core.<*> x Core..:? "MaxConcurrency"
                     Core.<*> x Core..:? "MaxErrors"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "Priority"
                     Core.<*> x Core..:? "ServiceRoleArn"
                     Core.<*> x Core..:? "Targets"
                     Core.<*> x Core..:? "TaskArn"
                     Core.<*> x Core..:? "TaskInvocationParameters"
                     Core.<*> x Core..:? "TaskParameters"
                     Core.<*> x Core..:? "WindowId"
                     Core.<*> x Core..:? "WindowTaskId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateMaintenanceWindowTaskResponse' smart constructor.
data UpdateMaintenanceWindowTaskResponse = UpdateMaintenanceWindowTaskResponse'
  { description :: Core.Maybe Types.Description
    -- ^ The updated task description.
  , loggingInfo :: Core.Maybe Types.LoggingInfo
    -- ^ The updated logging information in Amazon S3.
  , maxConcurrency :: Core.Maybe Types.MaxConcurrency
    -- ^ The updated MaxConcurrency value.
  , maxErrors :: Core.Maybe Types.MaxErrors
    -- ^ The updated MaxErrors value.
  , name :: Core.Maybe Types.MaintenanceWindowName
    -- ^ The updated task name.
  , priority :: Core.Maybe Core.Natural
    -- ^ The updated priority value.
  , serviceRoleArn :: Core.Maybe Types.ServiceRoleArn
    -- ^ The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
  , targets :: Core.Maybe [Types.Target]
    -- ^ The updated target values.
  , taskArn :: Core.Maybe Types.TaskArn
    -- ^ The updated task ARN value.
  , taskInvocationParameters :: Core.Maybe Types.MaintenanceWindowTaskInvocationParameters
    -- ^ The updated parameter values.
  , taskParameters :: Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression)
    -- ^ The updated parameter values.
  , windowId :: Core.Maybe Types.MaintenanceWindowId
    -- ^ The ID of the maintenance window that was updated.
  , windowTaskId :: Core.Maybe Types.MaintenanceWindowTaskId
    -- ^ The task ID of the maintenance window that was updated.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMaintenanceWindowTaskResponse' value with any optional fields omitted.
mkUpdateMaintenanceWindowTaskResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateMaintenanceWindowTaskResponse
mkUpdateMaintenanceWindowTaskResponse responseStatus
  = UpdateMaintenanceWindowTaskResponse'{description = Core.Nothing,
                                         loggingInfo = Core.Nothing, maxConcurrency = Core.Nothing,
                                         maxErrors = Core.Nothing, name = Core.Nothing,
                                         priority = Core.Nothing, serviceRoleArn = Core.Nothing,
                                         targets = Core.Nothing, taskArn = Core.Nothing,
                                         taskInvocationParameters = Core.Nothing,
                                         taskParameters = Core.Nothing, windowId = Core.Nothing,
                                         windowTaskId = Core.Nothing, responseStatus}

-- | The updated task description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsDescription :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.Description)
umwtrrsDescription = Lens.field @"description"
{-# INLINEABLE umwtrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The updated logging information in Amazon S3.
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsLoggingInfo :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.LoggingInfo)
umwtrrsLoggingInfo = Lens.field @"loggingInfo"
{-# INLINEABLE umwtrrsLoggingInfo #-}
{-# DEPRECATED loggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead"  #-}

-- | The updated MaxConcurrency value.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsMaxConcurrency :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.MaxConcurrency)
umwtrrsMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE umwtrrsMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The updated MaxErrors value.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsMaxErrors :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.MaxErrors)
umwtrrsMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE umwtrrsMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

-- | The updated task name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsName :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.MaintenanceWindowName)
umwtrrsName = Lens.field @"name"
{-# INLINEABLE umwtrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The updated priority value.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsPriority :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Core.Natural)
umwtrrsPriority = Lens.field @"priority"
{-# INLINEABLE umwtrrsPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | The ARN of the IAM service role to use to publish Amazon Simple Notification Service (Amazon SNS) notifications for maintenance window Run Command tasks.
--
-- /Note:/ Consider using 'serviceRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsServiceRoleArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.ServiceRoleArn)
umwtrrsServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE umwtrrsServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | The updated target values.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsTargets :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe [Types.Target])
umwtrrsTargets = Lens.field @"targets"
{-# INLINEABLE umwtrrsTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The updated task ARN value.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsTaskArn :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.TaskArn)
umwtrrsTaskArn = Lens.field @"taskArn"
{-# INLINEABLE umwtrrsTaskArn #-}
{-# DEPRECATED taskArn "Use generic-lens or generic-optics with 'taskArn' instead"  #-}

-- | The updated parameter values.
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsTaskInvocationParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.MaintenanceWindowTaskInvocationParameters)
umwtrrsTaskInvocationParameters = Lens.field @"taskInvocationParameters"
{-# INLINEABLE umwtrrsTaskInvocationParameters #-}
{-# DEPRECATED taskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead"  #-}

-- | The updated parameter values.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsTaskParameters :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression))
umwtrrsTaskParameters = Lens.field @"taskParameters"
{-# INLINEABLE umwtrrsTaskParameters #-}
{-# DEPRECATED taskParameters "Use generic-lens or generic-optics with 'taskParameters' instead"  #-}

-- | The ID of the maintenance window that was updated.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsWindowId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.MaintenanceWindowId)
umwtrrsWindowId = Lens.field @"windowId"
{-# INLINEABLE umwtrrsWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

-- | The task ID of the maintenance window that was updated.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsWindowTaskId :: Lens.Lens' UpdateMaintenanceWindowTaskResponse (Core.Maybe Types.MaintenanceWindowTaskId)
umwtrrsWindowTaskId = Lens.field @"windowTaskId"
{-# INLINEABLE umwtrrsWindowTaskId #-}
{-# DEPRECATED windowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umwtrrsResponseStatus :: Lens.Lens' UpdateMaintenanceWindowTaskResponse Core.Int
umwtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE umwtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
