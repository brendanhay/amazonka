{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new task to a maintenance window.
module Network.AWS.SSM.RegisterTaskWithMaintenanceWindow
    (
    -- * Creating a request
      RegisterTaskWithMaintenanceWindow (..)
    , mkRegisterTaskWithMaintenanceWindow
    -- ** Request lenses
    , rtwmwWindowId
    , rtwmwTargets
    , rtwmwTaskArn
    , rtwmwTaskType
    , rtwmwMaxConcurrency
    , rtwmwMaxErrors
    , rtwmwClientToken
    , rtwmwDescription
    , rtwmwLoggingInfo
    , rtwmwName
    , rtwmwPriority
    , rtwmwServiceRoleArn
    , rtwmwTaskInvocationParameters
    , rtwmwTaskParameters

    -- * Destructuring the response
    , RegisterTaskWithMaintenanceWindowResponse (..)
    , mkRegisterTaskWithMaintenanceWindowResponse
    -- ** Response lenses
    , rtwmwrrsWindowTaskId
    , rtwmwrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkRegisterTaskWithMaintenanceWindow' smart constructor.
data RegisterTaskWithMaintenanceWindow = RegisterTaskWithMaintenanceWindow'
  { windowId :: Types.WindowId
    -- ^ The ID of the maintenance window the task should be added to.
  , targets :: [Types.Target]
    -- ^ The targets (either instances or maintenance window targets).
--
-- Specify instances using the following format: 
-- @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@ 
-- Specify maintenance window targets using the following format:
-- @Key=WindowTargetIds;,Values=<window-target-id-1>,<window-target-id-2>@ 
  , taskArn :: Types.TaskArn
    -- ^ The ARN of the task to run.
  , taskType :: Types.MaintenanceWindowTaskType
    -- ^ The type of task being registered.
  , maxConcurrency :: Types.MaxConcurrency
    -- ^ The maximum number of targets this task can be run for in parallel.
  , maxErrors :: Types.MaxErrors
    -- ^ The maximum number of errors allowed before this task stops being scheduled.
  , clientToken :: Core.Maybe Types.ClientToken
    -- ^ User-provided idempotency token.
  , description :: Core.Maybe Types.Description
    -- ^ An optional description for the task.
  , loggingInfo :: Core.Maybe Types.LoggingInfo
    -- ^ A structure containing information about an S3 bucket to write instance-level logs to. 
  , name :: Core.Maybe Types.MaintenanceWindowName
    -- ^ An optional name for the task.
  , priority :: Core.Maybe Core.Natural
    -- ^ The priority of the task in the maintenance window, the lower the number the higher the priority. Tasks in a maintenance window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
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
  , taskInvocationParameters :: Core.Maybe Types.MaintenanceWindowTaskInvocationParameters
    -- ^ The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty. 
  , taskParameters :: Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression)
    -- ^ The parameters that should be passed to the task when it is run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTaskWithMaintenanceWindow' value with any optional fields omitted.
mkRegisterTaskWithMaintenanceWindow
    :: Types.WindowId -- ^ 'windowId'
    -> Types.TaskArn -- ^ 'taskArn'
    -> Types.MaintenanceWindowTaskType -- ^ 'taskType'
    -> Types.MaxConcurrency -- ^ 'maxConcurrency'
    -> Types.MaxErrors -- ^ 'maxErrors'
    -> RegisterTaskWithMaintenanceWindow
mkRegisterTaskWithMaintenanceWindow windowId taskArn taskType
  maxConcurrency maxErrors
  = RegisterTaskWithMaintenanceWindow'{windowId,
                                       targets = Core.mempty, taskArn, taskType, maxConcurrency,
                                       maxErrors, clientToken = Core.Nothing,
                                       description = Core.Nothing, loggingInfo = Core.Nothing,
                                       name = Core.Nothing, priority = Core.Nothing,
                                       serviceRoleArn = Core.Nothing,
                                       taskInvocationParameters = Core.Nothing,
                                       taskParameters = Core.Nothing}

-- | The ID of the maintenance window the task should be added to.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwWindowId :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.WindowId
rtwmwWindowId = Lens.field @"windowId"
{-# INLINEABLE rtwmwWindowId #-}
{-# DEPRECATED windowId "Use generic-lens or generic-optics with 'windowId' instead"  #-}

-- | The targets (either instances or maintenance window targets).
--
-- Specify instances using the following format: 
-- @Key=InstanceIds,Values=<instance-id-1>,<instance-id-2>@ 
-- Specify maintenance window targets using the following format:
-- @Key=WindowTargetIds;,Values=<window-target-id-1>,<window-target-id-2>@ 
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTargets :: Lens.Lens' RegisterTaskWithMaintenanceWindow [Types.Target]
rtwmwTargets = Lens.field @"targets"
{-# INLINEABLE rtwmwTargets #-}
{-# DEPRECATED targets "Use generic-lens or generic-optics with 'targets' instead"  #-}

-- | The ARN of the task to run.
--
-- /Note:/ Consider using 'taskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskArn :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.TaskArn
rtwmwTaskArn = Lens.field @"taskArn"
{-# INLINEABLE rtwmwTaskArn #-}
{-# DEPRECATED taskArn "Use generic-lens or generic-optics with 'taskArn' instead"  #-}

-- | The type of task being registered.
--
-- /Note:/ Consider using 'taskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskType :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.MaintenanceWindowTaskType
rtwmwTaskType = Lens.field @"taskType"
{-# INLINEABLE rtwmwTaskType #-}
{-# DEPRECATED taskType "Use generic-lens or generic-optics with 'taskType' instead"  #-}

-- | The maximum number of targets this task can be run for in parallel.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwMaxConcurrency :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.MaxConcurrency
rtwmwMaxConcurrency = Lens.field @"maxConcurrency"
{-# INLINEABLE rtwmwMaxConcurrency #-}
{-# DEPRECATED maxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead"  #-}

-- | The maximum number of errors allowed before this task stops being scheduled.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwMaxErrors :: Lens.Lens' RegisterTaskWithMaintenanceWindow Types.MaxErrors
rtwmwMaxErrors = Lens.field @"maxErrors"
{-# INLINEABLE rtwmwMaxErrors #-}
{-# DEPRECATED maxErrors "Use generic-lens or generic-optics with 'maxErrors' instead"  #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwClientToken :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.ClientToken)
rtwmwClientToken = Lens.field @"clientToken"
{-# INLINEABLE rtwmwClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | An optional description for the task.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwDescription :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.Description)
rtwmwDescription = Lens.field @"description"
{-# INLINEABLE rtwmwDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A structure containing information about an S3 bucket to write instance-level logs to. 
--
-- /Note:/ Consider using 'loggingInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwLoggingInfo :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.LoggingInfo)
rtwmwLoggingInfo = Lens.field @"loggingInfo"
{-# INLINEABLE rtwmwLoggingInfo #-}
{-# DEPRECATED loggingInfo "Use generic-lens or generic-optics with 'loggingInfo' instead"  #-}

-- | An optional name for the task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwName :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.MaintenanceWindowName)
rtwmwName = Lens.field @"name"
{-# INLINEABLE rtwmwName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The priority of the task in the maintenance window, the lower the number the higher the priority. Tasks in a maintenance window are scheduled in priority order with tasks that have the same priority scheduled in parallel.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwPriority :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Core.Natural)
rtwmwPriority = Lens.field @"priority"
{-# INLINEABLE rtwmwPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

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
rtwmwServiceRoleArn :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.ServiceRoleArn)
rtwmwServiceRoleArn = Lens.field @"serviceRoleArn"
{-# INLINEABLE rtwmwServiceRoleArn #-}
{-# DEPRECATED serviceRoleArn "Use generic-lens or generic-optics with 'serviceRoleArn' instead"  #-}

-- | The parameters that the task should use during execution. Populate only the fields that match the task type. All other fields should be empty. 
--
-- /Note:/ Consider using 'taskInvocationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskInvocationParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe Types.MaintenanceWindowTaskInvocationParameters)
rtwmwTaskInvocationParameters = Lens.field @"taskInvocationParameters"
{-# INLINEABLE rtwmwTaskInvocationParameters #-}
{-# DEPRECATED taskInvocationParameters "Use generic-lens or generic-optics with 'taskInvocationParameters' instead"  #-}

-- | The parameters that should be passed to the task when it is run.
--
-- /Note:/ Consider using 'taskParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwTaskParameters :: Lens.Lens' RegisterTaskWithMaintenanceWindow (Core.Maybe (Core.HashMap Types.MaintenanceWindowTaskParameterName Types.MaintenanceWindowTaskParameterValueExpression))
rtwmwTaskParameters = Lens.field @"taskParameters"
{-# INLINEABLE rtwmwTaskParameters #-}
{-# DEPRECATED taskParameters "Use generic-lens or generic-optics with 'taskParameters' instead"  #-}

instance Core.ToQuery RegisterTaskWithMaintenanceWindow where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterTaskWithMaintenanceWindow where
        toHeaders RegisterTaskWithMaintenanceWindow{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.RegisterTaskWithMaintenanceWindow")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RegisterTaskWithMaintenanceWindow where
        toJSON RegisterTaskWithMaintenanceWindow{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WindowId" Core..= windowId),
                  Core.Just ("Targets" Core..= targets),
                  Core.Just ("TaskArn" Core..= taskArn),
                  Core.Just ("TaskType" Core..= taskType),
                  Core.Just ("MaxConcurrency" Core..= maxConcurrency),
                  Core.Just ("MaxErrors" Core..= maxErrors),
                  ("ClientToken" Core..=) Core.<$> clientToken,
                  ("Description" Core..=) Core.<$> description,
                  ("LoggingInfo" Core..=) Core.<$> loggingInfo,
                  ("Name" Core..=) Core.<$> name,
                  ("Priority" Core..=) Core.<$> priority,
                  ("ServiceRoleArn" Core..=) Core.<$> serviceRoleArn,
                  ("TaskInvocationParameters" Core..=) Core.<$>
                    taskInvocationParameters,
                  ("TaskParameters" Core..=) Core.<$> taskParameters])

instance Core.AWSRequest RegisterTaskWithMaintenanceWindow where
        type Rs RegisterTaskWithMaintenanceWindow =
             RegisterTaskWithMaintenanceWindowResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 RegisterTaskWithMaintenanceWindowResponse' Core.<$>
                   (x Core..:? "WindowTaskId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterTaskWithMaintenanceWindowResponse' smart constructor.
data RegisterTaskWithMaintenanceWindowResponse = RegisterTaskWithMaintenanceWindowResponse'
  { windowTaskId :: Core.Maybe Types.WindowTaskId
    -- ^ The ID of the task in the maintenance window.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterTaskWithMaintenanceWindowResponse' value with any optional fields omitted.
mkRegisterTaskWithMaintenanceWindowResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterTaskWithMaintenanceWindowResponse
mkRegisterTaskWithMaintenanceWindowResponse responseStatus
  = RegisterTaskWithMaintenanceWindowResponse'{windowTaskId =
                                                 Core.Nothing,
                                               responseStatus}

-- | The ID of the task in the maintenance window.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwrrsWindowTaskId :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse (Core.Maybe Types.WindowTaskId)
rtwmwrrsWindowTaskId = Lens.field @"windowTaskId"
{-# INLINEABLE rtwmwrrsWindowTaskId #-}
{-# DEPRECATED windowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtwmwrrsResponseStatus :: Lens.Lens' RegisterTaskWithMaintenanceWindowResponse Core.Int
rtwmwrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rtwmwrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
