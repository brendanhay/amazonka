{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterWorkflowType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /workflow type/ and its configuration settings in the specified domain.
--
-- The retention period for the workflow history is set by the 'RegisterDomain' action.
-- /Important:/ If the type already exists, then a @TypeAlreadyExists@ fault is returned. You cannot change the configuration settings of a workflow type once it is registered and it must be registered as a new version.
-- __Access Control__ 
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @defaultTaskList.name@ : String constraint. The key is @swf:defaultTaskList.name@ .
--
--
--     * @name@ : String constraint. The key is @swf:name@ .
--
--
--     * @version@ : String constraint. The key is @swf:version@ .
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.RegisterWorkflowType
    (
    -- * Creating a request
      RegisterWorkflowType (..)
    , mkRegisterWorkflowType
    -- ** Request lenses
    , rwtDomain
    , rwtName
    , rwtVersion
    , rwtDefaultChildPolicy
    , rwtDefaultExecutionStartToCloseTimeout
    , rwtDefaultLambdaRole
    , rwtDefaultTaskList
    , rwtDefaultTaskPriority
    , rwtDefaultTaskStartToCloseTimeout
    , rwtDescription

    -- * Destructuring the response
    , RegisterWorkflowTypeResponse (..)
    , mkRegisterWorkflowTypeResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkRegisterWorkflowType' smart constructor.
data RegisterWorkflowType = RegisterWorkflowType'
  { domain :: Types.Domain
    -- ^ The name of the domain in which to register the workflow type.
  , name :: Types.Name
    -- ^ The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
  , version :: Types.Version
    -- ^ The version of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
  , defaultChildPolicy :: Core.Maybe Types.ChildPolicy
    -- ^ If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
  , defaultExecutionStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ If set, specifies the default maximum duration for executions of this workflow type. You can override this default when starting an execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds; an integer greater than or equal to 0. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for @defaultExecutionStartToCloseTimeout@ ; there is a one-year max limit on the time that a workflow execution can run. Exceeding this limit always causes the workflow execution to time out.
  , defaultLambdaRole :: Core.Maybe Types.Arn
    -- ^ The default IAM role attached to this workflow type.
  , defaultTaskList :: Core.Maybe Types.TaskList
    -- ^ If set, specifies the default task list to use for scheduling decision tasks for executions of this workflow type. This default is used only if a task list isn't provided when starting the execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
  , defaultTaskPriority :: Core.Maybe Types.DefaultTaskPriority
    -- ^ The default task priority to assign to the workflow type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
  , defaultTaskStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ If set, specifies the default maximum duration of decision tasks for this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , description :: Core.Maybe Types.Description
    -- ^ Textual description of the workflow type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterWorkflowType' value with any optional fields omitted.
mkRegisterWorkflowType
    :: Types.Domain -- ^ 'domain'
    -> Types.Name -- ^ 'name'
    -> Types.Version -- ^ 'version'
    -> RegisterWorkflowType
mkRegisterWorkflowType domain name version
  = RegisterWorkflowType'{domain, name, version,
                          defaultChildPolicy = Core.Nothing,
                          defaultExecutionStartToCloseTimeout = Core.Nothing,
                          defaultLambdaRole = Core.Nothing, defaultTaskList = Core.Nothing,
                          defaultTaskPriority = Core.Nothing,
                          defaultTaskStartToCloseTimeout = Core.Nothing,
                          description = Core.Nothing}

-- | The name of the domain in which to register the workflow type.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDomain :: Lens.Lens' RegisterWorkflowType Types.Domain
rwtDomain = Lens.field @"domain"
{-# INLINEABLE rwtDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The name of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtName :: Lens.Lens' RegisterWorkflowType Types.Name
rwtName = Lens.field @"name"
{-# INLINEABLE rwtName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the workflow type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtVersion :: Lens.Lens' RegisterWorkflowType Types.Version
rwtVersion = Lens.field @"version"
{-# INLINEABLE rwtVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | If set, specifies the default policy to use for the child workflow executions when a workflow execution of this type is terminated, by calling the 'TerminateWorkflowExecution' action explicitly or due to an expired timeout. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
--
-- /Note:/ Consider using 'defaultChildPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultChildPolicy :: Lens.Lens' RegisterWorkflowType (Core.Maybe Types.ChildPolicy)
rwtDefaultChildPolicy = Lens.field @"defaultChildPolicy"
{-# INLINEABLE rwtDefaultChildPolicy #-}
{-# DEPRECATED defaultChildPolicy "Use generic-lens or generic-optics with 'defaultChildPolicy' instead"  #-}

-- | If set, specifies the default maximum duration for executions of this workflow type. You can override this default when starting an execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds; an integer greater than or equal to 0. Unlike some of the other timeout parameters in Amazon SWF, you cannot specify a value of "NONE" for @defaultExecutionStartToCloseTimeout@ ; there is a one-year max limit on the time that a workflow execution can run. Exceeding this limit always causes the workflow execution to time out.
--
-- /Note:/ Consider using 'defaultExecutionStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultExecutionStartToCloseTimeout :: Lens.Lens' RegisterWorkflowType (Core.Maybe Types.DurationInSecondsOptional)
rwtDefaultExecutionStartToCloseTimeout = Lens.field @"defaultExecutionStartToCloseTimeout"
{-# INLINEABLE rwtDefaultExecutionStartToCloseTimeout #-}
{-# DEPRECATED defaultExecutionStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultExecutionStartToCloseTimeout' instead"  #-}

-- | The default IAM role attached to this workflow type.
--
-- /Note:/ Consider using 'defaultLambdaRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultLambdaRole :: Lens.Lens' RegisterWorkflowType (Core.Maybe Types.Arn)
rwtDefaultLambdaRole = Lens.field @"defaultLambdaRole"
{-# INLINEABLE rwtDefaultLambdaRole #-}
{-# DEPRECATED defaultLambdaRole "Use generic-lens or generic-optics with 'defaultLambdaRole' instead"  #-}

-- | If set, specifies the default task list to use for scheduling decision tasks for executions of this workflow type. This default is used only if a task list isn't provided when starting the execution through the 'StartWorkflowExecution' Action or @StartChildWorkflowExecution@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultTaskList :: Lens.Lens' RegisterWorkflowType (Core.Maybe Types.TaskList)
rwtDefaultTaskList = Lens.field @"defaultTaskList"
{-# INLINEABLE rwtDefaultTaskList #-}
{-# DEPRECATED defaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead"  #-}

-- | The default task priority to assign to the workflow type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultTaskPriority :: Lens.Lens' RegisterWorkflowType (Core.Maybe Types.DefaultTaskPriority)
rwtDefaultTaskPriority = Lens.field @"defaultTaskPriority"
{-# INLINEABLE rwtDefaultTaskPriority #-}
{-# DEPRECATED defaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead"  #-}

-- | If set, specifies the default maximum duration of decision tasks for this workflow type. This default can be overridden when starting a workflow execution using the 'StartWorkflowExecution' action or the @StartChildWorkflowExecution@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDefaultTaskStartToCloseTimeout :: Lens.Lens' RegisterWorkflowType (Core.Maybe Types.DurationInSecondsOptional)
rwtDefaultTaskStartToCloseTimeout = Lens.field @"defaultTaskStartToCloseTimeout"
{-# INLINEABLE rwtDefaultTaskStartToCloseTimeout #-}
{-# DEPRECATED defaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead"  #-}

-- | Textual description of the workflow type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwtDescription :: Lens.Lens' RegisterWorkflowType (Core.Maybe Types.Description)
rwtDescription = Lens.field @"description"
{-# INLINEABLE rwtDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery RegisterWorkflowType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterWorkflowType where
        toHeaders RegisterWorkflowType{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.RegisterWorkflowType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON RegisterWorkflowType where
        toJSON RegisterWorkflowType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("name" Core..= name),
                  Core.Just ("version" Core..= version),
                  ("defaultChildPolicy" Core..=) Core.<$> defaultChildPolicy,
                  ("defaultExecutionStartToCloseTimeout" Core..=) Core.<$>
                    defaultExecutionStartToCloseTimeout,
                  ("defaultLambdaRole" Core..=) Core.<$> defaultLambdaRole,
                  ("defaultTaskList" Core..=) Core.<$> defaultTaskList,
                  ("defaultTaskPriority" Core..=) Core.<$> defaultTaskPriority,
                  ("defaultTaskStartToCloseTimeout" Core..=) Core.<$>
                    defaultTaskStartToCloseTimeout,
                  ("description" Core..=) Core.<$> description])

instance Core.AWSRequest RegisterWorkflowType where
        type Rs RegisterWorkflowType = RegisterWorkflowTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RegisterWorkflowTypeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterWorkflowTypeResponse' smart constructor.
data RegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterWorkflowTypeResponse' value with any optional fields omitted.
mkRegisterWorkflowTypeResponse
    :: RegisterWorkflowTypeResponse
mkRegisterWorkflowTypeResponse = RegisterWorkflowTypeResponse'
