{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RegisterActivityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /activity type/ along with its configuration settings in the specified domain.
--
-- /Important:/ A @TypeAlreadyExists@ fault is returned if the type already exists in the domain. You cannot change any configuration settings of the type after its registration, and it must be registered as a new version.
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
module Network.AWS.SWF.RegisterActivityType
    (
    -- * Creating a request
      RegisterActivityType (..)
    , mkRegisterActivityType
    -- ** Request lenses
    , ratDomain
    , ratName
    , ratVersion
    , ratDefaultTaskHeartbeatTimeout
    , ratDefaultTaskList
    , ratDefaultTaskPriority
    , ratDefaultTaskScheduleToCloseTimeout
    , ratDefaultTaskScheduleToStartTimeout
    , ratDefaultTaskStartToCloseTimeout
    , ratDescription

    -- * Destructuring the response
    , RegisterActivityTypeResponse (..)
    , mkRegisterActivityTypeResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SWF.Types as Types

-- | /See:/ 'mkRegisterActivityType' smart constructor.
data RegisterActivityType = RegisterActivityType'
  { domain :: Types.DomainName
    -- ^ The name of the domain in which this activity is to be registered.
  , name :: Types.Name
    -- ^ The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
  , version :: Types.Version
    -- ^ The version of the activity type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
  , defaultTaskHeartbeatTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ If set, specifies the default maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , defaultTaskList :: Core.Maybe Types.TaskList
    -- ^ If set, specifies the default task list to use for scheduling tasks of this activity type. This default task list is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' .
  , defaultTaskPriority :: Core.Maybe Types.TaskPriority
    -- ^ The default task priority to assign to the activity type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /in the \/Amazon SWF Developer Guide\/ ./ .
  , defaultTaskScheduleToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ If set, specifies the default maximum duration for a task of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , defaultTaskScheduleToStartTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ If set, specifies the default maximum duration that a task of this activity type can wait before being assigned to a worker. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , defaultTaskStartToCloseTimeout :: Core.Maybe Types.DurationInSecondsOptional
    -- ^ If set, specifies the default maximum duration that a worker can take to process tasks of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
  , description :: Core.Maybe Types.Description
    -- ^ A textual description of the activity type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterActivityType' value with any optional fields omitted.
mkRegisterActivityType
    :: Types.DomainName -- ^ 'domain'
    -> Types.Name -- ^ 'name'
    -> Types.Version -- ^ 'version'
    -> RegisterActivityType
mkRegisterActivityType domain name version
  = RegisterActivityType'{domain, name, version,
                          defaultTaskHeartbeatTimeout = Core.Nothing,
                          defaultTaskList = Core.Nothing, defaultTaskPriority = Core.Nothing,
                          defaultTaskScheduleToCloseTimeout = Core.Nothing,
                          defaultTaskScheduleToStartTimeout = Core.Nothing,
                          defaultTaskStartToCloseTimeout = Core.Nothing,
                          description = Core.Nothing}

-- | The name of the domain in which this activity is to be registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDomain :: Lens.Lens' RegisterActivityType Types.DomainName
ratDomain = Lens.field @"domain"
{-# INLINEABLE ratDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratName :: Lens.Lens' RegisterActivityType Types.Name
ratName = Lens.field @"name"
{-# INLINEABLE ratName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the activity type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratVersion :: Lens.Lens' RegisterActivityType Types.Version
ratVersion = Lens.field @"version"
{-# INLINEABLE ratVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | If set, specifies the default maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskHeartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskHeartbeatTimeout :: Lens.Lens' RegisterActivityType (Core.Maybe Types.DurationInSecondsOptional)
ratDefaultTaskHeartbeatTimeout = Lens.field @"defaultTaskHeartbeatTimeout"
{-# INLINEABLE ratDefaultTaskHeartbeatTimeout #-}
{-# DEPRECATED defaultTaskHeartbeatTimeout "Use generic-lens or generic-optics with 'defaultTaskHeartbeatTimeout' instead"  #-}

-- | If set, specifies the default task list to use for scheduling tasks of this activity type. This default task list is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskList :: Lens.Lens' RegisterActivityType (Core.Maybe Types.TaskList)
ratDefaultTaskList = Lens.field @"defaultTaskList"
{-# INLINEABLE ratDefaultTaskList #-}
{-# DEPRECATED defaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead"  #-}

-- | The default task priority to assign to the activity type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /in the \/Amazon SWF Developer Guide\/ ./ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskPriority :: Lens.Lens' RegisterActivityType (Core.Maybe Types.TaskPriority)
ratDefaultTaskPriority = Lens.field @"defaultTaskPriority"
{-# INLINEABLE ratDefaultTaskPriority #-}
{-# DEPRECATED defaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead"  #-}

-- | If set, specifies the default maximum duration for a task of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskScheduleToCloseTimeout :: Lens.Lens' RegisterActivityType (Core.Maybe Types.DurationInSecondsOptional)
ratDefaultTaskScheduleToCloseTimeout = Lens.field @"defaultTaskScheduleToCloseTimeout"
{-# INLINEABLE ratDefaultTaskScheduleToCloseTimeout #-}
{-# DEPRECATED defaultTaskScheduleToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToCloseTimeout' instead"  #-}

-- | If set, specifies the default maximum duration that a task of this activity type can wait before being assigned to a worker. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskScheduleToStartTimeout :: Lens.Lens' RegisterActivityType (Core.Maybe Types.DurationInSecondsOptional)
ratDefaultTaskScheduleToStartTimeout = Lens.field @"defaultTaskScheduleToStartTimeout"
{-# INLINEABLE ratDefaultTaskScheduleToStartTimeout #-}
{-# DEPRECATED defaultTaskScheduleToStartTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToStartTimeout' instead"  #-}

-- | If set, specifies the default maximum duration that a worker can take to process tasks of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskStartToCloseTimeout :: Lens.Lens' RegisterActivityType (Core.Maybe Types.DurationInSecondsOptional)
ratDefaultTaskStartToCloseTimeout = Lens.field @"defaultTaskStartToCloseTimeout"
{-# INLINEABLE ratDefaultTaskStartToCloseTimeout #-}
{-# DEPRECATED defaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead"  #-}

-- | A textual description of the activity type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDescription :: Lens.Lens' RegisterActivityType (Core.Maybe Types.Description)
ratDescription = Lens.field @"description"
{-# INLINEABLE ratDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.ToQuery RegisterActivityType where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RegisterActivityType where
        toHeaders RegisterActivityType{..}
          = Core.pure
              ("X-Amz-Target", "SimpleWorkflowService.RegisterActivityType")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON RegisterActivityType where
        toJSON RegisterActivityType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("domain" Core..= domain),
                  Core.Just ("name" Core..= name),
                  Core.Just ("version" Core..= version),
                  ("defaultTaskHeartbeatTimeout" Core..=) Core.<$>
                    defaultTaskHeartbeatTimeout,
                  ("defaultTaskList" Core..=) Core.<$> defaultTaskList,
                  ("defaultTaskPriority" Core..=) Core.<$> defaultTaskPriority,
                  ("defaultTaskScheduleToCloseTimeout" Core..=) Core.<$>
                    defaultTaskScheduleToCloseTimeout,
                  ("defaultTaskScheduleToStartTimeout" Core..=) Core.<$>
                    defaultTaskScheduleToStartTimeout,
                  ("defaultTaskStartToCloseTimeout" Core..=) Core.<$>
                    defaultTaskStartToCloseTimeout,
                  ("description" Core..=) Core.<$> description])

instance Core.AWSRequest RegisterActivityType where
        type Rs RegisterActivityType = RegisterActivityTypeResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RegisterActivityTypeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRegisterActivityTypeResponse' smart constructor.
data RegisterActivityTypeResponse = RegisterActivityTypeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterActivityTypeResponse' value with any optional fields omitted.
mkRegisterActivityTypeResponse
    :: RegisterActivityTypeResponse
mkRegisterActivityTypeResponse = RegisterActivityTypeResponse'
