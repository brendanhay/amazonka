{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    RegisterActivityType (..),
    mkRegisterActivityType,

    -- ** Request lenses
    ratDefaultTaskScheduleToStartTimeout,
    ratDefaultTaskList,
    ratDefaultTaskPriority,
    ratDefaultTaskHeartbeatTimeout,
    ratDefaultTaskScheduleToCloseTimeout,
    ratDefaultTaskStartToCloseTimeout,
    ratDescription,
    ratDomain,
    ratName,
    ratVersion,

    -- * Destructuring the response
    RegisterActivityTypeResponse (..),
    mkRegisterActivityTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkRegisterActivityType' smart constructor.
data RegisterActivityType = RegisterActivityType'
  { defaultTaskScheduleToStartTimeout ::
      Lude.Maybe Lude.Text,
    defaultTaskList :: Lude.Maybe TaskList,
    defaultTaskPriority :: Lude.Maybe Lude.Text,
    defaultTaskHeartbeatTimeout ::
      Lude.Maybe Lude.Text,
    defaultTaskScheduleToCloseTimeout ::
      Lude.Maybe Lude.Text,
    defaultTaskStartToCloseTimeout ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    domain :: Lude.Text,
    name :: Lude.Text,
    version :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterActivityType' with the minimum fields required to make a request.
--
-- * 'defaultTaskHeartbeatTimeout' - If set, specifies the default maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'defaultTaskList' - If set, specifies the default task list to use for scheduling tasks of this activity type. This default task list is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' .
-- * 'defaultTaskPriority' - The default task priority to assign to the activity type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /in the \/Amazon SWF Developer Guide\/ ./ .
-- * 'defaultTaskScheduleToCloseTimeout' - If set, specifies the default maximum duration for a task of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'defaultTaskScheduleToStartTimeout' - If set, specifies the default maximum duration that a task of this activity type can wait before being assigned to a worker. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'defaultTaskStartToCloseTimeout' - If set, specifies the default maximum duration that a worker can take to process tasks of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
-- * 'description' - A textual description of the activity type.
-- * 'domain' - The name of the domain in which this activity is to be registered.
-- * 'name' - The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
-- * 'version' - The version of the activity type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
mkRegisterActivityType ::
  -- | 'domain'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  RegisterActivityType
mkRegisterActivityType pDomain_ pName_ pVersion_ =
  RegisterActivityType'
    { defaultTaskScheduleToStartTimeout =
        Lude.Nothing,
      defaultTaskList = Lude.Nothing,
      defaultTaskPriority = Lude.Nothing,
      defaultTaskHeartbeatTimeout = Lude.Nothing,
      defaultTaskScheduleToCloseTimeout = Lude.Nothing,
      defaultTaskStartToCloseTimeout = Lude.Nothing,
      description = Lude.Nothing,
      domain = pDomain_,
      name = pName_,
      version = pVersion_
    }

-- | If set, specifies the default maximum duration that a task of this activity type can wait before being assigned to a worker. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToStartTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskScheduleToStartTimeout :: Lens.Lens' RegisterActivityType (Lude.Maybe Lude.Text)
ratDefaultTaskScheduleToStartTimeout = Lens.lens (defaultTaskScheduleToStartTimeout :: RegisterActivityType -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskScheduleToStartTimeout = a} :: RegisterActivityType)
{-# DEPRECATED ratDefaultTaskScheduleToStartTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToStartTimeout' instead." #-}

-- | If set, specifies the default task list to use for scheduling tasks of this activity type. This default task list is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' .
--
-- /Note:/ Consider using 'defaultTaskList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskList :: Lens.Lens' RegisterActivityType (Lude.Maybe TaskList)
ratDefaultTaskList = Lens.lens (defaultTaskList :: RegisterActivityType -> Lude.Maybe TaskList) (\s a -> s {defaultTaskList = a} :: RegisterActivityType)
{-# DEPRECATED ratDefaultTaskList "Use generic-lens or generic-optics with 'defaultTaskList' instead." #-}

-- | The default task priority to assign to the activity type. If not assigned, then @0@ is used. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /in the \/Amazon SWF Developer Guide\/ ./ .
--
-- /Note:/ Consider using 'defaultTaskPriority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskPriority :: Lens.Lens' RegisterActivityType (Lude.Maybe Lude.Text)
ratDefaultTaskPriority = Lens.lens (defaultTaskPriority :: RegisterActivityType -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskPriority = a} :: RegisterActivityType)
{-# DEPRECATED ratDefaultTaskPriority "Use generic-lens or generic-optics with 'defaultTaskPriority' instead." #-}

-- | If set, specifies the default maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskHeartbeatTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskHeartbeatTimeout :: Lens.Lens' RegisterActivityType (Lude.Maybe Lude.Text)
ratDefaultTaskHeartbeatTimeout = Lens.lens (defaultTaskHeartbeatTimeout :: RegisterActivityType -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskHeartbeatTimeout = a} :: RegisterActivityType)
{-# DEPRECATED ratDefaultTaskHeartbeatTimeout "Use generic-lens or generic-optics with 'defaultTaskHeartbeatTimeout' instead." #-}

-- | If set, specifies the default maximum duration for a task of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskScheduleToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskScheduleToCloseTimeout :: Lens.Lens' RegisterActivityType (Lude.Maybe Lude.Text)
ratDefaultTaskScheduleToCloseTimeout = Lens.lens (defaultTaskScheduleToCloseTimeout :: RegisterActivityType -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskScheduleToCloseTimeout = a} :: RegisterActivityType)
{-# DEPRECATED ratDefaultTaskScheduleToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskScheduleToCloseTimeout' instead." #-}

-- | If set, specifies the default maximum duration that a worker can take to process tasks of this activity type. This default can be overridden when scheduling an activity task using the @ScheduleActivityTask@ 'Decision' .
--
-- The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- /Note:/ Consider using 'defaultTaskStartToCloseTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDefaultTaskStartToCloseTimeout :: Lens.Lens' RegisterActivityType (Lude.Maybe Lude.Text)
ratDefaultTaskStartToCloseTimeout = Lens.lens (defaultTaskStartToCloseTimeout :: RegisterActivityType -> Lude.Maybe Lude.Text) (\s a -> s {defaultTaskStartToCloseTimeout = a} :: RegisterActivityType)
{-# DEPRECATED ratDefaultTaskStartToCloseTimeout "Use generic-lens or generic-optics with 'defaultTaskStartToCloseTimeout' instead." #-}

-- | A textual description of the activity type.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDescription :: Lens.Lens' RegisterActivityType (Lude.Maybe Lude.Text)
ratDescription = Lens.lens (description :: RegisterActivityType -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RegisterActivityType)
{-# DEPRECATED ratDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the domain in which this activity is to be registered.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratDomain :: Lens.Lens' RegisterActivityType Lude.Text
ratDomain = Lens.lens (domain :: RegisterActivityType -> Lude.Text) (\s a -> s {domain = a} :: RegisterActivityType)
{-# DEPRECATED ratDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratName :: Lens.Lens' RegisterActivityType Lude.Text
ratName = Lens.lens (name :: RegisterActivityType -> Lude.Text) (\s a -> s {name = a} :: RegisterActivityType)
{-# DEPRECATED ratName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the activity type.
--
-- The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not /be/ the literal string @arn@ .
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ratVersion :: Lens.Lens' RegisterActivityType Lude.Text
ratVersion = Lens.lens (version :: RegisterActivityType -> Lude.Text) (\s a -> s {version = a} :: RegisterActivityType)
{-# DEPRECATED ratVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Lude.AWSRequest RegisterActivityType where
  type Rs RegisterActivityType = RegisterActivityTypeResponse
  request = Req.postJSON swfService
  response = Res.receiveNull RegisterActivityTypeResponse'

instance Lude.ToHeaders RegisterActivityType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SimpleWorkflowService.RegisterActivityType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterActivityType where
  toJSON RegisterActivityType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("defaultTaskScheduleToStartTimeout" Lude..=)
              Lude.<$> defaultTaskScheduleToStartTimeout,
            ("defaultTaskList" Lude..=) Lude.<$> defaultTaskList,
            ("defaultTaskPriority" Lude..=) Lude.<$> defaultTaskPriority,
            ("defaultTaskHeartbeatTimeout" Lude..=)
              Lude.<$> defaultTaskHeartbeatTimeout,
            ("defaultTaskScheduleToCloseTimeout" Lude..=)
              Lude.<$> defaultTaskScheduleToCloseTimeout,
            ("defaultTaskStartToCloseTimeout" Lude..=)
              Lude.<$> defaultTaskStartToCloseTimeout,
            ("description" Lude..=) Lude.<$> description,
            Lude.Just ("domain" Lude..= domain),
            Lude.Just ("name" Lude..= name),
            Lude.Just ("version" Lude..= version)
          ]
      )

instance Lude.ToPath RegisterActivityType where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterActivityType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterActivityTypeResponse' smart constructor.
data RegisterActivityTypeResponse = RegisterActivityTypeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterActivityTypeResponse' with the minimum fields required to make a request.
mkRegisterActivityTypeResponse ::
  RegisterActivityTypeResponse
mkRegisterActivityTypeResponse = RegisterActivityTypeResponse'
