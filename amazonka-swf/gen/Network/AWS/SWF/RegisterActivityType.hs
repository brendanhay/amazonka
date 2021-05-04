{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SWF.RegisterActivityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new /activity type/ along with its configuration settings in
-- the specified domain.
--
-- A @TypeAlreadyExists@ fault is returned if the type already exists in
-- the domain. You cannot change any configuration settings of the type
-- after its registration, and it must be registered as a new version.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   Constrain the following parameters by using a @Condition@ element
--     with the appropriate keys.
--
--     -   @defaultTaskList.name@: String constraint. The key is
--         @swf:defaultTaskList.name@.
--
--     -   @name@: String constraint. The key is @swf:name@.
--
--     -   @version@: String constraint. The key is @swf:version@.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Network.AWS.SWF.RegisterActivityType
  ( -- * Creating a Request
    RegisterActivityType (..),
    newRegisterActivityType,

    -- * Request Lenses
    registerActivityType_defaultTaskPriority,
    registerActivityType_defaultTaskList,
    registerActivityType_description,
    registerActivityType_defaultTaskScheduleToStartTimeout,
    registerActivityType_defaultTaskStartToCloseTimeout,
    registerActivityType_defaultTaskHeartbeatTimeout,
    registerActivityType_defaultTaskScheduleToCloseTimeout,
    registerActivityType_domain,
    registerActivityType_name,
    registerActivityType_version,

    -- * Destructuring the Response
    RegisterActivityTypeResponse (..),
    newRegisterActivityTypeResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newRegisterActivityType' smart constructor.
data RegisterActivityType = RegisterActivityType'
  { -- | The default task priority to assign to the activity type. If not
    -- assigned, then @0@ is used. Valid values are integers that range from
    -- Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
    -- (2147483647). Higher numbers indicate higher priority.
    --
    -- For more information about setting task priority, see
    -- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
    -- in the /in the /Amazon SWF Developer Guide/./.
    defaultTaskPriority :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the default task list to use for scheduling tasks of
    -- this activity type. This default task list is used if a task list isn\'t
    -- provided when a task is scheduled through the @ScheduleActivityTask@
    -- Decision.
    defaultTaskList :: Prelude.Maybe TaskList,
    -- | A textual description of the activity type.
    description :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the default maximum duration that a task of this
    -- activity type can wait before being assigned to a worker. This default
    -- can be overridden when scheduling an activity task using the
    -- @ScheduleActivityTask@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskScheduleToStartTimeout :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the default maximum duration that a worker can take to
    -- process tasks of this activity type. This default can be overridden when
    -- scheduling an activity task using the @ScheduleActivityTask@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskStartToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the default maximum time before which a worker
    -- processing a task of this type must report progress by calling
    -- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
    -- task is automatically timed out. This default can be overridden when
    -- scheduling an activity task using the @ScheduleActivityTask@ Decision.
    -- If the activity worker subsequently attempts to record a heartbeat or
    -- returns a result, the activity worker receives an @UnknownResource@
    -- fault. In this case, Amazon SWF no longer considers the activity task to
    -- be valid; the activity worker should clean up the activity task.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskHeartbeatTimeout :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the default maximum duration for a task of this
    -- activity type. This default can be overridden when scheduling an
    -- activity task using the @ScheduleActivityTask@ Decision.
    --
    -- The duration is specified in seconds, an integer greater than or equal
    -- to @0@. You can use @NONE@ to specify unlimited duration.
    defaultTaskScheduleToCloseTimeout :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain in which this activity is to be registered.
    domain :: Prelude.Text,
    -- | The name of the activity type within the domain.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    name :: Prelude.Text,
    -- | The version of the activity type.
    --
    -- The activity type consists of the name and version, the combination of
    -- which must be unique within the domain.
    --
    -- The specified string must not start or end with whitespace. It must not
    -- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
    -- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
    -- /be/ the literal string @arn@.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterActivityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultTaskPriority', 'registerActivityType_defaultTaskPriority' - The default task priority to assign to the activity type. If not
-- assigned, then @0@ is used. Valid values are integers that range from
-- Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
-- (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /in the /Amazon SWF Developer Guide/./.
--
-- 'defaultTaskList', 'registerActivityType_defaultTaskList' - If set, specifies the default task list to use for scheduling tasks of
-- this activity type. This default task list is used if a task list isn\'t
-- provided when a task is scheduled through the @ScheduleActivityTask@
-- Decision.
--
-- 'description', 'registerActivityType_description' - A textual description of the activity type.
--
-- 'defaultTaskScheduleToStartTimeout', 'registerActivityType_defaultTaskScheduleToStartTimeout' - If set, specifies the default maximum duration that a task of this
-- activity type can wait before being assigned to a worker. This default
-- can be overridden when scheduling an activity task using the
-- @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'defaultTaskStartToCloseTimeout', 'registerActivityType_defaultTaskStartToCloseTimeout' - If set, specifies the default maximum duration that a worker can take to
-- process tasks of this activity type. This default can be overridden when
-- scheduling an activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'defaultTaskHeartbeatTimeout', 'registerActivityType_defaultTaskHeartbeatTimeout' - If set, specifies the default maximum time before which a worker
-- processing a task of this type must report progress by calling
-- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
-- task is automatically timed out. This default can be overridden when
-- scheduling an activity task using the @ScheduleActivityTask@ Decision.
-- If the activity worker subsequently attempts to record a heartbeat or
-- returns a result, the activity worker receives an @UnknownResource@
-- fault. In this case, Amazon SWF no longer considers the activity task to
-- be valid; the activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'defaultTaskScheduleToCloseTimeout', 'registerActivityType_defaultTaskScheduleToCloseTimeout' - If set, specifies the default maximum duration for a task of this
-- activity type. This default can be overridden when scheduling an
-- activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
--
-- 'domain', 'registerActivityType_domain' - The name of the domain in which this activity is to be registered.
--
-- 'name', 'registerActivityType_name' - The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
--
-- 'version', 'registerActivityType_version' - The version of the activity type.
--
-- The activity type consists of the name and version, the combination of
-- which must be unique within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
newRegisterActivityType ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  RegisterActivityType
newRegisterActivityType pDomain_ pName_ pVersion_ =
  RegisterActivityType'
    { defaultTaskPriority =
        Prelude.Nothing,
      defaultTaskList = Prelude.Nothing,
      description = Prelude.Nothing,
      defaultTaskScheduleToStartTimeout = Prelude.Nothing,
      defaultTaskStartToCloseTimeout = Prelude.Nothing,
      defaultTaskHeartbeatTimeout = Prelude.Nothing,
      defaultTaskScheduleToCloseTimeout = Prelude.Nothing,
      domain = pDomain_,
      name = pName_,
      version = pVersion_
    }

-- | The default task priority to assign to the activity type. If not
-- assigned, then @0@ is used. Valid values are integers that range from
-- Java\'s @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@
-- (2147483647). Higher numbers indicate higher priority.
--
-- For more information about setting task priority, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority>
-- in the /in the /Amazon SWF Developer Guide/./.
registerActivityType_defaultTaskPriority :: Lens.Lens' RegisterActivityType (Prelude.Maybe Prelude.Text)
registerActivityType_defaultTaskPriority = Lens.lens (\RegisterActivityType' {defaultTaskPriority} -> defaultTaskPriority) (\s@RegisterActivityType' {} a -> s {defaultTaskPriority = a} :: RegisterActivityType)

-- | If set, specifies the default task list to use for scheduling tasks of
-- this activity type. This default task list is used if a task list isn\'t
-- provided when a task is scheduled through the @ScheduleActivityTask@
-- Decision.
registerActivityType_defaultTaskList :: Lens.Lens' RegisterActivityType (Prelude.Maybe TaskList)
registerActivityType_defaultTaskList = Lens.lens (\RegisterActivityType' {defaultTaskList} -> defaultTaskList) (\s@RegisterActivityType' {} a -> s {defaultTaskList = a} :: RegisterActivityType)

-- | A textual description of the activity type.
registerActivityType_description :: Lens.Lens' RegisterActivityType (Prelude.Maybe Prelude.Text)
registerActivityType_description = Lens.lens (\RegisterActivityType' {description} -> description) (\s@RegisterActivityType' {} a -> s {description = a} :: RegisterActivityType)

-- | If set, specifies the default maximum duration that a task of this
-- activity type can wait before being assigned to a worker. This default
-- can be overridden when scheduling an activity task using the
-- @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
registerActivityType_defaultTaskScheduleToStartTimeout :: Lens.Lens' RegisterActivityType (Prelude.Maybe Prelude.Text)
registerActivityType_defaultTaskScheduleToStartTimeout = Lens.lens (\RegisterActivityType' {defaultTaskScheduleToStartTimeout} -> defaultTaskScheduleToStartTimeout) (\s@RegisterActivityType' {} a -> s {defaultTaskScheduleToStartTimeout = a} :: RegisterActivityType)

-- | If set, specifies the default maximum duration that a worker can take to
-- process tasks of this activity type. This default can be overridden when
-- scheduling an activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
registerActivityType_defaultTaskStartToCloseTimeout :: Lens.Lens' RegisterActivityType (Prelude.Maybe Prelude.Text)
registerActivityType_defaultTaskStartToCloseTimeout = Lens.lens (\RegisterActivityType' {defaultTaskStartToCloseTimeout} -> defaultTaskStartToCloseTimeout) (\s@RegisterActivityType' {} a -> s {defaultTaskStartToCloseTimeout = a} :: RegisterActivityType)

-- | If set, specifies the default maximum time before which a worker
-- processing a task of this type must report progress by calling
-- RecordActivityTaskHeartbeat. If the timeout is exceeded, the activity
-- task is automatically timed out. This default can be overridden when
-- scheduling an activity task using the @ScheduleActivityTask@ Decision.
-- If the activity worker subsequently attempts to record a heartbeat or
-- returns a result, the activity worker receives an @UnknownResource@
-- fault. In this case, Amazon SWF no longer considers the activity task to
-- be valid; the activity worker should clean up the activity task.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
registerActivityType_defaultTaskHeartbeatTimeout :: Lens.Lens' RegisterActivityType (Prelude.Maybe Prelude.Text)
registerActivityType_defaultTaskHeartbeatTimeout = Lens.lens (\RegisterActivityType' {defaultTaskHeartbeatTimeout} -> defaultTaskHeartbeatTimeout) (\s@RegisterActivityType' {} a -> s {defaultTaskHeartbeatTimeout = a} :: RegisterActivityType)

-- | If set, specifies the default maximum duration for a task of this
-- activity type. This default can be overridden when scheduling an
-- activity task using the @ScheduleActivityTask@ Decision.
--
-- The duration is specified in seconds, an integer greater than or equal
-- to @0@. You can use @NONE@ to specify unlimited duration.
registerActivityType_defaultTaskScheduleToCloseTimeout :: Lens.Lens' RegisterActivityType (Prelude.Maybe Prelude.Text)
registerActivityType_defaultTaskScheduleToCloseTimeout = Lens.lens (\RegisterActivityType' {defaultTaskScheduleToCloseTimeout} -> defaultTaskScheduleToCloseTimeout) (\s@RegisterActivityType' {} a -> s {defaultTaskScheduleToCloseTimeout = a} :: RegisterActivityType)

-- | The name of the domain in which this activity is to be registered.
registerActivityType_domain :: Lens.Lens' RegisterActivityType Prelude.Text
registerActivityType_domain = Lens.lens (\RegisterActivityType' {domain} -> domain) (\s@RegisterActivityType' {} a -> s {domain = a} :: RegisterActivityType)

-- | The name of the activity type within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
registerActivityType_name :: Lens.Lens' RegisterActivityType Prelude.Text
registerActivityType_name = Lens.lens (\RegisterActivityType' {name} -> name) (\s@RegisterActivityType' {} a -> s {name = a} :: RegisterActivityType)

-- | The version of the activity type.
--
-- The activity type consists of the name and version, the combination of
-- which must be unique within the domain.
--
-- The specified string must not start or end with whitespace. It must not
-- contain a @:@ (colon), @\/@ (slash), @|@ (vertical bar), or any control
-- characters (@\\u0000-\\u001f@ | @\\u007f-\\u009f@). Also, it must not
-- /be/ the literal string @arn@.
registerActivityType_version :: Lens.Lens' RegisterActivityType Prelude.Text
registerActivityType_version = Lens.lens (\RegisterActivityType' {version} -> version) (\s@RegisterActivityType' {} a -> s {version = a} :: RegisterActivityType)

instance Prelude.AWSRequest RegisterActivityType where
  type
    Rs RegisterActivityType =
      RegisterActivityTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull RegisterActivityTypeResponse'

instance Prelude.Hashable RegisterActivityType

instance Prelude.NFData RegisterActivityType

instance Prelude.ToHeaders RegisterActivityType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SimpleWorkflowService.RegisterActivityType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RegisterActivityType where
  toJSON RegisterActivityType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("defaultTaskPriority" Prelude..=)
              Prelude.<$> defaultTaskPriority,
            ("defaultTaskList" Prelude..=)
              Prelude.<$> defaultTaskList,
            ("description" Prelude..=) Prelude.<$> description,
            ("defaultTaskScheduleToStartTimeout" Prelude..=)
              Prelude.<$> defaultTaskScheduleToStartTimeout,
            ("defaultTaskStartToCloseTimeout" Prelude..=)
              Prelude.<$> defaultTaskStartToCloseTimeout,
            ("defaultTaskHeartbeatTimeout" Prelude..=)
              Prelude.<$> defaultTaskHeartbeatTimeout,
            ("defaultTaskScheduleToCloseTimeout" Prelude..=)
              Prelude.<$> defaultTaskScheduleToCloseTimeout,
            Prelude.Just ("domain" Prelude..= domain),
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("version" Prelude..= version)
          ]
      )

instance Prelude.ToPath RegisterActivityType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterActivityType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterActivityTypeResponse' smart constructor.
data RegisterActivityTypeResponse = RegisterActivityTypeResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterActivityTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterActivityTypeResponse ::
  RegisterActivityTypeResponse
newRegisterActivityTypeResponse =
  RegisterActivityTypeResponse'

instance Prelude.NFData RegisterActivityTypeResponse
