{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleActivityTaskDecisionAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.TaskList

-- | Provides the details of the @ScheduleActivityTask@ decision.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * Constrain the following parameters by using a @Condition@ element with the appropriate keys.
--
--     * @activityType.name@ – String constraint. The key is @swf:activityType.name@ .
--
--     * @activityType.version@ – String constraint. The key is @swf:activityType.version@ .
--
--     * @taskList@ – String constraint. The key is @swf:taskList.name@ .
--
--
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- /See:/ 'scheduleActivityTaskDecisionAttributes' smart constructor.
data ScheduleActivityTaskDecisionAttributes = ScheduleActivityTaskDecisionAttributes'
  { _satdaControl ::
      !(Maybe Text),
    _satdaHeartbeatTimeout ::
      !(Maybe Text),
    _satdaScheduleToCloseTimeout ::
      !(Maybe Text),
    _satdaInput ::
      !(Maybe Text),
    _satdaTaskList ::
      !( Maybe
           TaskList
       ),
    _satdaTaskPriority ::
      !(Maybe Text),
    _satdaScheduleToStartTimeout ::
      !(Maybe Text),
    _satdaStartToCloseTimeout ::
      !(Maybe Text),
    _satdaActivityType ::
      !ActivityType,
    _satdaActivityId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleActivityTaskDecisionAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'satdaControl' - Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
--
-- * 'satdaHeartbeatTimeout' - If set, specifies the maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or returns a result, it is ignored. This overrides the default heartbeat timeout specified when registering the activity type using 'RegisterActivityType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'satdaScheduleToCloseTimeout' - The maximum duration for this activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'satdaInput' - The input provided to the activity task.
--
-- * 'satdaTaskList' - If set, specifies the name of the task list in which to schedule the activity task. If not specified, the @defaultTaskList@ registered with the activity type is used. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
--
-- * 'satdaTaskPriority' - If set, specifies the priority with which the activity task is to be assigned to a worker. This overrides the defaultTaskPriority specified when registering the activity type using 'RegisterActivityType' . Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'satdaScheduleToStartTimeout' - If set, specifies the maximum duration the activity task can wait to be assigned to a worker. This overrides the default schedule-to-start timeout specified when registering the activity type using 'RegisterActivityType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'satdaStartToCloseTimeout' - If set, specifies the maximum duration a worker may take to process this activity task. This overrides the default start-to-close timeout specified when registering the activity type using 'RegisterActivityType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'satdaActivityType' - The type of the activity task to schedule.
--
-- * 'satdaActivityId' - The @activityId@ of the activity task. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
scheduleActivityTaskDecisionAttributes ::
  -- | 'satdaActivityType'
  ActivityType ->
  -- | 'satdaActivityId'
  Text ->
  ScheduleActivityTaskDecisionAttributes
scheduleActivityTaskDecisionAttributes pActivityType_ pActivityId_ =
  ScheduleActivityTaskDecisionAttributes'
    { _satdaControl = Nothing,
      _satdaHeartbeatTimeout = Nothing,
      _satdaScheduleToCloseTimeout = Nothing,
      _satdaInput = Nothing,
      _satdaTaskList = Nothing,
      _satdaTaskPriority = Nothing,
      _satdaScheduleToStartTimeout = Nothing,
      _satdaStartToCloseTimeout = Nothing,
      _satdaActivityType = pActivityType_,
      _satdaActivityId = pActivityId_
    }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
satdaControl :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaControl = lens _satdaControl (\s a -> s {_satdaControl = a})

-- | If set, specifies the maximum time before which a worker processing a task of this type must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or returns a result, it is ignored. This overrides the default heartbeat timeout specified when registering the activity type using 'RegisterActivityType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
satdaHeartbeatTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaHeartbeatTimeout = lens _satdaHeartbeatTimeout (\s a -> s {_satdaHeartbeatTimeout = a})

-- | The maximum duration for this activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
satdaScheduleToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToCloseTimeout = lens _satdaScheduleToCloseTimeout (\s a -> s {_satdaScheduleToCloseTimeout = a})

-- | The input provided to the activity task.
satdaInput :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaInput = lens _satdaInput (\s a -> s {_satdaInput = a})

-- | If set, specifies the name of the task list in which to schedule the activity task. If not specified, the @defaultTaskList@ registered with the activity type is used. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
satdaTaskList :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe TaskList)
satdaTaskList = lens _satdaTaskList (\s a -> s {_satdaTaskList = a})

-- | If set, specifies the priority with which the activity task is to be assigned to a worker. This overrides the defaultTaskPriority specified when registering the activity type using 'RegisterActivityType' . Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
satdaTaskPriority :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaTaskPriority = lens _satdaTaskPriority (\s a -> s {_satdaTaskPriority = a})

-- | If set, specifies the maximum duration the activity task can wait to be assigned to a worker. This overrides the default schedule-to-start timeout specified when registering the activity type using 'RegisterActivityType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
satdaScheduleToStartTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaScheduleToStartTimeout = lens _satdaScheduleToStartTimeout (\s a -> s {_satdaScheduleToStartTimeout = a})

-- | If set, specifies the maximum duration a worker may take to process this activity task. This overrides the default start-to-close timeout specified when registering the activity type using 'RegisterActivityType' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
satdaStartToCloseTimeout :: Lens' ScheduleActivityTaskDecisionAttributes (Maybe Text)
satdaStartToCloseTimeout = lens _satdaStartToCloseTimeout (\s a -> s {_satdaStartToCloseTimeout = a})

-- | The type of the activity task to schedule.
satdaActivityType :: Lens' ScheduleActivityTaskDecisionAttributes ActivityType
satdaActivityType = lens _satdaActivityType (\s a -> s {_satdaActivityType = a})

-- | The @activityId@ of the activity task. The specified string must not start or end with whitespace. It must not contain a @:@ (colon), @/@ (slash), @|@ (vertical bar), or any control characters (@\u0000-\u001f@ | @\u007f-\u009f@ ). Also, it must not contain the literal string @arn@ .
satdaActivityId :: Lens' ScheduleActivityTaskDecisionAttributes Text
satdaActivityId = lens _satdaActivityId (\s a -> s {_satdaActivityId = a})

instance Hashable ScheduleActivityTaskDecisionAttributes

instance NFData ScheduleActivityTaskDecisionAttributes

instance ToJSON ScheduleActivityTaskDecisionAttributes where
  toJSON ScheduleActivityTaskDecisionAttributes' {..} =
    object
      ( catMaybes
          [ ("control" .=) <$> _satdaControl,
            ("heartbeatTimeout" .=) <$> _satdaHeartbeatTimeout,
            ("scheduleToCloseTimeout" .=) <$> _satdaScheduleToCloseTimeout,
            ("input" .=) <$> _satdaInput,
            ("taskList" .=) <$> _satdaTaskList,
            ("taskPriority" .=) <$> _satdaTaskPriority,
            ("scheduleToStartTimeout" .=) <$> _satdaScheduleToStartTimeout,
            ("startToCloseTimeout" .=) <$> _satdaStartToCloseTimeout,
            Just ("activityType" .= _satdaActivityType),
            Just ("activityId" .= _satdaActivityId)
          ]
      )
