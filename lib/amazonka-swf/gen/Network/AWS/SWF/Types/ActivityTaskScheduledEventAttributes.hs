{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTaskScheduledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.ActivityType
import Network.AWS.SWF.Types.TaskList

-- | Provides the details of the @ActivityTaskScheduled@ event.
--
--
--
-- /See:/ 'activityTaskScheduledEventAttributes' smart constructor.
data ActivityTaskScheduledEventAttributes = ActivityTaskScheduledEventAttributes'
  { _atseaControl ::
      !(Maybe Text),
    _atseaHeartbeatTimeout ::
      !(Maybe Text),
    _atseaScheduleToCloseTimeout ::
      !(Maybe Text),
    _atseaInput ::
      !(Maybe Text),
    _atseaTaskPriority ::
      !(Maybe Text),
    _atseaScheduleToStartTimeout ::
      !(Maybe Text),
    _atseaStartToCloseTimeout ::
      !(Maybe Text),
    _atseaActivityType ::
      !ActivityType,
    _atseaActivityId ::
      !Text,
    _atseaTaskList ::
      !TaskList,
    _atseaDecisionTaskCompletedEventId ::
      !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTaskScheduledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atseaControl' - Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
--
-- * 'atseaHeartbeatTimeout' - The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
--
-- * 'atseaScheduleToCloseTimeout' - The maximum amount of time for this activity task.
--
-- * 'atseaInput' - The input provided to the activity task.
--
-- * 'atseaTaskPriority' - The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'atseaScheduleToStartTimeout' - The maximum amount of time the activity task can wait to be assigned to a worker.
--
-- * 'atseaStartToCloseTimeout' - The maximum amount of time a worker may take to process the activity task.
--
-- * 'atseaActivityType' - The type of the activity task.
--
-- * 'atseaActivityId' - The unique ID of the activity task.
--
-- * 'atseaTaskList' - The task list in which the activity task has been scheduled.
--
-- * 'atseaDecisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
activityTaskScheduledEventAttributes ::
  -- | 'atseaActivityType'
  ActivityType ->
  -- | 'atseaActivityId'
  Text ->
  -- | 'atseaTaskList'
  TaskList ->
  -- | 'atseaDecisionTaskCompletedEventId'
  Integer ->
  ActivityTaskScheduledEventAttributes
activityTaskScheduledEventAttributes
  pActivityType_
  pActivityId_
  pTaskList_
  pDecisionTaskCompletedEventId_ =
    ActivityTaskScheduledEventAttributes'
      { _atseaControl = Nothing,
        _atseaHeartbeatTimeout = Nothing,
        _atseaScheduleToCloseTimeout = Nothing,
        _atseaInput = Nothing,
        _atseaTaskPriority = Nothing,
        _atseaScheduleToStartTimeout = Nothing,
        _atseaStartToCloseTimeout = Nothing,
        _atseaActivityType = pActivityType_,
        _atseaActivityId = pActivityId_,
        _atseaTaskList = pTaskList_,
        _atseaDecisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks. This data isn't sent to the activity.
atseaControl :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaControl = lens _atseaControl (\s a -> s {_atseaControl = a})

-- | The maximum time before which the worker processing this task must report progress by calling 'RecordActivityTaskHeartbeat' . If the timeout is exceeded, the activity task is automatically timed out. If the worker subsequently attempts to record a heartbeat or return a result, it is ignored.
atseaHeartbeatTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaHeartbeatTimeout = lens _atseaHeartbeatTimeout (\s a -> s {_atseaHeartbeatTimeout = a})

-- | The maximum amount of time for this activity task.
atseaScheduleToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToCloseTimeout = lens _atseaScheduleToCloseTimeout (\s a -> s {_atseaScheduleToCloseTimeout = a})

-- | The input provided to the activity task.
atseaInput :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaInput = lens _atseaInput (\s a -> s {_atseaInput = a})

-- | The priority to assign to the scheduled activity task. If set, this overrides any default priority value that was assigned when the activity type was registered. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
atseaTaskPriority :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaTaskPriority = lens _atseaTaskPriority (\s a -> s {_atseaTaskPriority = a})

-- | The maximum amount of time the activity task can wait to be assigned to a worker.
atseaScheduleToStartTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaScheduleToStartTimeout = lens _atseaScheduleToStartTimeout (\s a -> s {_atseaScheduleToStartTimeout = a})

-- | The maximum amount of time a worker may take to process the activity task.
atseaStartToCloseTimeout :: Lens' ActivityTaskScheduledEventAttributes (Maybe Text)
atseaStartToCloseTimeout = lens _atseaStartToCloseTimeout (\s a -> s {_atseaStartToCloseTimeout = a})

-- | The type of the activity task.
atseaActivityType :: Lens' ActivityTaskScheduledEventAttributes ActivityType
atseaActivityType = lens _atseaActivityType (\s a -> s {_atseaActivityType = a})

-- | The unique ID of the activity task.
atseaActivityId :: Lens' ActivityTaskScheduledEventAttributes Text
atseaActivityId = lens _atseaActivityId (\s a -> s {_atseaActivityId = a})

-- | The task list in which the activity task has been scheduled.
atseaTaskList :: Lens' ActivityTaskScheduledEventAttributes TaskList
atseaTaskList = lens _atseaTaskList (\s a -> s {_atseaTaskList = a})

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision that resulted in the scheduling of this activity task. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
atseaDecisionTaskCompletedEventId :: Lens' ActivityTaskScheduledEventAttributes Integer
atseaDecisionTaskCompletedEventId = lens _atseaDecisionTaskCompletedEventId (\s a -> s {_atseaDecisionTaskCompletedEventId = a})

instance FromJSON ActivityTaskScheduledEventAttributes where
  parseJSON =
    withObject
      "ActivityTaskScheduledEventAttributes"
      ( \x ->
          ActivityTaskScheduledEventAttributes'
            <$> (x .:? "control")
            <*> (x .:? "heartbeatTimeout")
            <*> (x .:? "scheduleToCloseTimeout")
            <*> (x .:? "input")
            <*> (x .:? "taskPriority")
            <*> (x .:? "scheduleToStartTimeout")
            <*> (x .:? "startToCloseTimeout")
            <*> (x .: "activityType")
            <*> (x .: "activityId")
            <*> (x .: "taskList")
            <*> (x .: "decisionTaskCompletedEventId")
      )

instance Hashable ActivityTaskScheduledEventAttributes

instance NFData ActivityTaskScheduledEventAttributes
