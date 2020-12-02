{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ActivityTypeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ActivityTypeConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.TaskList

-- | Configuration settings registered with the activity type.
--
--
--
-- /See:/ 'activityTypeConfiguration' smart constructor.
data ActivityTypeConfiguration = ActivityTypeConfiguration'
  { _atcDefaultTaskScheduleToStartTimeout ::
      !(Maybe Text),
    _atcDefaultTaskList ::
      !(Maybe TaskList),
    _atcDefaultTaskPriority ::
      !(Maybe Text),
    _atcDefaultTaskHeartbeatTimeout ::
      !(Maybe Text),
    _atcDefaultTaskScheduleToCloseTimeout ::
      !(Maybe Text),
    _atcDefaultTaskStartToCloseTimeout ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ActivityTypeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atcDefaultTaskScheduleToStartTimeout' - The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'atcDefaultTaskList' - The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
--
-- * 'atcDefaultTaskPriority' - The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'atcDefaultTaskHeartbeatTimeout' - The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' . You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'atcDefaultTaskScheduleToCloseTimeout' - The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'atcDefaultTaskStartToCloseTimeout' - The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
activityTypeConfiguration ::
  ActivityTypeConfiguration
activityTypeConfiguration =
  ActivityTypeConfiguration'
    { _atcDefaultTaskScheduleToStartTimeout =
        Nothing,
      _atcDefaultTaskList = Nothing,
      _atcDefaultTaskPriority = Nothing,
      _atcDefaultTaskHeartbeatTimeout = Nothing,
      _atcDefaultTaskScheduleToCloseTimeout = Nothing,
      _atcDefaultTaskStartToCloseTimeout = Nothing
    }

-- | The default maximum duration, specified when registering the activity type, that a task of an activity type can wait before being assigned to a worker. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskScheduleToStartTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToStartTimeout = lens _atcDefaultTaskScheduleToStartTimeout (\s a -> s {_atcDefaultTaskScheduleToStartTimeout = a})

-- | The default task list specified for this activity type at registration. This default is used if a task list isn't provided when a task is scheduled through the @ScheduleActivityTask@ 'Decision' . You can override the default registered task list when scheduling a task through the @ScheduleActivityTask@ 'Decision' .
atcDefaultTaskList :: Lens' ActivityTypeConfiguration (Maybe TaskList)
atcDefaultTaskList = lens _atcDefaultTaskList (\s a -> s {_atcDefaultTaskList = a})

-- | The default task priority for tasks of this activity type, specified at registration. If not set, then @0@ is used as the default priority. This default can be overridden when scheduling an activity task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
atcDefaultTaskPriority :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskPriority = lens _atcDefaultTaskPriority (\s a -> s {_atcDefaultTaskPriority = a})

-- | The default maximum time, in seconds, before which a worker processing a task must report progress by calling 'RecordActivityTaskHeartbeat' . You can specify this value only when /registering/ an activity type. The registered default value can be overridden when you schedule a task through the @ScheduleActivityTask@ 'Decision' . If the activity worker subsequently attempts to record a heartbeat or returns a result, the activity worker receives an @UnknownResource@ fault. In this case, Amazon SWF no longer considers the activity task to be valid; the activity worker should clean up the activity task. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskHeartbeatTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskHeartbeatTimeout = lens _atcDefaultTaskHeartbeatTimeout (\s a -> s {_atcDefaultTaskHeartbeatTimeout = a})

-- | The default maximum duration, specified when registering the activity type, for tasks of this activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskScheduleToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskScheduleToCloseTimeout = lens _atcDefaultTaskScheduleToCloseTimeout (\s a -> s {_atcDefaultTaskScheduleToCloseTimeout = a})

-- | The default maximum duration for tasks of an activity type specified when registering the activity type. You can override this default when scheduling a task through the @ScheduleActivityTask@ 'Decision' . The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
atcDefaultTaskStartToCloseTimeout :: Lens' ActivityTypeConfiguration (Maybe Text)
atcDefaultTaskStartToCloseTimeout = lens _atcDefaultTaskStartToCloseTimeout (\s a -> s {_atcDefaultTaskStartToCloseTimeout = a})

instance FromJSON ActivityTypeConfiguration where
  parseJSON =
    withObject
      "ActivityTypeConfiguration"
      ( \x ->
          ActivityTypeConfiguration'
            <$> (x .:? "defaultTaskScheduleToStartTimeout")
            <*> (x .:? "defaultTaskList")
            <*> (x .:? "defaultTaskPriority")
            <*> (x .:? "defaultTaskHeartbeatTimeout")
            <*> (x .:? "defaultTaskScheduleToCloseTimeout")
            <*> (x .:? "defaultTaskStartToCloseTimeout")
      )

instance Hashable ActivityTypeConfiguration

instance NFData ActivityTypeConfiguration
