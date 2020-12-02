{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a scheduled scaling action.
--
--
--
-- /See:/ 'scheduledUpdateGroupAction' smart constructor.
data ScheduledUpdateGroupAction = ScheduledUpdateGroupAction'
  { _sugaScheduledActionARN ::
      !(Maybe Text),
    _sugaStartTime :: !(Maybe ISO8601),
    _sugaTime :: !(Maybe ISO8601),
    _sugaScheduledActionName ::
      !(Maybe Text),
    _sugaMaxSize :: !(Maybe Int),
    _sugaRecurrence :: !(Maybe Text),
    _sugaDesiredCapacity :: !(Maybe Int),
    _sugaMinSize :: !(Maybe Int),
    _sugaAutoScalingGroupName ::
      !(Maybe Text),
    _sugaEndTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledUpdateGroupAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sugaScheduledActionARN' - The Amazon Resource Name (ARN) of the scheduled action.
--
-- * 'sugaStartTime' - The date and time in UTC for this action to start. For example, @"2019-06-01T00:00:00Z"@ .
--
-- * 'sugaTime' - This parameter is no longer used.
--
-- * 'sugaScheduledActionName' - The name of the scheduled action.
--
-- * 'sugaMaxSize' - The maximum size of the Auto Scaling group.
--
-- * 'sugaRecurrence' - The recurring schedule for the action, in Unix cron syntax format. When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- * 'sugaDesiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
--
-- * 'sugaMinSize' - The minimum size of the Auto Scaling group.
--
-- * 'sugaAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'sugaEndTime' - The date and time in UTC for the recurring schedule to end. For example, @"2019-06-01T00:00:00Z"@ .
scheduledUpdateGroupAction ::
  ScheduledUpdateGroupAction
scheduledUpdateGroupAction =
  ScheduledUpdateGroupAction'
    { _sugaScheduledActionARN = Nothing,
      _sugaStartTime = Nothing,
      _sugaTime = Nothing,
      _sugaScheduledActionName = Nothing,
      _sugaMaxSize = Nothing,
      _sugaRecurrence = Nothing,
      _sugaDesiredCapacity = Nothing,
      _sugaMinSize = Nothing,
      _sugaAutoScalingGroupName = Nothing,
      _sugaEndTime = Nothing
    }

-- | The Amazon Resource Name (ARN) of the scheduled action.
sugaScheduledActionARN :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionARN = lens _sugaScheduledActionARN (\s a -> s {_sugaScheduledActionARN = a})

-- | The date and time in UTC for this action to start. For example, @"2019-06-01T00:00:00Z"@ .
sugaStartTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaStartTime = lens _sugaStartTime (\s a -> s {_sugaStartTime = a}) . mapping _Time

-- | This parameter is no longer used.
sugaTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaTime = lens _sugaTime (\s a -> s {_sugaTime = a}) . mapping _Time

-- | The name of the scheduled action.
sugaScheduledActionName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaScheduledActionName = lens _sugaScheduledActionName (\s a -> s {_sugaScheduledActionName = a})

-- | The maximum size of the Auto Scaling group.
sugaMaxSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMaxSize = lens _sugaMaxSize (\s a -> s {_sugaMaxSize = a})

-- | The recurring schedule for the action, in Unix cron syntax format. When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
sugaRecurrence :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaRecurrence = lens _sugaRecurrence (\s a -> s {_sugaRecurrence = a})

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
sugaDesiredCapacity :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaDesiredCapacity = lens _sugaDesiredCapacity (\s a -> s {_sugaDesiredCapacity = a})

-- | The minimum size of the Auto Scaling group.
sugaMinSize :: Lens' ScheduledUpdateGroupAction (Maybe Int)
sugaMinSize = lens _sugaMinSize (\s a -> s {_sugaMinSize = a})

-- | The name of the Auto Scaling group.
sugaAutoScalingGroupName :: Lens' ScheduledUpdateGroupAction (Maybe Text)
sugaAutoScalingGroupName = lens _sugaAutoScalingGroupName (\s a -> s {_sugaAutoScalingGroupName = a})

-- | The date and time in UTC for the recurring schedule to end. For example, @"2019-06-01T00:00:00Z"@ .
sugaEndTime :: Lens' ScheduledUpdateGroupAction (Maybe UTCTime)
sugaEndTime = lens _sugaEndTime (\s a -> s {_sugaEndTime = a}) . mapping _Time

instance FromXML ScheduledUpdateGroupAction where
  parseXML x =
    ScheduledUpdateGroupAction'
      <$> (x .@? "ScheduledActionARN")
      <*> (x .@? "StartTime")
      <*> (x .@? "Time")
      <*> (x .@? "ScheduledActionName")
      <*> (x .@? "MaxSize")
      <*> (x .@? "Recurrence")
      <*> (x .@? "DesiredCapacity")
      <*> (x .@? "MinSize")
      <*> (x .@? "AutoScalingGroupName")
      <*> (x .@? "EndTime")

instance Hashable ScheduledUpdateGroupAction

instance NFData ScheduledUpdateGroupAction
