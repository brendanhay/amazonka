{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DecisionTaskScheduledEventAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SWF.Types.TaskList

-- | Provides details about the @DecisionTaskScheduled@ event.
--
--
--
-- /See:/ 'decisionTaskScheduledEventAttributes' smart constructor.
data DecisionTaskScheduledEventAttributes = DecisionTaskScheduledEventAttributes'
  { _dtseaTaskPriority ::
      !(Maybe Text),
    _dtseaStartToCloseTimeout ::
      !(Maybe Text),
    _dtseaTaskList ::
      !TaskList
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DecisionTaskScheduledEventAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtseaTaskPriority' - A task priority that, if set, specifies the priority for this decision task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
--
-- * 'dtseaStartToCloseTimeout' - The maximum duration for this decision task. The task is considered timed out if it doesn't completed within this duration. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
--
-- * 'dtseaTaskList' - The name of the task list in which the decision task was scheduled.
decisionTaskScheduledEventAttributes ::
  -- | 'dtseaTaskList'
  TaskList ->
  DecisionTaskScheduledEventAttributes
decisionTaskScheduledEventAttributes pTaskList_ =
  DecisionTaskScheduledEventAttributes'
    { _dtseaTaskPriority =
        Nothing,
      _dtseaStartToCloseTimeout = Nothing,
      _dtseaTaskList = pTaskList_
    }

-- | A task priority that, if set, specifies the priority for this decision task. Valid values are integers that range from Java's @Integer.MIN_VALUE@ (-2147483648) to @Integer.MAX_VALUE@ (2147483647). Higher numbers indicate higher priority. For more information about setting task priority, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/programming-priority.html Setting Task Priority> in the /Amazon SWF Developer Guide/ .
dtseaTaskPriority :: Lens' DecisionTaskScheduledEventAttributes (Maybe Text)
dtseaTaskPriority = lens _dtseaTaskPriority (\s a -> s {_dtseaTaskPriority = a})

-- | The maximum duration for this decision task. The task is considered timed out if it doesn't completed within this duration. The duration is specified in seconds, an integer greater than or equal to @0@ . You can use @NONE@ to specify unlimited duration.
dtseaStartToCloseTimeout :: Lens' DecisionTaskScheduledEventAttributes (Maybe Text)
dtseaStartToCloseTimeout = lens _dtseaStartToCloseTimeout (\s a -> s {_dtseaStartToCloseTimeout = a})

-- | The name of the task list in which the decision task was scheduled.
dtseaTaskList :: Lens' DecisionTaskScheduledEventAttributes TaskList
dtseaTaskList = lens _dtseaTaskList (\s a -> s {_dtseaTaskList = a})

instance FromJSON DecisionTaskScheduledEventAttributes where
  parseJSON =
    withObject
      "DecisionTaskScheduledEventAttributes"
      ( \x ->
          DecisionTaskScheduledEventAttributes'
            <$> (x .:? "taskPriority")
            <*> (x .:? "startToCloseTimeout")
            <*> (x .: "taskList")
      )

instance Hashable DecisionTaskScheduledEventAttributes

instance NFData DecisionTaskScheduledEventAttributes
