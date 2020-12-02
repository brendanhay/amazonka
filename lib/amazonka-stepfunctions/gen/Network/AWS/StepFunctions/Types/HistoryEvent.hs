{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEvent where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.ActivityFailedEventDetails
import Network.AWS.StepFunctions.Types.ActivityScheduleFailedEventDetails
import Network.AWS.StepFunctions.Types.ActivityScheduledEventDetails
import Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
import Network.AWS.StepFunctions.Types.ActivitySucceededEventDetails
import Network.AWS.StepFunctions.Types.ActivityTimedOutEventDetails
import Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
import Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
import Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
import Network.AWS.StepFunctions.Types.HistoryEventType
import Network.AWS.StepFunctions.Types.LambdaFunctionFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduleFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionSucceededEventDetails
import Network.AWS.StepFunctions.Types.LambdaFunctionTimedOutEventDetails
import Network.AWS.StepFunctions.Types.MapIterationEventDetails
import Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
import Network.AWS.StepFunctions.Types.StateEnteredEventDetails
import Network.AWS.StepFunctions.Types.StateExitedEventDetails
import Network.AWS.StepFunctions.Types.TaskFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskScheduledEventDetails
import Network.AWS.StepFunctions.Types.TaskStartFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskStartedEventDetails
import Network.AWS.StepFunctions.Types.TaskSubmitFailedEventDetails
import Network.AWS.StepFunctions.Types.TaskSubmittedEventDetails
import Network.AWS.StepFunctions.Types.TaskSucceededEventDetails
import Network.AWS.StepFunctions.Types.TaskTimedOutEventDetails

-- | Contains details about the events of an execution.
--
--
--
-- /See:/ 'historyEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { _heMapStateStartedEventDetails ::
      !(Maybe MapStateStartedEventDetails),
    _heTaskSubmitFailedEventDetails ::
      !(Maybe TaskSubmitFailedEventDetails),
    _heTaskStartedEventDetails :: !(Maybe TaskStartedEventDetails),
    _heActivityStartedEventDetails ::
      !(Maybe ActivityStartedEventDetails),
    _heTaskSubmittedEventDetails ::
      !(Maybe TaskSubmittedEventDetails),
    _heLambdaFunctionStartFailedEventDetails ::
      !(Maybe LambdaFunctionStartFailedEventDetails),
    _heTaskStartFailedEventDetails ::
      !(Maybe TaskStartFailedEventDetails),
    _heStateExitedEventDetails :: !(Maybe StateExitedEventDetails),
    _heLambdaFunctionSucceededEventDetails ::
      !(Maybe LambdaFunctionSucceededEventDetails),
    _heTaskSucceededEventDetails ::
      !(Maybe TaskSucceededEventDetails),
    _heActivitySucceededEventDetails ::
      !(Maybe ActivitySucceededEventDetails),
    _heMapIterationAbortedEventDetails ::
      !(Maybe MapIterationEventDetails),
    _heMapIterationSucceededEventDetails ::
      !(Maybe MapIterationEventDetails),
    _heMapIterationStartedEventDetails ::
      !(Maybe MapIterationEventDetails),
    _heLambdaFunctionTimedOutEventDetails ::
      !(Maybe LambdaFunctionTimedOutEventDetails),
    _heTaskTimedOutEventDetails :: !(Maybe TaskTimedOutEventDetails),
    _heActivityTimedOutEventDetails ::
      !(Maybe ActivityTimedOutEventDetails),
    _heExecutionFailedEventDetails ::
      !(Maybe ExecutionFailedEventDetails),
    _heExecutionAbortedEventDetails ::
      !(Maybe ExecutionAbortedEventDetails),
    _heExecutionSucceededEventDetails ::
      !(Maybe ExecutionSucceededEventDetails),
    _heLambdaFunctionScheduledEventDetails ::
      !(Maybe LambdaFunctionScheduledEventDetails),
    _heTaskScheduledEventDetails ::
      !(Maybe TaskScheduledEventDetails),
    _heActivityScheduledEventDetails ::
      !(Maybe ActivityScheduledEventDetails),
    _heExecutionStartedEventDetails ::
      !(Maybe ExecutionStartedEventDetails),
    _heActivityScheduleFailedEventDetails ::
      !(Maybe ActivityScheduleFailedEventDetails),
    _heLambdaFunctionScheduleFailedEventDetails ::
      !(Maybe LambdaFunctionScheduleFailedEventDetails),
    _heStateEnteredEventDetails :: !(Maybe StateEnteredEventDetails),
    _hePreviousEventId :: !(Maybe Integer),
    _heActivityFailedEventDetails ::
      !(Maybe ActivityFailedEventDetails),
    _heTaskFailedEventDetails :: !(Maybe TaskFailedEventDetails),
    _heLambdaFunctionFailedEventDetails ::
      !(Maybe LambdaFunctionFailedEventDetails),
    _heExecutionTimedOutEventDetails ::
      !(Maybe ExecutionTimedOutEventDetails),
    _heMapIterationFailedEventDetails ::
      !(Maybe MapIterationEventDetails),
    _heTimestamp :: !POSIX,
    _heType :: !HistoryEventType,
    _heId :: !Integer
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HistoryEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'heMapStateStartedEventDetails' - Contains details about Map state that was started.
--
-- * 'heTaskSubmitFailedEventDetails' - Contains details about a task that where the submit failed.
--
-- * 'heTaskStartedEventDetails' - Contains details about a task that was started.
--
-- * 'heActivityStartedEventDetails' - Undocumented member.
--
-- * 'heTaskSubmittedEventDetails' - Contains details about a submitted task.
--
-- * 'heLambdaFunctionStartFailedEventDetails' - Contains details about a lambda function that failed to start during an execution.
--
-- * 'heTaskStartFailedEventDetails' - Contains details about a task that failed to start.
--
-- * 'heStateExitedEventDetails' - Undocumented member.
--
-- * 'heLambdaFunctionSucceededEventDetails' - Contains details about a lambda function that terminated successfully during an execution.
--
-- * 'heTaskSucceededEventDetails' - Contains details about a task that succeeded.
--
-- * 'heActivitySucceededEventDetails' - Undocumented member.
--
-- * 'heMapIterationAbortedEventDetails' - Contains details about an iteration of a Map state that was aborted.
--
-- * 'heMapIterationSucceededEventDetails' - Contains details about an iteration of a Map state that succeeded.
--
-- * 'heMapIterationStartedEventDetails' - Contains details about an iteration of a Map state that was started.
--
-- * 'heLambdaFunctionTimedOutEventDetails' - Undocumented member.
--
-- * 'heTaskTimedOutEventDetails' - Contains details about a task that timed out.
--
-- * 'heActivityTimedOutEventDetails' - Undocumented member.
--
-- * 'heExecutionFailedEventDetails' - Undocumented member.
--
-- * 'heExecutionAbortedEventDetails' - Undocumented member.
--
-- * 'heExecutionSucceededEventDetails' - Undocumented member.
--
-- * 'heLambdaFunctionScheduledEventDetails' - Undocumented member.
--
-- * 'heTaskScheduledEventDetails' - Contains details about a task that was scheduled.
--
-- * 'heActivityScheduledEventDetails' - Undocumented member.
--
-- * 'heExecutionStartedEventDetails' - Undocumented member.
--
-- * 'heActivityScheduleFailedEventDetails' - Contains details about an activity schedule event that failed during an execution.
--
-- * 'heLambdaFunctionScheduleFailedEventDetails' - Undocumented member.
--
-- * 'heStateEnteredEventDetails' - Undocumented member.
--
-- * 'hePreviousEventId' - The id of the previous event.
--
-- * 'heActivityFailedEventDetails' - Undocumented member.
--
-- * 'heTaskFailedEventDetails' - Contains details about the failure of a task.
--
-- * 'heLambdaFunctionFailedEventDetails' - Undocumented member.
--
-- * 'heExecutionTimedOutEventDetails' - Undocumented member.
--
-- * 'heMapIterationFailedEventDetails' - Contains details about an iteration of a Map state that failed.
--
-- * 'heTimestamp' - The date and time the event occurred.
--
-- * 'heType' - The type of the event.
--
-- * 'heId' - The id of the event. Events are numbered sequentially, starting at one.
historyEvent ::
  -- | 'heTimestamp'
  UTCTime ->
  -- | 'heType'
  HistoryEventType ->
  -- | 'heId'
  Integer ->
  HistoryEvent
historyEvent pTimestamp_ pType_ pId_ =
  HistoryEvent'
    { _heMapStateStartedEventDetails = Nothing,
      _heTaskSubmitFailedEventDetails = Nothing,
      _heTaskStartedEventDetails = Nothing,
      _heActivityStartedEventDetails = Nothing,
      _heTaskSubmittedEventDetails = Nothing,
      _heLambdaFunctionStartFailedEventDetails = Nothing,
      _heTaskStartFailedEventDetails = Nothing,
      _heStateExitedEventDetails = Nothing,
      _heLambdaFunctionSucceededEventDetails = Nothing,
      _heTaskSucceededEventDetails = Nothing,
      _heActivitySucceededEventDetails = Nothing,
      _heMapIterationAbortedEventDetails = Nothing,
      _heMapIterationSucceededEventDetails = Nothing,
      _heMapIterationStartedEventDetails = Nothing,
      _heLambdaFunctionTimedOutEventDetails = Nothing,
      _heTaskTimedOutEventDetails = Nothing,
      _heActivityTimedOutEventDetails = Nothing,
      _heExecutionFailedEventDetails = Nothing,
      _heExecutionAbortedEventDetails = Nothing,
      _heExecutionSucceededEventDetails = Nothing,
      _heLambdaFunctionScheduledEventDetails = Nothing,
      _heTaskScheduledEventDetails = Nothing,
      _heActivityScheduledEventDetails = Nothing,
      _heExecutionStartedEventDetails = Nothing,
      _heActivityScheduleFailedEventDetails = Nothing,
      _heLambdaFunctionScheduleFailedEventDetails = Nothing,
      _heStateEnteredEventDetails = Nothing,
      _hePreviousEventId = Nothing,
      _heActivityFailedEventDetails = Nothing,
      _heTaskFailedEventDetails = Nothing,
      _heLambdaFunctionFailedEventDetails = Nothing,
      _heExecutionTimedOutEventDetails = Nothing,
      _heMapIterationFailedEventDetails = Nothing,
      _heTimestamp = _Time # pTimestamp_,
      _heType = pType_,
      _heId = pId_
    }

-- | Contains details about Map state that was started.
heMapStateStartedEventDetails :: Lens' HistoryEvent (Maybe MapStateStartedEventDetails)
heMapStateStartedEventDetails = lens _heMapStateStartedEventDetails (\s a -> s {_heMapStateStartedEventDetails = a})

-- | Contains details about a task that where the submit failed.
heTaskSubmitFailedEventDetails :: Lens' HistoryEvent (Maybe TaskSubmitFailedEventDetails)
heTaskSubmitFailedEventDetails = lens _heTaskSubmitFailedEventDetails (\s a -> s {_heTaskSubmitFailedEventDetails = a})

-- | Contains details about a task that was started.
heTaskStartedEventDetails :: Lens' HistoryEvent (Maybe TaskStartedEventDetails)
heTaskStartedEventDetails = lens _heTaskStartedEventDetails (\s a -> s {_heTaskStartedEventDetails = a})

-- | Undocumented member.
heActivityStartedEventDetails :: Lens' HistoryEvent (Maybe ActivityStartedEventDetails)
heActivityStartedEventDetails = lens _heActivityStartedEventDetails (\s a -> s {_heActivityStartedEventDetails = a})

-- | Contains details about a submitted task.
heTaskSubmittedEventDetails :: Lens' HistoryEvent (Maybe TaskSubmittedEventDetails)
heTaskSubmittedEventDetails = lens _heTaskSubmittedEventDetails (\s a -> s {_heTaskSubmittedEventDetails = a})

-- | Contains details about a lambda function that failed to start during an execution.
heLambdaFunctionStartFailedEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionStartFailedEventDetails)
heLambdaFunctionStartFailedEventDetails = lens _heLambdaFunctionStartFailedEventDetails (\s a -> s {_heLambdaFunctionStartFailedEventDetails = a})

-- | Contains details about a task that failed to start.
heTaskStartFailedEventDetails :: Lens' HistoryEvent (Maybe TaskStartFailedEventDetails)
heTaskStartFailedEventDetails = lens _heTaskStartFailedEventDetails (\s a -> s {_heTaskStartFailedEventDetails = a})

-- | Undocumented member.
heStateExitedEventDetails :: Lens' HistoryEvent (Maybe StateExitedEventDetails)
heStateExitedEventDetails = lens _heStateExitedEventDetails (\s a -> s {_heStateExitedEventDetails = a})

-- | Contains details about a lambda function that terminated successfully during an execution.
heLambdaFunctionSucceededEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionSucceededEventDetails)
heLambdaFunctionSucceededEventDetails = lens _heLambdaFunctionSucceededEventDetails (\s a -> s {_heLambdaFunctionSucceededEventDetails = a})

-- | Contains details about a task that succeeded.
heTaskSucceededEventDetails :: Lens' HistoryEvent (Maybe TaskSucceededEventDetails)
heTaskSucceededEventDetails = lens _heTaskSucceededEventDetails (\s a -> s {_heTaskSucceededEventDetails = a})

-- | Undocumented member.
heActivitySucceededEventDetails :: Lens' HistoryEvent (Maybe ActivitySucceededEventDetails)
heActivitySucceededEventDetails = lens _heActivitySucceededEventDetails (\s a -> s {_heActivitySucceededEventDetails = a})

-- | Contains details about an iteration of a Map state that was aborted.
heMapIterationAbortedEventDetails :: Lens' HistoryEvent (Maybe MapIterationEventDetails)
heMapIterationAbortedEventDetails = lens _heMapIterationAbortedEventDetails (\s a -> s {_heMapIterationAbortedEventDetails = a})

-- | Contains details about an iteration of a Map state that succeeded.
heMapIterationSucceededEventDetails :: Lens' HistoryEvent (Maybe MapIterationEventDetails)
heMapIterationSucceededEventDetails = lens _heMapIterationSucceededEventDetails (\s a -> s {_heMapIterationSucceededEventDetails = a})

-- | Contains details about an iteration of a Map state that was started.
heMapIterationStartedEventDetails :: Lens' HistoryEvent (Maybe MapIterationEventDetails)
heMapIterationStartedEventDetails = lens _heMapIterationStartedEventDetails (\s a -> s {_heMapIterationStartedEventDetails = a})

-- | Undocumented member.
heLambdaFunctionTimedOutEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionTimedOutEventDetails)
heLambdaFunctionTimedOutEventDetails = lens _heLambdaFunctionTimedOutEventDetails (\s a -> s {_heLambdaFunctionTimedOutEventDetails = a})

-- | Contains details about a task that timed out.
heTaskTimedOutEventDetails :: Lens' HistoryEvent (Maybe TaskTimedOutEventDetails)
heTaskTimedOutEventDetails = lens _heTaskTimedOutEventDetails (\s a -> s {_heTaskTimedOutEventDetails = a})

-- | Undocumented member.
heActivityTimedOutEventDetails :: Lens' HistoryEvent (Maybe ActivityTimedOutEventDetails)
heActivityTimedOutEventDetails = lens _heActivityTimedOutEventDetails (\s a -> s {_heActivityTimedOutEventDetails = a})

-- | Undocumented member.
heExecutionFailedEventDetails :: Lens' HistoryEvent (Maybe ExecutionFailedEventDetails)
heExecutionFailedEventDetails = lens _heExecutionFailedEventDetails (\s a -> s {_heExecutionFailedEventDetails = a})

-- | Undocumented member.
heExecutionAbortedEventDetails :: Lens' HistoryEvent (Maybe ExecutionAbortedEventDetails)
heExecutionAbortedEventDetails = lens _heExecutionAbortedEventDetails (\s a -> s {_heExecutionAbortedEventDetails = a})

-- | Undocumented member.
heExecutionSucceededEventDetails :: Lens' HistoryEvent (Maybe ExecutionSucceededEventDetails)
heExecutionSucceededEventDetails = lens _heExecutionSucceededEventDetails (\s a -> s {_heExecutionSucceededEventDetails = a})

-- | Undocumented member.
heLambdaFunctionScheduledEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionScheduledEventDetails)
heLambdaFunctionScheduledEventDetails = lens _heLambdaFunctionScheduledEventDetails (\s a -> s {_heLambdaFunctionScheduledEventDetails = a})

-- | Contains details about a task that was scheduled.
heTaskScheduledEventDetails :: Lens' HistoryEvent (Maybe TaskScheduledEventDetails)
heTaskScheduledEventDetails = lens _heTaskScheduledEventDetails (\s a -> s {_heTaskScheduledEventDetails = a})

-- | Undocumented member.
heActivityScheduledEventDetails :: Lens' HistoryEvent (Maybe ActivityScheduledEventDetails)
heActivityScheduledEventDetails = lens _heActivityScheduledEventDetails (\s a -> s {_heActivityScheduledEventDetails = a})

-- | Undocumented member.
heExecutionStartedEventDetails :: Lens' HistoryEvent (Maybe ExecutionStartedEventDetails)
heExecutionStartedEventDetails = lens _heExecutionStartedEventDetails (\s a -> s {_heExecutionStartedEventDetails = a})

-- | Contains details about an activity schedule event that failed during an execution.
heActivityScheduleFailedEventDetails :: Lens' HistoryEvent (Maybe ActivityScheduleFailedEventDetails)
heActivityScheduleFailedEventDetails = lens _heActivityScheduleFailedEventDetails (\s a -> s {_heActivityScheduleFailedEventDetails = a})

-- | Undocumented member.
heLambdaFunctionScheduleFailedEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionScheduleFailedEventDetails)
heLambdaFunctionScheduleFailedEventDetails = lens _heLambdaFunctionScheduleFailedEventDetails (\s a -> s {_heLambdaFunctionScheduleFailedEventDetails = a})

-- | Undocumented member.
heStateEnteredEventDetails :: Lens' HistoryEvent (Maybe StateEnteredEventDetails)
heStateEnteredEventDetails = lens _heStateEnteredEventDetails (\s a -> s {_heStateEnteredEventDetails = a})

-- | The id of the previous event.
hePreviousEventId :: Lens' HistoryEvent (Maybe Integer)
hePreviousEventId = lens _hePreviousEventId (\s a -> s {_hePreviousEventId = a})

-- | Undocumented member.
heActivityFailedEventDetails :: Lens' HistoryEvent (Maybe ActivityFailedEventDetails)
heActivityFailedEventDetails = lens _heActivityFailedEventDetails (\s a -> s {_heActivityFailedEventDetails = a})

-- | Contains details about the failure of a task.
heTaskFailedEventDetails :: Lens' HistoryEvent (Maybe TaskFailedEventDetails)
heTaskFailedEventDetails = lens _heTaskFailedEventDetails (\s a -> s {_heTaskFailedEventDetails = a})

-- | Undocumented member.
heLambdaFunctionFailedEventDetails :: Lens' HistoryEvent (Maybe LambdaFunctionFailedEventDetails)
heLambdaFunctionFailedEventDetails = lens _heLambdaFunctionFailedEventDetails (\s a -> s {_heLambdaFunctionFailedEventDetails = a})

-- | Undocumented member.
heExecutionTimedOutEventDetails :: Lens' HistoryEvent (Maybe ExecutionTimedOutEventDetails)
heExecutionTimedOutEventDetails = lens _heExecutionTimedOutEventDetails (\s a -> s {_heExecutionTimedOutEventDetails = a})

-- | Contains details about an iteration of a Map state that failed.
heMapIterationFailedEventDetails :: Lens' HistoryEvent (Maybe MapIterationEventDetails)
heMapIterationFailedEventDetails = lens _heMapIterationFailedEventDetails (\s a -> s {_heMapIterationFailedEventDetails = a})

-- | The date and time the event occurred.
heTimestamp :: Lens' HistoryEvent UTCTime
heTimestamp = lens _heTimestamp (\s a -> s {_heTimestamp = a}) . _Time

-- | The type of the event.
heType :: Lens' HistoryEvent HistoryEventType
heType = lens _heType (\s a -> s {_heType = a})

-- | The id of the event. Events are numbered sequentially, starting at one.
heId :: Lens' HistoryEvent Integer
heId = lens _heId (\s a -> s {_heId = a})

instance FromJSON HistoryEvent where
  parseJSON =
    withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            <$> (x .:? "mapStateStartedEventDetails")
            <*> (x .:? "taskSubmitFailedEventDetails")
            <*> (x .:? "taskStartedEventDetails")
            <*> (x .:? "activityStartedEventDetails")
            <*> (x .:? "taskSubmittedEventDetails")
            <*> (x .:? "lambdaFunctionStartFailedEventDetails")
            <*> (x .:? "taskStartFailedEventDetails")
            <*> (x .:? "stateExitedEventDetails")
            <*> (x .:? "lambdaFunctionSucceededEventDetails")
            <*> (x .:? "taskSucceededEventDetails")
            <*> (x .:? "activitySucceededEventDetails")
            <*> (x .:? "mapIterationAbortedEventDetails")
            <*> (x .:? "mapIterationSucceededEventDetails")
            <*> (x .:? "mapIterationStartedEventDetails")
            <*> (x .:? "lambdaFunctionTimedOutEventDetails")
            <*> (x .:? "taskTimedOutEventDetails")
            <*> (x .:? "activityTimedOutEventDetails")
            <*> (x .:? "executionFailedEventDetails")
            <*> (x .:? "executionAbortedEventDetails")
            <*> (x .:? "executionSucceededEventDetails")
            <*> (x .:? "lambdaFunctionScheduledEventDetails")
            <*> (x .:? "taskScheduledEventDetails")
            <*> (x .:? "activityScheduledEventDetails")
            <*> (x .:? "executionStartedEventDetails")
            <*> (x .:? "activityScheduleFailedEventDetails")
            <*> (x .:? "lambdaFunctionScheduleFailedEventDetails")
            <*> (x .:? "stateEnteredEventDetails")
            <*> (x .:? "previousEventId")
            <*> (x .:? "activityFailedEventDetails")
            <*> (x .:? "taskFailedEventDetails")
            <*> (x .:? "lambdaFunctionFailedEventDetails")
            <*> (x .:? "executionTimedOutEventDetails")
            <*> (x .:? "mapIterationFailedEventDetails")
            <*> (x .: "timestamp")
            <*> (x .: "type")
            <*> (x .: "id")
      )

instance Hashable HistoryEvent

instance NFData HistoryEvent
