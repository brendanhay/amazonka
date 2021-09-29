{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEvent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
-- /See:/ 'newHistoryEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { executionFailedEventDetails :: Prelude.Maybe ExecutionFailedEventDetails,
    -- | Contains details about a task that where the submit failed.
    taskSubmitFailedEventDetails :: Prelude.Maybe TaskSubmitFailedEventDetails,
    -- | Contains details about an iteration of a Map state that was started.
    mapIterationStartedEventDetails :: Prelude.Maybe MapIterationEventDetails,
    -- | Contains details about an iteration of a Map state that succeeded.
    mapIterationSucceededEventDetails :: Prelude.Maybe MapIterationEventDetails,
    -- | Contains details about an iteration of a Map state that failed.
    mapIterationFailedEventDetails :: Prelude.Maybe MapIterationEventDetails,
    -- | Contains details about an iteration of a Map state that was aborted.
    mapIterationAbortedEventDetails :: Prelude.Maybe MapIterationEventDetails,
    executionTimedOutEventDetails :: Prelude.Maybe ExecutionTimedOutEventDetails,
    -- | The id of the previous event.
    previousEventId :: Prelude.Maybe Prelude.Integer,
    executionStartedEventDetails :: Prelude.Maybe ExecutionStartedEventDetails,
    lambdaFunctionScheduleFailedEventDetails :: Prelude.Maybe LambdaFunctionScheduleFailedEventDetails,
    -- | Contains details about an activity schedule event that failed during an
    -- execution.
    activityScheduleFailedEventDetails :: Prelude.Maybe ActivityScheduleFailedEventDetails,
    -- | Contains details about a task that was scheduled.
    taskScheduledEventDetails :: Prelude.Maybe TaskScheduledEventDetails,
    activityScheduledEventDetails :: Prelude.Maybe ActivityScheduledEventDetails,
    lambdaFunctionScheduledEventDetails :: Prelude.Maybe LambdaFunctionScheduledEventDetails,
    executionSucceededEventDetails :: Prelude.Maybe ExecutionSucceededEventDetails,
    executionAbortedEventDetails :: Prelude.Maybe ExecutionAbortedEventDetails,
    lambdaFunctionTimedOutEventDetails :: Prelude.Maybe LambdaFunctionTimedOutEventDetails,
    -- | Contains details about a task that timed out.
    taskTimedOutEventDetails :: Prelude.Maybe TaskTimedOutEventDetails,
    activityTimedOutEventDetails :: Prelude.Maybe ActivityTimedOutEventDetails,
    -- | Contains details about Map state that was started.
    mapStateStartedEventDetails :: Prelude.Maybe MapStateStartedEventDetails,
    activitySucceededEventDetails :: Prelude.Maybe ActivitySucceededEventDetails,
    stateExitedEventDetails :: Prelude.Maybe StateExitedEventDetails,
    activityFailedEventDetails :: Prelude.Maybe ActivityFailedEventDetails,
    stateEnteredEventDetails :: Prelude.Maybe StateEnteredEventDetails,
    -- | Contains details about a task that failed to start.
    taskStartFailedEventDetails :: Prelude.Maybe TaskStartFailedEventDetails,
    -- | Contains details about a task that succeeded.
    taskSucceededEventDetails :: Prelude.Maybe TaskSucceededEventDetails,
    -- | Contains details about the failure of a task.
    taskFailedEventDetails :: Prelude.Maybe TaskFailedEventDetails,
    -- | Contains details about a lambda function that failed to start during an
    -- execution.
    lambdaFunctionStartFailedEventDetails :: Prelude.Maybe LambdaFunctionStartFailedEventDetails,
    -- | Contains details about a lambda function that terminated successfully
    -- during an execution.
    lambdaFunctionSucceededEventDetails :: Prelude.Maybe LambdaFunctionSucceededEventDetails,
    lambdaFunctionFailedEventDetails :: Prelude.Maybe LambdaFunctionFailedEventDetails,
    -- | Contains details about a task that was started.
    taskStartedEventDetails :: Prelude.Maybe TaskStartedEventDetails,
    -- | Contains details about a submitted task.
    taskSubmittedEventDetails :: Prelude.Maybe TaskSubmittedEventDetails,
    activityStartedEventDetails :: Prelude.Maybe ActivityStartedEventDetails,
    -- | The date and time the event occurred.
    timestamp :: Core.POSIX,
    -- | The type of the event.
    type' :: HistoryEventType,
    -- | The id of the event. Events are numbered sequentially, starting at one.
    id :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistoryEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionFailedEventDetails', 'historyEvent_executionFailedEventDetails' - Undocumented member.
--
-- 'taskSubmitFailedEventDetails', 'historyEvent_taskSubmitFailedEventDetails' - Contains details about a task that where the submit failed.
--
-- 'mapIterationStartedEventDetails', 'historyEvent_mapIterationStartedEventDetails' - Contains details about an iteration of a Map state that was started.
--
-- 'mapIterationSucceededEventDetails', 'historyEvent_mapIterationSucceededEventDetails' - Contains details about an iteration of a Map state that succeeded.
--
-- 'mapIterationFailedEventDetails', 'historyEvent_mapIterationFailedEventDetails' - Contains details about an iteration of a Map state that failed.
--
-- 'mapIterationAbortedEventDetails', 'historyEvent_mapIterationAbortedEventDetails' - Contains details about an iteration of a Map state that was aborted.
--
-- 'executionTimedOutEventDetails', 'historyEvent_executionTimedOutEventDetails' - Undocumented member.
--
-- 'previousEventId', 'historyEvent_previousEventId' - The id of the previous event.
--
-- 'executionStartedEventDetails', 'historyEvent_executionStartedEventDetails' - Undocumented member.
--
-- 'lambdaFunctionScheduleFailedEventDetails', 'historyEvent_lambdaFunctionScheduleFailedEventDetails' - Undocumented member.
--
-- 'activityScheduleFailedEventDetails', 'historyEvent_activityScheduleFailedEventDetails' - Contains details about an activity schedule event that failed during an
-- execution.
--
-- 'taskScheduledEventDetails', 'historyEvent_taskScheduledEventDetails' - Contains details about a task that was scheduled.
--
-- 'activityScheduledEventDetails', 'historyEvent_activityScheduledEventDetails' - Undocumented member.
--
-- 'lambdaFunctionScheduledEventDetails', 'historyEvent_lambdaFunctionScheduledEventDetails' - Undocumented member.
--
-- 'executionSucceededEventDetails', 'historyEvent_executionSucceededEventDetails' - Undocumented member.
--
-- 'executionAbortedEventDetails', 'historyEvent_executionAbortedEventDetails' - Undocumented member.
--
-- 'lambdaFunctionTimedOutEventDetails', 'historyEvent_lambdaFunctionTimedOutEventDetails' - Undocumented member.
--
-- 'taskTimedOutEventDetails', 'historyEvent_taskTimedOutEventDetails' - Contains details about a task that timed out.
--
-- 'activityTimedOutEventDetails', 'historyEvent_activityTimedOutEventDetails' - Undocumented member.
--
-- 'mapStateStartedEventDetails', 'historyEvent_mapStateStartedEventDetails' - Contains details about Map state that was started.
--
-- 'activitySucceededEventDetails', 'historyEvent_activitySucceededEventDetails' - Undocumented member.
--
-- 'stateExitedEventDetails', 'historyEvent_stateExitedEventDetails' - Undocumented member.
--
-- 'activityFailedEventDetails', 'historyEvent_activityFailedEventDetails' - Undocumented member.
--
-- 'stateEnteredEventDetails', 'historyEvent_stateEnteredEventDetails' - Undocumented member.
--
-- 'taskStartFailedEventDetails', 'historyEvent_taskStartFailedEventDetails' - Contains details about a task that failed to start.
--
-- 'taskSucceededEventDetails', 'historyEvent_taskSucceededEventDetails' - Contains details about a task that succeeded.
--
-- 'taskFailedEventDetails', 'historyEvent_taskFailedEventDetails' - Contains details about the failure of a task.
--
-- 'lambdaFunctionStartFailedEventDetails', 'historyEvent_lambdaFunctionStartFailedEventDetails' - Contains details about a lambda function that failed to start during an
-- execution.
--
-- 'lambdaFunctionSucceededEventDetails', 'historyEvent_lambdaFunctionSucceededEventDetails' - Contains details about a lambda function that terminated successfully
-- during an execution.
--
-- 'lambdaFunctionFailedEventDetails', 'historyEvent_lambdaFunctionFailedEventDetails' - Undocumented member.
--
-- 'taskStartedEventDetails', 'historyEvent_taskStartedEventDetails' - Contains details about a task that was started.
--
-- 'taskSubmittedEventDetails', 'historyEvent_taskSubmittedEventDetails' - Contains details about a submitted task.
--
-- 'activityStartedEventDetails', 'historyEvent_activityStartedEventDetails' - Undocumented member.
--
-- 'timestamp', 'historyEvent_timestamp' - The date and time the event occurred.
--
-- 'type'', 'historyEvent_type' - The type of the event.
--
-- 'id', 'historyEvent_id' - The id of the event. Events are numbered sequentially, starting at one.
newHistoryEvent ::
  -- | 'timestamp'
  Prelude.UTCTime ->
  -- | 'type''
  HistoryEventType ->
  -- | 'id'
  Prelude.Integer ->
  HistoryEvent
newHistoryEvent pTimestamp_ pType_ pId_ =
  HistoryEvent'
    { executionFailedEventDetails =
        Prelude.Nothing,
      taskSubmitFailedEventDetails = Prelude.Nothing,
      mapIterationStartedEventDetails = Prelude.Nothing,
      mapIterationSucceededEventDetails = Prelude.Nothing,
      mapIterationFailedEventDetails = Prelude.Nothing,
      mapIterationAbortedEventDetails = Prelude.Nothing,
      executionTimedOutEventDetails = Prelude.Nothing,
      previousEventId = Prelude.Nothing,
      executionStartedEventDetails = Prelude.Nothing,
      lambdaFunctionScheduleFailedEventDetails =
        Prelude.Nothing,
      activityScheduleFailedEventDetails = Prelude.Nothing,
      taskScheduledEventDetails = Prelude.Nothing,
      activityScheduledEventDetails = Prelude.Nothing,
      lambdaFunctionScheduledEventDetails =
        Prelude.Nothing,
      executionSucceededEventDetails = Prelude.Nothing,
      executionAbortedEventDetails = Prelude.Nothing,
      lambdaFunctionTimedOutEventDetails = Prelude.Nothing,
      taskTimedOutEventDetails = Prelude.Nothing,
      activityTimedOutEventDetails = Prelude.Nothing,
      mapStateStartedEventDetails = Prelude.Nothing,
      activitySucceededEventDetails = Prelude.Nothing,
      stateExitedEventDetails = Prelude.Nothing,
      activityFailedEventDetails = Prelude.Nothing,
      stateEnteredEventDetails = Prelude.Nothing,
      taskStartFailedEventDetails = Prelude.Nothing,
      taskSucceededEventDetails = Prelude.Nothing,
      taskFailedEventDetails = Prelude.Nothing,
      lambdaFunctionStartFailedEventDetails =
        Prelude.Nothing,
      lambdaFunctionSucceededEventDetails =
        Prelude.Nothing,
      lambdaFunctionFailedEventDetails = Prelude.Nothing,
      taskStartedEventDetails = Prelude.Nothing,
      taskSubmittedEventDetails = Prelude.Nothing,
      activityStartedEventDetails = Prelude.Nothing,
      timestamp = Core._Time Lens.# pTimestamp_,
      type' = pType_,
      id = pId_
    }

-- | Undocumented member.
historyEvent_executionFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ExecutionFailedEventDetails)
historyEvent_executionFailedEventDetails = Lens.lens (\HistoryEvent' {executionFailedEventDetails} -> executionFailedEventDetails) (\s@HistoryEvent' {} a -> s {executionFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that where the submit failed.
historyEvent_taskSubmitFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskSubmitFailedEventDetails)
historyEvent_taskSubmitFailedEventDetails = Lens.lens (\HistoryEvent' {taskSubmitFailedEventDetails} -> taskSubmitFailedEventDetails) (\s@HistoryEvent' {} a -> s {taskSubmitFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that was started.
historyEvent_mapIterationStartedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe MapIterationEventDetails)
historyEvent_mapIterationStartedEventDetails = Lens.lens (\HistoryEvent' {mapIterationStartedEventDetails} -> mapIterationStartedEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationStartedEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that succeeded.
historyEvent_mapIterationSucceededEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe MapIterationEventDetails)
historyEvent_mapIterationSucceededEventDetails = Lens.lens (\HistoryEvent' {mapIterationSucceededEventDetails} -> mapIterationSucceededEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationSucceededEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that failed.
historyEvent_mapIterationFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe MapIterationEventDetails)
historyEvent_mapIterationFailedEventDetails = Lens.lens (\HistoryEvent' {mapIterationFailedEventDetails} -> mapIterationFailedEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that was aborted.
historyEvent_mapIterationAbortedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe MapIterationEventDetails)
historyEvent_mapIterationAbortedEventDetails = Lens.lens (\HistoryEvent' {mapIterationAbortedEventDetails} -> mapIterationAbortedEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationAbortedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ExecutionTimedOutEventDetails)
historyEvent_executionTimedOutEventDetails = Lens.lens (\HistoryEvent' {executionTimedOutEventDetails} -> executionTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {executionTimedOutEventDetails = a} :: HistoryEvent)

-- | The id of the previous event.
historyEvent_previousEventId :: Lens.Lens' HistoryEvent (Prelude.Maybe Prelude.Integer)
historyEvent_previousEventId = Lens.lens (\HistoryEvent' {previousEventId} -> previousEventId) (\s@HistoryEvent' {} a -> s {previousEventId = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionStartedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ExecutionStartedEventDetails)
historyEvent_executionStartedEventDetails = Lens.lens (\HistoryEvent' {executionStartedEventDetails} -> executionStartedEventDetails) (\s@HistoryEvent' {} a -> s {executionStartedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionScheduleFailedEventDetails)
historyEvent_lambdaFunctionScheduleFailedEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionScheduleFailedEventDetails} -> lambdaFunctionScheduleFailedEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionScheduleFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about an activity schedule event that failed during an
-- execution.
historyEvent_activityScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityScheduleFailedEventDetails)
historyEvent_activityScheduleFailedEventDetails = Lens.lens (\HistoryEvent' {activityScheduleFailedEventDetails} -> activityScheduleFailedEventDetails) (\s@HistoryEvent' {} a -> s {activityScheduleFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that was scheduled.
historyEvent_taskScheduledEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskScheduledEventDetails)
historyEvent_taskScheduledEventDetails = Lens.lens (\HistoryEvent' {taskScheduledEventDetails} -> taskScheduledEventDetails) (\s@HistoryEvent' {} a -> s {taskScheduledEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityScheduledEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityScheduledEventDetails)
historyEvent_activityScheduledEventDetails = Lens.lens (\HistoryEvent' {activityScheduledEventDetails} -> activityScheduledEventDetails) (\s@HistoryEvent' {} a -> s {activityScheduledEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionScheduledEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionScheduledEventDetails)
historyEvent_lambdaFunctionScheduledEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionScheduledEventDetails} -> lambdaFunctionScheduledEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionScheduledEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionSucceededEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ExecutionSucceededEventDetails)
historyEvent_executionSucceededEventDetails = Lens.lens (\HistoryEvent' {executionSucceededEventDetails} -> executionSucceededEventDetails) (\s@HistoryEvent' {} a -> s {executionSucceededEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionAbortedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ExecutionAbortedEventDetails)
historyEvent_executionAbortedEventDetails = Lens.lens (\HistoryEvent' {executionAbortedEventDetails} -> executionAbortedEventDetails) (\s@HistoryEvent' {} a -> s {executionAbortedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionTimedOutEventDetails)
historyEvent_lambdaFunctionTimedOutEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionTimedOutEventDetails} -> lambdaFunctionTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionTimedOutEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that timed out.
historyEvent_taskTimedOutEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskTimedOutEventDetails)
historyEvent_taskTimedOutEventDetails = Lens.lens (\HistoryEvent' {taskTimedOutEventDetails} -> taskTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {taskTimedOutEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityTimedOutEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTimedOutEventDetails)
historyEvent_activityTimedOutEventDetails = Lens.lens (\HistoryEvent' {activityTimedOutEventDetails} -> activityTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {activityTimedOutEventDetails = a} :: HistoryEvent)

-- | Contains details about Map state that was started.
historyEvent_mapStateStartedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe MapStateStartedEventDetails)
historyEvent_mapStateStartedEventDetails = Lens.lens (\HistoryEvent' {mapStateStartedEventDetails} -> mapStateStartedEventDetails) (\s@HistoryEvent' {} a -> s {mapStateStartedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activitySucceededEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivitySucceededEventDetails)
historyEvent_activitySucceededEventDetails = Lens.lens (\HistoryEvent' {activitySucceededEventDetails} -> activitySucceededEventDetails) (\s@HistoryEvent' {} a -> s {activitySucceededEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_stateExitedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe StateExitedEventDetails)
historyEvent_stateExitedEventDetails = Lens.lens (\HistoryEvent' {stateExitedEventDetails} -> stateExitedEventDetails) (\s@HistoryEvent' {} a -> s {stateExitedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityFailedEventDetails)
historyEvent_activityFailedEventDetails = Lens.lens (\HistoryEvent' {activityFailedEventDetails} -> activityFailedEventDetails) (\s@HistoryEvent' {} a -> s {activityFailedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_stateEnteredEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe StateEnteredEventDetails)
historyEvent_stateEnteredEventDetails = Lens.lens (\HistoryEvent' {stateEnteredEventDetails} -> stateEnteredEventDetails) (\s@HistoryEvent' {} a -> s {stateEnteredEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that failed to start.
historyEvent_taskStartFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskStartFailedEventDetails)
historyEvent_taskStartFailedEventDetails = Lens.lens (\HistoryEvent' {taskStartFailedEventDetails} -> taskStartFailedEventDetails) (\s@HistoryEvent' {} a -> s {taskStartFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that succeeded.
historyEvent_taskSucceededEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskSucceededEventDetails)
historyEvent_taskSucceededEventDetails = Lens.lens (\HistoryEvent' {taskSucceededEventDetails} -> taskSucceededEventDetails) (\s@HistoryEvent' {} a -> s {taskSucceededEventDetails = a} :: HistoryEvent)

-- | Contains details about the failure of a task.
historyEvent_taskFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskFailedEventDetails)
historyEvent_taskFailedEventDetails = Lens.lens (\HistoryEvent' {taskFailedEventDetails} -> taskFailedEventDetails) (\s@HistoryEvent' {} a -> s {taskFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a lambda function that failed to start during an
-- execution.
historyEvent_lambdaFunctionStartFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionStartFailedEventDetails)
historyEvent_lambdaFunctionStartFailedEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionStartFailedEventDetails} -> lambdaFunctionStartFailedEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionStartFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a lambda function that terminated successfully
-- during an execution.
historyEvent_lambdaFunctionSucceededEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionSucceededEventDetails)
historyEvent_lambdaFunctionSucceededEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionSucceededEventDetails} -> lambdaFunctionSucceededEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionSucceededEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionFailedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionFailedEventDetails)
historyEvent_lambdaFunctionFailedEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionFailedEventDetails} -> lambdaFunctionFailedEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that was started.
historyEvent_taskStartedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskStartedEventDetails)
historyEvent_taskStartedEventDetails = Lens.lens (\HistoryEvent' {taskStartedEventDetails} -> taskStartedEventDetails) (\s@HistoryEvent' {} a -> s {taskStartedEventDetails = a} :: HistoryEvent)

-- | Contains details about a submitted task.
historyEvent_taskSubmittedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe TaskSubmittedEventDetails)
historyEvent_taskSubmittedEventDetails = Lens.lens (\HistoryEvent' {taskSubmittedEventDetails} -> taskSubmittedEventDetails) (\s@HistoryEvent' {} a -> s {taskSubmittedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityStartedEventDetails :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityStartedEventDetails)
historyEvent_activityStartedEventDetails = Lens.lens (\HistoryEvent' {activityStartedEventDetails} -> activityStartedEventDetails) (\s@HistoryEvent' {} a -> s {activityStartedEventDetails = a} :: HistoryEvent)

-- | The date and time the event occurred.
historyEvent_timestamp :: Lens.Lens' HistoryEvent Prelude.UTCTime
historyEvent_timestamp = Lens.lens (\HistoryEvent' {timestamp} -> timestamp) (\s@HistoryEvent' {} a -> s {timestamp = a} :: HistoryEvent) Prelude.. Core._Time

-- | The type of the event.
historyEvent_type :: Lens.Lens' HistoryEvent HistoryEventType
historyEvent_type = Lens.lens (\HistoryEvent' {type'} -> type') (\s@HistoryEvent' {} a -> s {type' = a} :: HistoryEvent)

-- | The id of the event. Events are numbered sequentially, starting at one.
historyEvent_id :: Lens.Lens' HistoryEvent Prelude.Integer
historyEvent_id = Lens.lens (\HistoryEvent' {id} -> id) (\s@HistoryEvent' {} a -> s {id = a} :: HistoryEvent)

instance Core.FromJSON HistoryEvent where
  parseJSON =
    Core.withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            Prelude.<$> (x Core..:? "executionFailedEventDetails")
            Prelude.<*> (x Core..:? "taskSubmitFailedEventDetails")
            Prelude.<*> (x Core..:? "mapIterationStartedEventDetails")
            Prelude.<*> (x Core..:? "mapIterationSucceededEventDetails")
            Prelude.<*> (x Core..:? "mapIterationFailedEventDetails")
            Prelude.<*> (x Core..:? "mapIterationAbortedEventDetails")
            Prelude.<*> (x Core..:? "executionTimedOutEventDetails")
            Prelude.<*> (x Core..:? "previousEventId")
            Prelude.<*> (x Core..:? "executionStartedEventDetails")
            Prelude.<*> ( x
                            Core..:? "lambdaFunctionScheduleFailedEventDetails"
                        )
            Prelude.<*> (x Core..:? "activityScheduleFailedEventDetails")
            Prelude.<*> (x Core..:? "taskScheduledEventDetails")
            Prelude.<*> (x Core..:? "activityScheduledEventDetails")
            Prelude.<*> (x Core..:? "lambdaFunctionScheduledEventDetails")
            Prelude.<*> (x Core..:? "executionSucceededEventDetails")
            Prelude.<*> (x Core..:? "executionAbortedEventDetails")
            Prelude.<*> (x Core..:? "lambdaFunctionTimedOutEventDetails")
            Prelude.<*> (x Core..:? "taskTimedOutEventDetails")
            Prelude.<*> (x Core..:? "activityTimedOutEventDetails")
            Prelude.<*> (x Core..:? "mapStateStartedEventDetails")
            Prelude.<*> (x Core..:? "activitySucceededEventDetails")
            Prelude.<*> (x Core..:? "stateExitedEventDetails")
            Prelude.<*> (x Core..:? "activityFailedEventDetails")
            Prelude.<*> (x Core..:? "stateEnteredEventDetails")
            Prelude.<*> (x Core..:? "taskStartFailedEventDetails")
            Prelude.<*> (x Core..:? "taskSucceededEventDetails")
            Prelude.<*> (x Core..:? "taskFailedEventDetails")
            Prelude.<*> (x Core..:? "lambdaFunctionStartFailedEventDetails")
            Prelude.<*> (x Core..:? "lambdaFunctionSucceededEventDetails")
            Prelude.<*> (x Core..:? "lambdaFunctionFailedEventDetails")
            Prelude.<*> (x Core..:? "taskStartedEventDetails")
            Prelude.<*> (x Core..:? "taskSubmittedEventDetails")
            Prelude.<*> (x Core..:? "activityStartedEventDetails")
            Prelude.<*> (x Core..: "timestamp")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "id")
      )

instance Prelude.Hashable HistoryEvent

instance Prelude.NFData HistoryEvent
