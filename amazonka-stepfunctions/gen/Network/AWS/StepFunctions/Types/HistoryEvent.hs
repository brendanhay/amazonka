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
  { executionFailedEventDetails :: Core.Maybe ExecutionFailedEventDetails,
    -- | Contains details about an iteration of a Map state that was started.
    mapIterationStartedEventDetails :: Core.Maybe MapIterationEventDetails,
    -- | Contains details about a task that where the submit failed.
    taskSubmitFailedEventDetails :: Core.Maybe TaskSubmitFailedEventDetails,
    -- | Contains details about an iteration of a Map state that was aborted.
    mapIterationAbortedEventDetails :: Core.Maybe MapIterationEventDetails,
    -- | Contains details about an iteration of a Map state that succeeded.
    mapIterationSucceededEventDetails :: Core.Maybe MapIterationEventDetails,
    -- | Contains details about an iteration of a Map state that failed.
    mapIterationFailedEventDetails :: Core.Maybe MapIterationEventDetails,
    executionTimedOutEventDetails :: Core.Maybe ExecutionTimedOutEventDetails,
    -- | The id of the previous event.
    previousEventId :: Core.Maybe Core.Integer,
    executionStartedEventDetails :: Core.Maybe ExecutionStartedEventDetails,
    lambdaFunctionScheduleFailedEventDetails :: Core.Maybe LambdaFunctionScheduleFailedEventDetails,
    -- | Contains details about an activity schedule event that failed during an
    -- execution.
    activityScheduleFailedEventDetails :: Core.Maybe ActivityScheduleFailedEventDetails,
    -- | Contains details about a task that was scheduled.
    taskScheduledEventDetails :: Core.Maybe TaskScheduledEventDetails,
    activityScheduledEventDetails :: Core.Maybe ActivityScheduledEventDetails,
    lambdaFunctionScheduledEventDetails :: Core.Maybe LambdaFunctionScheduledEventDetails,
    executionSucceededEventDetails :: Core.Maybe ExecutionSucceededEventDetails,
    executionAbortedEventDetails :: Core.Maybe ExecutionAbortedEventDetails,
    -- | Contains details about Map state that was started.
    mapStateStartedEventDetails :: Core.Maybe MapStateStartedEventDetails,
    lambdaFunctionTimedOutEventDetails :: Core.Maybe LambdaFunctionTimedOutEventDetails,
    activityTimedOutEventDetails :: Core.Maybe ActivityTimedOutEventDetails,
    -- | Contains details about a task that timed out.
    taskTimedOutEventDetails :: Core.Maybe TaskTimedOutEventDetails,
    -- | Contains details about a lambda function that failed to start during an
    -- execution.
    lambdaFunctionStartFailedEventDetails :: Core.Maybe LambdaFunctionStartFailedEventDetails,
    -- | Contains details about a task that failed to start.
    taskStartFailedEventDetails :: Core.Maybe TaskStartFailedEventDetails,
    -- | Contains details about the failure of a task.
    taskFailedEventDetails :: Core.Maybe TaskFailedEventDetails,
    -- | Contains details about a task that succeeded.
    taskSucceededEventDetails :: Core.Maybe TaskSucceededEventDetails,
    stateExitedEventDetails :: Core.Maybe StateExitedEventDetails,
    stateEnteredEventDetails :: Core.Maybe StateEnteredEventDetails,
    lambdaFunctionFailedEventDetails :: Core.Maybe LambdaFunctionFailedEventDetails,
    activityFailedEventDetails :: Core.Maybe ActivityFailedEventDetails,
    activitySucceededEventDetails :: Core.Maybe ActivitySucceededEventDetails,
    -- | Contains details about a lambda function that terminated successfully
    -- during an execution.
    lambdaFunctionSucceededEventDetails :: Core.Maybe LambdaFunctionSucceededEventDetails,
    -- | Contains details about a submitted task.
    taskSubmittedEventDetails :: Core.Maybe TaskSubmittedEventDetails,
    activityStartedEventDetails :: Core.Maybe ActivityStartedEventDetails,
    -- | Contains details about a task that was started.
    taskStartedEventDetails :: Core.Maybe TaskStartedEventDetails,
    -- | The date and time the event occurred.
    timestamp :: Core.POSIX,
    -- | The type of the event.
    type' :: HistoryEventType,
    -- | The id of the event. Events are numbered sequentially, starting at one.
    id :: Core.Integer
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'mapIterationStartedEventDetails', 'historyEvent_mapIterationStartedEventDetails' - Contains details about an iteration of a Map state that was started.
--
-- 'taskSubmitFailedEventDetails', 'historyEvent_taskSubmitFailedEventDetails' - Contains details about a task that where the submit failed.
--
-- 'mapIterationAbortedEventDetails', 'historyEvent_mapIterationAbortedEventDetails' - Contains details about an iteration of a Map state that was aborted.
--
-- 'mapIterationSucceededEventDetails', 'historyEvent_mapIterationSucceededEventDetails' - Contains details about an iteration of a Map state that succeeded.
--
-- 'mapIterationFailedEventDetails', 'historyEvent_mapIterationFailedEventDetails' - Contains details about an iteration of a Map state that failed.
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
-- 'mapStateStartedEventDetails', 'historyEvent_mapStateStartedEventDetails' - Contains details about Map state that was started.
--
-- 'lambdaFunctionTimedOutEventDetails', 'historyEvent_lambdaFunctionTimedOutEventDetails' - Undocumented member.
--
-- 'activityTimedOutEventDetails', 'historyEvent_activityTimedOutEventDetails' - Undocumented member.
--
-- 'taskTimedOutEventDetails', 'historyEvent_taskTimedOutEventDetails' - Contains details about a task that timed out.
--
-- 'lambdaFunctionStartFailedEventDetails', 'historyEvent_lambdaFunctionStartFailedEventDetails' - Contains details about a lambda function that failed to start during an
-- execution.
--
-- 'taskStartFailedEventDetails', 'historyEvent_taskStartFailedEventDetails' - Contains details about a task that failed to start.
--
-- 'taskFailedEventDetails', 'historyEvent_taskFailedEventDetails' - Contains details about the failure of a task.
--
-- 'taskSucceededEventDetails', 'historyEvent_taskSucceededEventDetails' - Contains details about a task that succeeded.
--
-- 'stateExitedEventDetails', 'historyEvent_stateExitedEventDetails' - Undocumented member.
--
-- 'stateEnteredEventDetails', 'historyEvent_stateEnteredEventDetails' - Undocumented member.
--
-- 'lambdaFunctionFailedEventDetails', 'historyEvent_lambdaFunctionFailedEventDetails' - Undocumented member.
--
-- 'activityFailedEventDetails', 'historyEvent_activityFailedEventDetails' - Undocumented member.
--
-- 'activitySucceededEventDetails', 'historyEvent_activitySucceededEventDetails' - Undocumented member.
--
-- 'lambdaFunctionSucceededEventDetails', 'historyEvent_lambdaFunctionSucceededEventDetails' - Contains details about a lambda function that terminated successfully
-- during an execution.
--
-- 'taskSubmittedEventDetails', 'historyEvent_taskSubmittedEventDetails' - Contains details about a submitted task.
--
-- 'activityStartedEventDetails', 'historyEvent_activityStartedEventDetails' - Undocumented member.
--
-- 'taskStartedEventDetails', 'historyEvent_taskStartedEventDetails' - Contains details about a task that was started.
--
-- 'timestamp', 'historyEvent_timestamp' - The date and time the event occurred.
--
-- 'type'', 'historyEvent_type' - The type of the event.
--
-- 'id', 'historyEvent_id' - The id of the event. Events are numbered sequentially, starting at one.
newHistoryEvent ::
  -- | 'timestamp'
  Core.UTCTime ->
  -- | 'type''
  HistoryEventType ->
  -- | 'id'
  Core.Integer ->
  HistoryEvent
newHistoryEvent pTimestamp_ pType_ pId_ =
  HistoryEvent'
    { executionFailedEventDetails =
        Core.Nothing,
      mapIterationStartedEventDetails = Core.Nothing,
      taskSubmitFailedEventDetails = Core.Nothing,
      mapIterationAbortedEventDetails = Core.Nothing,
      mapIterationSucceededEventDetails = Core.Nothing,
      mapIterationFailedEventDetails = Core.Nothing,
      executionTimedOutEventDetails = Core.Nothing,
      previousEventId = Core.Nothing,
      executionStartedEventDetails = Core.Nothing,
      lambdaFunctionScheduleFailedEventDetails =
        Core.Nothing,
      activityScheduleFailedEventDetails = Core.Nothing,
      taskScheduledEventDetails = Core.Nothing,
      activityScheduledEventDetails = Core.Nothing,
      lambdaFunctionScheduledEventDetails = Core.Nothing,
      executionSucceededEventDetails = Core.Nothing,
      executionAbortedEventDetails = Core.Nothing,
      mapStateStartedEventDetails = Core.Nothing,
      lambdaFunctionTimedOutEventDetails = Core.Nothing,
      activityTimedOutEventDetails = Core.Nothing,
      taskTimedOutEventDetails = Core.Nothing,
      lambdaFunctionStartFailedEventDetails = Core.Nothing,
      taskStartFailedEventDetails = Core.Nothing,
      taskFailedEventDetails = Core.Nothing,
      taskSucceededEventDetails = Core.Nothing,
      stateExitedEventDetails = Core.Nothing,
      stateEnteredEventDetails = Core.Nothing,
      lambdaFunctionFailedEventDetails = Core.Nothing,
      activityFailedEventDetails = Core.Nothing,
      activitySucceededEventDetails = Core.Nothing,
      lambdaFunctionSucceededEventDetails = Core.Nothing,
      taskSubmittedEventDetails = Core.Nothing,
      activityStartedEventDetails = Core.Nothing,
      taskStartedEventDetails = Core.Nothing,
      timestamp = Core._Time Lens.# pTimestamp_,
      type' = pType_,
      id = pId_
    }

-- | Undocumented member.
historyEvent_executionFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ExecutionFailedEventDetails)
historyEvent_executionFailedEventDetails = Lens.lens (\HistoryEvent' {executionFailedEventDetails} -> executionFailedEventDetails) (\s@HistoryEvent' {} a -> s {executionFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that was started.
historyEvent_mapIterationStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe MapIterationEventDetails)
historyEvent_mapIterationStartedEventDetails = Lens.lens (\HistoryEvent' {mapIterationStartedEventDetails} -> mapIterationStartedEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationStartedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that where the submit failed.
historyEvent_taskSubmitFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskSubmitFailedEventDetails)
historyEvent_taskSubmitFailedEventDetails = Lens.lens (\HistoryEvent' {taskSubmitFailedEventDetails} -> taskSubmitFailedEventDetails) (\s@HistoryEvent' {} a -> s {taskSubmitFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that was aborted.
historyEvent_mapIterationAbortedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe MapIterationEventDetails)
historyEvent_mapIterationAbortedEventDetails = Lens.lens (\HistoryEvent' {mapIterationAbortedEventDetails} -> mapIterationAbortedEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationAbortedEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that succeeded.
historyEvent_mapIterationSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe MapIterationEventDetails)
historyEvent_mapIterationSucceededEventDetails = Lens.lens (\HistoryEvent' {mapIterationSucceededEventDetails} -> mapIterationSucceededEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationSucceededEventDetails = a} :: HistoryEvent)

-- | Contains details about an iteration of a Map state that failed.
historyEvent_mapIterationFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe MapIterationEventDetails)
historyEvent_mapIterationFailedEventDetails = Lens.lens (\HistoryEvent' {mapIterationFailedEventDetails} -> mapIterationFailedEventDetails) (\s@HistoryEvent' {} a -> s {mapIterationFailedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ExecutionTimedOutEventDetails)
historyEvent_executionTimedOutEventDetails = Lens.lens (\HistoryEvent' {executionTimedOutEventDetails} -> executionTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {executionTimedOutEventDetails = a} :: HistoryEvent)

-- | The id of the previous event.
historyEvent_previousEventId :: Lens.Lens' HistoryEvent (Core.Maybe Core.Integer)
historyEvent_previousEventId = Lens.lens (\HistoryEvent' {previousEventId} -> previousEventId) (\s@HistoryEvent' {} a -> s {previousEventId = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ExecutionStartedEventDetails)
historyEvent_executionStartedEventDetails = Lens.lens (\HistoryEvent' {executionStartedEventDetails} -> executionStartedEventDetails) (\s@HistoryEvent' {} a -> s {executionStartedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionScheduleFailedEventDetails)
historyEvent_lambdaFunctionScheduleFailedEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionScheduleFailedEventDetails} -> lambdaFunctionScheduleFailedEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionScheduleFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about an activity schedule event that failed during an
-- execution.
historyEvent_activityScheduleFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ActivityScheduleFailedEventDetails)
historyEvent_activityScheduleFailedEventDetails = Lens.lens (\HistoryEvent' {activityScheduleFailedEventDetails} -> activityScheduleFailedEventDetails) (\s@HistoryEvent' {} a -> s {activityScheduleFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that was scheduled.
historyEvent_taskScheduledEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskScheduledEventDetails)
historyEvent_taskScheduledEventDetails = Lens.lens (\HistoryEvent' {taskScheduledEventDetails} -> taskScheduledEventDetails) (\s@HistoryEvent' {} a -> s {taskScheduledEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityScheduledEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ActivityScheduledEventDetails)
historyEvent_activityScheduledEventDetails = Lens.lens (\HistoryEvent' {activityScheduledEventDetails} -> activityScheduledEventDetails) (\s@HistoryEvent' {} a -> s {activityScheduledEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionScheduledEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionScheduledEventDetails)
historyEvent_lambdaFunctionScheduledEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionScheduledEventDetails} -> lambdaFunctionScheduledEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionScheduledEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ExecutionSucceededEventDetails)
historyEvent_executionSucceededEventDetails = Lens.lens (\HistoryEvent' {executionSucceededEventDetails} -> executionSucceededEventDetails) (\s@HistoryEvent' {} a -> s {executionSucceededEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_executionAbortedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ExecutionAbortedEventDetails)
historyEvent_executionAbortedEventDetails = Lens.lens (\HistoryEvent' {executionAbortedEventDetails} -> executionAbortedEventDetails) (\s@HistoryEvent' {} a -> s {executionAbortedEventDetails = a} :: HistoryEvent)

-- | Contains details about Map state that was started.
historyEvent_mapStateStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe MapStateStartedEventDetails)
historyEvent_mapStateStartedEventDetails = Lens.lens (\HistoryEvent' {mapStateStartedEventDetails} -> mapStateStartedEventDetails) (\s@HistoryEvent' {} a -> s {mapStateStartedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionTimedOutEventDetails)
historyEvent_lambdaFunctionTimedOutEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionTimedOutEventDetails} -> lambdaFunctionTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionTimedOutEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ActivityTimedOutEventDetails)
historyEvent_activityTimedOutEventDetails = Lens.lens (\HistoryEvent' {activityTimedOutEventDetails} -> activityTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {activityTimedOutEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that timed out.
historyEvent_taskTimedOutEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskTimedOutEventDetails)
historyEvent_taskTimedOutEventDetails = Lens.lens (\HistoryEvent' {taskTimedOutEventDetails} -> taskTimedOutEventDetails) (\s@HistoryEvent' {} a -> s {taskTimedOutEventDetails = a} :: HistoryEvent)

-- | Contains details about a lambda function that failed to start during an
-- execution.
historyEvent_lambdaFunctionStartFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionStartFailedEventDetails)
historyEvent_lambdaFunctionStartFailedEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionStartFailedEventDetails} -> lambdaFunctionStartFailedEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionStartFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that failed to start.
historyEvent_taskStartFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskStartFailedEventDetails)
historyEvent_taskStartFailedEventDetails = Lens.lens (\HistoryEvent' {taskStartFailedEventDetails} -> taskStartFailedEventDetails) (\s@HistoryEvent' {} a -> s {taskStartFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about the failure of a task.
historyEvent_taskFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskFailedEventDetails)
historyEvent_taskFailedEventDetails = Lens.lens (\HistoryEvent' {taskFailedEventDetails} -> taskFailedEventDetails) (\s@HistoryEvent' {} a -> s {taskFailedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that succeeded.
historyEvent_taskSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskSucceededEventDetails)
historyEvent_taskSucceededEventDetails = Lens.lens (\HistoryEvent' {taskSucceededEventDetails} -> taskSucceededEventDetails) (\s@HistoryEvent' {} a -> s {taskSucceededEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_stateExitedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe StateExitedEventDetails)
historyEvent_stateExitedEventDetails = Lens.lens (\HistoryEvent' {stateExitedEventDetails} -> stateExitedEventDetails) (\s@HistoryEvent' {} a -> s {stateExitedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_stateEnteredEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe StateEnteredEventDetails)
historyEvent_stateEnteredEventDetails = Lens.lens (\HistoryEvent' {stateEnteredEventDetails} -> stateEnteredEventDetails) (\s@HistoryEvent' {} a -> s {stateEnteredEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_lambdaFunctionFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionFailedEventDetails)
historyEvent_lambdaFunctionFailedEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionFailedEventDetails} -> lambdaFunctionFailedEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionFailedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityFailedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ActivityFailedEventDetails)
historyEvent_activityFailedEventDetails = Lens.lens (\HistoryEvent' {activityFailedEventDetails} -> activityFailedEventDetails) (\s@HistoryEvent' {} a -> s {activityFailedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activitySucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ActivitySucceededEventDetails)
historyEvent_activitySucceededEventDetails = Lens.lens (\HistoryEvent' {activitySucceededEventDetails} -> activitySucceededEventDetails) (\s@HistoryEvent' {} a -> s {activitySucceededEventDetails = a} :: HistoryEvent)

-- | Contains details about a lambda function that terminated successfully
-- during an execution.
historyEvent_lambdaFunctionSucceededEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe LambdaFunctionSucceededEventDetails)
historyEvent_lambdaFunctionSucceededEventDetails = Lens.lens (\HistoryEvent' {lambdaFunctionSucceededEventDetails} -> lambdaFunctionSucceededEventDetails) (\s@HistoryEvent' {} a -> s {lambdaFunctionSucceededEventDetails = a} :: HistoryEvent)

-- | Contains details about a submitted task.
historyEvent_taskSubmittedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskSubmittedEventDetails)
historyEvent_taskSubmittedEventDetails = Lens.lens (\HistoryEvent' {taskSubmittedEventDetails} -> taskSubmittedEventDetails) (\s@HistoryEvent' {} a -> s {taskSubmittedEventDetails = a} :: HistoryEvent)

-- | Undocumented member.
historyEvent_activityStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe ActivityStartedEventDetails)
historyEvent_activityStartedEventDetails = Lens.lens (\HistoryEvent' {activityStartedEventDetails} -> activityStartedEventDetails) (\s@HistoryEvent' {} a -> s {activityStartedEventDetails = a} :: HistoryEvent)

-- | Contains details about a task that was started.
historyEvent_taskStartedEventDetails :: Lens.Lens' HistoryEvent (Core.Maybe TaskStartedEventDetails)
historyEvent_taskStartedEventDetails = Lens.lens (\HistoryEvent' {taskStartedEventDetails} -> taskStartedEventDetails) (\s@HistoryEvent' {} a -> s {taskStartedEventDetails = a} :: HistoryEvent)

-- | The date and time the event occurred.
historyEvent_timestamp :: Lens.Lens' HistoryEvent Core.UTCTime
historyEvent_timestamp = Lens.lens (\HistoryEvent' {timestamp} -> timestamp) (\s@HistoryEvent' {} a -> s {timestamp = a} :: HistoryEvent) Core.. Core._Time

-- | The type of the event.
historyEvent_type :: Lens.Lens' HistoryEvent HistoryEventType
historyEvent_type = Lens.lens (\HistoryEvent' {type'} -> type') (\s@HistoryEvent' {} a -> s {type' = a} :: HistoryEvent)

-- | The id of the event. Events are numbered sequentially, starting at one.
historyEvent_id :: Lens.Lens' HistoryEvent Core.Integer
historyEvent_id = Lens.lens (\HistoryEvent' {id} -> id) (\s@HistoryEvent' {} a -> s {id = a} :: HistoryEvent)

instance Core.FromJSON HistoryEvent where
  parseJSON =
    Core.withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            Core.<$> (x Core..:? "executionFailedEventDetails")
            Core.<*> (x Core..:? "mapIterationStartedEventDetails")
            Core.<*> (x Core..:? "taskSubmitFailedEventDetails")
            Core.<*> (x Core..:? "mapIterationAbortedEventDetails")
            Core.<*> (x Core..:? "mapIterationSucceededEventDetails")
            Core.<*> (x Core..:? "mapIterationFailedEventDetails")
            Core.<*> (x Core..:? "executionTimedOutEventDetails")
            Core.<*> (x Core..:? "previousEventId")
            Core.<*> (x Core..:? "executionStartedEventDetails")
            Core.<*> ( x
                         Core..:? "lambdaFunctionScheduleFailedEventDetails"
                     )
            Core.<*> (x Core..:? "activityScheduleFailedEventDetails")
            Core.<*> (x Core..:? "taskScheduledEventDetails")
            Core.<*> (x Core..:? "activityScheduledEventDetails")
            Core.<*> (x Core..:? "lambdaFunctionScheduledEventDetails")
            Core.<*> (x Core..:? "executionSucceededEventDetails")
            Core.<*> (x Core..:? "executionAbortedEventDetails")
            Core.<*> (x Core..:? "mapStateStartedEventDetails")
            Core.<*> (x Core..:? "lambdaFunctionTimedOutEventDetails")
            Core.<*> (x Core..:? "activityTimedOutEventDetails")
            Core.<*> (x Core..:? "taskTimedOutEventDetails")
            Core.<*> (x Core..:? "lambdaFunctionStartFailedEventDetails")
            Core.<*> (x Core..:? "taskStartFailedEventDetails")
            Core.<*> (x Core..:? "taskFailedEventDetails")
            Core.<*> (x Core..:? "taskSucceededEventDetails")
            Core.<*> (x Core..:? "stateExitedEventDetails")
            Core.<*> (x Core..:? "stateEnteredEventDetails")
            Core.<*> (x Core..:? "lambdaFunctionFailedEventDetails")
            Core.<*> (x Core..:? "activityFailedEventDetails")
            Core.<*> (x Core..:? "activitySucceededEventDetails")
            Core.<*> (x Core..:? "lambdaFunctionSucceededEventDetails")
            Core.<*> (x Core..:? "taskSubmittedEventDetails")
            Core.<*> (x Core..:? "activityStartedEventDetails")
            Core.<*> (x Core..:? "taskStartedEventDetails")
            Core.<*> (x Core..: "timestamp")
            Core.<*> (x Core..: "type")
            Core.<*> (x Core..: "id")
      )

instance Core.Hashable HistoryEvent

instance Core.NFData HistoryEvent
