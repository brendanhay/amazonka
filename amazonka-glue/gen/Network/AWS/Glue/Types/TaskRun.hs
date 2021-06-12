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
-- Module      : Network.AWS.Glue.Types.TaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TaskRun where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.TaskRunProperties
import Network.AWS.Glue.Types.TaskStatusType
import qualified Network.AWS.Lens as Lens

-- | The sampling parameters that are associated with the machine learning
-- transform.
--
-- /See:/ 'newTaskRun' smart constructor.
data TaskRun = TaskRun'
  { -- | The amount of time (in seconds) that the task run consumed resources.
    executionTime :: Core.Maybe Core.Int,
    -- | The current status of the requested task run.
    status :: Core.Maybe TaskStatusType,
    -- | The unique identifier for the transform.
    transformId :: Core.Maybe Core.Text,
    -- | The unique identifier for this task run.
    taskRunId :: Core.Maybe Core.Text,
    -- | The list of error strings associated with this task run.
    errorString :: Core.Maybe Core.Text,
    -- | The last point in time that the requested task run was updated.
    lastModifiedOn :: Core.Maybe Core.POSIX,
    -- | The names of the log group for secure logging, associated with this task
    -- run.
    logGroupName :: Core.Maybe Core.Text,
    -- | The last point in time that the requested task run was completed.
    completedOn :: Core.Maybe Core.POSIX,
    -- | Specifies configuration properties associated with this task run.
    properties :: Core.Maybe TaskRunProperties,
    -- | The date and time that this task run started.
    startedOn :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionTime', 'taskRun_executionTime' - The amount of time (in seconds) that the task run consumed resources.
--
-- 'status', 'taskRun_status' - The current status of the requested task run.
--
-- 'transformId', 'taskRun_transformId' - The unique identifier for the transform.
--
-- 'taskRunId', 'taskRun_taskRunId' - The unique identifier for this task run.
--
-- 'errorString', 'taskRun_errorString' - The list of error strings associated with this task run.
--
-- 'lastModifiedOn', 'taskRun_lastModifiedOn' - The last point in time that the requested task run was updated.
--
-- 'logGroupName', 'taskRun_logGroupName' - The names of the log group for secure logging, associated with this task
-- run.
--
-- 'completedOn', 'taskRun_completedOn' - The last point in time that the requested task run was completed.
--
-- 'properties', 'taskRun_properties' - Specifies configuration properties associated with this task run.
--
-- 'startedOn', 'taskRun_startedOn' - The date and time that this task run started.
newTaskRun ::
  TaskRun
newTaskRun =
  TaskRun'
    { executionTime = Core.Nothing,
      status = Core.Nothing,
      transformId = Core.Nothing,
      taskRunId = Core.Nothing,
      errorString = Core.Nothing,
      lastModifiedOn = Core.Nothing,
      logGroupName = Core.Nothing,
      completedOn = Core.Nothing,
      properties = Core.Nothing,
      startedOn = Core.Nothing
    }

-- | The amount of time (in seconds) that the task run consumed resources.
taskRun_executionTime :: Lens.Lens' TaskRun (Core.Maybe Core.Int)
taskRun_executionTime = Lens.lens (\TaskRun' {executionTime} -> executionTime) (\s@TaskRun' {} a -> s {executionTime = a} :: TaskRun)

-- | The current status of the requested task run.
taskRun_status :: Lens.Lens' TaskRun (Core.Maybe TaskStatusType)
taskRun_status = Lens.lens (\TaskRun' {status} -> status) (\s@TaskRun' {} a -> s {status = a} :: TaskRun)

-- | The unique identifier for the transform.
taskRun_transformId :: Lens.Lens' TaskRun (Core.Maybe Core.Text)
taskRun_transformId = Lens.lens (\TaskRun' {transformId} -> transformId) (\s@TaskRun' {} a -> s {transformId = a} :: TaskRun)

-- | The unique identifier for this task run.
taskRun_taskRunId :: Lens.Lens' TaskRun (Core.Maybe Core.Text)
taskRun_taskRunId = Lens.lens (\TaskRun' {taskRunId} -> taskRunId) (\s@TaskRun' {} a -> s {taskRunId = a} :: TaskRun)

-- | The list of error strings associated with this task run.
taskRun_errorString :: Lens.Lens' TaskRun (Core.Maybe Core.Text)
taskRun_errorString = Lens.lens (\TaskRun' {errorString} -> errorString) (\s@TaskRun' {} a -> s {errorString = a} :: TaskRun)

-- | The last point in time that the requested task run was updated.
taskRun_lastModifiedOn :: Lens.Lens' TaskRun (Core.Maybe Core.UTCTime)
taskRun_lastModifiedOn = Lens.lens (\TaskRun' {lastModifiedOn} -> lastModifiedOn) (\s@TaskRun' {} a -> s {lastModifiedOn = a} :: TaskRun) Core.. Lens.mapping Core._Time

-- | The names of the log group for secure logging, associated with this task
-- run.
taskRun_logGroupName :: Lens.Lens' TaskRun (Core.Maybe Core.Text)
taskRun_logGroupName = Lens.lens (\TaskRun' {logGroupName} -> logGroupName) (\s@TaskRun' {} a -> s {logGroupName = a} :: TaskRun)

-- | The last point in time that the requested task run was completed.
taskRun_completedOn :: Lens.Lens' TaskRun (Core.Maybe Core.UTCTime)
taskRun_completedOn = Lens.lens (\TaskRun' {completedOn} -> completedOn) (\s@TaskRun' {} a -> s {completedOn = a} :: TaskRun) Core.. Lens.mapping Core._Time

-- | Specifies configuration properties associated with this task run.
taskRun_properties :: Lens.Lens' TaskRun (Core.Maybe TaskRunProperties)
taskRun_properties = Lens.lens (\TaskRun' {properties} -> properties) (\s@TaskRun' {} a -> s {properties = a} :: TaskRun)

-- | The date and time that this task run started.
taskRun_startedOn :: Lens.Lens' TaskRun (Core.Maybe Core.UTCTime)
taskRun_startedOn = Lens.lens (\TaskRun' {startedOn} -> startedOn) (\s@TaskRun' {} a -> s {startedOn = a} :: TaskRun) Core.. Lens.mapping Core._Time

instance Core.FromJSON TaskRun where
  parseJSON =
    Core.withObject
      "TaskRun"
      ( \x ->
          TaskRun'
            Core.<$> (x Core..:? "ExecutionTime")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "TransformId")
            Core.<*> (x Core..:? "TaskRunId")
            Core.<*> (x Core..:? "ErrorString")
            Core.<*> (x Core..:? "LastModifiedOn")
            Core.<*> (x Core..:? "LogGroupName")
            Core.<*> (x Core..:? "CompletedOn")
            Core.<*> (x Core..:? "Properties")
            Core.<*> (x Core..:? "StartedOn")
      )

instance Core.Hashable TaskRun

instance Core.NFData TaskRun
