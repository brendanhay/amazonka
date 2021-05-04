{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Glue.Types.TaskRunProperties
import Network.AWS.Glue.Types.TaskStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The sampling parameters that are associated with the machine learning
-- transform.
--
-- /See:/ 'newTaskRun' smart constructor.
data TaskRun = TaskRun'
  { -- | The amount of time (in seconds) that the task run consumed resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | The current status of the requested task run.
    status :: Prelude.Maybe TaskStatusType,
    -- | The unique identifier for the transform.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for this task run.
    taskRunId :: Prelude.Maybe Prelude.Text,
    -- | The list of error strings associated with this task run.
    errorString :: Prelude.Maybe Prelude.Text,
    -- | The last point in time that the requested task run was updated.
    lastModifiedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The names of the log group for secure logging, associated with this task
    -- run.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The last point in time that the requested task run was completed.
    completedOn :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies configuration properties associated with this task run.
    properties :: Prelude.Maybe TaskRunProperties,
    -- | The date and time that this task run started.
    startedOn :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { executionTime = Prelude.Nothing,
      status = Prelude.Nothing,
      transformId = Prelude.Nothing,
      taskRunId = Prelude.Nothing,
      errorString = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      properties = Prelude.Nothing,
      startedOn = Prelude.Nothing
    }

-- | The amount of time (in seconds) that the task run consumed resources.
taskRun_executionTime :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Int)
taskRun_executionTime = Lens.lens (\TaskRun' {executionTime} -> executionTime) (\s@TaskRun' {} a -> s {executionTime = a} :: TaskRun)

-- | The current status of the requested task run.
taskRun_status :: Lens.Lens' TaskRun (Prelude.Maybe TaskStatusType)
taskRun_status = Lens.lens (\TaskRun' {status} -> status) (\s@TaskRun' {} a -> s {status = a} :: TaskRun)

-- | The unique identifier for the transform.
taskRun_transformId :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_transformId = Lens.lens (\TaskRun' {transformId} -> transformId) (\s@TaskRun' {} a -> s {transformId = a} :: TaskRun)

-- | The unique identifier for this task run.
taskRun_taskRunId :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_taskRunId = Lens.lens (\TaskRun' {taskRunId} -> taskRunId) (\s@TaskRun' {} a -> s {taskRunId = a} :: TaskRun)

-- | The list of error strings associated with this task run.
taskRun_errorString :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_errorString = Lens.lens (\TaskRun' {errorString} -> errorString) (\s@TaskRun' {} a -> s {errorString = a} :: TaskRun)

-- | The last point in time that the requested task run was updated.
taskRun_lastModifiedOn :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.UTCTime)
taskRun_lastModifiedOn = Lens.lens (\TaskRun' {lastModifiedOn} -> lastModifiedOn) (\s@TaskRun' {} a -> s {lastModifiedOn = a} :: TaskRun) Prelude.. Lens.mapping Prelude._Time

-- | The names of the log group for secure logging, associated with this task
-- run.
taskRun_logGroupName :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_logGroupName = Lens.lens (\TaskRun' {logGroupName} -> logGroupName) (\s@TaskRun' {} a -> s {logGroupName = a} :: TaskRun)

-- | The last point in time that the requested task run was completed.
taskRun_completedOn :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.UTCTime)
taskRun_completedOn = Lens.lens (\TaskRun' {completedOn} -> completedOn) (\s@TaskRun' {} a -> s {completedOn = a} :: TaskRun) Prelude.. Lens.mapping Prelude._Time

-- | Specifies configuration properties associated with this task run.
taskRun_properties :: Lens.Lens' TaskRun (Prelude.Maybe TaskRunProperties)
taskRun_properties = Lens.lens (\TaskRun' {properties} -> properties) (\s@TaskRun' {} a -> s {properties = a} :: TaskRun)

-- | The date and time that this task run started.
taskRun_startedOn :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.UTCTime)
taskRun_startedOn = Lens.lens (\TaskRun' {startedOn} -> startedOn) (\s@TaskRun' {} a -> s {startedOn = a} :: TaskRun) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TaskRun where
  parseJSON =
    Prelude.withObject
      "TaskRun"
      ( \x ->
          TaskRun'
            Prelude.<$> (x Prelude..:? "ExecutionTime")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "TransformId")
            Prelude.<*> (x Prelude..:? "TaskRunId")
            Prelude.<*> (x Prelude..:? "ErrorString")
            Prelude.<*> (x Prelude..:? "LastModifiedOn")
            Prelude.<*> (x Prelude..:? "LogGroupName")
            Prelude.<*> (x Prelude..:? "CompletedOn")
            Prelude.<*> (x Prelude..:? "Properties")
            Prelude.<*> (x Prelude..:? "StartedOn")
      )

instance Prelude.Hashable TaskRun

instance Prelude.NFData TaskRun
