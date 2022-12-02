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
-- Module      : Amazonka.IoT.Types.DetectMitigationActionsTaskSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DetectMitigationActionsTaskSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.DetectMitigationActionsTaskStatistics
import Amazonka.IoT.Types.DetectMitigationActionsTaskStatus
import Amazonka.IoT.Types.DetectMitigationActionsTaskTarget
import Amazonka.IoT.Types.MitigationAction
import Amazonka.IoT.Types.ViolationEventOccurrenceRange
import qualified Amazonka.Prelude as Prelude

-- | The summary of the mitigation action tasks.
--
-- /See:/ 'newDetectMitigationActionsTaskSummary' smart constructor.
data DetectMitigationActionsTaskSummary = DetectMitigationActionsTaskSummary'
  { -- | Includes suppressed alerts.
    suppressedAlertsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | The date the task started.
    taskStartTime :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The status of the task.
    taskStatus :: Prelude.Maybe DetectMitigationActionsTaskStatus,
    -- | Specifies the ML Detect findings to which the mitigation actions are
    -- applied.
    target :: Prelude.Maybe DetectMitigationActionsTaskTarget,
    -- | The definition of the actions.
    actionsDefinition :: Prelude.Maybe [MitigationAction],
    -- | Includes only active violations.
    onlyActiveViolationsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the time period of which violation events occurred between.
    violationEventOccurrenceRange :: Prelude.Maybe ViolationEventOccurrenceRange,
    -- | The statistics of a mitigation action task.
    taskStatistics :: Prelude.Maybe DetectMitigationActionsTaskStatistics,
    -- | The date the task ended.
    taskEndTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectMitigationActionsTaskSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suppressedAlertsIncluded', 'detectMitigationActionsTaskSummary_suppressedAlertsIncluded' - Includes suppressed alerts.
--
-- 'taskStartTime', 'detectMitigationActionsTaskSummary_taskStartTime' - The date the task started.
--
-- 'taskId', 'detectMitigationActionsTaskSummary_taskId' - The unique identifier of the task.
--
-- 'taskStatus', 'detectMitigationActionsTaskSummary_taskStatus' - The status of the task.
--
-- 'target', 'detectMitigationActionsTaskSummary_target' - Specifies the ML Detect findings to which the mitigation actions are
-- applied.
--
-- 'actionsDefinition', 'detectMitigationActionsTaskSummary_actionsDefinition' - The definition of the actions.
--
-- 'onlyActiveViolationsIncluded', 'detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded' - Includes only active violations.
--
-- 'violationEventOccurrenceRange', 'detectMitigationActionsTaskSummary_violationEventOccurrenceRange' - Specifies the time period of which violation events occurred between.
--
-- 'taskStatistics', 'detectMitigationActionsTaskSummary_taskStatistics' - The statistics of a mitigation action task.
--
-- 'taskEndTime', 'detectMitigationActionsTaskSummary_taskEndTime' - The date the task ended.
newDetectMitigationActionsTaskSummary ::
  DetectMitigationActionsTaskSummary
newDetectMitigationActionsTaskSummary =
  DetectMitigationActionsTaskSummary'
    { suppressedAlertsIncluded =
        Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      taskId = Prelude.Nothing,
      taskStatus = Prelude.Nothing,
      target = Prelude.Nothing,
      actionsDefinition = Prelude.Nothing,
      onlyActiveViolationsIncluded =
        Prelude.Nothing,
      violationEventOccurrenceRange =
        Prelude.Nothing,
      taskStatistics = Prelude.Nothing,
      taskEndTime = Prelude.Nothing
    }

-- | Includes suppressed alerts.
detectMitigationActionsTaskSummary_suppressedAlertsIncluded :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.Bool)
detectMitigationActionsTaskSummary_suppressedAlertsIncluded = Lens.lens (\DetectMitigationActionsTaskSummary' {suppressedAlertsIncluded} -> suppressedAlertsIncluded) (\s@DetectMitigationActionsTaskSummary' {} a -> s {suppressedAlertsIncluded = a} :: DetectMitigationActionsTaskSummary)

-- | The date the task started.
detectMitigationActionsTaskSummary_taskStartTime :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionsTaskSummary_taskStartTime = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStartTime} -> taskStartTime) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStartTime = a} :: DetectMitigationActionsTaskSummary) Prelude.. Lens.mapping Data._Time

-- | The unique identifier of the task.
detectMitigationActionsTaskSummary_taskId :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.Text)
detectMitigationActionsTaskSummary_taskId = Lens.lens (\DetectMitigationActionsTaskSummary' {taskId} -> taskId) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskId = a} :: DetectMitigationActionsTaskSummary)

-- | The status of the task.
detectMitigationActionsTaskSummary_taskStatus :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe DetectMitigationActionsTaskStatus)
detectMitigationActionsTaskSummary_taskStatus = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStatus} -> taskStatus) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStatus = a} :: DetectMitigationActionsTaskSummary)

-- | Specifies the ML Detect findings to which the mitigation actions are
-- applied.
detectMitigationActionsTaskSummary_target :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe DetectMitigationActionsTaskTarget)
detectMitigationActionsTaskSummary_target = Lens.lens (\DetectMitigationActionsTaskSummary' {target} -> target) (\s@DetectMitigationActionsTaskSummary' {} a -> s {target = a} :: DetectMitigationActionsTaskSummary)

-- | The definition of the actions.
detectMitigationActionsTaskSummary_actionsDefinition :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe [MitigationAction])
detectMitigationActionsTaskSummary_actionsDefinition = Lens.lens (\DetectMitigationActionsTaskSummary' {actionsDefinition} -> actionsDefinition) (\s@DetectMitigationActionsTaskSummary' {} a -> s {actionsDefinition = a} :: DetectMitigationActionsTaskSummary) Prelude.. Lens.mapping Lens.coerced

-- | Includes only active violations.
detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.Bool)
detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded = Lens.lens (\DetectMitigationActionsTaskSummary' {onlyActiveViolationsIncluded} -> onlyActiveViolationsIncluded) (\s@DetectMitigationActionsTaskSummary' {} a -> s {onlyActiveViolationsIncluded = a} :: DetectMitigationActionsTaskSummary)

-- | Specifies the time period of which violation events occurred between.
detectMitigationActionsTaskSummary_violationEventOccurrenceRange :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe ViolationEventOccurrenceRange)
detectMitigationActionsTaskSummary_violationEventOccurrenceRange = Lens.lens (\DetectMitigationActionsTaskSummary' {violationEventOccurrenceRange} -> violationEventOccurrenceRange) (\s@DetectMitigationActionsTaskSummary' {} a -> s {violationEventOccurrenceRange = a} :: DetectMitigationActionsTaskSummary)

-- | The statistics of a mitigation action task.
detectMitigationActionsTaskSummary_taskStatistics :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe DetectMitigationActionsTaskStatistics)
detectMitigationActionsTaskSummary_taskStatistics = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStatistics} -> taskStatistics) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStatistics = a} :: DetectMitigationActionsTaskSummary)

-- | The date the task ended.
detectMitigationActionsTaskSummary_taskEndTime :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionsTaskSummary_taskEndTime = Lens.lens (\DetectMitigationActionsTaskSummary' {taskEndTime} -> taskEndTime) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskEndTime = a} :: DetectMitigationActionsTaskSummary) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    DetectMitigationActionsTaskSummary
  where
  parseJSON =
    Data.withObject
      "DetectMitigationActionsTaskSummary"
      ( \x ->
          DetectMitigationActionsTaskSummary'
            Prelude.<$> (x Data..:? "suppressedAlertsIncluded")
            Prelude.<*> (x Data..:? "taskStartTime")
            Prelude.<*> (x Data..:? "taskId")
            Prelude.<*> (x Data..:? "taskStatus")
            Prelude.<*> (x Data..:? "target")
            Prelude.<*> ( x Data..:? "actionsDefinition"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "onlyActiveViolationsIncluded")
            Prelude.<*> (x Data..:? "violationEventOccurrenceRange")
            Prelude.<*> (x Data..:? "taskStatistics")
            Prelude.<*> (x Data..:? "taskEndTime")
      )

instance
  Prelude.Hashable
    DetectMitigationActionsTaskSummary
  where
  hashWithSalt
    _salt
    DetectMitigationActionsTaskSummary' {..} =
      _salt
        `Prelude.hashWithSalt` suppressedAlertsIncluded
        `Prelude.hashWithSalt` taskStartTime
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` taskStatus
        `Prelude.hashWithSalt` target
        `Prelude.hashWithSalt` actionsDefinition
        `Prelude.hashWithSalt` onlyActiveViolationsIncluded
        `Prelude.hashWithSalt` violationEventOccurrenceRange
        `Prelude.hashWithSalt` taskStatistics
        `Prelude.hashWithSalt` taskEndTime

instance
  Prelude.NFData
    DetectMitigationActionsTaskSummary
  where
  rnf DetectMitigationActionsTaskSummary' {..} =
    Prelude.rnf suppressedAlertsIncluded
      `Prelude.seq` Prelude.rnf taskStartTime
      `Prelude.seq` Prelude.rnf taskId
      `Prelude.seq` Prelude.rnf taskStatus
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf actionsDefinition
      `Prelude.seq` Prelude.rnf onlyActiveViolationsIncluded
      `Prelude.seq` Prelude.rnf violationEventOccurrenceRange
      `Prelude.seq` Prelude.rnf taskStatistics
      `Prelude.seq` Prelude.rnf taskEndTime
