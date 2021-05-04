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
-- Module      : Network.AWS.IoT.Types.DetectMitigationActionsTaskSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DetectMitigationActionsTaskSummary where

import Network.AWS.IoT.Types.DetectMitigationActionsTaskStatistics
import Network.AWS.IoT.Types.DetectMitigationActionsTaskStatus
import Network.AWS.IoT.Types.DetectMitigationActionsTaskTarget
import Network.AWS.IoT.Types.MitigationAction
import Network.AWS.IoT.Types.ViolationEventOccurrenceRange
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary of the mitigation action tasks.
--
-- /See:/ 'newDetectMitigationActionsTaskSummary' smart constructor.
data DetectMitigationActionsTaskSummary = DetectMitigationActionsTaskSummary'
  { -- | The date the task ended.
    taskEndTime :: Prelude.Maybe Prelude.POSIX,
    -- | The statistics of a mitigation action task.
    taskStatistics :: Prelude.Maybe DetectMitigationActionsTaskStatistics,
    -- | The unique identifier of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time period of which violation events occurred between.
    violationEventOccurrenceRange :: Prelude.Maybe ViolationEventOccurrenceRange,
    -- | Includes only active violations.
    onlyActiveViolationsIncluded :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the ML Detect findings to which the mitigation actions are
    -- applied.
    target :: Prelude.Maybe DetectMitigationActionsTaskTarget,
    -- | The status of the task.
    taskStatus :: Prelude.Maybe DetectMitigationActionsTaskStatus,
    -- | The definition of the actions.
    actionsDefinition :: Prelude.Maybe [MitigationAction],
    -- | The date the task started.
    taskStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | Includes suppressed alerts.
    suppressedAlertsIncluded :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetectMitigationActionsTaskSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskEndTime', 'detectMitigationActionsTaskSummary_taskEndTime' - The date the task ended.
--
-- 'taskStatistics', 'detectMitigationActionsTaskSummary_taskStatistics' - The statistics of a mitigation action task.
--
-- 'taskId', 'detectMitigationActionsTaskSummary_taskId' - The unique identifier of the task.
--
-- 'violationEventOccurrenceRange', 'detectMitigationActionsTaskSummary_violationEventOccurrenceRange' - Specifies the time period of which violation events occurred between.
--
-- 'onlyActiveViolationsIncluded', 'detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded' - Includes only active violations.
--
-- 'target', 'detectMitigationActionsTaskSummary_target' - Specifies the ML Detect findings to which the mitigation actions are
-- applied.
--
-- 'taskStatus', 'detectMitigationActionsTaskSummary_taskStatus' - The status of the task.
--
-- 'actionsDefinition', 'detectMitigationActionsTaskSummary_actionsDefinition' - The definition of the actions.
--
-- 'taskStartTime', 'detectMitigationActionsTaskSummary_taskStartTime' - The date the task started.
--
-- 'suppressedAlertsIncluded', 'detectMitigationActionsTaskSummary_suppressedAlertsIncluded' - Includes suppressed alerts.
newDetectMitigationActionsTaskSummary ::
  DetectMitigationActionsTaskSummary
newDetectMitigationActionsTaskSummary =
  DetectMitigationActionsTaskSummary'
    { taskEndTime =
        Prelude.Nothing,
      taskStatistics = Prelude.Nothing,
      taskId = Prelude.Nothing,
      violationEventOccurrenceRange =
        Prelude.Nothing,
      onlyActiveViolationsIncluded =
        Prelude.Nothing,
      target = Prelude.Nothing,
      taskStatus = Prelude.Nothing,
      actionsDefinition = Prelude.Nothing,
      taskStartTime = Prelude.Nothing,
      suppressedAlertsIncluded =
        Prelude.Nothing
    }

-- | The date the task ended.
detectMitigationActionsTaskSummary_taskEndTime :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionsTaskSummary_taskEndTime = Lens.lens (\DetectMitigationActionsTaskSummary' {taskEndTime} -> taskEndTime) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskEndTime = a} :: DetectMitigationActionsTaskSummary) Prelude.. Lens.mapping Prelude._Time

-- | The statistics of a mitigation action task.
detectMitigationActionsTaskSummary_taskStatistics :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe DetectMitigationActionsTaskStatistics)
detectMitigationActionsTaskSummary_taskStatistics = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStatistics} -> taskStatistics) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStatistics = a} :: DetectMitigationActionsTaskSummary)

-- | The unique identifier of the task.
detectMitigationActionsTaskSummary_taskId :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.Text)
detectMitigationActionsTaskSummary_taskId = Lens.lens (\DetectMitigationActionsTaskSummary' {taskId} -> taskId) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskId = a} :: DetectMitigationActionsTaskSummary)

-- | Specifies the time period of which violation events occurred between.
detectMitigationActionsTaskSummary_violationEventOccurrenceRange :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe ViolationEventOccurrenceRange)
detectMitigationActionsTaskSummary_violationEventOccurrenceRange = Lens.lens (\DetectMitigationActionsTaskSummary' {violationEventOccurrenceRange} -> violationEventOccurrenceRange) (\s@DetectMitigationActionsTaskSummary' {} a -> s {violationEventOccurrenceRange = a} :: DetectMitigationActionsTaskSummary)

-- | Includes only active violations.
detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.Bool)
detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded = Lens.lens (\DetectMitigationActionsTaskSummary' {onlyActiveViolationsIncluded} -> onlyActiveViolationsIncluded) (\s@DetectMitigationActionsTaskSummary' {} a -> s {onlyActiveViolationsIncluded = a} :: DetectMitigationActionsTaskSummary)

-- | Specifies the ML Detect findings to which the mitigation actions are
-- applied.
detectMitigationActionsTaskSummary_target :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe DetectMitigationActionsTaskTarget)
detectMitigationActionsTaskSummary_target = Lens.lens (\DetectMitigationActionsTaskSummary' {target} -> target) (\s@DetectMitigationActionsTaskSummary' {} a -> s {target = a} :: DetectMitigationActionsTaskSummary)

-- | The status of the task.
detectMitigationActionsTaskSummary_taskStatus :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe DetectMitigationActionsTaskStatus)
detectMitigationActionsTaskSummary_taskStatus = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStatus} -> taskStatus) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStatus = a} :: DetectMitigationActionsTaskSummary)

-- | The definition of the actions.
detectMitigationActionsTaskSummary_actionsDefinition :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe [MitigationAction])
detectMitigationActionsTaskSummary_actionsDefinition = Lens.lens (\DetectMitigationActionsTaskSummary' {actionsDefinition} -> actionsDefinition) (\s@DetectMitigationActionsTaskSummary' {} a -> s {actionsDefinition = a} :: DetectMitigationActionsTaskSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The date the task started.
detectMitigationActionsTaskSummary_taskStartTime :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionsTaskSummary_taskStartTime = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStartTime} -> taskStartTime) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStartTime = a} :: DetectMitigationActionsTaskSummary) Prelude.. Lens.mapping Prelude._Time

-- | Includes suppressed alerts.
detectMitigationActionsTaskSummary_suppressedAlertsIncluded :: Lens.Lens' DetectMitigationActionsTaskSummary (Prelude.Maybe Prelude.Bool)
detectMitigationActionsTaskSummary_suppressedAlertsIncluded = Lens.lens (\DetectMitigationActionsTaskSummary' {suppressedAlertsIncluded} -> suppressedAlertsIncluded) (\s@DetectMitigationActionsTaskSummary' {} a -> s {suppressedAlertsIncluded = a} :: DetectMitigationActionsTaskSummary)

instance
  Prelude.FromJSON
    DetectMitigationActionsTaskSummary
  where
  parseJSON =
    Prelude.withObject
      "DetectMitigationActionsTaskSummary"
      ( \x ->
          DetectMitigationActionsTaskSummary'
            Prelude.<$> (x Prelude..:? "taskEndTime")
            Prelude.<*> (x Prelude..:? "taskStatistics")
            Prelude.<*> (x Prelude..:? "taskId")
            Prelude.<*> (x Prelude..:? "violationEventOccurrenceRange")
            Prelude.<*> (x Prelude..:? "onlyActiveViolationsIncluded")
            Prelude.<*> (x Prelude..:? "target")
            Prelude.<*> (x Prelude..:? "taskStatus")
            Prelude.<*> ( x Prelude..:? "actionsDefinition"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "taskStartTime")
            Prelude.<*> (x Prelude..:? "suppressedAlertsIncluded")
      )

instance
  Prelude.Hashable
    DetectMitigationActionsTaskSummary

instance
  Prelude.NFData
    DetectMitigationActionsTaskSummary
