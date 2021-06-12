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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.DetectMitigationActionsTaskStatistics
import Network.AWS.IoT.Types.DetectMitigationActionsTaskStatus
import Network.AWS.IoT.Types.DetectMitigationActionsTaskTarget
import Network.AWS.IoT.Types.MitigationAction
import Network.AWS.IoT.Types.ViolationEventOccurrenceRange
import qualified Network.AWS.Lens as Lens

-- | The summary of the mitigation action tasks.
--
-- /See:/ 'newDetectMitigationActionsTaskSummary' smart constructor.
data DetectMitigationActionsTaskSummary = DetectMitigationActionsTaskSummary'
  { -- | The date the task ended.
    taskEndTime :: Core.Maybe Core.POSIX,
    -- | The statistics of a mitigation action task.
    taskStatistics :: Core.Maybe DetectMitigationActionsTaskStatistics,
    -- | The unique identifier of the task.
    taskId :: Core.Maybe Core.Text,
    -- | Specifies the time period of which violation events occurred between.
    violationEventOccurrenceRange :: Core.Maybe ViolationEventOccurrenceRange,
    -- | Includes only active violations.
    onlyActiveViolationsIncluded :: Core.Maybe Core.Bool,
    -- | Specifies the ML Detect findings to which the mitigation actions are
    -- applied.
    target :: Core.Maybe DetectMitigationActionsTaskTarget,
    -- | The status of the task.
    taskStatus :: Core.Maybe DetectMitigationActionsTaskStatus,
    -- | The definition of the actions.
    actionsDefinition :: Core.Maybe [MitigationAction],
    -- | The date the task started.
    taskStartTime :: Core.Maybe Core.POSIX,
    -- | Includes suppressed alerts.
    suppressedAlertsIncluded :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      taskStatistics = Core.Nothing,
      taskId = Core.Nothing,
      violationEventOccurrenceRange =
        Core.Nothing,
      onlyActiveViolationsIncluded =
        Core.Nothing,
      target = Core.Nothing,
      taskStatus = Core.Nothing,
      actionsDefinition = Core.Nothing,
      taskStartTime = Core.Nothing,
      suppressedAlertsIncluded = Core.Nothing
    }

-- | The date the task ended.
detectMitigationActionsTaskSummary_taskEndTime :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe Core.UTCTime)
detectMitigationActionsTaskSummary_taskEndTime = Lens.lens (\DetectMitigationActionsTaskSummary' {taskEndTime} -> taskEndTime) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskEndTime = a} :: DetectMitigationActionsTaskSummary) Core.. Lens.mapping Core._Time

-- | The statistics of a mitigation action task.
detectMitigationActionsTaskSummary_taskStatistics :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe DetectMitigationActionsTaskStatistics)
detectMitigationActionsTaskSummary_taskStatistics = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStatistics} -> taskStatistics) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStatistics = a} :: DetectMitigationActionsTaskSummary)

-- | The unique identifier of the task.
detectMitigationActionsTaskSummary_taskId :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe Core.Text)
detectMitigationActionsTaskSummary_taskId = Lens.lens (\DetectMitigationActionsTaskSummary' {taskId} -> taskId) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskId = a} :: DetectMitigationActionsTaskSummary)

-- | Specifies the time period of which violation events occurred between.
detectMitigationActionsTaskSummary_violationEventOccurrenceRange :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe ViolationEventOccurrenceRange)
detectMitigationActionsTaskSummary_violationEventOccurrenceRange = Lens.lens (\DetectMitigationActionsTaskSummary' {violationEventOccurrenceRange} -> violationEventOccurrenceRange) (\s@DetectMitigationActionsTaskSummary' {} a -> s {violationEventOccurrenceRange = a} :: DetectMitigationActionsTaskSummary)

-- | Includes only active violations.
detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe Core.Bool)
detectMitigationActionsTaskSummary_onlyActiveViolationsIncluded = Lens.lens (\DetectMitigationActionsTaskSummary' {onlyActiveViolationsIncluded} -> onlyActiveViolationsIncluded) (\s@DetectMitigationActionsTaskSummary' {} a -> s {onlyActiveViolationsIncluded = a} :: DetectMitigationActionsTaskSummary)

-- | Specifies the ML Detect findings to which the mitigation actions are
-- applied.
detectMitigationActionsTaskSummary_target :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe DetectMitigationActionsTaskTarget)
detectMitigationActionsTaskSummary_target = Lens.lens (\DetectMitigationActionsTaskSummary' {target} -> target) (\s@DetectMitigationActionsTaskSummary' {} a -> s {target = a} :: DetectMitigationActionsTaskSummary)

-- | The status of the task.
detectMitigationActionsTaskSummary_taskStatus :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe DetectMitigationActionsTaskStatus)
detectMitigationActionsTaskSummary_taskStatus = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStatus} -> taskStatus) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStatus = a} :: DetectMitigationActionsTaskSummary)

-- | The definition of the actions.
detectMitigationActionsTaskSummary_actionsDefinition :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe [MitigationAction])
detectMitigationActionsTaskSummary_actionsDefinition = Lens.lens (\DetectMitigationActionsTaskSummary' {actionsDefinition} -> actionsDefinition) (\s@DetectMitigationActionsTaskSummary' {} a -> s {actionsDefinition = a} :: DetectMitigationActionsTaskSummary) Core.. Lens.mapping Lens._Coerce

-- | The date the task started.
detectMitigationActionsTaskSummary_taskStartTime :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe Core.UTCTime)
detectMitigationActionsTaskSummary_taskStartTime = Lens.lens (\DetectMitigationActionsTaskSummary' {taskStartTime} -> taskStartTime) (\s@DetectMitigationActionsTaskSummary' {} a -> s {taskStartTime = a} :: DetectMitigationActionsTaskSummary) Core.. Lens.mapping Core._Time

-- | Includes suppressed alerts.
detectMitigationActionsTaskSummary_suppressedAlertsIncluded :: Lens.Lens' DetectMitigationActionsTaskSummary (Core.Maybe Core.Bool)
detectMitigationActionsTaskSummary_suppressedAlertsIncluded = Lens.lens (\DetectMitigationActionsTaskSummary' {suppressedAlertsIncluded} -> suppressedAlertsIncluded) (\s@DetectMitigationActionsTaskSummary' {} a -> s {suppressedAlertsIncluded = a} :: DetectMitigationActionsTaskSummary)

instance
  Core.FromJSON
    DetectMitigationActionsTaskSummary
  where
  parseJSON =
    Core.withObject
      "DetectMitigationActionsTaskSummary"
      ( \x ->
          DetectMitigationActionsTaskSummary'
            Core.<$> (x Core..:? "taskEndTime")
            Core.<*> (x Core..:? "taskStatistics")
            Core.<*> (x Core..:? "taskId")
            Core.<*> (x Core..:? "violationEventOccurrenceRange")
            Core.<*> (x Core..:? "onlyActiveViolationsIncluded")
            Core.<*> (x Core..:? "target")
            Core.<*> (x Core..:? "taskStatus")
            Core.<*> (x Core..:? "actionsDefinition" Core..!= Core.mempty)
            Core.<*> (x Core..:? "taskStartTime")
            Core.<*> (x Core..:? "suppressedAlertsIncluded")
      )

instance
  Core.Hashable
    DetectMitigationActionsTaskSummary

instance
  Core.NFData
    DetectMitigationActionsTaskSummary
