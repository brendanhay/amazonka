{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatisticsForAuditCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatisticsForAuditCheck
  ( TaskStatisticsForAuditCheck (..),

    -- * Smart constructor
    mkTaskStatisticsForAuditCheck,

    -- * Lenses
    tsfacCanceledFindingsCount,
    tsfacFailedFindingsCount,
    tsfacSkippedFindingsCount,
    tsfacSucceededFindingsCount,
    tsfacTotalFindingsCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides summary counts of how many tasks for findings are in a particular state. This information is included in the response from DescribeAuditMitigationActionsTask.
--
-- /See:/ 'mkTaskStatisticsForAuditCheck' smart constructor.
data TaskStatisticsForAuditCheck = TaskStatisticsForAuditCheck'
  { -- | The number of findings to which the mitigation action task was canceled when applied.
    canceledFindingsCount :: Core.Maybe Core.Integer,
    -- | The number of findings for which at least one of the actions failed when applied.
    failedFindingsCount :: Core.Maybe Core.Integer,
    -- | The number of findings skipped because of filter conditions provided in the parameters to the command.
    skippedFindingsCount :: Core.Maybe Core.Integer,
    -- | The number of findings for which all mitigation actions succeeded when applied.
    succeededFindingsCount :: Core.Maybe Core.Integer,
    -- | The total number of findings to which a task is being applied.
    totalFindingsCount :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TaskStatisticsForAuditCheck' value with any optional fields omitted.
mkTaskStatisticsForAuditCheck ::
  TaskStatisticsForAuditCheck
mkTaskStatisticsForAuditCheck =
  TaskStatisticsForAuditCheck'
    { canceledFindingsCount =
        Core.Nothing,
      failedFindingsCount = Core.Nothing,
      skippedFindingsCount = Core.Nothing,
      succeededFindingsCount = Core.Nothing,
      totalFindingsCount = Core.Nothing
    }

-- | The number of findings to which the mitigation action task was canceled when applied.
--
-- /Note:/ Consider using 'canceledFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacCanceledFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Core.Maybe Core.Integer)
tsfacCanceledFindingsCount = Lens.field @"canceledFindingsCount"
{-# DEPRECATED tsfacCanceledFindingsCount "Use generic-lens or generic-optics with 'canceledFindingsCount' instead." #-}

-- | The number of findings for which at least one of the actions failed when applied.
--
-- /Note:/ Consider using 'failedFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacFailedFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Core.Maybe Core.Integer)
tsfacFailedFindingsCount = Lens.field @"failedFindingsCount"
{-# DEPRECATED tsfacFailedFindingsCount "Use generic-lens or generic-optics with 'failedFindingsCount' instead." #-}

-- | The number of findings skipped because of filter conditions provided in the parameters to the command.
--
-- /Note:/ Consider using 'skippedFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacSkippedFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Core.Maybe Core.Integer)
tsfacSkippedFindingsCount = Lens.field @"skippedFindingsCount"
{-# DEPRECATED tsfacSkippedFindingsCount "Use generic-lens or generic-optics with 'skippedFindingsCount' instead." #-}

-- | The number of findings for which all mitigation actions succeeded when applied.
--
-- /Note:/ Consider using 'succeededFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacSucceededFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Core.Maybe Core.Integer)
tsfacSucceededFindingsCount = Lens.field @"succeededFindingsCount"
{-# DEPRECATED tsfacSucceededFindingsCount "Use generic-lens or generic-optics with 'succeededFindingsCount' instead." #-}

-- | The total number of findings to which a task is being applied.
--
-- /Note:/ Consider using 'totalFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacTotalFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Core.Maybe Core.Integer)
tsfacTotalFindingsCount = Lens.field @"totalFindingsCount"
{-# DEPRECATED tsfacTotalFindingsCount "Use generic-lens or generic-optics with 'totalFindingsCount' instead." #-}

instance Core.FromJSON TaskStatisticsForAuditCheck where
  parseJSON =
    Core.withObject "TaskStatisticsForAuditCheck" Core.$
      \x ->
        TaskStatisticsForAuditCheck'
          Core.<$> (x Core..:? "canceledFindingsCount")
          Core.<*> (x Core..:? "failedFindingsCount")
          Core.<*> (x Core..:? "skippedFindingsCount")
          Core.<*> (x Core..:? "succeededFindingsCount")
          Core.<*> (x Core..:? "totalFindingsCount")
