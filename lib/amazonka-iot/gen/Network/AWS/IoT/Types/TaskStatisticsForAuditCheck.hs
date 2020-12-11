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
    tsfacSkippedFindingsCount,
    tsfacTotalFindingsCount,
    tsfacFailedFindingsCount,
    tsfacSucceededFindingsCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides summary counts of how many tasks for findings are in a particular state. This information is included in the response from DescribeAuditMitigationActionsTask.
--
-- /See:/ 'mkTaskStatisticsForAuditCheck' smart constructor.
data TaskStatisticsForAuditCheck = TaskStatisticsForAuditCheck'
  { canceledFindingsCount ::
      Lude.Maybe Lude.Integer,
    skippedFindingsCount ::
      Lude.Maybe Lude.Integer,
    totalFindingsCount ::
      Lude.Maybe Lude.Integer,
    failedFindingsCount ::
      Lude.Maybe Lude.Integer,
    succeededFindingsCount ::
      Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskStatisticsForAuditCheck' with the minimum fields required to make a request.
--
-- * 'canceledFindingsCount' - The number of findings to which the mitigation action task was canceled when applied.
-- * 'failedFindingsCount' - The number of findings for which at least one of the actions failed when applied.
-- * 'skippedFindingsCount' - The number of findings skipped because of filter conditions provided in the parameters to the command.
-- * 'succeededFindingsCount' - The number of findings for which all mitigation actions succeeded when applied.
-- * 'totalFindingsCount' - The total number of findings to which a task is being applied.
mkTaskStatisticsForAuditCheck ::
  TaskStatisticsForAuditCheck
mkTaskStatisticsForAuditCheck =
  TaskStatisticsForAuditCheck'
    { canceledFindingsCount =
        Lude.Nothing,
      skippedFindingsCount = Lude.Nothing,
      totalFindingsCount = Lude.Nothing,
      failedFindingsCount = Lude.Nothing,
      succeededFindingsCount = Lude.Nothing
    }

-- | The number of findings to which the mitigation action task was canceled when applied.
--
-- /Note:/ Consider using 'canceledFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacCanceledFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Lude.Maybe Lude.Integer)
tsfacCanceledFindingsCount = Lens.lens (canceledFindingsCount :: TaskStatisticsForAuditCheck -> Lude.Maybe Lude.Integer) (\s a -> s {canceledFindingsCount = a} :: TaskStatisticsForAuditCheck)
{-# DEPRECATED tsfacCanceledFindingsCount "Use generic-lens or generic-optics with 'canceledFindingsCount' instead." #-}

-- | The number of findings skipped because of filter conditions provided in the parameters to the command.
--
-- /Note:/ Consider using 'skippedFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacSkippedFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Lude.Maybe Lude.Integer)
tsfacSkippedFindingsCount = Lens.lens (skippedFindingsCount :: TaskStatisticsForAuditCheck -> Lude.Maybe Lude.Integer) (\s a -> s {skippedFindingsCount = a} :: TaskStatisticsForAuditCheck)
{-# DEPRECATED tsfacSkippedFindingsCount "Use generic-lens or generic-optics with 'skippedFindingsCount' instead." #-}

-- | The total number of findings to which a task is being applied.
--
-- /Note:/ Consider using 'totalFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacTotalFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Lude.Maybe Lude.Integer)
tsfacTotalFindingsCount = Lens.lens (totalFindingsCount :: TaskStatisticsForAuditCheck -> Lude.Maybe Lude.Integer) (\s a -> s {totalFindingsCount = a} :: TaskStatisticsForAuditCheck)
{-# DEPRECATED tsfacTotalFindingsCount "Use generic-lens or generic-optics with 'totalFindingsCount' instead." #-}

-- | The number of findings for which at least one of the actions failed when applied.
--
-- /Note:/ Consider using 'failedFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacFailedFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Lude.Maybe Lude.Integer)
tsfacFailedFindingsCount = Lens.lens (failedFindingsCount :: TaskStatisticsForAuditCheck -> Lude.Maybe Lude.Integer) (\s a -> s {failedFindingsCount = a} :: TaskStatisticsForAuditCheck)
{-# DEPRECATED tsfacFailedFindingsCount "Use generic-lens or generic-optics with 'failedFindingsCount' instead." #-}

-- | The number of findings for which all mitigation actions succeeded when applied.
--
-- /Note:/ Consider using 'succeededFindingsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsfacSucceededFindingsCount :: Lens.Lens' TaskStatisticsForAuditCheck (Lude.Maybe Lude.Integer)
tsfacSucceededFindingsCount = Lens.lens (succeededFindingsCount :: TaskStatisticsForAuditCheck -> Lude.Maybe Lude.Integer) (\s a -> s {succeededFindingsCount = a} :: TaskStatisticsForAuditCheck)
{-# DEPRECATED tsfacSucceededFindingsCount "Use generic-lens or generic-optics with 'succeededFindingsCount' instead." #-}

instance Lude.FromJSON TaskStatisticsForAuditCheck where
  parseJSON =
    Lude.withObject
      "TaskStatisticsForAuditCheck"
      ( \x ->
          TaskStatisticsForAuditCheck'
            Lude.<$> (x Lude..:? "canceledFindingsCount")
            Lude.<*> (x Lude..:? "skippedFindingsCount")
            Lude.<*> (x Lude..:? "totalFindingsCount")
            Lude.<*> (x Lude..:? "failedFindingsCount")
            Lude.<*> (x Lude..:? "succeededFindingsCount")
      )
