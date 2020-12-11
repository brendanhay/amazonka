-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TaskStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TaskStatistics
  ( TaskStatistics (..),

    -- * Smart constructor
    mkTaskStatistics,

    -- * Lenses
    tsNonCompliantChecks,
    tsWaitingForDataCollectionChecks,
    tsFailedChecks,
    tsTotalChecks,
    tsInProgressChecks,
    tsCompliantChecks,
    tsCanceledChecks,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Statistics for the checks performed during the audit.
--
-- /See:/ 'mkTaskStatistics' smart constructor.
data TaskStatistics = TaskStatistics'
  { nonCompliantChecks ::
      Lude.Maybe Lude.Int,
    waitingForDataCollectionChecks :: Lude.Maybe Lude.Int,
    failedChecks :: Lude.Maybe Lude.Int,
    totalChecks :: Lude.Maybe Lude.Int,
    inProgressChecks :: Lude.Maybe Lude.Int,
    compliantChecks :: Lude.Maybe Lude.Int,
    canceledChecks :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TaskStatistics' with the minimum fields required to make a request.
--
-- * 'canceledChecks' - The number of checks that did not run because the audit was canceled.
-- * 'compliantChecks' - The number of checks that found compliant resources.
-- * 'failedChecks' - The number of checks.
-- * 'inProgressChecks' - The number of checks in progress.
-- * 'nonCompliantChecks' - The number of checks that found noncompliant resources.
-- * 'totalChecks' - The number of checks in this audit.
-- * 'waitingForDataCollectionChecks' - The number of checks waiting for data collection.
mkTaskStatistics ::
  TaskStatistics
mkTaskStatistics =
  TaskStatistics'
    { nonCompliantChecks = Lude.Nothing,
      waitingForDataCollectionChecks = Lude.Nothing,
      failedChecks = Lude.Nothing,
      totalChecks = Lude.Nothing,
      inProgressChecks = Lude.Nothing,
      compliantChecks = Lude.Nothing,
      canceledChecks = Lude.Nothing
    }

-- | The number of checks that found noncompliant resources.
--
-- /Note:/ Consider using 'nonCompliantChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsNonCompliantChecks :: Lens.Lens' TaskStatistics (Lude.Maybe Lude.Int)
tsNonCompliantChecks = Lens.lens (nonCompliantChecks :: TaskStatistics -> Lude.Maybe Lude.Int) (\s a -> s {nonCompliantChecks = a} :: TaskStatistics)
{-# DEPRECATED tsNonCompliantChecks "Use generic-lens or generic-optics with 'nonCompliantChecks' instead." #-}

-- | The number of checks waiting for data collection.
--
-- /Note:/ Consider using 'waitingForDataCollectionChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsWaitingForDataCollectionChecks :: Lens.Lens' TaskStatistics (Lude.Maybe Lude.Int)
tsWaitingForDataCollectionChecks = Lens.lens (waitingForDataCollectionChecks :: TaskStatistics -> Lude.Maybe Lude.Int) (\s a -> s {waitingForDataCollectionChecks = a} :: TaskStatistics)
{-# DEPRECATED tsWaitingForDataCollectionChecks "Use generic-lens or generic-optics with 'waitingForDataCollectionChecks' instead." #-}

-- | The number of checks.
--
-- /Note:/ Consider using 'failedChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFailedChecks :: Lens.Lens' TaskStatistics (Lude.Maybe Lude.Int)
tsFailedChecks = Lens.lens (failedChecks :: TaskStatistics -> Lude.Maybe Lude.Int) (\s a -> s {failedChecks = a} :: TaskStatistics)
{-# DEPRECATED tsFailedChecks "Use generic-lens or generic-optics with 'failedChecks' instead." #-}

-- | The number of checks in this audit.
--
-- /Note:/ Consider using 'totalChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsTotalChecks :: Lens.Lens' TaskStatistics (Lude.Maybe Lude.Int)
tsTotalChecks = Lens.lens (totalChecks :: TaskStatistics -> Lude.Maybe Lude.Int) (\s a -> s {totalChecks = a} :: TaskStatistics)
{-# DEPRECATED tsTotalChecks "Use generic-lens or generic-optics with 'totalChecks' instead." #-}

-- | The number of checks in progress.
--
-- /Note:/ Consider using 'inProgressChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsInProgressChecks :: Lens.Lens' TaskStatistics (Lude.Maybe Lude.Int)
tsInProgressChecks = Lens.lens (inProgressChecks :: TaskStatistics -> Lude.Maybe Lude.Int) (\s a -> s {inProgressChecks = a} :: TaskStatistics)
{-# DEPRECATED tsInProgressChecks "Use generic-lens or generic-optics with 'inProgressChecks' instead." #-}

-- | The number of checks that found compliant resources.
--
-- /Note:/ Consider using 'compliantChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCompliantChecks :: Lens.Lens' TaskStatistics (Lude.Maybe Lude.Int)
tsCompliantChecks = Lens.lens (compliantChecks :: TaskStatistics -> Lude.Maybe Lude.Int) (\s a -> s {compliantChecks = a} :: TaskStatistics)
{-# DEPRECATED tsCompliantChecks "Use generic-lens or generic-optics with 'compliantChecks' instead." #-}

-- | The number of checks that did not run because the audit was canceled.
--
-- /Note:/ Consider using 'canceledChecks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsCanceledChecks :: Lens.Lens' TaskStatistics (Lude.Maybe Lude.Int)
tsCanceledChecks = Lens.lens (canceledChecks :: TaskStatistics -> Lude.Maybe Lude.Int) (\s a -> s {canceledChecks = a} :: TaskStatistics)
{-# DEPRECATED tsCanceledChecks "Use generic-lens or generic-optics with 'canceledChecks' instead." #-}

instance Lude.FromJSON TaskStatistics where
  parseJSON =
    Lude.withObject
      "TaskStatistics"
      ( \x ->
          TaskStatistics'
            Lude.<$> (x Lude..:? "nonCompliantChecks")
            Lude.<*> (x Lude..:? "waitingForDataCollectionChecks")
            Lude.<*> (x Lude..:? "failedChecks")
            Lude.<*> (x Lude..:? "totalChecks")
            Lude.<*> (x Lude..:? "inProgressChecks")
            Lude.<*> (x Lude..:? "compliantChecks")
            Lude.<*> (x Lude..:? "canceledChecks")
      )
