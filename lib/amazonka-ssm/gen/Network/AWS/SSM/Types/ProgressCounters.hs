{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ProgressCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ProgressCounters
  ( ProgressCounters (..),

    -- * Smart constructor
    mkProgressCounters,

    -- * Lenses
    pcFailedSteps,
    pcCancelledSteps,
    pcSuccessSteps,
    pcTotalSteps,
    pcTimedOutSteps,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An aggregate of step execution statuses displayed in the AWS Console for a multi-Region and multi-account Automation execution.
--
-- /See:/ 'mkProgressCounters' smart constructor.
data ProgressCounters = ProgressCounters'
  { -- | The total number of steps that failed to run in all specified AWS Regions and accounts for the current Automation execution.
    failedSteps :: Lude.Maybe Lude.Int,
    -- | The total number of steps that the system cancelled in all specified AWS Regions and accounts for the current Automation execution.
    cancelledSteps :: Lude.Maybe Lude.Int,
    -- | The total number of steps that successfully completed in all specified AWS Regions and accounts for the current Automation execution.
    successSteps :: Lude.Maybe Lude.Int,
    -- | The total number of steps run in all specified AWS Regions and accounts for the current Automation execution.
    totalSteps :: Lude.Maybe Lude.Int,
    -- | The total number of steps that timed out in all specified AWS Regions and accounts for the current Automation execution.
    timedOutSteps :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProgressCounters' with the minimum fields required to make a request.
--
-- * 'failedSteps' - The total number of steps that failed to run in all specified AWS Regions and accounts for the current Automation execution.
-- * 'cancelledSteps' - The total number of steps that the system cancelled in all specified AWS Regions and accounts for the current Automation execution.
-- * 'successSteps' - The total number of steps that successfully completed in all specified AWS Regions and accounts for the current Automation execution.
-- * 'totalSteps' - The total number of steps run in all specified AWS Regions and accounts for the current Automation execution.
-- * 'timedOutSteps' - The total number of steps that timed out in all specified AWS Regions and accounts for the current Automation execution.
mkProgressCounters ::
  ProgressCounters
mkProgressCounters =
  ProgressCounters'
    { failedSteps = Lude.Nothing,
      cancelledSteps = Lude.Nothing,
      successSteps = Lude.Nothing,
      totalSteps = Lude.Nothing,
      timedOutSteps = Lude.Nothing
    }

-- | The total number of steps that failed to run in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'failedSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcFailedSteps :: Lens.Lens' ProgressCounters (Lude.Maybe Lude.Int)
pcFailedSteps = Lens.lens (failedSteps :: ProgressCounters -> Lude.Maybe Lude.Int) (\s a -> s {failedSteps = a} :: ProgressCounters)
{-# DEPRECATED pcFailedSteps "Use generic-lens or generic-optics with 'failedSteps' instead." #-}

-- | The total number of steps that the system cancelled in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'cancelledSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcCancelledSteps :: Lens.Lens' ProgressCounters (Lude.Maybe Lude.Int)
pcCancelledSteps = Lens.lens (cancelledSteps :: ProgressCounters -> Lude.Maybe Lude.Int) (\s a -> s {cancelledSteps = a} :: ProgressCounters)
{-# DEPRECATED pcCancelledSteps "Use generic-lens or generic-optics with 'cancelledSteps' instead." #-}

-- | The total number of steps that successfully completed in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'successSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcSuccessSteps :: Lens.Lens' ProgressCounters (Lude.Maybe Lude.Int)
pcSuccessSteps = Lens.lens (successSteps :: ProgressCounters -> Lude.Maybe Lude.Int) (\s a -> s {successSteps = a} :: ProgressCounters)
{-# DEPRECATED pcSuccessSteps "Use generic-lens or generic-optics with 'successSteps' instead." #-}

-- | The total number of steps run in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'totalSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcTotalSteps :: Lens.Lens' ProgressCounters (Lude.Maybe Lude.Int)
pcTotalSteps = Lens.lens (totalSteps :: ProgressCounters -> Lude.Maybe Lude.Int) (\s a -> s {totalSteps = a} :: ProgressCounters)
{-# DEPRECATED pcTotalSteps "Use generic-lens or generic-optics with 'totalSteps' instead." #-}

-- | The total number of steps that timed out in all specified AWS Regions and accounts for the current Automation execution.
--
-- /Note:/ Consider using 'timedOutSteps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcTimedOutSteps :: Lens.Lens' ProgressCounters (Lude.Maybe Lude.Int)
pcTimedOutSteps = Lens.lens (timedOutSteps :: ProgressCounters -> Lude.Maybe Lude.Int) (\s a -> s {timedOutSteps = a} :: ProgressCounters)
{-# DEPRECATED pcTimedOutSteps "Use generic-lens or generic-optics with 'timedOutSteps' instead." #-}

instance Lude.FromJSON ProgressCounters where
  parseJSON =
    Lude.withObject
      "ProgressCounters"
      ( \x ->
          ProgressCounters'
            Lude.<$> (x Lude..:? "FailedSteps")
            Lude.<*> (x Lude..:? "CancelledSteps")
            Lude.<*> (x Lude..:? "SuccessSteps")
            Lude.<*> (x Lude..:? "TotalSteps")
            Lude.<*> (x Lude..:? "TimedOutSteps")
      )
