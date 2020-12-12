{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelCounters
  ( LabelCounters (..),

    -- * Smart constructor
    mkLabelCounters,

    -- * Lenses
    lcMachineLabeled,
    lcTotalLabeled,
    lcFailedNonRetryableError,
    lcUnlabeled,
    lcHumanLabeled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a breakdown of the number of objects labeled.
--
-- /See:/ 'mkLabelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { machineLabeled ::
      Lude.Maybe Lude.Natural,
    totalLabeled :: Lude.Maybe Lude.Natural,
    failedNonRetryableError :: Lude.Maybe Lude.Natural,
    unlabeled :: Lude.Maybe Lude.Natural,
    humanLabeled :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelCounters' with the minimum fields required to make a request.
--
-- * 'failedNonRetryableError' - The total number of objects that could not be labeled due to an error.
-- * 'humanLabeled' - The total number of objects labeled by a human worker.
-- * 'machineLabeled' - The total number of objects labeled by automated data labeling.
-- * 'totalLabeled' - The total number of objects labeled.
-- * 'unlabeled' - The total number of objects not yet labeled.
mkLabelCounters ::
  LabelCounters
mkLabelCounters =
  LabelCounters'
    { machineLabeled = Lude.Nothing,
      totalLabeled = Lude.Nothing,
      failedNonRetryableError = Lude.Nothing,
      unlabeled = Lude.Nothing,
      humanLabeled = Lude.Nothing
    }

-- | The total number of objects labeled by automated data labeling.
--
-- /Note:/ Consider using 'machineLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMachineLabeled :: Lens.Lens' LabelCounters (Lude.Maybe Lude.Natural)
lcMachineLabeled = Lens.lens (machineLabeled :: LabelCounters -> Lude.Maybe Lude.Natural) (\s a -> s {machineLabeled = a} :: LabelCounters)
{-# DEPRECATED lcMachineLabeled "Use generic-lens or generic-optics with 'machineLabeled' instead." #-}

-- | The total number of objects labeled.
--
-- /Note:/ Consider using 'totalLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcTotalLabeled :: Lens.Lens' LabelCounters (Lude.Maybe Lude.Natural)
lcTotalLabeled = Lens.lens (totalLabeled :: LabelCounters -> Lude.Maybe Lude.Natural) (\s a -> s {totalLabeled = a} :: LabelCounters)
{-# DEPRECATED lcTotalLabeled "Use generic-lens or generic-optics with 'totalLabeled' instead." #-}

-- | The total number of objects that could not be labeled due to an error.
--
-- /Note:/ Consider using 'failedNonRetryableError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcFailedNonRetryableError :: Lens.Lens' LabelCounters (Lude.Maybe Lude.Natural)
lcFailedNonRetryableError = Lens.lens (failedNonRetryableError :: LabelCounters -> Lude.Maybe Lude.Natural) (\s a -> s {failedNonRetryableError = a} :: LabelCounters)
{-# DEPRECATED lcFailedNonRetryableError "Use generic-lens or generic-optics with 'failedNonRetryableError' instead." #-}

-- | The total number of objects not yet labeled.
--
-- /Note:/ Consider using 'unlabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcUnlabeled :: Lens.Lens' LabelCounters (Lude.Maybe Lude.Natural)
lcUnlabeled = Lens.lens (unlabeled :: LabelCounters -> Lude.Maybe Lude.Natural) (\s a -> s {unlabeled = a} :: LabelCounters)
{-# DEPRECATED lcUnlabeled "Use generic-lens or generic-optics with 'unlabeled' instead." #-}

-- | The total number of objects labeled by a human worker.
--
-- /Note:/ Consider using 'humanLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcHumanLabeled :: Lens.Lens' LabelCounters (Lude.Maybe Lude.Natural)
lcHumanLabeled = Lens.lens (humanLabeled :: LabelCounters -> Lude.Maybe Lude.Natural) (\s a -> s {humanLabeled = a} :: LabelCounters)
{-# DEPRECATED lcHumanLabeled "Use generic-lens or generic-optics with 'humanLabeled' instead." #-}

instance Lude.FromJSON LabelCounters where
  parseJSON =
    Lude.withObject
      "LabelCounters"
      ( \x ->
          LabelCounters'
            Lude.<$> (x Lude..:? "MachineLabeled")
            Lude.<*> (x Lude..:? "TotalLabeled")
            Lude.<*> (x Lude..:? "FailedNonRetryableError")
            Lude.<*> (x Lude..:? "Unlabeled")
            Lude.<*> (x Lude..:? "HumanLabeled")
      )
