{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelCountersForWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelCountersForWorkteam
  ( LabelCountersForWorkteam (..),

    -- * Smart constructor
    mkLabelCountersForWorkteam,

    -- * Lenses
    lcfwPendingHuman,
    lcfwTotal,
    lcfwHumanLabeled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides counts for human-labeled tasks in the labeling job.
--
-- /See:/ 'mkLabelCountersForWorkteam' smart constructor.
data LabelCountersForWorkteam = LabelCountersForWorkteam'
  { pendingHuman ::
      Lude.Maybe Lude.Natural,
    total :: Lude.Maybe Lude.Natural,
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

-- | Creates a value of 'LabelCountersForWorkteam' with the minimum fields required to make a request.
--
-- * 'humanLabeled' - The total number of data objects labeled by a human worker.
-- * 'pendingHuman' - The total number of data objects that need to be labeled by a human worker.
-- * 'total' - The total number of tasks in the labeling job.
mkLabelCountersForWorkteam ::
  LabelCountersForWorkteam
mkLabelCountersForWorkteam =
  LabelCountersForWorkteam'
    { pendingHuman = Lude.Nothing,
      total = Lude.Nothing,
      humanLabeled = Lude.Nothing
    }

-- | The total number of data objects that need to be labeled by a human worker.
--
-- /Note:/ Consider using 'pendingHuman' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfwPendingHuman :: Lens.Lens' LabelCountersForWorkteam (Lude.Maybe Lude.Natural)
lcfwPendingHuman = Lens.lens (pendingHuman :: LabelCountersForWorkteam -> Lude.Maybe Lude.Natural) (\s a -> s {pendingHuman = a} :: LabelCountersForWorkteam)
{-# DEPRECATED lcfwPendingHuman "Use generic-lens or generic-optics with 'pendingHuman' instead." #-}

-- | The total number of tasks in the labeling job.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfwTotal :: Lens.Lens' LabelCountersForWorkteam (Lude.Maybe Lude.Natural)
lcfwTotal = Lens.lens (total :: LabelCountersForWorkteam -> Lude.Maybe Lude.Natural) (\s a -> s {total = a} :: LabelCountersForWorkteam)
{-# DEPRECATED lcfwTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The total number of data objects labeled by a human worker.
--
-- /Note:/ Consider using 'humanLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfwHumanLabeled :: Lens.Lens' LabelCountersForWorkteam (Lude.Maybe Lude.Natural)
lcfwHumanLabeled = Lens.lens (humanLabeled :: LabelCountersForWorkteam -> Lude.Maybe Lude.Natural) (\s a -> s {humanLabeled = a} :: LabelCountersForWorkteam)
{-# DEPRECATED lcfwHumanLabeled "Use generic-lens or generic-optics with 'humanLabeled' instead." #-}

instance Lude.FromJSON LabelCountersForWorkteam where
  parseJSON =
    Lude.withObject
      "LabelCountersForWorkteam"
      ( \x ->
          LabelCountersForWorkteam'
            Lude.<$> (x Lude..:? "PendingHuman")
            Lude.<*> (x Lude..:? "Total")
            Lude.<*> (x Lude..:? "HumanLabeled")
      )
