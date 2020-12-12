{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilization
  ( SavingsPlansUtilization (..),

    -- * Smart constructor
    mkSavingsPlansUtilization,

    -- * Lenses
    spuUnusedCommitment,
    spuUtilizationPercentage,
    spuTotalCommitment,
    spuUsedCommitment,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The measurement of how well you are using your existing Savings Plans.
--
-- /See:/ 'mkSavingsPlansUtilization' smart constructor.
data SavingsPlansUtilization = SavingsPlansUtilization'
  { unusedCommitment ::
      Lude.Maybe Lude.Text,
    utilizationPercentage ::
      Lude.Maybe Lude.Text,
    totalCommitment :: Lude.Maybe Lude.Text,
    usedCommitment :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansUtilization' with the minimum fields required to make a request.
--
-- * 'totalCommitment' - The total amount of Savings Plans commitment that's been purchased in an account (or set of accounts).
-- * 'unusedCommitment' - The amount of your Savings Plans commitment that was not consumed from Savings Plans eligible usage in a specific period.
-- * 'usedCommitment' - The amount of your Savings Plans commitment that was consumed from Savings Plans eligible usage in a specific period.
-- * 'utilizationPercentage' - The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your Savings Plans.
mkSavingsPlansUtilization ::
  SavingsPlansUtilization
mkSavingsPlansUtilization =
  SavingsPlansUtilization'
    { unusedCommitment = Lude.Nothing,
      utilizationPercentage = Lude.Nothing,
      totalCommitment = Lude.Nothing,
      usedCommitment = Lude.Nothing
    }

-- | The amount of your Savings Plans commitment that was not consumed from Savings Plans eligible usage in a specific period.
--
-- /Note:/ Consider using 'unusedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuUnusedCommitment :: Lens.Lens' SavingsPlansUtilization (Lude.Maybe Lude.Text)
spuUnusedCommitment = Lens.lens (unusedCommitment :: SavingsPlansUtilization -> Lude.Maybe Lude.Text) (\s a -> s {unusedCommitment = a} :: SavingsPlansUtilization)
{-# DEPRECATED spuUnusedCommitment "Use generic-lens or generic-optics with 'unusedCommitment' instead." #-}

-- | The amount of @UsedCommitment@ divided by the @TotalCommitment@ for your Savings Plans.
--
-- /Note:/ Consider using 'utilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuUtilizationPercentage :: Lens.Lens' SavingsPlansUtilization (Lude.Maybe Lude.Text)
spuUtilizationPercentage = Lens.lens (utilizationPercentage :: SavingsPlansUtilization -> Lude.Maybe Lude.Text) (\s a -> s {utilizationPercentage = a} :: SavingsPlansUtilization)
{-# DEPRECATED spuUtilizationPercentage "Use generic-lens or generic-optics with 'utilizationPercentage' instead." #-}

-- | The total amount of Savings Plans commitment that's been purchased in an account (or set of accounts).
--
-- /Note:/ Consider using 'totalCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuTotalCommitment :: Lens.Lens' SavingsPlansUtilization (Lude.Maybe Lude.Text)
spuTotalCommitment = Lens.lens (totalCommitment :: SavingsPlansUtilization -> Lude.Maybe Lude.Text) (\s a -> s {totalCommitment = a} :: SavingsPlansUtilization)
{-# DEPRECATED spuTotalCommitment "Use generic-lens or generic-optics with 'totalCommitment' instead." #-}

-- | The amount of your Savings Plans commitment that was consumed from Savings Plans eligible usage in a specific period.
--
-- /Note:/ Consider using 'usedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuUsedCommitment :: Lens.Lens' SavingsPlansUtilization (Lude.Maybe Lude.Text)
spuUsedCommitment = Lens.lens (usedCommitment :: SavingsPlansUtilization -> Lude.Maybe Lude.Text) (\s a -> s {usedCommitment = a} :: SavingsPlansUtilization)
{-# DEPRECATED spuUsedCommitment "Use generic-lens or generic-optics with 'usedCommitment' instead." #-}

instance Lude.FromJSON SavingsPlansUtilization where
  parseJSON =
    Lude.withObject
      "SavingsPlansUtilization"
      ( \x ->
          SavingsPlansUtilization'
            Lude.<$> (x Lude..:? "UnusedCommitment")
            Lude.<*> (x Lude..:? "UtilizationPercentage")
            Lude.<*> (x Lude..:? "TotalCommitment")
            Lude.<*> (x Lude..:? "UsedCommitment")
      )
