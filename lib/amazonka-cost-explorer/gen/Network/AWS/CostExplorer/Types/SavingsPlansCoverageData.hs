{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansCoverageData
  ( SavingsPlansCoverageData (..),

    -- * Smart constructor
    mkSavingsPlansCoverageData,

    -- * Lenses
    spcdOnDemandCost,
    spcdSpendCoveredBySavingsPlans,
    spcdCoveragePercentage,
    spcdTotalCost,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specific coverage percentage, On-Demand costs, and spend covered by Savings Plans, and total Savings Plans costs for an account.
--
-- /See:/ 'mkSavingsPlansCoverageData' smart constructor.
data SavingsPlansCoverageData = SavingsPlansCoverageData'
  { onDemandCost ::
      Lude.Maybe Lude.Text,
    spendCoveredBySavingsPlans ::
      Lude.Maybe Lude.Text,
    coveragePercentage ::
      Lude.Maybe Lude.Text,
    totalCost :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansCoverageData' with the minimum fields required to make a request.
--
-- * 'coveragePercentage' - The percentage of your existing Savings Plans covered usage, divided by all of your eligible Savings Plans usage in an account(or set of accounts).
-- * 'onDemandCost' - The cost of your AWS usage at the public On-Demand rate.
-- * 'spendCoveredBySavingsPlans' - The amount of your AWS usage that is covered by a Savings Plans.
-- * 'totalCost' - The total cost of your AWS usage, regardless of your purchase option.
mkSavingsPlansCoverageData ::
  SavingsPlansCoverageData
mkSavingsPlansCoverageData =
  SavingsPlansCoverageData'
    { onDemandCost = Lude.Nothing,
      spendCoveredBySavingsPlans = Lude.Nothing,
      coveragePercentage = Lude.Nothing,
      totalCost = Lude.Nothing
    }

-- | The cost of your AWS usage at the public On-Demand rate.
--
-- /Note:/ Consider using 'onDemandCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdOnDemandCost :: Lens.Lens' SavingsPlansCoverageData (Lude.Maybe Lude.Text)
spcdOnDemandCost = Lens.lens (onDemandCost :: SavingsPlansCoverageData -> Lude.Maybe Lude.Text) (\s a -> s {onDemandCost = a} :: SavingsPlansCoverageData)
{-# DEPRECATED spcdOnDemandCost "Use generic-lens or generic-optics with 'onDemandCost' instead." #-}

-- | The amount of your AWS usage that is covered by a Savings Plans.
--
-- /Note:/ Consider using 'spendCoveredBySavingsPlans' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdSpendCoveredBySavingsPlans :: Lens.Lens' SavingsPlansCoverageData (Lude.Maybe Lude.Text)
spcdSpendCoveredBySavingsPlans = Lens.lens (spendCoveredBySavingsPlans :: SavingsPlansCoverageData -> Lude.Maybe Lude.Text) (\s a -> s {spendCoveredBySavingsPlans = a} :: SavingsPlansCoverageData)
{-# DEPRECATED spcdSpendCoveredBySavingsPlans "Use generic-lens or generic-optics with 'spendCoveredBySavingsPlans' instead." #-}

-- | The percentage of your existing Savings Plans covered usage, divided by all of your eligible Savings Plans usage in an account(or set of accounts).
--
-- /Note:/ Consider using 'coveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdCoveragePercentage :: Lens.Lens' SavingsPlansCoverageData (Lude.Maybe Lude.Text)
spcdCoveragePercentage = Lens.lens (coveragePercentage :: SavingsPlansCoverageData -> Lude.Maybe Lude.Text) (\s a -> s {coveragePercentage = a} :: SavingsPlansCoverageData)
{-# DEPRECATED spcdCoveragePercentage "Use generic-lens or generic-optics with 'coveragePercentage' instead." #-}

-- | The total cost of your AWS usage, regardless of your purchase option.
--
-- /Note:/ Consider using 'totalCost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spcdTotalCost :: Lens.Lens' SavingsPlansCoverageData (Lude.Maybe Lude.Text)
spcdTotalCost = Lens.lens (totalCost :: SavingsPlansCoverageData -> Lude.Maybe Lude.Text) (\s a -> s {totalCost = a} :: SavingsPlansCoverageData)
{-# DEPRECATED spcdTotalCost "Use generic-lens or generic-optics with 'totalCost' instead." #-}

instance Lude.FromJSON SavingsPlansCoverageData where
  parseJSON =
    Lude.withObject
      "SavingsPlansCoverageData"
      ( \x ->
          SavingsPlansCoverageData'
            Lude.<$> (x Lude..:? "OnDemandCost")
            Lude.<*> (x Lude..:? "SpendCoveredBySavingsPlans")
            Lude.<*> (x Lude..:? "CoveragePercentage")
            Lude.<*> (x Lude..:? "TotalCost")
      )
