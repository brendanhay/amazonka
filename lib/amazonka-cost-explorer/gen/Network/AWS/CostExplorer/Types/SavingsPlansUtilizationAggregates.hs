{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationAggregates
  ( SavingsPlansUtilizationAggregates (..),

    -- * Smart constructor
    mkSavingsPlansUtilizationAggregates,

    -- * Lenses
    spuaAmortizedCommitment,
    spuaSavings,
    spuaUtilization,
  )
where

import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The aggregated utilization metrics for your Savings Plans usage.
--
-- /See:/ 'mkSavingsPlansUtilizationAggregates' smart constructor.
data SavingsPlansUtilizationAggregates = SavingsPlansUtilizationAggregates'
  { amortizedCommitment ::
      Lude.Maybe
        SavingsPlansAmortizedCommitment,
    savings ::
      Lude.Maybe
        SavingsPlansSavings,
    utilization ::
      SavingsPlansUtilization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansUtilizationAggregates' with the minimum fields required to make a request.
--
-- * 'amortizedCommitment' - The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
-- * 'savings' - The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans, as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
-- * 'utilization' - A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
mkSavingsPlansUtilizationAggregates ::
  -- | 'utilization'
  SavingsPlansUtilization ->
  SavingsPlansUtilizationAggregates
mkSavingsPlansUtilizationAggregates pUtilization_ =
  SavingsPlansUtilizationAggregates'
    { amortizedCommitment =
        Lude.Nothing,
      savings = Lude.Nothing,
      utilization = pUtilization_
    }

-- | The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
--
-- /Note:/ Consider using 'amortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuaAmortizedCommitment :: Lens.Lens' SavingsPlansUtilizationAggregates (Lude.Maybe SavingsPlansAmortizedCommitment)
spuaAmortizedCommitment = Lens.lens (amortizedCommitment :: SavingsPlansUtilizationAggregates -> Lude.Maybe SavingsPlansAmortizedCommitment) (\s a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationAggregates)
{-# DEPRECATED spuaAmortizedCommitment "Use generic-lens or generic-optics with 'amortizedCommitment' instead." #-}

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans, as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- /Note:/ Consider using 'savings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuaSavings :: Lens.Lens' SavingsPlansUtilizationAggregates (Lude.Maybe SavingsPlansSavings)
spuaSavings = Lens.lens (savings :: SavingsPlansUtilizationAggregates -> Lude.Maybe SavingsPlansSavings) (\s a -> s {savings = a} :: SavingsPlansUtilizationAggregates)
{-# DEPRECATED spuaSavings "Use generic-lens or generic-optics with 'savings' instead." #-}

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
--
-- /Note:/ Consider using 'utilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spuaUtilization :: Lens.Lens' SavingsPlansUtilizationAggregates SavingsPlansUtilization
spuaUtilization = Lens.lens (utilization :: SavingsPlansUtilizationAggregates -> SavingsPlansUtilization) (\s a -> s {utilization = a} :: SavingsPlansUtilizationAggregates)
{-# DEPRECATED spuaUtilization "Use generic-lens or generic-optics with 'utilization' instead." #-}

instance Lude.FromJSON SavingsPlansUtilizationAggregates where
  parseJSON =
    Lude.withObject
      "SavingsPlansUtilizationAggregates"
      ( \x ->
          SavingsPlansUtilizationAggregates'
            Lude.<$> (x Lude..:? "AmortizedCommitment")
            Lude.<*> (x Lude..:? "Savings")
            Lude.<*> (x Lude..: "Utilization")
      )
