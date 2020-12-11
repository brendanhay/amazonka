-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationByTime
  ( SavingsPlansUtilizationByTime (..),

    -- * Smart constructor
    mkSavingsPlansUtilizationByTime,

    -- * Lenses
    spubtAmortizedCommitment,
    spubtSavings,
    spubtTimePeriod,
    spubtUtilization,
  )
where

import Network.AWS.CostExplorer.Types.DateInterval
import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The amount of Savings Plans utilization, in hours.
--
-- /See:/ 'mkSavingsPlansUtilizationByTime' smart constructor.
data SavingsPlansUtilizationByTime = SavingsPlansUtilizationByTime'
  { amortizedCommitment ::
      Lude.Maybe
        SavingsPlansAmortizedCommitment,
    savings ::
      Lude.Maybe SavingsPlansSavings,
    timePeriod :: DateInterval,
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

-- | Creates a value of 'SavingsPlansUtilizationByTime' with the minimum fields required to make a request.
--
-- * 'amortizedCommitment' - The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
-- * 'savings' - The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
-- * 'timePeriod' - Undocumented field.
-- * 'utilization' - A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
mkSavingsPlansUtilizationByTime ::
  -- | 'timePeriod'
  DateInterval ->
  -- | 'utilization'
  SavingsPlansUtilization ->
  SavingsPlansUtilizationByTime
mkSavingsPlansUtilizationByTime pTimePeriod_ pUtilization_ =
  SavingsPlansUtilizationByTime'
    { amortizedCommitment =
        Lude.Nothing,
      savings = Lude.Nothing,
      timePeriod = pTimePeriod_,
      utilization = pUtilization_
    }

-- | The total amortized commitment for a Savings Plans. This includes the sum of the upfront and recurring Savings Plans fees.
--
-- /Note:/ Consider using 'amortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtAmortizedCommitment :: Lens.Lens' SavingsPlansUtilizationByTime (Lude.Maybe SavingsPlansAmortizedCommitment)
spubtAmortizedCommitment = Lens.lens (amortizedCommitment :: SavingsPlansUtilizationByTime -> Lude.Maybe SavingsPlansAmortizedCommitment) (\s a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationByTime)
{-# DEPRECATED spubtAmortizedCommitment "Use generic-lens or generic-optics with 'amortizedCommitment' instead." #-}

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from Savings Plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- /Note:/ Consider using 'savings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtSavings :: Lens.Lens' SavingsPlansUtilizationByTime (Lude.Maybe SavingsPlansSavings)
spubtSavings = Lens.lens (savings :: SavingsPlansUtilizationByTime -> Lude.Maybe SavingsPlansSavings) (\s a -> s {savings = a} :: SavingsPlansUtilizationByTime)
{-# DEPRECATED spubtSavings "Use generic-lens or generic-optics with 'savings' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtTimePeriod :: Lens.Lens' SavingsPlansUtilizationByTime DateInterval
spubtTimePeriod = Lens.lens (timePeriod :: SavingsPlansUtilizationByTime -> DateInterval) (\s a -> s {timePeriod = a} :: SavingsPlansUtilizationByTime)
{-# DEPRECATED spubtTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
--
-- /Note:/ Consider using 'utilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spubtUtilization :: Lens.Lens' SavingsPlansUtilizationByTime SavingsPlansUtilization
spubtUtilization = Lens.lens (utilization :: SavingsPlansUtilizationByTime -> SavingsPlansUtilization) (\s a -> s {utilization = a} :: SavingsPlansUtilizationByTime)
{-# DEPRECATED spubtUtilization "Use generic-lens or generic-optics with 'utilization' instead." #-}

instance Lude.FromJSON SavingsPlansUtilizationByTime where
  parseJSON =
    Lude.withObject
      "SavingsPlansUtilizationByTime"
      ( \x ->
          SavingsPlansUtilizationByTime'
            Lude.<$> (x Lude..:? "AmortizedCommitment")
            Lude.<*> (x Lude..:? "Savings")
            Lude.<*> (x Lude..: "TimePeriod")
            Lude.<*> (x Lude..: "Utilization")
      )
