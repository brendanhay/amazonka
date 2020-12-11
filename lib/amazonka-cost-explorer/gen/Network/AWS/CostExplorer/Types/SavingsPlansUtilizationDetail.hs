-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansUtilizationDetail
  ( SavingsPlansUtilizationDetail (..),

    -- * Smart constructor
    mkSavingsPlansUtilizationDetail,

    -- * Lenses
    spudAmortizedCommitment,
    spudSavings,
    spudAttributes,
    spudUtilization,
    spudSavingsPlanARN,
  )
where

import Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
import Network.AWS.CostExplorer.Types.SavingsPlansSavings
import Network.AWS.CostExplorer.Types.SavingsPlansUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A single daily or monthly Savings Plans utilization rate, and details for your account. A management account in an organization have access to member accounts. You can use @GetDimensionValues@ to determine the possible dimension values.
--
-- /See:/ 'mkSavingsPlansUtilizationDetail' smart constructor.
data SavingsPlansUtilizationDetail = SavingsPlansUtilizationDetail'
  { amortizedCommitment ::
      Lude.Maybe
        SavingsPlansAmortizedCommitment,
    savings ::
      Lude.Maybe SavingsPlansSavings,
    attributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    utilization ::
      Lude.Maybe
        SavingsPlansUtilization,
    savingsPlanARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SavingsPlansUtilizationDetail' with the minimum fields required to make a request.
--
-- * 'amortizedCommitment' - The total amortized commitment for a Savings Plans. Includes the sum of the upfront and recurring Savings Plans fees.
-- * 'attributes' - The attribute that applies to a specific @Dimension@ .
-- * 'savings' - The amount saved by using existing Savings Plans. Savings returns both net savings from savings plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
-- * 'savingsPlanARN' - The unique Amazon Resource Name (ARN) for a particular Savings Plan.
-- * 'utilization' - A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
mkSavingsPlansUtilizationDetail ::
  SavingsPlansUtilizationDetail
mkSavingsPlansUtilizationDetail =
  SavingsPlansUtilizationDetail'
    { amortizedCommitment =
        Lude.Nothing,
      savings = Lude.Nothing,
      attributes = Lude.Nothing,
      utilization = Lude.Nothing,
      savingsPlanARN = Lude.Nothing
    }

-- | The total amortized commitment for a Savings Plans. Includes the sum of the upfront and recurring Savings Plans fees.
--
-- /Note:/ Consider using 'amortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudAmortizedCommitment :: Lens.Lens' SavingsPlansUtilizationDetail (Lude.Maybe SavingsPlansAmortizedCommitment)
spudAmortizedCommitment = Lens.lens (amortizedCommitment :: SavingsPlansUtilizationDetail -> Lude.Maybe SavingsPlansAmortizedCommitment) (\s a -> s {amortizedCommitment = a} :: SavingsPlansUtilizationDetail)
{-# DEPRECATED spudAmortizedCommitment "Use generic-lens or generic-optics with 'amortizedCommitment' instead." #-}

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from savings plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- /Note:/ Consider using 'savings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudSavings :: Lens.Lens' SavingsPlansUtilizationDetail (Lude.Maybe SavingsPlansSavings)
spudSavings = Lens.lens (savings :: SavingsPlansUtilizationDetail -> Lude.Maybe SavingsPlansSavings) (\s a -> s {savings = a} :: SavingsPlansUtilizationDetail)
{-# DEPRECATED spudSavings "Use generic-lens or generic-optics with 'savings' instead." #-}

-- | The attribute that applies to a specific @Dimension@ .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudAttributes :: Lens.Lens' SavingsPlansUtilizationDetail (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
spudAttributes = Lens.lens (attributes :: SavingsPlansUtilizationDetail -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: SavingsPlansUtilizationDetail)
{-# DEPRECATED spudAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
--
-- /Note:/ Consider using 'utilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudUtilization :: Lens.Lens' SavingsPlansUtilizationDetail (Lude.Maybe SavingsPlansUtilization)
spudUtilization = Lens.lens (utilization :: SavingsPlansUtilizationDetail -> Lude.Maybe SavingsPlansUtilization) (\s a -> s {utilization = a} :: SavingsPlansUtilizationDetail)
{-# DEPRECATED spudUtilization "Use generic-lens or generic-optics with 'utilization' instead." #-}

-- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
--
-- /Note:/ Consider using 'savingsPlanARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudSavingsPlanARN :: Lens.Lens' SavingsPlansUtilizationDetail (Lude.Maybe Lude.Text)
spudSavingsPlanARN = Lens.lens (savingsPlanARN :: SavingsPlansUtilizationDetail -> Lude.Maybe Lude.Text) (\s a -> s {savingsPlanARN = a} :: SavingsPlansUtilizationDetail)
{-# DEPRECATED spudSavingsPlanARN "Use generic-lens or generic-optics with 'savingsPlanARN' instead." #-}

instance Lude.FromJSON SavingsPlansUtilizationDetail where
  parseJSON =
    Lude.withObject
      "SavingsPlansUtilizationDetail"
      ( \x ->
          SavingsPlansUtilizationDetail'
            Lude.<$> (x Lude..:? "AmortizedCommitment")
            Lude.<*> (x Lude..:? "Savings")
            Lude.<*> (x Lude..:? "Attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Utilization")
            Lude.<*> (x Lude..:? "SavingsPlanArn")
      )
