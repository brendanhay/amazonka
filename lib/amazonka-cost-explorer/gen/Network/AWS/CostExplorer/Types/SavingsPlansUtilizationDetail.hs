{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    spudAttributes,
    spudSavings,
    spudSavingsPlanArn,
    spudUtilization,
  )
where

import qualified Network.AWS.CostExplorer.Types.AttributeType as Types
import qualified Network.AWS.CostExplorer.Types.AttributeValue as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlanArn as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansSavings as Types
import qualified Network.AWS.CostExplorer.Types.SavingsPlansUtilization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A single daily or monthly Savings Plans utilization rate, and details for your account. A management account in an organization have access to member accounts. You can use @GetDimensionValues@ to determine the possible dimension values.
--
-- /See:/ 'mkSavingsPlansUtilizationDetail' smart constructor.
data SavingsPlansUtilizationDetail = SavingsPlansUtilizationDetail'
  { -- | The total amortized commitment for a Savings Plans. Includes the sum of the upfront and recurring Savings Plans fees.
    amortizedCommitment :: Core.Maybe Types.SavingsPlansAmortizedCommitment,
    -- | The attribute that applies to a specific @Dimension@ .
    attributes :: Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue),
    -- | The amount saved by using existing Savings Plans. Savings returns both net savings from savings plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
    savings :: Core.Maybe Types.SavingsPlansSavings,
    -- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
    savingsPlanArn :: Core.Maybe Types.SavingsPlanArn,
    -- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
    utilization :: Core.Maybe Types.SavingsPlansUtilization
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansUtilizationDetail' value with any optional fields omitted.
mkSavingsPlansUtilizationDetail ::
  SavingsPlansUtilizationDetail
mkSavingsPlansUtilizationDetail =
  SavingsPlansUtilizationDetail'
    { amortizedCommitment =
        Core.Nothing,
      attributes = Core.Nothing,
      savings = Core.Nothing,
      savingsPlanArn = Core.Nothing,
      utilization = Core.Nothing
    }

-- | The total amortized commitment for a Savings Plans. Includes the sum of the upfront and recurring Savings Plans fees.
--
-- /Note:/ Consider using 'amortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudAmortizedCommitment :: Lens.Lens' SavingsPlansUtilizationDetail (Core.Maybe Types.SavingsPlansAmortizedCommitment)
spudAmortizedCommitment = Lens.field @"amortizedCommitment"
{-# DEPRECATED spudAmortizedCommitment "Use generic-lens or generic-optics with 'amortizedCommitment' instead." #-}

-- | The attribute that applies to a specific @Dimension@ .
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudAttributes :: Lens.Lens' SavingsPlansUtilizationDetail (Core.Maybe (Core.HashMap Types.AttributeType Types.AttributeValue))
spudAttributes = Lens.field @"attributes"
{-# DEPRECATED spudAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The amount saved by using existing Savings Plans. Savings returns both net savings from savings plans as well as the @onDemandCostEquivalent@ of the Savings Plans when considering the utilization rate.
--
-- /Note:/ Consider using 'savings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudSavings :: Lens.Lens' SavingsPlansUtilizationDetail (Core.Maybe Types.SavingsPlansSavings)
spudSavings = Lens.field @"savings"
{-# DEPRECATED spudSavings "Use generic-lens or generic-optics with 'savings' instead." #-}

-- | The unique Amazon Resource Name (ARN) for a particular Savings Plan.
--
-- /Note:/ Consider using 'savingsPlanArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudSavingsPlanArn :: Lens.Lens' SavingsPlansUtilizationDetail (Core.Maybe Types.SavingsPlanArn)
spudSavingsPlanArn = Lens.field @"savingsPlanArn"
{-# DEPRECATED spudSavingsPlanArn "Use generic-lens or generic-optics with 'savingsPlanArn' instead." #-}

-- | A ratio of your effectiveness of using existing Savings Plans to apply to workloads that are Savings Plans eligible.
--
-- /Note:/ Consider using 'utilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spudUtilization :: Lens.Lens' SavingsPlansUtilizationDetail (Core.Maybe Types.SavingsPlansUtilization)
spudUtilization = Lens.field @"utilization"
{-# DEPRECATED spudUtilization "Use generic-lens or generic-optics with 'utilization' instead." #-}

instance Core.FromJSON SavingsPlansUtilizationDetail where
  parseJSON =
    Core.withObject "SavingsPlansUtilizationDetail" Core.$
      \x ->
        SavingsPlansUtilizationDetail'
          Core.<$> (x Core..:? "AmortizedCommitment")
          Core.<*> (x Core..:? "Attributes")
          Core.<*> (x Core..:? "Savings")
          Core.<*> (x Core..:? "SavingsPlanArn")
          Core.<*> (x Core..:? "Utilization")
