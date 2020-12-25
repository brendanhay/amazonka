{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SavingsPlansAmortizedCommitment
  ( SavingsPlansAmortizedCommitment (..),

    -- * Smart constructor
    mkSavingsPlansAmortizedCommitment,

    -- * Lenses
    spacAmortizedRecurringCommitment,
    spacAmortizedUpfrontCommitment,
    spacTotalAmortizedCommitment,
  )
where

import qualified Network.AWS.CostExplorer.Types.GenericString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The amortized amount of Savings Plans purchased in a specific account during a specific time interval.
--
-- /See:/ 'mkSavingsPlansAmortizedCommitment' smart constructor.
data SavingsPlansAmortizedCommitment = SavingsPlansAmortizedCommitment'
  { -- | The amortized amount of your Savings Plans commitment that was purchased with either a @Partial@ or a @NoUpfront@ .
    amortizedRecurringCommitment :: Core.Maybe Types.GenericString,
    -- | The amortized amount of your Savings Plans commitment that was purchased with an @Upfront@ or @PartialUpfront@ Savings Plans.
    amortizedUpfrontCommitment :: Core.Maybe Types.GenericString,
    -- | The total amortized amount of your Savings Plans commitment, regardless of your Savings Plans purchase method.
    totalAmortizedCommitment :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SavingsPlansAmortizedCommitment' value with any optional fields omitted.
mkSavingsPlansAmortizedCommitment ::
  SavingsPlansAmortizedCommitment
mkSavingsPlansAmortizedCommitment =
  SavingsPlansAmortizedCommitment'
    { amortizedRecurringCommitment =
        Core.Nothing,
      amortizedUpfrontCommitment = Core.Nothing,
      totalAmortizedCommitment = Core.Nothing
    }

-- | The amortized amount of your Savings Plans commitment that was purchased with either a @Partial@ or a @NoUpfront@ .
--
-- /Note:/ Consider using 'amortizedRecurringCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spacAmortizedRecurringCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Core.Maybe Types.GenericString)
spacAmortizedRecurringCommitment = Lens.field @"amortizedRecurringCommitment"
{-# DEPRECATED spacAmortizedRecurringCommitment "Use generic-lens or generic-optics with 'amortizedRecurringCommitment' instead." #-}

-- | The amortized amount of your Savings Plans commitment that was purchased with an @Upfront@ or @PartialUpfront@ Savings Plans.
--
-- /Note:/ Consider using 'amortizedUpfrontCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spacAmortizedUpfrontCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Core.Maybe Types.GenericString)
spacAmortizedUpfrontCommitment = Lens.field @"amortizedUpfrontCommitment"
{-# DEPRECATED spacAmortizedUpfrontCommitment "Use generic-lens or generic-optics with 'amortizedUpfrontCommitment' instead." #-}

-- | The total amortized amount of your Savings Plans commitment, regardless of your Savings Plans purchase method.
--
-- /Note:/ Consider using 'totalAmortizedCommitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spacTotalAmortizedCommitment :: Lens.Lens' SavingsPlansAmortizedCommitment (Core.Maybe Types.GenericString)
spacTotalAmortizedCommitment = Lens.field @"totalAmortizedCommitment"
{-# DEPRECATED spacTotalAmortizedCommitment "Use generic-lens or generic-optics with 'totalAmortizedCommitment' instead." #-}

instance Core.FromJSON SavingsPlansAmortizedCommitment where
  parseJSON =
    Core.withObject "SavingsPlansAmortizedCommitment" Core.$
      \x ->
        SavingsPlansAmortizedCommitment'
          Core.<$> (x Core..:? "AmortizedRecurringCommitment")
          Core.<*> (x Core..:? "AmortizedUpfrontCommitment")
          Core.<*> (x Core..:? "TotalAmortizedCommitment")
