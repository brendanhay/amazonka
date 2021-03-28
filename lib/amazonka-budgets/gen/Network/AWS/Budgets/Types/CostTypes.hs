{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.CostTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.CostTypes
  ( CostTypes (..)
  -- * Smart constructor
  , mkCostTypes
  -- * Lenses
  , ctIncludeCredit
  , ctIncludeDiscount
  , ctIncludeOtherSubscription
  , ctIncludeRecurring
  , ctIncludeRefund
  , ctIncludeSubscription
  , ctIncludeSupport
  , ctIncludeTax
  , ctIncludeUpfront
  , ctUseAmortized
  , ctUseBlended
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The types of cost that are included in a @COST@ budget, such as tax and subscriptions.
--
-- @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
--
-- /See:/ 'mkCostTypes' smart constructor.
data CostTypes = CostTypes'
  { includeCredit :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes credits.
--
-- The default value is @true@ .
  , includeDiscount :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes discounts.
--
-- The default value is @true@ .
  , includeOtherSubscription :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes non-RI subscription costs.
--
-- The default value is @true@ .
  , includeRecurring :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes recurring fees such as monthly RI fees.
--
-- The default value is @true@ .
  , includeRefund :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes refunds.
--
-- The default value is @true@ .
  , includeSubscription :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes subscriptions.
--
-- The default value is @true@ .
  , includeSupport :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@ .
  , includeTax :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes taxes.
--
-- The default value is @true@ .
  , includeUpfront :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@ .
  , useAmortized :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@ .
  , useBlended :: Core.Maybe Core.Bool
    -- ^ Specifies whether a budget uses a blended rate.
--
-- The default value is @false@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CostTypes' value with any optional fields omitted.
mkCostTypes
    :: CostTypes
mkCostTypes
  = CostTypes'{includeCredit = Core.Nothing,
               includeDiscount = Core.Nothing,
               includeOtherSubscription = Core.Nothing,
               includeRecurring = Core.Nothing, includeRefund = Core.Nothing,
               includeSubscription = Core.Nothing, includeSupport = Core.Nothing,
               includeTax = Core.Nothing, includeUpfront = Core.Nothing,
               useAmortized = Core.Nothing, useBlended = Core.Nothing}

-- | Specifies whether a budget includes credits.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeCredit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeCredit :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeCredit = Lens.field @"includeCredit"
{-# INLINEABLE ctIncludeCredit #-}
{-# DEPRECATED includeCredit "Use generic-lens or generic-optics with 'includeCredit' instead"  #-}

-- | Specifies whether a budget includes discounts.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeDiscount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeDiscount :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeDiscount = Lens.field @"includeDiscount"
{-# INLINEABLE ctIncludeDiscount #-}
{-# DEPRECATED includeDiscount "Use generic-lens or generic-optics with 'includeDiscount' instead"  #-}

-- | Specifies whether a budget includes non-RI subscription costs.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeOtherSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeOtherSubscription :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeOtherSubscription = Lens.field @"includeOtherSubscription"
{-# INLINEABLE ctIncludeOtherSubscription #-}
{-# DEPRECATED includeOtherSubscription "Use generic-lens or generic-optics with 'includeOtherSubscription' instead"  #-}

-- | Specifies whether a budget includes recurring fees such as monthly RI fees.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeRecurring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeRecurring :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeRecurring = Lens.field @"includeRecurring"
{-# INLINEABLE ctIncludeRecurring #-}
{-# DEPRECATED includeRecurring "Use generic-lens or generic-optics with 'includeRecurring' instead"  #-}

-- | Specifies whether a budget includes refunds.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeRefund' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeRefund :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeRefund = Lens.field @"includeRefund"
{-# INLINEABLE ctIncludeRefund #-}
{-# DEPRECATED includeRefund "Use generic-lens or generic-optics with 'includeRefund' instead"  #-}

-- | Specifies whether a budget includes subscriptions.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeSubscription :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeSubscription = Lens.field @"includeSubscription"
{-# INLINEABLE ctIncludeSubscription #-}
{-# DEPRECATED includeSubscription "Use generic-lens or generic-optics with 'includeSubscription' instead"  #-}

-- | Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeSupport :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeSupport = Lens.field @"includeSupport"
{-# INLINEABLE ctIncludeSupport #-}
{-# DEPRECATED includeSupport "Use generic-lens or generic-optics with 'includeSupport' instead"  #-}

-- | Specifies whether a budget includes taxes.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeTax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeTax :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeTax = Lens.field @"includeTax"
{-# INLINEABLE ctIncludeTax #-}
{-# DEPRECATED includeTax "Use generic-lens or generic-optics with 'includeTax' instead"  #-}

-- | Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeUpfront' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeUpfront :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctIncludeUpfront = Lens.field @"includeUpfront"
{-# INLINEABLE ctIncludeUpfront #-}
{-# DEPRECATED includeUpfront "Use generic-lens or generic-optics with 'includeUpfront' instead"  #-}

-- | Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'useAmortized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctUseAmortized :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctUseAmortized = Lens.field @"useAmortized"
{-# INLINEABLE ctUseAmortized #-}
{-# DEPRECATED useAmortized "Use generic-lens or generic-optics with 'useAmortized' instead"  #-}

-- | Specifies whether a budget uses a blended rate.
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'useBlended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctUseBlended :: Lens.Lens' CostTypes (Core.Maybe Core.Bool)
ctUseBlended = Lens.field @"useBlended"
{-# INLINEABLE ctUseBlended #-}
{-# DEPRECATED useBlended "Use generic-lens or generic-optics with 'useBlended' instead"  #-}

instance Core.FromJSON CostTypes where
        toJSON CostTypes{..}
          = Core.object
              (Core.catMaybes
                 [("IncludeCredit" Core..=) Core.<$> includeCredit,
                  ("IncludeDiscount" Core..=) Core.<$> includeDiscount,
                  ("IncludeOtherSubscription" Core..=) Core.<$>
                    includeOtherSubscription,
                  ("IncludeRecurring" Core..=) Core.<$> includeRecurring,
                  ("IncludeRefund" Core..=) Core.<$> includeRefund,
                  ("IncludeSubscription" Core..=) Core.<$> includeSubscription,
                  ("IncludeSupport" Core..=) Core.<$> includeSupport,
                  ("IncludeTax" Core..=) Core.<$> includeTax,
                  ("IncludeUpfront" Core..=) Core.<$> includeUpfront,
                  ("UseAmortized" Core..=) Core.<$> useAmortized,
                  ("UseBlended" Core..=) Core.<$> useBlended])

instance Core.FromJSON CostTypes where
        parseJSON
          = Core.withObject "CostTypes" Core.$
              \ x ->
                CostTypes' Core.<$>
                  (x Core..:? "IncludeCredit") Core.<*> x Core..:? "IncludeDiscount"
                    Core.<*> x Core..:? "IncludeOtherSubscription"
                    Core.<*> x Core..:? "IncludeRecurring"
                    Core.<*> x Core..:? "IncludeRefund"
                    Core.<*> x Core..:? "IncludeSubscription"
                    Core.<*> x Core..:? "IncludeSupport"
                    Core.<*> x Core..:? "IncludeTax"
                    Core.<*> x Core..:? "IncludeUpfront"
                    Core.<*> x Core..:? "UseAmortized"
                    Core.<*> x Core..:? "UseBlended"
