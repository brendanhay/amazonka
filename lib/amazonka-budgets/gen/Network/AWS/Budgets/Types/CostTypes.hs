-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.CostTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.CostTypes
  ( CostTypes (..),

    -- * Smart constructor
    mkCostTypes,

    -- * Lenses
    ctUseAmortized,
    ctIncludeRecurring,
    ctUseBlended,
    ctIncludeSupport,
    ctIncludeDiscount,
    ctIncludeSubscription,
    ctIncludeRefund,
    ctIncludeUpfront,
    ctIncludeOtherSubscription,
    ctIncludeTax,
    ctIncludeCredit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The types of cost that are included in a @COST@ budget, such as tax and subscriptions.
--
-- @USAGE@ , @RI_UTILIZATION@ , @RI_COVERAGE@ , @SAVINGS_PLANS_UTILIZATION@ , and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@ .
--
-- /See:/ 'mkCostTypes' smart constructor.
data CostTypes = CostTypes'
  { useAmortized :: Lude.Maybe Lude.Bool,
    includeRecurring :: Lude.Maybe Lude.Bool,
    useBlended :: Lude.Maybe Lude.Bool,
    includeSupport :: Lude.Maybe Lude.Bool,
    includeDiscount :: Lude.Maybe Lude.Bool,
    includeSubscription :: Lude.Maybe Lude.Bool,
    includeRefund :: Lude.Maybe Lude.Bool,
    includeUpfront :: Lude.Maybe Lude.Bool,
    includeOtherSubscription :: Lude.Maybe Lude.Bool,
    includeTax :: Lude.Maybe Lude.Bool,
    includeCredit :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CostTypes' with the minimum fields required to make a request.
--
-- * 'includeCredit' - Specifies whether a budget includes credits.
--
-- The default value is @true@ .
-- * 'includeDiscount' - Specifies whether a budget includes discounts.
--
-- The default value is @true@ .
-- * 'includeOtherSubscription' - Specifies whether a budget includes non-RI subscription costs.
--
-- The default value is @true@ .
-- * 'includeRecurring' - Specifies whether a budget includes recurring fees such as monthly RI fees.
--
-- The default value is @true@ .
-- * 'includeRefund' - Specifies whether a budget includes refunds.
--
-- The default value is @true@ .
-- * 'includeSubscription' - Specifies whether a budget includes subscriptions.
--
-- The default value is @true@ .
-- * 'includeSupport' - Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@ .
-- * 'includeTax' - Specifies whether a budget includes taxes.
--
-- The default value is @true@ .
-- * 'includeUpfront' - Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@ .
-- * 'useAmortized' - Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@ .
-- * 'useBlended' - Specifies whether a budget uses a blended rate.
--
-- The default value is @false@ .
mkCostTypes ::
  CostTypes
mkCostTypes =
  CostTypes'
    { useAmortized = Lude.Nothing,
      includeRecurring = Lude.Nothing,
      useBlended = Lude.Nothing,
      includeSupport = Lude.Nothing,
      includeDiscount = Lude.Nothing,
      includeSubscription = Lude.Nothing,
      includeRefund = Lude.Nothing,
      includeUpfront = Lude.Nothing,
      includeOtherSubscription = Lude.Nothing,
      includeTax = Lude.Nothing,
      includeCredit = Lude.Nothing
    }

-- | Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'useAmortized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctUseAmortized :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctUseAmortized = Lens.lens (useAmortized :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {useAmortized = a} :: CostTypes)
{-# DEPRECATED ctUseAmortized "Use generic-lens or generic-optics with 'useAmortized' instead." #-}

-- | Specifies whether a budget includes recurring fees such as monthly RI fees.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeRecurring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeRecurring :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeRecurring = Lens.lens (includeRecurring :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeRecurring = a} :: CostTypes)
{-# DEPRECATED ctIncludeRecurring "Use generic-lens or generic-optics with 'includeRecurring' instead." #-}

-- | Specifies whether a budget uses a blended rate.
--
-- The default value is @false@ .
--
-- /Note:/ Consider using 'useBlended' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctUseBlended :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctUseBlended = Lens.lens (useBlended :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {useBlended = a} :: CostTypes)
{-# DEPRECATED ctUseBlended "Use generic-lens or generic-optics with 'useBlended' instead." #-}

-- | Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeSupport :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeSupport = Lens.lens (includeSupport :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeSupport = a} :: CostTypes)
{-# DEPRECATED ctIncludeSupport "Use generic-lens or generic-optics with 'includeSupport' instead." #-}

-- | Specifies whether a budget includes discounts.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeDiscount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeDiscount :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeDiscount = Lens.lens (includeDiscount :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeDiscount = a} :: CostTypes)
{-# DEPRECATED ctIncludeDiscount "Use generic-lens or generic-optics with 'includeDiscount' instead." #-}

-- | Specifies whether a budget includes subscriptions.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeSubscription :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeSubscription = Lens.lens (includeSubscription :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeSubscription = a} :: CostTypes)
{-# DEPRECATED ctIncludeSubscription "Use generic-lens or generic-optics with 'includeSubscription' instead." #-}

-- | Specifies whether a budget includes refunds.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeRefund' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeRefund :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeRefund = Lens.lens (includeRefund :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeRefund = a} :: CostTypes)
{-# DEPRECATED ctIncludeRefund "Use generic-lens or generic-optics with 'includeRefund' instead." #-}

-- | Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeUpfront' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeUpfront :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeUpfront = Lens.lens (includeUpfront :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeUpfront = a} :: CostTypes)
{-# DEPRECATED ctIncludeUpfront "Use generic-lens or generic-optics with 'includeUpfront' instead." #-}

-- | Specifies whether a budget includes non-RI subscription costs.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeOtherSubscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeOtherSubscription :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeOtherSubscription = Lens.lens (includeOtherSubscription :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeOtherSubscription = a} :: CostTypes)
{-# DEPRECATED ctIncludeOtherSubscription "Use generic-lens or generic-optics with 'includeOtherSubscription' instead." #-}

-- | Specifies whether a budget includes taxes.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeTax' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeTax :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeTax = Lens.lens (includeTax :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeTax = a} :: CostTypes)
{-# DEPRECATED ctIncludeTax "Use generic-lens or generic-optics with 'includeTax' instead." #-}

-- | Specifies whether a budget includes credits.
--
-- The default value is @true@ .
--
-- /Note:/ Consider using 'includeCredit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctIncludeCredit :: Lens.Lens' CostTypes (Lude.Maybe Lude.Bool)
ctIncludeCredit = Lens.lens (includeCredit :: CostTypes -> Lude.Maybe Lude.Bool) (\s a -> s {includeCredit = a} :: CostTypes)
{-# DEPRECATED ctIncludeCredit "Use generic-lens or generic-optics with 'includeCredit' instead." #-}

instance Lude.FromJSON CostTypes where
  parseJSON =
    Lude.withObject
      "CostTypes"
      ( \x ->
          CostTypes'
            Lude.<$> (x Lude..:? "UseAmortized")
            Lude.<*> (x Lude..:? "IncludeRecurring")
            Lude.<*> (x Lude..:? "UseBlended")
            Lude.<*> (x Lude..:? "IncludeSupport")
            Lude.<*> (x Lude..:? "IncludeDiscount")
            Lude.<*> (x Lude..:? "IncludeSubscription")
            Lude.<*> (x Lude..:? "IncludeRefund")
            Lude.<*> (x Lude..:? "IncludeUpfront")
            Lude.<*> (x Lude..:? "IncludeOtherSubscription")
            Lude.<*> (x Lude..:? "IncludeTax")
            Lude.<*> (x Lude..:? "IncludeCredit")
      )

instance Lude.ToJSON CostTypes where
  toJSON CostTypes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UseAmortized" Lude..=) Lude.<$> useAmortized,
            ("IncludeRecurring" Lude..=) Lude.<$> includeRecurring,
            ("UseBlended" Lude..=) Lude.<$> useBlended,
            ("IncludeSupport" Lude..=) Lude.<$> includeSupport,
            ("IncludeDiscount" Lude..=) Lude.<$> includeDiscount,
            ("IncludeSubscription" Lude..=) Lude.<$> includeSubscription,
            ("IncludeRefund" Lude..=) Lude.<$> includeRefund,
            ("IncludeUpfront" Lude..=) Lude.<$> includeUpfront,
            ("IncludeOtherSubscription" Lude..=)
              Lude.<$> includeOtherSubscription,
            ("IncludeTax" Lude..=) Lude.<$> includeTax,
            ("IncludeCredit" Lude..=) Lude.<$> includeCredit
          ]
      )
