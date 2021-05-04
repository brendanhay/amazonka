{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.CostTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.CostTypes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The types of cost that are included in a @COST@ budget, such as tax and
-- subscriptions.
--
-- @USAGE@, @RI_UTILIZATION@, @RI_COVERAGE@, @SAVINGS_PLANS_UTILIZATION@,
-- and @SAVINGS_PLANS_COVERAGE@ budgets do not have @CostTypes@.
--
-- /See:/ 'newCostTypes' smart constructor.
data CostTypes = CostTypes'
  { -- | Specifies whether a budget includes subscriptions.
    --
    -- The default value is @true@.
    includeSubscription :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget uses the amortized rate.
    --
    -- The default value is @false@.
    useAmortized :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes credits.
    --
    -- The default value is @true@.
    includeCredit :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget uses a blended rate.
    --
    -- The default value is @false@.
    useBlended :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes support subscription fees.
    --
    -- The default value is @true@.
    includeSupport :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes refunds.
    --
    -- The default value is @true@.
    includeRefund :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes taxes.
    --
    -- The default value is @true@.
    includeTax :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes discounts.
    --
    -- The default value is @true@.
    includeDiscount :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes non-RI subscription costs.
    --
    -- The default value is @true@.
    includeOtherSubscription :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes upfront RI costs.
    --
    -- The default value is @true@.
    includeUpfront :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes recurring fees such as monthly RI
    -- fees.
    --
    -- The default value is @true@.
    includeRecurring :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CostTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeSubscription', 'costTypes_includeSubscription' - Specifies whether a budget includes subscriptions.
--
-- The default value is @true@.
--
-- 'useAmortized', 'costTypes_useAmortized' - Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@.
--
-- 'includeCredit', 'costTypes_includeCredit' - Specifies whether a budget includes credits.
--
-- The default value is @true@.
--
-- 'useBlended', 'costTypes_useBlended' - Specifies whether a budget uses a blended rate.
--
-- The default value is @false@.
--
-- 'includeSupport', 'costTypes_includeSupport' - Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@.
--
-- 'includeRefund', 'costTypes_includeRefund' - Specifies whether a budget includes refunds.
--
-- The default value is @true@.
--
-- 'includeTax', 'costTypes_includeTax' - Specifies whether a budget includes taxes.
--
-- The default value is @true@.
--
-- 'includeDiscount', 'costTypes_includeDiscount' - Specifies whether a budget includes discounts.
--
-- The default value is @true@.
--
-- 'includeOtherSubscription', 'costTypes_includeOtherSubscription' - Specifies whether a budget includes non-RI subscription costs.
--
-- The default value is @true@.
--
-- 'includeUpfront', 'costTypes_includeUpfront' - Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@.
--
-- 'includeRecurring', 'costTypes_includeRecurring' - Specifies whether a budget includes recurring fees such as monthly RI
-- fees.
--
-- The default value is @true@.
newCostTypes ::
  CostTypes
newCostTypes =
  CostTypes'
    { includeSubscription = Prelude.Nothing,
      useAmortized = Prelude.Nothing,
      includeCredit = Prelude.Nothing,
      useBlended = Prelude.Nothing,
      includeSupport = Prelude.Nothing,
      includeRefund = Prelude.Nothing,
      includeTax = Prelude.Nothing,
      includeDiscount = Prelude.Nothing,
      includeOtherSubscription = Prelude.Nothing,
      includeUpfront = Prelude.Nothing,
      includeRecurring = Prelude.Nothing
    }

-- | Specifies whether a budget includes subscriptions.
--
-- The default value is @true@.
costTypes_includeSubscription :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeSubscription = Lens.lens (\CostTypes' {includeSubscription} -> includeSubscription) (\s@CostTypes' {} a -> s {includeSubscription = a} :: CostTypes)

-- | Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@.
costTypes_useAmortized :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_useAmortized = Lens.lens (\CostTypes' {useAmortized} -> useAmortized) (\s@CostTypes' {} a -> s {useAmortized = a} :: CostTypes)

-- | Specifies whether a budget includes credits.
--
-- The default value is @true@.
costTypes_includeCredit :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeCredit = Lens.lens (\CostTypes' {includeCredit} -> includeCredit) (\s@CostTypes' {} a -> s {includeCredit = a} :: CostTypes)

-- | Specifies whether a budget uses a blended rate.
--
-- The default value is @false@.
costTypes_useBlended :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_useBlended = Lens.lens (\CostTypes' {useBlended} -> useBlended) (\s@CostTypes' {} a -> s {useBlended = a} :: CostTypes)

-- | Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@.
costTypes_includeSupport :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeSupport = Lens.lens (\CostTypes' {includeSupport} -> includeSupport) (\s@CostTypes' {} a -> s {includeSupport = a} :: CostTypes)

-- | Specifies whether a budget includes refunds.
--
-- The default value is @true@.
costTypes_includeRefund :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeRefund = Lens.lens (\CostTypes' {includeRefund} -> includeRefund) (\s@CostTypes' {} a -> s {includeRefund = a} :: CostTypes)

-- | Specifies whether a budget includes taxes.
--
-- The default value is @true@.
costTypes_includeTax :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeTax = Lens.lens (\CostTypes' {includeTax} -> includeTax) (\s@CostTypes' {} a -> s {includeTax = a} :: CostTypes)

-- | Specifies whether a budget includes discounts.
--
-- The default value is @true@.
costTypes_includeDiscount :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeDiscount = Lens.lens (\CostTypes' {includeDiscount} -> includeDiscount) (\s@CostTypes' {} a -> s {includeDiscount = a} :: CostTypes)

-- | Specifies whether a budget includes non-RI subscription costs.
--
-- The default value is @true@.
costTypes_includeOtherSubscription :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeOtherSubscription = Lens.lens (\CostTypes' {includeOtherSubscription} -> includeOtherSubscription) (\s@CostTypes' {} a -> s {includeOtherSubscription = a} :: CostTypes)

-- | Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@.
costTypes_includeUpfront :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeUpfront = Lens.lens (\CostTypes' {includeUpfront} -> includeUpfront) (\s@CostTypes' {} a -> s {includeUpfront = a} :: CostTypes)

-- | Specifies whether a budget includes recurring fees such as monthly RI
-- fees.
--
-- The default value is @true@.
costTypes_includeRecurring :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeRecurring = Lens.lens (\CostTypes' {includeRecurring} -> includeRecurring) (\s@CostTypes' {} a -> s {includeRecurring = a} :: CostTypes)

instance Prelude.FromJSON CostTypes where
  parseJSON =
    Prelude.withObject
      "CostTypes"
      ( \x ->
          CostTypes'
            Prelude.<$> (x Prelude..:? "IncludeSubscription")
            Prelude.<*> (x Prelude..:? "UseAmortized")
            Prelude.<*> (x Prelude..:? "IncludeCredit")
            Prelude.<*> (x Prelude..:? "UseBlended")
            Prelude.<*> (x Prelude..:? "IncludeSupport")
            Prelude.<*> (x Prelude..:? "IncludeRefund")
            Prelude.<*> (x Prelude..:? "IncludeTax")
            Prelude.<*> (x Prelude..:? "IncludeDiscount")
            Prelude.<*> (x Prelude..:? "IncludeOtherSubscription")
            Prelude.<*> (x Prelude..:? "IncludeUpfront")
            Prelude.<*> (x Prelude..:? "IncludeRecurring")
      )

instance Prelude.Hashable CostTypes

instance Prelude.NFData CostTypes

instance Prelude.ToJSON CostTypes where
  toJSON CostTypes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IncludeSubscription" Prelude..=)
              Prelude.<$> includeSubscription,
            ("UseAmortized" Prelude..=) Prelude.<$> useAmortized,
            ("IncludeCredit" Prelude..=)
              Prelude.<$> includeCredit,
            ("UseBlended" Prelude..=) Prelude.<$> useBlended,
            ("IncludeSupport" Prelude..=)
              Prelude.<$> includeSupport,
            ("IncludeRefund" Prelude..=)
              Prelude.<$> includeRefund,
            ("IncludeTax" Prelude..=) Prelude.<$> includeTax,
            ("IncludeDiscount" Prelude..=)
              Prelude.<$> includeDiscount,
            ("IncludeOtherSubscription" Prelude..=)
              Prelude.<$> includeOtherSubscription,
            ("IncludeUpfront" Prelude..=)
              Prelude.<$> includeUpfront,
            ("IncludeRecurring" Prelude..=)
              Prelude.<$> includeRecurring
          ]
      )
