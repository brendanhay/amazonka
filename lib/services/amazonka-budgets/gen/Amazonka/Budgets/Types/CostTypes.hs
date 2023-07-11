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
-- Module      : Amazonka.Budgets.Types.CostTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.CostTypes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The types of cost that are included in a @COST@ budget, such as tax and
-- subscriptions.
--
-- @USAGE@, @RI_UTILIZATION@, @RI_COVERAGE@, @SAVINGS_PLANS_UTILIZATION@,
-- and @SAVINGS_PLANS_COVERAGE@ budgets don\'t have @CostTypes@.
--
-- /See:/ 'newCostTypes' smart constructor.
data CostTypes = CostTypes'
  { -- | Specifies whether a budget includes credits.
    --
    -- The default value is @true@.
    includeCredit :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes discounts.
    --
    -- The default value is @true@.
    includeDiscount :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes non-RI subscription costs.
    --
    -- The default value is @true@.
    includeOtherSubscription :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes recurring fees such as monthly RI
    -- fees.
    --
    -- The default value is @true@.
    includeRecurring :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes refunds.
    --
    -- The default value is @true@.
    includeRefund :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes subscriptions.
    --
    -- The default value is @true@.
    includeSubscription :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes support subscription fees.
    --
    -- The default value is @true@.
    includeSupport :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes taxes.
    --
    -- The default value is @true@.
    includeTax :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget includes upfront RI costs.
    --
    -- The default value is @true@.
    includeUpfront :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget uses the amortized rate.
    --
    -- The default value is @false@.
    useAmortized :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether a budget uses a blended rate.
    --
    -- The default value is @false@.
    useBlended :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeCredit', 'costTypes_includeCredit' - Specifies whether a budget includes credits.
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
-- 'includeRecurring', 'costTypes_includeRecurring' - Specifies whether a budget includes recurring fees such as monthly RI
-- fees.
--
-- The default value is @true@.
--
-- 'includeRefund', 'costTypes_includeRefund' - Specifies whether a budget includes refunds.
--
-- The default value is @true@.
--
-- 'includeSubscription', 'costTypes_includeSubscription' - Specifies whether a budget includes subscriptions.
--
-- The default value is @true@.
--
-- 'includeSupport', 'costTypes_includeSupport' - Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@.
--
-- 'includeTax', 'costTypes_includeTax' - Specifies whether a budget includes taxes.
--
-- The default value is @true@.
--
-- 'includeUpfront', 'costTypes_includeUpfront' - Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@.
--
-- 'useAmortized', 'costTypes_useAmortized' - Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@.
--
-- 'useBlended', 'costTypes_useBlended' - Specifies whether a budget uses a blended rate.
--
-- The default value is @false@.
newCostTypes ::
  CostTypes
newCostTypes =
  CostTypes'
    { includeCredit = Prelude.Nothing,
      includeDiscount = Prelude.Nothing,
      includeOtherSubscription = Prelude.Nothing,
      includeRecurring = Prelude.Nothing,
      includeRefund = Prelude.Nothing,
      includeSubscription = Prelude.Nothing,
      includeSupport = Prelude.Nothing,
      includeTax = Prelude.Nothing,
      includeUpfront = Prelude.Nothing,
      useAmortized = Prelude.Nothing,
      useBlended = Prelude.Nothing
    }

-- | Specifies whether a budget includes credits.
--
-- The default value is @true@.
costTypes_includeCredit :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeCredit = Lens.lens (\CostTypes' {includeCredit} -> includeCredit) (\s@CostTypes' {} a -> s {includeCredit = a} :: CostTypes)

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

-- | Specifies whether a budget includes recurring fees such as monthly RI
-- fees.
--
-- The default value is @true@.
costTypes_includeRecurring :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeRecurring = Lens.lens (\CostTypes' {includeRecurring} -> includeRecurring) (\s@CostTypes' {} a -> s {includeRecurring = a} :: CostTypes)

-- | Specifies whether a budget includes refunds.
--
-- The default value is @true@.
costTypes_includeRefund :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeRefund = Lens.lens (\CostTypes' {includeRefund} -> includeRefund) (\s@CostTypes' {} a -> s {includeRefund = a} :: CostTypes)

-- | Specifies whether a budget includes subscriptions.
--
-- The default value is @true@.
costTypes_includeSubscription :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeSubscription = Lens.lens (\CostTypes' {includeSubscription} -> includeSubscription) (\s@CostTypes' {} a -> s {includeSubscription = a} :: CostTypes)

-- | Specifies whether a budget includes support subscription fees.
--
-- The default value is @true@.
costTypes_includeSupport :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeSupport = Lens.lens (\CostTypes' {includeSupport} -> includeSupport) (\s@CostTypes' {} a -> s {includeSupport = a} :: CostTypes)

-- | Specifies whether a budget includes taxes.
--
-- The default value is @true@.
costTypes_includeTax :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeTax = Lens.lens (\CostTypes' {includeTax} -> includeTax) (\s@CostTypes' {} a -> s {includeTax = a} :: CostTypes)

-- | Specifies whether a budget includes upfront RI costs.
--
-- The default value is @true@.
costTypes_includeUpfront :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_includeUpfront = Lens.lens (\CostTypes' {includeUpfront} -> includeUpfront) (\s@CostTypes' {} a -> s {includeUpfront = a} :: CostTypes)

-- | Specifies whether a budget uses the amortized rate.
--
-- The default value is @false@.
costTypes_useAmortized :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_useAmortized = Lens.lens (\CostTypes' {useAmortized} -> useAmortized) (\s@CostTypes' {} a -> s {useAmortized = a} :: CostTypes)

-- | Specifies whether a budget uses a blended rate.
--
-- The default value is @false@.
costTypes_useBlended :: Lens.Lens' CostTypes (Prelude.Maybe Prelude.Bool)
costTypes_useBlended = Lens.lens (\CostTypes' {useBlended} -> useBlended) (\s@CostTypes' {} a -> s {useBlended = a} :: CostTypes)

instance Data.FromJSON CostTypes where
  parseJSON =
    Data.withObject
      "CostTypes"
      ( \x ->
          CostTypes'
            Prelude.<$> (x Data..:? "IncludeCredit")
            Prelude.<*> (x Data..:? "IncludeDiscount")
            Prelude.<*> (x Data..:? "IncludeOtherSubscription")
            Prelude.<*> (x Data..:? "IncludeRecurring")
            Prelude.<*> (x Data..:? "IncludeRefund")
            Prelude.<*> (x Data..:? "IncludeSubscription")
            Prelude.<*> (x Data..:? "IncludeSupport")
            Prelude.<*> (x Data..:? "IncludeTax")
            Prelude.<*> (x Data..:? "IncludeUpfront")
            Prelude.<*> (x Data..:? "UseAmortized")
            Prelude.<*> (x Data..:? "UseBlended")
      )

instance Prelude.Hashable CostTypes where
  hashWithSalt _salt CostTypes' {..} =
    _salt
      `Prelude.hashWithSalt` includeCredit
      `Prelude.hashWithSalt` includeDiscount
      `Prelude.hashWithSalt` includeOtherSubscription
      `Prelude.hashWithSalt` includeRecurring
      `Prelude.hashWithSalt` includeRefund
      `Prelude.hashWithSalt` includeSubscription
      `Prelude.hashWithSalt` includeSupport
      `Prelude.hashWithSalt` includeTax
      `Prelude.hashWithSalt` includeUpfront
      `Prelude.hashWithSalt` useAmortized
      `Prelude.hashWithSalt` useBlended

instance Prelude.NFData CostTypes where
  rnf CostTypes' {..} =
    Prelude.rnf includeCredit
      `Prelude.seq` Prelude.rnf includeDiscount
      `Prelude.seq` Prelude.rnf includeOtherSubscription
      `Prelude.seq` Prelude.rnf includeRecurring
      `Prelude.seq` Prelude.rnf includeRefund
      `Prelude.seq` Prelude.rnf includeSubscription
      `Prelude.seq` Prelude.rnf includeSupport
      `Prelude.seq` Prelude.rnf includeTax
      `Prelude.seq` Prelude.rnf includeUpfront
      `Prelude.seq` Prelude.rnf useAmortized
      `Prelude.seq` Prelude.rnf useBlended

instance Data.ToJSON CostTypes where
  toJSON CostTypes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IncludeCredit" Data..=) Prelude.<$> includeCredit,
            ("IncludeDiscount" Data..=)
              Prelude.<$> includeDiscount,
            ("IncludeOtherSubscription" Data..=)
              Prelude.<$> includeOtherSubscription,
            ("IncludeRecurring" Data..=)
              Prelude.<$> includeRecurring,
            ("IncludeRefund" Data..=) Prelude.<$> includeRefund,
            ("IncludeSubscription" Data..=)
              Prelude.<$> includeSubscription,
            ("IncludeSupport" Data..=)
              Prelude.<$> includeSupport,
            ("IncludeTax" Data..=) Prelude.<$> includeTax,
            ("IncludeUpfront" Data..=)
              Prelude.<$> includeUpfront,
            ("UseAmortized" Data..=) Prelude.<$> useAmortized,
            ("UseBlended" Data..=) Prelude.<$> useBlended
          ]
      )
