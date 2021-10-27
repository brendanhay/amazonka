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
-- Module      : Network.AWS.SavingsPlans.Types.SavingsPlanRate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Types.SavingsPlanRate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SavingsPlans.Types.CurrencyCode
import Network.AWS.SavingsPlans.Types.SavingsPlanProductType
import Network.AWS.SavingsPlans.Types.SavingsPlanRateProperty
import Network.AWS.SavingsPlans.Types.SavingsPlanRateServiceCode
import Network.AWS.SavingsPlans.Types.SavingsPlanRateUnit

-- | Information about a Savings Plan rate.
--
-- /See:/ 'newSavingsPlanRate' smart constructor.
data SavingsPlanRate = SavingsPlanRate'
  { -- | The specific AWS operation for the line item in the billing report.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The usage details of the line item in the billing report.
    usageType :: Prelude.Maybe Prelude.Text,
    -- | The product type.
    productType :: Prelude.Maybe SavingsPlanProductType,
    -- | The currency.
    currency :: Prelude.Maybe CurrencyCode,
    -- | The rate.
    rate :: Prelude.Maybe Prelude.Text,
    -- | The service.
    serviceCode :: Prelude.Maybe SavingsPlanRateServiceCode,
    -- | The unit.
    unit :: Prelude.Maybe SavingsPlanRateUnit,
    -- | The properties.
    properties :: Prelude.Maybe [SavingsPlanRateProperty]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanRate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'savingsPlanRate_operation' - The specific AWS operation for the line item in the billing report.
--
-- 'usageType', 'savingsPlanRate_usageType' - The usage details of the line item in the billing report.
--
-- 'productType', 'savingsPlanRate_productType' - The product type.
--
-- 'currency', 'savingsPlanRate_currency' - The currency.
--
-- 'rate', 'savingsPlanRate_rate' - The rate.
--
-- 'serviceCode', 'savingsPlanRate_serviceCode' - The service.
--
-- 'unit', 'savingsPlanRate_unit' - The unit.
--
-- 'properties', 'savingsPlanRate_properties' - The properties.
newSavingsPlanRate ::
  SavingsPlanRate
newSavingsPlanRate =
  SavingsPlanRate'
    { operation = Prelude.Nothing,
      usageType = Prelude.Nothing,
      productType = Prelude.Nothing,
      currency = Prelude.Nothing,
      rate = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      unit = Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- | The specific AWS operation for the line item in the billing report.
savingsPlanRate_operation :: Lens.Lens' SavingsPlanRate (Prelude.Maybe Prelude.Text)
savingsPlanRate_operation = Lens.lens (\SavingsPlanRate' {operation} -> operation) (\s@SavingsPlanRate' {} a -> s {operation = a} :: SavingsPlanRate)

-- | The usage details of the line item in the billing report.
savingsPlanRate_usageType :: Lens.Lens' SavingsPlanRate (Prelude.Maybe Prelude.Text)
savingsPlanRate_usageType = Lens.lens (\SavingsPlanRate' {usageType} -> usageType) (\s@SavingsPlanRate' {} a -> s {usageType = a} :: SavingsPlanRate)

-- | The product type.
savingsPlanRate_productType :: Lens.Lens' SavingsPlanRate (Prelude.Maybe SavingsPlanProductType)
savingsPlanRate_productType = Lens.lens (\SavingsPlanRate' {productType} -> productType) (\s@SavingsPlanRate' {} a -> s {productType = a} :: SavingsPlanRate)

-- | The currency.
savingsPlanRate_currency :: Lens.Lens' SavingsPlanRate (Prelude.Maybe CurrencyCode)
savingsPlanRate_currency = Lens.lens (\SavingsPlanRate' {currency} -> currency) (\s@SavingsPlanRate' {} a -> s {currency = a} :: SavingsPlanRate)

-- | The rate.
savingsPlanRate_rate :: Lens.Lens' SavingsPlanRate (Prelude.Maybe Prelude.Text)
savingsPlanRate_rate = Lens.lens (\SavingsPlanRate' {rate} -> rate) (\s@SavingsPlanRate' {} a -> s {rate = a} :: SavingsPlanRate)

-- | The service.
savingsPlanRate_serviceCode :: Lens.Lens' SavingsPlanRate (Prelude.Maybe SavingsPlanRateServiceCode)
savingsPlanRate_serviceCode = Lens.lens (\SavingsPlanRate' {serviceCode} -> serviceCode) (\s@SavingsPlanRate' {} a -> s {serviceCode = a} :: SavingsPlanRate)

-- | The unit.
savingsPlanRate_unit :: Lens.Lens' SavingsPlanRate (Prelude.Maybe SavingsPlanRateUnit)
savingsPlanRate_unit = Lens.lens (\SavingsPlanRate' {unit} -> unit) (\s@SavingsPlanRate' {} a -> s {unit = a} :: SavingsPlanRate)

-- | The properties.
savingsPlanRate_properties :: Lens.Lens' SavingsPlanRate (Prelude.Maybe [SavingsPlanRateProperty])
savingsPlanRate_properties = Lens.lens (\SavingsPlanRate' {properties} -> properties) (\s@SavingsPlanRate' {} a -> s {properties = a} :: SavingsPlanRate) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SavingsPlanRate where
  parseJSON =
    Core.withObject
      "SavingsPlanRate"
      ( \x ->
          SavingsPlanRate'
            Prelude.<$> (x Core..:? "operation")
            Prelude.<*> (x Core..:? "usageType")
            Prelude.<*> (x Core..:? "productType")
            Prelude.<*> (x Core..:? "currency")
            Prelude.<*> (x Core..:? "rate")
            Prelude.<*> (x Core..:? "serviceCode")
            Prelude.<*> (x Core..:? "unit")
            Prelude.<*> (x Core..:? "properties" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SavingsPlanRate

instance Prelude.NFData SavingsPlanRate
