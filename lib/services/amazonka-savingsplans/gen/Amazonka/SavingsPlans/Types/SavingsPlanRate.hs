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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanRate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanRate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.CurrencyCode
import Amazonka.SavingsPlans.Types.SavingsPlanProductType
import Amazonka.SavingsPlans.Types.SavingsPlanRateProperty
import Amazonka.SavingsPlans.Types.SavingsPlanRateServiceCode
import Amazonka.SavingsPlans.Types.SavingsPlanRateUnit

-- | Information about a Savings Plan rate.
--
-- /See:/ 'newSavingsPlanRate' smart constructor.
data SavingsPlanRate = SavingsPlanRate'
  { -- | The currency.
    currency :: Prelude.Maybe CurrencyCode,
    -- | The specific AWS operation for the line item in the billing report.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The product type.
    productType :: Prelude.Maybe SavingsPlanProductType,
    -- | The properties.
    properties :: Prelude.Maybe [SavingsPlanRateProperty],
    -- | The rate.
    rate :: Prelude.Maybe Prelude.Text,
    -- | The service.
    serviceCode :: Prelude.Maybe SavingsPlanRateServiceCode,
    -- | The unit.
    unit :: Prelude.Maybe SavingsPlanRateUnit,
    -- | The usage details of the line item in the billing report.
    usageType :: Prelude.Maybe Prelude.Text
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
-- 'currency', 'savingsPlanRate_currency' - The currency.
--
-- 'operation', 'savingsPlanRate_operation' - The specific AWS operation for the line item in the billing report.
--
-- 'productType', 'savingsPlanRate_productType' - The product type.
--
-- 'properties', 'savingsPlanRate_properties' - The properties.
--
-- 'rate', 'savingsPlanRate_rate' - The rate.
--
-- 'serviceCode', 'savingsPlanRate_serviceCode' - The service.
--
-- 'unit', 'savingsPlanRate_unit' - The unit.
--
-- 'usageType', 'savingsPlanRate_usageType' - The usage details of the line item in the billing report.
newSavingsPlanRate ::
  SavingsPlanRate
newSavingsPlanRate =
  SavingsPlanRate'
    { currency = Prelude.Nothing,
      operation = Prelude.Nothing,
      productType = Prelude.Nothing,
      properties = Prelude.Nothing,
      rate = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      unit = Prelude.Nothing,
      usageType = Prelude.Nothing
    }

-- | The currency.
savingsPlanRate_currency :: Lens.Lens' SavingsPlanRate (Prelude.Maybe CurrencyCode)
savingsPlanRate_currency = Lens.lens (\SavingsPlanRate' {currency} -> currency) (\s@SavingsPlanRate' {} a -> s {currency = a} :: SavingsPlanRate)

-- | The specific AWS operation for the line item in the billing report.
savingsPlanRate_operation :: Lens.Lens' SavingsPlanRate (Prelude.Maybe Prelude.Text)
savingsPlanRate_operation = Lens.lens (\SavingsPlanRate' {operation} -> operation) (\s@SavingsPlanRate' {} a -> s {operation = a} :: SavingsPlanRate)

-- | The product type.
savingsPlanRate_productType :: Lens.Lens' SavingsPlanRate (Prelude.Maybe SavingsPlanProductType)
savingsPlanRate_productType = Lens.lens (\SavingsPlanRate' {productType} -> productType) (\s@SavingsPlanRate' {} a -> s {productType = a} :: SavingsPlanRate)

-- | The properties.
savingsPlanRate_properties :: Lens.Lens' SavingsPlanRate (Prelude.Maybe [SavingsPlanRateProperty])
savingsPlanRate_properties = Lens.lens (\SavingsPlanRate' {properties} -> properties) (\s@SavingsPlanRate' {} a -> s {properties = a} :: SavingsPlanRate) Prelude.. Lens.mapping Lens.coerced

-- | The rate.
savingsPlanRate_rate :: Lens.Lens' SavingsPlanRate (Prelude.Maybe Prelude.Text)
savingsPlanRate_rate = Lens.lens (\SavingsPlanRate' {rate} -> rate) (\s@SavingsPlanRate' {} a -> s {rate = a} :: SavingsPlanRate)

-- | The service.
savingsPlanRate_serviceCode :: Lens.Lens' SavingsPlanRate (Prelude.Maybe SavingsPlanRateServiceCode)
savingsPlanRate_serviceCode = Lens.lens (\SavingsPlanRate' {serviceCode} -> serviceCode) (\s@SavingsPlanRate' {} a -> s {serviceCode = a} :: SavingsPlanRate)

-- | The unit.
savingsPlanRate_unit :: Lens.Lens' SavingsPlanRate (Prelude.Maybe SavingsPlanRateUnit)
savingsPlanRate_unit = Lens.lens (\SavingsPlanRate' {unit} -> unit) (\s@SavingsPlanRate' {} a -> s {unit = a} :: SavingsPlanRate)

-- | The usage details of the line item in the billing report.
savingsPlanRate_usageType :: Lens.Lens' SavingsPlanRate (Prelude.Maybe Prelude.Text)
savingsPlanRate_usageType = Lens.lens (\SavingsPlanRate' {usageType} -> usageType) (\s@SavingsPlanRate' {} a -> s {usageType = a} :: SavingsPlanRate)

instance Data.FromJSON SavingsPlanRate where
  parseJSON =
    Data.withObject
      "SavingsPlanRate"
      ( \x ->
          SavingsPlanRate'
            Prelude.<$> (x Data..:? "currency")
            Prelude.<*> (x Data..:? "operation")
            Prelude.<*> (x Data..:? "productType")
            Prelude.<*> (x Data..:? "properties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "rate")
            Prelude.<*> (x Data..:? "serviceCode")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..:? "usageType")
      )

instance Prelude.Hashable SavingsPlanRate where
  hashWithSalt _salt SavingsPlanRate' {..} =
    _salt
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` rate
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` usageType

instance Prelude.NFData SavingsPlanRate where
  rnf SavingsPlanRate' {..} =
    Prelude.rnf currency
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf rate
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf usageType
