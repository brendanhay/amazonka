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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.CurrencyCode
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingProperty
import Amazonka.SavingsPlans.Types.SavingsPlanPaymentOption
import Amazonka.SavingsPlans.Types.SavingsPlanProductType
import Amazonka.SavingsPlans.Types.SavingsPlanType

-- | Information about a Savings Plan offering.
--
-- /See:/ 'newSavingsPlanOffering' smart constructor.
data SavingsPlanOffering = SavingsPlanOffering'
  { -- | The currency.
    currency :: Prelude.Maybe CurrencyCode,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The duration, in seconds.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The specific AWS operation for the line item in the billing report.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The payment option.
    paymentOption :: Prelude.Maybe SavingsPlanPaymentOption,
    -- | The plan type.
    planType :: Prelude.Maybe SavingsPlanType,
    -- | The product type.
    productTypes :: Prelude.Maybe [SavingsPlanProductType],
    -- | The properties.
    properties :: Prelude.Maybe [SavingsPlanOfferingProperty],
    -- | The service.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The usage details of the line item in the billing report.
    usageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currency', 'savingsPlanOffering_currency' - The currency.
--
-- 'description', 'savingsPlanOffering_description' - The description.
--
-- 'durationSeconds', 'savingsPlanOffering_durationSeconds' - The duration, in seconds.
--
-- 'offeringId', 'savingsPlanOffering_offeringId' - The ID of the offering.
--
-- 'operation', 'savingsPlanOffering_operation' - The specific AWS operation for the line item in the billing report.
--
-- 'paymentOption', 'savingsPlanOffering_paymentOption' - The payment option.
--
-- 'planType', 'savingsPlanOffering_planType' - The plan type.
--
-- 'productTypes', 'savingsPlanOffering_productTypes' - The product type.
--
-- 'properties', 'savingsPlanOffering_properties' - The properties.
--
-- 'serviceCode', 'savingsPlanOffering_serviceCode' - The service.
--
-- 'usageType', 'savingsPlanOffering_usageType' - The usage details of the line item in the billing report.
newSavingsPlanOffering ::
  SavingsPlanOffering
newSavingsPlanOffering =
  SavingsPlanOffering'
    { currency = Prelude.Nothing,
      description = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      operation = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      planType = Prelude.Nothing,
      productTypes = Prelude.Nothing,
      properties = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      usageType = Prelude.Nothing
    }

-- | The currency.
savingsPlanOffering_currency :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe CurrencyCode)
savingsPlanOffering_currency = Lens.lens (\SavingsPlanOffering' {currency} -> currency) (\s@SavingsPlanOffering' {} a -> s {currency = a} :: SavingsPlanOffering)

-- | The description.
savingsPlanOffering_description :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_description = Lens.lens (\SavingsPlanOffering' {description} -> description) (\s@SavingsPlanOffering' {} a -> s {description = a} :: SavingsPlanOffering)

-- | The duration, in seconds.
savingsPlanOffering_durationSeconds :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Natural)
savingsPlanOffering_durationSeconds = Lens.lens (\SavingsPlanOffering' {durationSeconds} -> durationSeconds) (\s@SavingsPlanOffering' {} a -> s {durationSeconds = a} :: SavingsPlanOffering)

-- | The ID of the offering.
savingsPlanOffering_offeringId :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_offeringId = Lens.lens (\SavingsPlanOffering' {offeringId} -> offeringId) (\s@SavingsPlanOffering' {} a -> s {offeringId = a} :: SavingsPlanOffering)

-- | The specific AWS operation for the line item in the billing report.
savingsPlanOffering_operation :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_operation = Lens.lens (\SavingsPlanOffering' {operation} -> operation) (\s@SavingsPlanOffering' {} a -> s {operation = a} :: SavingsPlanOffering)

-- | The payment option.
savingsPlanOffering_paymentOption :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe SavingsPlanPaymentOption)
savingsPlanOffering_paymentOption = Lens.lens (\SavingsPlanOffering' {paymentOption} -> paymentOption) (\s@SavingsPlanOffering' {} a -> s {paymentOption = a} :: SavingsPlanOffering)

-- | The plan type.
savingsPlanOffering_planType :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe SavingsPlanType)
savingsPlanOffering_planType = Lens.lens (\SavingsPlanOffering' {planType} -> planType) (\s@SavingsPlanOffering' {} a -> s {planType = a} :: SavingsPlanOffering)

-- | The product type.
savingsPlanOffering_productTypes :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe [SavingsPlanProductType])
savingsPlanOffering_productTypes = Lens.lens (\SavingsPlanOffering' {productTypes} -> productTypes) (\s@SavingsPlanOffering' {} a -> s {productTypes = a} :: SavingsPlanOffering) Prelude.. Lens.mapping Lens.coerced

-- | The properties.
savingsPlanOffering_properties :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe [SavingsPlanOfferingProperty])
savingsPlanOffering_properties = Lens.lens (\SavingsPlanOffering' {properties} -> properties) (\s@SavingsPlanOffering' {} a -> s {properties = a} :: SavingsPlanOffering) Prelude.. Lens.mapping Lens.coerced

-- | The service.
savingsPlanOffering_serviceCode :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_serviceCode = Lens.lens (\SavingsPlanOffering' {serviceCode} -> serviceCode) (\s@SavingsPlanOffering' {} a -> s {serviceCode = a} :: SavingsPlanOffering)

-- | The usage details of the line item in the billing report.
savingsPlanOffering_usageType :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_usageType = Lens.lens (\SavingsPlanOffering' {usageType} -> usageType) (\s@SavingsPlanOffering' {} a -> s {usageType = a} :: SavingsPlanOffering)

instance Data.FromJSON SavingsPlanOffering where
  parseJSON =
    Data.withObject
      "SavingsPlanOffering"
      ( \x ->
          SavingsPlanOffering'
            Prelude.<$> (x Data..:? "currency")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "durationSeconds")
            Prelude.<*> (x Data..:? "offeringId")
            Prelude.<*> (x Data..:? "operation")
            Prelude.<*> (x Data..:? "paymentOption")
            Prelude.<*> (x Data..:? "planType")
            Prelude.<*> (x Data..:? "productTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "properties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "serviceCode")
            Prelude.<*> (x Data..:? "usageType")
      )

instance Prelude.Hashable SavingsPlanOffering where
  hashWithSalt _salt SavingsPlanOffering' {..} =
    _salt
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` offeringId
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` planType
      `Prelude.hashWithSalt` productTypes
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` usageType

instance Prelude.NFData SavingsPlanOffering where
  rnf SavingsPlanOffering' {..} =
    Prelude.rnf currency
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf planType
      `Prelude.seq` Prelude.rnf productTypes
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf usageType
