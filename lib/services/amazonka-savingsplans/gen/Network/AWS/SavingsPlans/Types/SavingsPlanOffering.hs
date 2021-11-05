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
-- Module      : Network.AWS.SavingsPlans.Types.SavingsPlanOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SavingsPlans.Types.SavingsPlanOffering where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SavingsPlans.Types.CurrencyCode
import Network.AWS.SavingsPlans.Types.SavingsPlanOfferingProperty
import Network.AWS.SavingsPlans.Types.SavingsPlanPaymentOption
import Network.AWS.SavingsPlans.Types.SavingsPlanProductType
import Network.AWS.SavingsPlans.Types.SavingsPlanType

-- | Information about a Savings Plan offering.
--
-- /See:/ 'newSavingsPlanOffering' smart constructor.
data SavingsPlanOffering = SavingsPlanOffering'
  { -- | The specific AWS operation for the line item in the billing report.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The usage details of the line item in the billing report.
    usageType :: Prelude.Maybe Prelude.Text,
    -- | The product type.
    productTypes :: Prelude.Maybe [SavingsPlanProductType],
    -- | The currency.
    currency :: Prelude.Maybe CurrencyCode,
    -- | The duration, in seconds.
    durationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the offering.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | The service.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The plan type.
    planType :: Prelude.Maybe SavingsPlanType,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The payment option.
    paymentOption :: Prelude.Maybe SavingsPlanPaymentOption,
    -- | The properties.
    properties :: Prelude.Maybe [SavingsPlanOfferingProperty]
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
-- 'operation', 'savingsPlanOffering_operation' - The specific AWS operation for the line item in the billing report.
--
-- 'usageType', 'savingsPlanOffering_usageType' - The usage details of the line item in the billing report.
--
-- 'productTypes', 'savingsPlanOffering_productTypes' - The product type.
--
-- 'currency', 'savingsPlanOffering_currency' - The currency.
--
-- 'durationSeconds', 'savingsPlanOffering_durationSeconds' - The duration, in seconds.
--
-- 'offeringId', 'savingsPlanOffering_offeringId' - The ID of the offering.
--
-- 'serviceCode', 'savingsPlanOffering_serviceCode' - The service.
--
-- 'planType', 'savingsPlanOffering_planType' - The plan type.
--
-- 'description', 'savingsPlanOffering_description' - The description.
--
-- 'paymentOption', 'savingsPlanOffering_paymentOption' - The payment option.
--
-- 'properties', 'savingsPlanOffering_properties' - The properties.
newSavingsPlanOffering ::
  SavingsPlanOffering
newSavingsPlanOffering =
  SavingsPlanOffering'
    { operation = Prelude.Nothing,
      usageType = Prelude.Nothing,
      productTypes = Prelude.Nothing,
      currency = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      planType = Prelude.Nothing,
      description = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- | The specific AWS operation for the line item in the billing report.
savingsPlanOffering_operation :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_operation = Lens.lens (\SavingsPlanOffering' {operation} -> operation) (\s@SavingsPlanOffering' {} a -> s {operation = a} :: SavingsPlanOffering)

-- | The usage details of the line item in the billing report.
savingsPlanOffering_usageType :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_usageType = Lens.lens (\SavingsPlanOffering' {usageType} -> usageType) (\s@SavingsPlanOffering' {} a -> s {usageType = a} :: SavingsPlanOffering)

-- | The product type.
savingsPlanOffering_productTypes :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe [SavingsPlanProductType])
savingsPlanOffering_productTypes = Lens.lens (\SavingsPlanOffering' {productTypes} -> productTypes) (\s@SavingsPlanOffering' {} a -> s {productTypes = a} :: SavingsPlanOffering) Prelude.. Lens.mapping Lens.coerced

-- | The currency.
savingsPlanOffering_currency :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe CurrencyCode)
savingsPlanOffering_currency = Lens.lens (\SavingsPlanOffering' {currency} -> currency) (\s@SavingsPlanOffering' {} a -> s {currency = a} :: SavingsPlanOffering)

-- | The duration, in seconds.
savingsPlanOffering_durationSeconds :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Natural)
savingsPlanOffering_durationSeconds = Lens.lens (\SavingsPlanOffering' {durationSeconds} -> durationSeconds) (\s@SavingsPlanOffering' {} a -> s {durationSeconds = a} :: SavingsPlanOffering)

-- | The ID of the offering.
savingsPlanOffering_offeringId :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_offeringId = Lens.lens (\SavingsPlanOffering' {offeringId} -> offeringId) (\s@SavingsPlanOffering' {} a -> s {offeringId = a} :: SavingsPlanOffering)

-- | The service.
savingsPlanOffering_serviceCode :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_serviceCode = Lens.lens (\SavingsPlanOffering' {serviceCode} -> serviceCode) (\s@SavingsPlanOffering' {} a -> s {serviceCode = a} :: SavingsPlanOffering)

-- | The plan type.
savingsPlanOffering_planType :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe SavingsPlanType)
savingsPlanOffering_planType = Lens.lens (\SavingsPlanOffering' {planType} -> planType) (\s@SavingsPlanOffering' {} a -> s {planType = a} :: SavingsPlanOffering)

-- | The description.
savingsPlanOffering_description :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe Prelude.Text)
savingsPlanOffering_description = Lens.lens (\SavingsPlanOffering' {description} -> description) (\s@SavingsPlanOffering' {} a -> s {description = a} :: SavingsPlanOffering)

-- | The payment option.
savingsPlanOffering_paymentOption :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe SavingsPlanPaymentOption)
savingsPlanOffering_paymentOption = Lens.lens (\SavingsPlanOffering' {paymentOption} -> paymentOption) (\s@SavingsPlanOffering' {} a -> s {paymentOption = a} :: SavingsPlanOffering)

-- | The properties.
savingsPlanOffering_properties :: Lens.Lens' SavingsPlanOffering (Prelude.Maybe [SavingsPlanOfferingProperty])
savingsPlanOffering_properties = Lens.lens (\SavingsPlanOffering' {properties} -> properties) (\s@SavingsPlanOffering' {} a -> s {properties = a} :: SavingsPlanOffering) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SavingsPlanOffering where
  parseJSON =
    Core.withObject
      "SavingsPlanOffering"
      ( \x ->
          SavingsPlanOffering'
            Prelude.<$> (x Core..:? "operation")
            Prelude.<*> (x Core..:? "usageType")
            Prelude.<*> (x Core..:? "productTypes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "currency")
            Prelude.<*> (x Core..:? "durationSeconds")
            Prelude.<*> (x Core..:? "offeringId")
            Prelude.<*> (x Core..:? "serviceCode")
            Prelude.<*> (x Core..:? "planType")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "paymentOption")
            Prelude.<*> (x Core..:? "properties" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SavingsPlanOffering

instance Prelude.NFData SavingsPlanOffering
