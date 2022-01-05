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
-- Module      : Amazonka.SavingsPlans.Types.SavingsPlanOfferingRate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SavingsPlans.Types.SavingsPlanOfferingRate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SavingsPlans.Types.ParentSavingsPlanOffering
import Amazonka.SavingsPlans.Types.SavingsPlanOfferingRateProperty
import Amazonka.SavingsPlans.Types.SavingsPlanProductType
import Amazonka.SavingsPlans.Types.SavingsPlanRateServiceCode
import Amazonka.SavingsPlans.Types.SavingsPlanRateUnit

-- | Information about a Savings Plan offering rate.
--
-- /See:/ 'newSavingsPlanOfferingRate' smart constructor.
data SavingsPlanOfferingRate = SavingsPlanOfferingRate'
  { -- | The specific AWS operation for the line item in the billing report.
    operation :: Prelude.Maybe Prelude.Text,
    -- | The usage details of the line item in the billing report.
    usageType :: Prelude.Maybe Prelude.Text,
    -- | The product type.
    productType :: Prelude.Maybe SavingsPlanProductType,
    -- | The Savings Plan offering.
    savingsPlanOffering :: Prelude.Maybe ParentSavingsPlanOffering,
    -- | The Savings Plan rate.
    rate :: Prelude.Maybe Prelude.Text,
    -- | The service.
    serviceCode :: Prelude.Maybe SavingsPlanRateServiceCode,
    -- | The unit.
    unit :: Prelude.Maybe SavingsPlanRateUnit,
    -- | The properties.
    properties :: Prelude.Maybe [SavingsPlanOfferingRateProperty]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlanOfferingRate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operation', 'savingsPlanOfferingRate_operation' - The specific AWS operation for the line item in the billing report.
--
-- 'usageType', 'savingsPlanOfferingRate_usageType' - The usage details of the line item in the billing report.
--
-- 'productType', 'savingsPlanOfferingRate_productType' - The product type.
--
-- 'savingsPlanOffering', 'savingsPlanOfferingRate_savingsPlanOffering' - The Savings Plan offering.
--
-- 'rate', 'savingsPlanOfferingRate_rate' - The Savings Plan rate.
--
-- 'serviceCode', 'savingsPlanOfferingRate_serviceCode' - The service.
--
-- 'unit', 'savingsPlanOfferingRate_unit' - The unit.
--
-- 'properties', 'savingsPlanOfferingRate_properties' - The properties.
newSavingsPlanOfferingRate ::
  SavingsPlanOfferingRate
newSavingsPlanOfferingRate =
  SavingsPlanOfferingRate'
    { operation =
        Prelude.Nothing,
      usageType = Prelude.Nothing,
      productType = Prelude.Nothing,
      savingsPlanOffering = Prelude.Nothing,
      rate = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      unit = Prelude.Nothing,
      properties = Prelude.Nothing
    }

-- | The specific AWS operation for the line item in the billing report.
savingsPlanOfferingRate_operation :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe Prelude.Text)
savingsPlanOfferingRate_operation = Lens.lens (\SavingsPlanOfferingRate' {operation} -> operation) (\s@SavingsPlanOfferingRate' {} a -> s {operation = a} :: SavingsPlanOfferingRate)

-- | The usage details of the line item in the billing report.
savingsPlanOfferingRate_usageType :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe Prelude.Text)
savingsPlanOfferingRate_usageType = Lens.lens (\SavingsPlanOfferingRate' {usageType} -> usageType) (\s@SavingsPlanOfferingRate' {} a -> s {usageType = a} :: SavingsPlanOfferingRate)

-- | The product type.
savingsPlanOfferingRate_productType :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe SavingsPlanProductType)
savingsPlanOfferingRate_productType = Lens.lens (\SavingsPlanOfferingRate' {productType} -> productType) (\s@SavingsPlanOfferingRate' {} a -> s {productType = a} :: SavingsPlanOfferingRate)

-- | The Savings Plan offering.
savingsPlanOfferingRate_savingsPlanOffering :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe ParentSavingsPlanOffering)
savingsPlanOfferingRate_savingsPlanOffering = Lens.lens (\SavingsPlanOfferingRate' {savingsPlanOffering} -> savingsPlanOffering) (\s@SavingsPlanOfferingRate' {} a -> s {savingsPlanOffering = a} :: SavingsPlanOfferingRate)

-- | The Savings Plan rate.
savingsPlanOfferingRate_rate :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe Prelude.Text)
savingsPlanOfferingRate_rate = Lens.lens (\SavingsPlanOfferingRate' {rate} -> rate) (\s@SavingsPlanOfferingRate' {} a -> s {rate = a} :: SavingsPlanOfferingRate)

-- | The service.
savingsPlanOfferingRate_serviceCode :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe SavingsPlanRateServiceCode)
savingsPlanOfferingRate_serviceCode = Lens.lens (\SavingsPlanOfferingRate' {serviceCode} -> serviceCode) (\s@SavingsPlanOfferingRate' {} a -> s {serviceCode = a} :: SavingsPlanOfferingRate)

-- | The unit.
savingsPlanOfferingRate_unit :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe SavingsPlanRateUnit)
savingsPlanOfferingRate_unit = Lens.lens (\SavingsPlanOfferingRate' {unit} -> unit) (\s@SavingsPlanOfferingRate' {} a -> s {unit = a} :: SavingsPlanOfferingRate)

-- | The properties.
savingsPlanOfferingRate_properties :: Lens.Lens' SavingsPlanOfferingRate (Prelude.Maybe [SavingsPlanOfferingRateProperty])
savingsPlanOfferingRate_properties = Lens.lens (\SavingsPlanOfferingRate' {properties} -> properties) (\s@SavingsPlanOfferingRate' {} a -> s {properties = a} :: SavingsPlanOfferingRate) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SavingsPlanOfferingRate where
  parseJSON =
    Core.withObject
      "SavingsPlanOfferingRate"
      ( \x ->
          SavingsPlanOfferingRate'
            Prelude.<$> (x Core..:? "operation")
            Prelude.<*> (x Core..:? "usageType")
            Prelude.<*> (x Core..:? "productType")
            Prelude.<*> (x Core..:? "savingsPlanOffering")
            Prelude.<*> (x Core..:? "rate")
            Prelude.<*> (x Core..:? "serviceCode")
            Prelude.<*> (x Core..:? "unit")
            Prelude.<*> (x Core..:? "properties" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SavingsPlanOfferingRate where
  hashWithSalt _salt SavingsPlanOfferingRate' {..} =
    _salt `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` usageType
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` savingsPlanOffering
      `Prelude.hashWithSalt` rate
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` properties

instance Prelude.NFData SavingsPlanOfferingRate where
  rnf SavingsPlanOfferingRate' {..} =
    Prelude.rnf operation
      `Prelude.seq` Prelude.rnf usageType
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf savingsPlanOffering
      `Prelude.seq` Prelude.rnf rate
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf properties
