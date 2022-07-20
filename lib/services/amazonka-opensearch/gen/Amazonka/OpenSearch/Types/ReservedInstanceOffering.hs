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
-- Module      : Amazonka.OpenSearch.Types.ReservedInstanceOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ReservedInstanceOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.RecurringCharge
import Amazonka.OpenSearch.Types.ReservedInstancePaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Details of a reserved OpenSearch instance offering.
--
-- /See:/ 'newReservedInstanceOffering' smart constructor.
data ReservedInstanceOffering = ReservedInstanceOffering'
  { -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The OpenSearch instance type offered by the reserved instance offering.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | The duration, in seconds, for which the offering will reserve the
    -- OpenSearch instance.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The currency code for the reserved OpenSearch instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The upfront fixed charge you will pay to purchase the specific reserved
    -- OpenSearch instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The OpenSearch reserved instance offering identifier.
    reservedInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | Payment option for the reserved OpenSearch instance offering
    paymentOption :: Prelude.Maybe ReservedInstancePaymentOption,
    -- | The rate you are charged for each hour the domain that is using the
    -- offering is running.
    usagePrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstanceOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recurringCharges', 'reservedInstanceOffering_recurringCharges' - The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
--
-- 'instanceType', 'reservedInstanceOffering_instanceType' - The OpenSearch instance type offered by the reserved instance offering.
--
-- 'duration', 'reservedInstanceOffering_duration' - The duration, in seconds, for which the offering will reserve the
-- OpenSearch instance.
--
-- 'currencyCode', 'reservedInstanceOffering_currencyCode' - The currency code for the reserved OpenSearch instance offering.
--
-- 'fixedPrice', 'reservedInstanceOffering_fixedPrice' - The upfront fixed charge you will pay to purchase the specific reserved
-- OpenSearch instance offering.
--
-- 'reservedInstanceOfferingId', 'reservedInstanceOffering_reservedInstanceOfferingId' - The OpenSearch reserved instance offering identifier.
--
-- 'paymentOption', 'reservedInstanceOffering_paymentOption' - Payment option for the reserved OpenSearch instance offering
--
-- 'usagePrice', 'reservedInstanceOffering_usagePrice' - The rate you are charged for each hour the domain that is using the
-- offering is running.
newReservedInstanceOffering ::
  ReservedInstanceOffering
newReservedInstanceOffering =
  ReservedInstanceOffering'
    { recurringCharges =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      reservedInstanceOfferingId = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedInstanceOffering_recurringCharges :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe [RecurringCharge])
reservedInstanceOffering_recurringCharges = Lens.lens (\ReservedInstanceOffering' {recurringCharges} -> recurringCharges) (\s@ReservedInstanceOffering' {} a -> s {recurringCharges = a} :: ReservedInstanceOffering) Prelude.. Lens.mapping Lens.coerced

-- | The OpenSearch instance type offered by the reserved instance offering.
reservedInstanceOffering_instanceType :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe OpenSearchPartitionInstanceType)
reservedInstanceOffering_instanceType = Lens.lens (\ReservedInstanceOffering' {instanceType} -> instanceType) (\s@ReservedInstanceOffering' {} a -> s {instanceType = a} :: ReservedInstanceOffering)

-- | The duration, in seconds, for which the offering will reserve the
-- OpenSearch instance.
reservedInstanceOffering_duration :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Int)
reservedInstanceOffering_duration = Lens.lens (\ReservedInstanceOffering' {duration} -> duration) (\s@ReservedInstanceOffering' {} a -> s {duration = a} :: ReservedInstanceOffering)

-- | The currency code for the reserved OpenSearch instance offering.
reservedInstanceOffering_currencyCode :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Text)
reservedInstanceOffering_currencyCode = Lens.lens (\ReservedInstanceOffering' {currencyCode} -> currencyCode) (\s@ReservedInstanceOffering' {} a -> s {currencyCode = a} :: ReservedInstanceOffering)

-- | The upfront fixed charge you will pay to purchase the specific reserved
-- OpenSearch instance offering.
reservedInstanceOffering_fixedPrice :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Double)
reservedInstanceOffering_fixedPrice = Lens.lens (\ReservedInstanceOffering' {fixedPrice} -> fixedPrice) (\s@ReservedInstanceOffering' {} a -> s {fixedPrice = a} :: ReservedInstanceOffering)

-- | The OpenSearch reserved instance offering identifier.
reservedInstanceOffering_reservedInstanceOfferingId :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Text)
reservedInstanceOffering_reservedInstanceOfferingId = Lens.lens (\ReservedInstanceOffering' {reservedInstanceOfferingId} -> reservedInstanceOfferingId) (\s@ReservedInstanceOffering' {} a -> s {reservedInstanceOfferingId = a} :: ReservedInstanceOffering)

-- | Payment option for the reserved OpenSearch instance offering
reservedInstanceOffering_paymentOption :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe ReservedInstancePaymentOption)
reservedInstanceOffering_paymentOption = Lens.lens (\ReservedInstanceOffering' {paymentOption} -> paymentOption) (\s@ReservedInstanceOffering' {} a -> s {paymentOption = a} :: ReservedInstanceOffering)

-- | The rate you are charged for each hour the domain that is using the
-- offering is running.
reservedInstanceOffering_usagePrice :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Double)
reservedInstanceOffering_usagePrice = Lens.lens (\ReservedInstanceOffering' {usagePrice} -> usagePrice) (\s@ReservedInstanceOffering' {} a -> s {usagePrice = a} :: ReservedInstanceOffering)

instance Core.FromJSON ReservedInstanceOffering where
  parseJSON =
    Core.withObject
      "ReservedInstanceOffering"
      ( \x ->
          ReservedInstanceOffering'
            Prelude.<$> ( x Core..:? "RecurringCharges"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "Duration")
            Prelude.<*> (x Core..:? "CurrencyCode")
            Prelude.<*> (x Core..:? "FixedPrice")
            Prelude.<*> (x Core..:? "ReservedInstanceOfferingId")
            Prelude.<*> (x Core..:? "PaymentOption")
            Prelude.<*> (x Core..:? "UsagePrice")
      )

instance Prelude.Hashable ReservedInstanceOffering where
  hashWithSalt _salt ReservedInstanceOffering' {..} =
    _salt `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` reservedInstanceOfferingId
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedInstanceOffering where
  rnf ReservedInstanceOffering' {..} =
    Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf reservedInstanceOfferingId
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf usagePrice
