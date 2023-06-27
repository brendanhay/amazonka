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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ReservedInstanceOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.RecurringCharge
import Amazonka.OpenSearch.Types.ReservedInstancePaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Details of an OpenSearch Reserved Instance offering.
--
-- /See:/ 'newReservedInstanceOffering' smart constructor.
data ReservedInstanceOffering = ReservedInstanceOffering'
  { -- | The currency code for the Reserved Instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The duration, in seconds, for which the offering will reserve the
    -- OpenSearch instance.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The upfront fixed charge you will pay to purchase the specific Reserved
    -- Instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The OpenSearch instance type offered by the Reserved Instance offering.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | Payment option for the Reserved Instance offering
    paymentOption :: Prelude.Maybe ReservedInstancePaymentOption,
    -- | The recurring charge to your account, regardless of whether you creates
    -- any domains using the offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The unique identifier of the Reserved Instance offering.
    reservedInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The hourly rate at which you\'re charged for the domain using this
    -- Reserved Instance.
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
-- 'currencyCode', 'reservedInstanceOffering_currencyCode' - The currency code for the Reserved Instance offering.
--
-- 'duration', 'reservedInstanceOffering_duration' - The duration, in seconds, for which the offering will reserve the
-- OpenSearch instance.
--
-- 'fixedPrice', 'reservedInstanceOffering_fixedPrice' - The upfront fixed charge you will pay to purchase the specific Reserved
-- Instance offering.
--
-- 'instanceType', 'reservedInstanceOffering_instanceType' - The OpenSearch instance type offered by the Reserved Instance offering.
--
-- 'paymentOption', 'reservedInstanceOffering_paymentOption' - Payment option for the Reserved Instance offering
--
-- 'recurringCharges', 'reservedInstanceOffering_recurringCharges' - The recurring charge to your account, regardless of whether you creates
-- any domains using the offering.
--
-- 'reservedInstanceOfferingId', 'reservedInstanceOffering_reservedInstanceOfferingId' - The unique identifier of the Reserved Instance offering.
--
-- 'usagePrice', 'reservedInstanceOffering_usagePrice' - The hourly rate at which you\'re charged for the domain using this
-- Reserved Instance.
newReservedInstanceOffering ::
  ReservedInstanceOffering
newReservedInstanceOffering =
  ReservedInstanceOffering'
    { currencyCode =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      reservedInstanceOfferingId = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The currency code for the Reserved Instance offering.
reservedInstanceOffering_currencyCode :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Text)
reservedInstanceOffering_currencyCode = Lens.lens (\ReservedInstanceOffering' {currencyCode} -> currencyCode) (\s@ReservedInstanceOffering' {} a -> s {currencyCode = a} :: ReservedInstanceOffering)

-- | The duration, in seconds, for which the offering will reserve the
-- OpenSearch instance.
reservedInstanceOffering_duration :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Int)
reservedInstanceOffering_duration = Lens.lens (\ReservedInstanceOffering' {duration} -> duration) (\s@ReservedInstanceOffering' {} a -> s {duration = a} :: ReservedInstanceOffering)

-- | The upfront fixed charge you will pay to purchase the specific Reserved
-- Instance offering.
reservedInstanceOffering_fixedPrice :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Double)
reservedInstanceOffering_fixedPrice = Lens.lens (\ReservedInstanceOffering' {fixedPrice} -> fixedPrice) (\s@ReservedInstanceOffering' {} a -> s {fixedPrice = a} :: ReservedInstanceOffering)

-- | The OpenSearch instance type offered by the Reserved Instance offering.
reservedInstanceOffering_instanceType :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe OpenSearchPartitionInstanceType)
reservedInstanceOffering_instanceType = Lens.lens (\ReservedInstanceOffering' {instanceType} -> instanceType) (\s@ReservedInstanceOffering' {} a -> s {instanceType = a} :: ReservedInstanceOffering)

-- | Payment option for the Reserved Instance offering
reservedInstanceOffering_paymentOption :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe ReservedInstancePaymentOption)
reservedInstanceOffering_paymentOption = Lens.lens (\ReservedInstanceOffering' {paymentOption} -> paymentOption) (\s@ReservedInstanceOffering' {} a -> s {paymentOption = a} :: ReservedInstanceOffering)

-- | The recurring charge to your account, regardless of whether you creates
-- any domains using the offering.
reservedInstanceOffering_recurringCharges :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe [RecurringCharge])
reservedInstanceOffering_recurringCharges = Lens.lens (\ReservedInstanceOffering' {recurringCharges} -> recurringCharges) (\s@ReservedInstanceOffering' {} a -> s {recurringCharges = a} :: ReservedInstanceOffering) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of the Reserved Instance offering.
reservedInstanceOffering_reservedInstanceOfferingId :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Text)
reservedInstanceOffering_reservedInstanceOfferingId = Lens.lens (\ReservedInstanceOffering' {reservedInstanceOfferingId} -> reservedInstanceOfferingId) (\s@ReservedInstanceOffering' {} a -> s {reservedInstanceOfferingId = a} :: ReservedInstanceOffering)

-- | The hourly rate at which you\'re charged for the domain using this
-- Reserved Instance.
reservedInstanceOffering_usagePrice :: Lens.Lens' ReservedInstanceOffering (Prelude.Maybe Prelude.Double)
reservedInstanceOffering_usagePrice = Lens.lens (\ReservedInstanceOffering' {usagePrice} -> usagePrice) (\s@ReservedInstanceOffering' {} a -> s {usagePrice = a} :: ReservedInstanceOffering)

instance Data.FromJSON ReservedInstanceOffering where
  parseJSON =
    Data.withObject
      "ReservedInstanceOffering"
      ( \x ->
          ReservedInstanceOffering'
            Prelude.<$> (x Data..:? "CurrencyCode")
            Prelude.<*> (x Data..:? "Duration")
            Prelude.<*> (x Data..:? "FixedPrice")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "PaymentOption")
            Prelude.<*> ( x
                            Data..:? "RecurringCharges"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReservedInstanceOfferingId")
            Prelude.<*> (x Data..:? "UsagePrice")
      )

instance Prelude.Hashable ReservedInstanceOffering where
  hashWithSalt _salt ReservedInstanceOffering' {..} =
    _salt
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` reservedInstanceOfferingId
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedInstanceOffering where
  rnf ReservedInstanceOffering' {..} =
    Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf reservedInstanceOfferingId
      `Prelude.seq` Prelude.rnf usagePrice
