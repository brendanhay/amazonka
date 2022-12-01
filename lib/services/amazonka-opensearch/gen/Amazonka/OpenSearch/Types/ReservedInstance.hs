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
-- Module      : Amazonka.OpenSearch.Types.ReservedInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ReservedInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.RecurringCharge
import Amazonka.OpenSearch.Types.ReservedInstancePaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Details of an OpenSearch Reserved Instance.
--
-- /See:/ 'newReservedInstance' smart constructor.
data ReservedInstance = ReservedInstance'
  { -- | The unique identifier of the billing subscription.
    billingSubscriptionId :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier for the reservation.
    reservedInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The recurring charge to your account, regardless of whether you create
    -- any domains using the Reserved Instance offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The state of the Reserved Instance.
    state :: Prelude.Maybe Prelude.Text,
    -- | The customer-specified identifier to track this reservation.
    reservationName :: Prelude.Maybe Prelude.Text,
    -- | The OpenSearch instance type offered by theReserved Instance offering.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | The duration, in seconds, for which the OpenSearch instance is reserved.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The currency code for the offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The number of OpenSearch instances that have been reserved.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The upfront fixed charge you will paid to purchase the specific Reserved
    -- Instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The unique identifier of the Reserved Instance offering.
    reservedInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the reservation was purchased.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The payment option as defined in the Reserved Instance offering.
    paymentOption :: Prelude.Maybe ReservedInstancePaymentOption,
    -- | The hourly rate at which you\'re charged for the domain using this
    -- Reserved Instance.
    usagePrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingSubscriptionId', 'reservedInstance_billingSubscriptionId' - The unique identifier of the billing subscription.
--
-- 'reservedInstanceId', 'reservedInstance_reservedInstanceId' - The unique identifier for the reservation.
--
-- 'recurringCharges', 'reservedInstance_recurringCharges' - The recurring charge to your account, regardless of whether you create
-- any domains using the Reserved Instance offering.
--
-- 'state', 'reservedInstance_state' - The state of the Reserved Instance.
--
-- 'reservationName', 'reservedInstance_reservationName' - The customer-specified identifier to track this reservation.
--
-- 'instanceType', 'reservedInstance_instanceType' - The OpenSearch instance type offered by theReserved Instance offering.
--
-- 'duration', 'reservedInstance_duration' - The duration, in seconds, for which the OpenSearch instance is reserved.
--
-- 'currencyCode', 'reservedInstance_currencyCode' - The currency code for the offering.
--
-- 'instanceCount', 'reservedInstance_instanceCount' - The number of OpenSearch instances that have been reserved.
--
-- 'fixedPrice', 'reservedInstance_fixedPrice' - The upfront fixed charge you will paid to purchase the specific Reserved
-- Instance offering.
--
-- 'reservedInstanceOfferingId', 'reservedInstance_reservedInstanceOfferingId' - The unique identifier of the Reserved Instance offering.
--
-- 'startTime', 'reservedInstance_startTime' - The date and time when the reservation was purchased.
--
-- 'paymentOption', 'reservedInstance_paymentOption' - The payment option as defined in the Reserved Instance offering.
--
-- 'usagePrice', 'reservedInstance_usagePrice' - The hourly rate at which you\'re charged for the domain using this
-- Reserved Instance.
newReservedInstance ::
  ReservedInstance
newReservedInstance =
  ReservedInstance'
    { billingSubscriptionId =
        Prelude.Nothing,
      reservedInstanceId = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      state = Prelude.Nothing,
      reservationName = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      reservedInstanceOfferingId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      paymentOption = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The unique identifier of the billing subscription.
reservedInstance_billingSubscriptionId :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Integer)
reservedInstance_billingSubscriptionId = Lens.lens (\ReservedInstance' {billingSubscriptionId} -> billingSubscriptionId) (\s@ReservedInstance' {} a -> s {billingSubscriptionId = a} :: ReservedInstance)

-- | The unique identifier for the reservation.
reservedInstance_reservedInstanceId :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_reservedInstanceId = Lens.lens (\ReservedInstance' {reservedInstanceId} -> reservedInstanceId) (\s@ReservedInstance' {} a -> s {reservedInstanceId = a} :: ReservedInstance)

-- | The recurring charge to your account, regardless of whether you create
-- any domains using the Reserved Instance offering.
reservedInstance_recurringCharges :: Lens.Lens' ReservedInstance (Prelude.Maybe [RecurringCharge])
reservedInstance_recurringCharges = Lens.lens (\ReservedInstance' {recurringCharges} -> recurringCharges) (\s@ReservedInstance' {} a -> s {recurringCharges = a} :: ReservedInstance) Prelude.. Lens.mapping Lens.coerced

-- | The state of the Reserved Instance.
reservedInstance_state :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_state = Lens.lens (\ReservedInstance' {state} -> state) (\s@ReservedInstance' {} a -> s {state = a} :: ReservedInstance)

-- | The customer-specified identifier to track this reservation.
reservedInstance_reservationName :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_reservationName = Lens.lens (\ReservedInstance' {reservationName} -> reservationName) (\s@ReservedInstance' {} a -> s {reservationName = a} :: ReservedInstance)

-- | The OpenSearch instance type offered by theReserved Instance offering.
reservedInstance_instanceType :: Lens.Lens' ReservedInstance (Prelude.Maybe OpenSearchPartitionInstanceType)
reservedInstance_instanceType = Lens.lens (\ReservedInstance' {instanceType} -> instanceType) (\s@ReservedInstance' {} a -> s {instanceType = a} :: ReservedInstance)

-- | The duration, in seconds, for which the OpenSearch instance is reserved.
reservedInstance_duration :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Int)
reservedInstance_duration = Lens.lens (\ReservedInstance' {duration} -> duration) (\s@ReservedInstance' {} a -> s {duration = a} :: ReservedInstance)

-- | The currency code for the offering.
reservedInstance_currencyCode :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_currencyCode = Lens.lens (\ReservedInstance' {currencyCode} -> currencyCode) (\s@ReservedInstance' {} a -> s {currencyCode = a} :: ReservedInstance)

-- | The number of OpenSearch instances that have been reserved.
reservedInstance_instanceCount :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Int)
reservedInstance_instanceCount = Lens.lens (\ReservedInstance' {instanceCount} -> instanceCount) (\s@ReservedInstance' {} a -> s {instanceCount = a} :: ReservedInstance)

-- | The upfront fixed charge you will paid to purchase the specific Reserved
-- Instance offering.
reservedInstance_fixedPrice :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Double)
reservedInstance_fixedPrice = Lens.lens (\ReservedInstance' {fixedPrice} -> fixedPrice) (\s@ReservedInstance' {} a -> s {fixedPrice = a} :: ReservedInstance)

-- | The unique identifier of the Reserved Instance offering.
reservedInstance_reservedInstanceOfferingId :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_reservedInstanceOfferingId = Lens.lens (\ReservedInstance' {reservedInstanceOfferingId} -> reservedInstanceOfferingId) (\s@ReservedInstance' {} a -> s {reservedInstanceOfferingId = a} :: ReservedInstance)

-- | The date and time when the reservation was purchased.
reservedInstance_startTime :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.UTCTime)
reservedInstance_startTime = Lens.lens (\ReservedInstance' {startTime} -> startTime) (\s@ReservedInstance' {} a -> s {startTime = a} :: ReservedInstance) Prelude.. Lens.mapping Core._Time

-- | The payment option as defined in the Reserved Instance offering.
reservedInstance_paymentOption :: Lens.Lens' ReservedInstance (Prelude.Maybe ReservedInstancePaymentOption)
reservedInstance_paymentOption = Lens.lens (\ReservedInstance' {paymentOption} -> paymentOption) (\s@ReservedInstance' {} a -> s {paymentOption = a} :: ReservedInstance)

-- | The hourly rate at which you\'re charged for the domain using this
-- Reserved Instance.
reservedInstance_usagePrice :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Double)
reservedInstance_usagePrice = Lens.lens (\ReservedInstance' {usagePrice} -> usagePrice) (\s@ReservedInstance' {} a -> s {usagePrice = a} :: ReservedInstance)

instance Core.FromJSON ReservedInstance where
  parseJSON =
    Core.withObject
      "ReservedInstance"
      ( \x ->
          ReservedInstance'
            Prelude.<$> (x Core..:? "BillingSubscriptionId")
            Prelude.<*> (x Core..:? "ReservedInstanceId")
            Prelude.<*> ( x Core..:? "RecurringCharges"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "ReservationName")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "Duration")
            Prelude.<*> (x Core..:? "CurrencyCode")
            Prelude.<*> (x Core..:? "InstanceCount")
            Prelude.<*> (x Core..:? "FixedPrice")
            Prelude.<*> (x Core..:? "ReservedInstanceOfferingId")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "PaymentOption")
            Prelude.<*> (x Core..:? "UsagePrice")
      )

instance Prelude.Hashable ReservedInstance where
  hashWithSalt _salt ReservedInstance' {..} =
    _salt `Prelude.hashWithSalt` billingSubscriptionId
      `Prelude.hashWithSalt` reservedInstanceId
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` reservationName
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` reservedInstanceOfferingId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedInstance where
  rnf ReservedInstance' {..} =
    Prelude.rnf billingSubscriptionId
      `Prelude.seq` Prelude.rnf reservedInstanceId
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf reservationName
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf reservedInstanceOfferingId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf usagePrice
