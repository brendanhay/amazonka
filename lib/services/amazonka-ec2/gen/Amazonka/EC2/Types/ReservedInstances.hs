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
-- Module      : Amazonka.EC2.Types.ReservedInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstances where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CurrencyCodeValues
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.OfferingClassType
import Amazonka.EC2.Types.OfferingTypeValues
import Amazonka.EC2.Types.RIProductDescription
import Amazonka.EC2.Types.RecurringCharge
import Amazonka.EC2.Types.ReservedInstanceState
import Amazonka.EC2.Types.Scope
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance.
--
-- /See:/ 'newReservedInstances' smart constructor.
data ReservedInstances = ReservedInstances'
  { -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The currency of the Reserved Instance. It\'s specified using ISO 4217
    -- standard currency codes. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The duration of the Reserved Instance, in seconds.
    duration :: Prelude.Maybe Prelude.Integer,
    -- | The time when the Reserved Instance expires.
    end :: Prelude.Maybe Data.ISO8601,
    -- | The purchase price of the Reserved Instance.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The number of reservations purchased.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The tenancy of the instance.
    instanceTenancy :: Prelude.Maybe Tenancy,
    -- | The instance type on which the Reserved Instance can be used.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The offering class of the Reserved Instance.
    offeringClass :: Prelude.Maybe OfferingClassType,
    -- | The Reserved Instance offering type.
    offeringType :: Prelude.Maybe OfferingTypeValues,
    -- | The Reserved Instance product platform description.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The recurring charge tag assigned to the resource.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | The scope of the Reserved Instance.
    scope :: Prelude.Maybe Scope,
    -- | The date and time the Reserved Instance started.
    start :: Prelude.Maybe Data.ISO8601,
    -- | The state of the Reserved Instance purchase.
    state :: Prelude.Maybe ReservedInstanceState,
    -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The usage price of the Reserved Instance, per hour.
    usagePrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'reservedInstances_availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- 'currencyCode', 'reservedInstances_currencyCode' - The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
--
-- 'duration', 'reservedInstances_duration' - The duration of the Reserved Instance, in seconds.
--
-- 'end', 'reservedInstances_end' - The time when the Reserved Instance expires.
--
-- 'fixedPrice', 'reservedInstances_fixedPrice' - The purchase price of the Reserved Instance.
--
-- 'instanceCount', 'reservedInstances_instanceCount' - The number of reservations purchased.
--
-- 'instanceTenancy', 'reservedInstances_instanceTenancy' - The tenancy of the instance.
--
-- 'instanceType', 'reservedInstances_instanceType' - The instance type on which the Reserved Instance can be used.
--
-- 'offeringClass', 'reservedInstances_offeringClass' - The offering class of the Reserved Instance.
--
-- 'offeringType', 'reservedInstances_offeringType' - The Reserved Instance offering type.
--
-- 'productDescription', 'reservedInstances_productDescription' - The Reserved Instance product platform description.
--
-- 'recurringCharges', 'reservedInstances_recurringCharges' - The recurring charge tag assigned to the resource.
--
-- 'reservedInstancesId', 'reservedInstances_reservedInstancesId' - The ID of the Reserved Instance.
--
-- 'scope', 'reservedInstances_scope' - The scope of the Reserved Instance.
--
-- 'start', 'reservedInstances_start' - The date and time the Reserved Instance started.
--
-- 'state', 'reservedInstances_state' - The state of the Reserved Instance purchase.
--
-- 'tags', 'reservedInstances_tags' - Any tags assigned to the resource.
--
-- 'usagePrice', 'reservedInstances_usagePrice' - The usage price of the Reserved Instance, per hour.
newReservedInstances ::
  ReservedInstances
newReservedInstances =
  ReservedInstances'
    { availabilityZone =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      duration = Prelude.Nothing,
      end = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      offeringClass = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      reservedInstancesId = Prelude.Nothing,
      scope = Prelude.Nothing,
      start = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The Availability Zone in which the Reserved Instance can be used.
reservedInstances_availabilityZone :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Text)
reservedInstances_availabilityZone = Lens.lens (\ReservedInstances' {availabilityZone} -> availabilityZone) (\s@ReservedInstances' {} a -> s {availabilityZone = a} :: ReservedInstances)

-- | The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
reservedInstances_currencyCode :: Lens.Lens' ReservedInstances (Prelude.Maybe CurrencyCodeValues)
reservedInstances_currencyCode = Lens.lens (\ReservedInstances' {currencyCode} -> currencyCode) (\s@ReservedInstances' {} a -> s {currencyCode = a} :: ReservedInstances)

-- | The duration of the Reserved Instance, in seconds.
reservedInstances_duration :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Integer)
reservedInstances_duration = Lens.lens (\ReservedInstances' {duration} -> duration) (\s@ReservedInstances' {} a -> s {duration = a} :: ReservedInstances)

-- | The time when the Reserved Instance expires.
reservedInstances_end :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.UTCTime)
reservedInstances_end = Lens.lens (\ReservedInstances' {end} -> end) (\s@ReservedInstances' {} a -> s {end = a} :: ReservedInstances) Prelude.. Lens.mapping Data._Time

-- | The purchase price of the Reserved Instance.
reservedInstances_fixedPrice :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Double)
reservedInstances_fixedPrice = Lens.lens (\ReservedInstances' {fixedPrice} -> fixedPrice) (\s@ReservedInstances' {} a -> s {fixedPrice = a} :: ReservedInstances)

-- | The number of reservations purchased.
reservedInstances_instanceCount :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Int)
reservedInstances_instanceCount = Lens.lens (\ReservedInstances' {instanceCount} -> instanceCount) (\s@ReservedInstances' {} a -> s {instanceCount = a} :: ReservedInstances)

-- | The tenancy of the instance.
reservedInstances_instanceTenancy :: Lens.Lens' ReservedInstances (Prelude.Maybe Tenancy)
reservedInstances_instanceTenancy = Lens.lens (\ReservedInstances' {instanceTenancy} -> instanceTenancy) (\s@ReservedInstances' {} a -> s {instanceTenancy = a} :: ReservedInstances)

-- | The instance type on which the Reserved Instance can be used.
reservedInstances_instanceType :: Lens.Lens' ReservedInstances (Prelude.Maybe InstanceType)
reservedInstances_instanceType = Lens.lens (\ReservedInstances' {instanceType} -> instanceType) (\s@ReservedInstances' {} a -> s {instanceType = a} :: ReservedInstances)

-- | The offering class of the Reserved Instance.
reservedInstances_offeringClass :: Lens.Lens' ReservedInstances (Prelude.Maybe OfferingClassType)
reservedInstances_offeringClass = Lens.lens (\ReservedInstances' {offeringClass} -> offeringClass) (\s@ReservedInstances' {} a -> s {offeringClass = a} :: ReservedInstances)

-- | The Reserved Instance offering type.
reservedInstances_offeringType :: Lens.Lens' ReservedInstances (Prelude.Maybe OfferingTypeValues)
reservedInstances_offeringType = Lens.lens (\ReservedInstances' {offeringType} -> offeringType) (\s@ReservedInstances' {} a -> s {offeringType = a} :: ReservedInstances)

-- | The Reserved Instance product platform description.
reservedInstances_productDescription :: Lens.Lens' ReservedInstances (Prelude.Maybe RIProductDescription)
reservedInstances_productDescription = Lens.lens (\ReservedInstances' {productDescription} -> productDescription) (\s@ReservedInstances' {} a -> s {productDescription = a} :: ReservedInstances)

-- | The recurring charge tag assigned to the resource.
reservedInstances_recurringCharges :: Lens.Lens' ReservedInstances (Prelude.Maybe [RecurringCharge])
reservedInstances_recurringCharges = Lens.lens (\ReservedInstances' {recurringCharges} -> recurringCharges) (\s@ReservedInstances' {} a -> s {recurringCharges = a} :: ReservedInstances) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Reserved Instance.
reservedInstances_reservedInstancesId :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Text)
reservedInstances_reservedInstancesId = Lens.lens (\ReservedInstances' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstances' {} a -> s {reservedInstancesId = a} :: ReservedInstances)

-- | The scope of the Reserved Instance.
reservedInstances_scope :: Lens.Lens' ReservedInstances (Prelude.Maybe Scope)
reservedInstances_scope = Lens.lens (\ReservedInstances' {scope} -> scope) (\s@ReservedInstances' {} a -> s {scope = a} :: ReservedInstances)

-- | The date and time the Reserved Instance started.
reservedInstances_start :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.UTCTime)
reservedInstances_start = Lens.lens (\ReservedInstances' {start} -> start) (\s@ReservedInstances' {} a -> s {start = a} :: ReservedInstances) Prelude.. Lens.mapping Data._Time

-- | The state of the Reserved Instance purchase.
reservedInstances_state :: Lens.Lens' ReservedInstances (Prelude.Maybe ReservedInstanceState)
reservedInstances_state = Lens.lens (\ReservedInstances' {state} -> state) (\s@ReservedInstances' {} a -> s {state = a} :: ReservedInstances)

-- | Any tags assigned to the resource.
reservedInstances_tags :: Lens.Lens' ReservedInstances (Prelude.Maybe [Tag])
reservedInstances_tags = Lens.lens (\ReservedInstances' {tags} -> tags) (\s@ReservedInstances' {} a -> s {tags = a} :: ReservedInstances) Prelude.. Lens.mapping Lens.coerced

-- | The usage price of the Reserved Instance, per hour.
reservedInstances_usagePrice :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Double)
reservedInstances_usagePrice = Lens.lens (\ReservedInstances' {usagePrice} -> usagePrice) (\s@ReservedInstances' {} a -> s {usagePrice = a} :: ReservedInstances)

instance Data.FromXML ReservedInstances where
  parseXML x =
    ReservedInstances'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "currencyCode")
      Prelude.<*> (x Data..@? "duration")
      Prelude.<*> (x Data..@? "end")
      Prelude.<*> (x Data..@? "fixedPrice")
      Prelude.<*> (x Data..@? "instanceCount")
      Prelude.<*> (x Data..@? "instanceTenancy")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "offeringClass")
      Prelude.<*> (x Data..@? "offeringType")
      Prelude.<*> (x Data..@? "productDescription")
      Prelude.<*> ( x Data..@? "recurringCharges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "reservedInstancesId")
      Prelude.<*> (x Data..@? "scope")
      Prelude.<*> (x Data..@? "start")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "usagePrice")

instance Prelude.Hashable ReservedInstances where
  hashWithSalt _salt ReservedInstances' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` instanceTenancy
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` offeringClass
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` reservedInstancesId
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedInstances where
  rnf ReservedInstances' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf instanceTenancy
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf offeringClass
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf reservedInstancesId
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf usagePrice
