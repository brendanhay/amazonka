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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstances where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The offering class of the Reserved Instance.
    offeringClass :: Prelude.Maybe OfferingClassType,
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | The recurring charge tag assigned to the resource.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The tenancy of the instance.
    instanceTenancy :: Prelude.Maybe Tenancy,
    -- | The date and time the Reserved Instance started.
    start :: Prelude.Maybe Core.ISO8601,
    -- | The state of the Reserved Instance purchase.
    state :: Prelude.Maybe ReservedInstanceState,
    -- | The Reserved Instance offering type.
    offeringType :: Prelude.Maybe OfferingTypeValues,
    -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance type on which the Reserved Instance can be used.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The duration of the Reserved Instance, in seconds.
    duration :: Prelude.Maybe Prelude.Integer,
    -- | The currency of the Reserved Instance. It\'s specified using ISO 4217
    -- standard currency codes. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The time when the Reserved Instance expires.
    end :: Prelude.Maybe Core.ISO8601,
    -- | The scope of the Reserved Instance.
    scope :: Prelude.Maybe Scope,
    -- | The number of reservations purchased.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The Reserved Instance product platform description.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The purchase price of the Reserved Instance.
    fixedPrice :: Prelude.Maybe Prelude.Double,
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
-- 'tags', 'reservedInstances_tags' - Any tags assigned to the resource.
--
-- 'offeringClass', 'reservedInstances_offeringClass' - The offering class of the Reserved Instance.
--
-- 'reservedInstancesId', 'reservedInstances_reservedInstancesId' - The ID of the Reserved Instance.
--
-- 'recurringCharges', 'reservedInstances_recurringCharges' - The recurring charge tag assigned to the resource.
--
-- 'instanceTenancy', 'reservedInstances_instanceTenancy' - The tenancy of the instance.
--
-- 'start', 'reservedInstances_start' - The date and time the Reserved Instance started.
--
-- 'state', 'reservedInstances_state' - The state of the Reserved Instance purchase.
--
-- 'offeringType', 'reservedInstances_offeringType' - The Reserved Instance offering type.
--
-- 'availabilityZone', 'reservedInstances_availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- 'instanceType', 'reservedInstances_instanceType' - The instance type on which the Reserved Instance can be used.
--
-- 'duration', 'reservedInstances_duration' - The duration of the Reserved Instance, in seconds.
--
-- 'currencyCode', 'reservedInstances_currencyCode' - The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
--
-- 'end', 'reservedInstances_end' - The time when the Reserved Instance expires.
--
-- 'scope', 'reservedInstances_scope' - The scope of the Reserved Instance.
--
-- 'instanceCount', 'reservedInstances_instanceCount' - The number of reservations purchased.
--
-- 'productDescription', 'reservedInstances_productDescription' - The Reserved Instance product platform description.
--
-- 'fixedPrice', 'reservedInstances_fixedPrice' - The purchase price of the Reserved Instance.
--
-- 'usagePrice', 'reservedInstances_usagePrice' - The usage price of the Reserved Instance, per hour.
newReservedInstances ::
  ReservedInstances
newReservedInstances =
  ReservedInstances'
    { tags = Prelude.Nothing,
      offeringClass = Prelude.Nothing,
      reservedInstancesId = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      start = Prelude.Nothing,
      state = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      duration = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      end = Prelude.Nothing,
      scope = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | Any tags assigned to the resource.
reservedInstances_tags :: Lens.Lens' ReservedInstances (Prelude.Maybe [Tag])
reservedInstances_tags = Lens.lens (\ReservedInstances' {tags} -> tags) (\s@ReservedInstances' {} a -> s {tags = a} :: ReservedInstances) Prelude.. Lens.mapping Lens.coerced

-- | The offering class of the Reserved Instance.
reservedInstances_offeringClass :: Lens.Lens' ReservedInstances (Prelude.Maybe OfferingClassType)
reservedInstances_offeringClass = Lens.lens (\ReservedInstances' {offeringClass} -> offeringClass) (\s@ReservedInstances' {} a -> s {offeringClass = a} :: ReservedInstances)

-- | The ID of the Reserved Instance.
reservedInstances_reservedInstancesId :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Text)
reservedInstances_reservedInstancesId = Lens.lens (\ReservedInstances' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstances' {} a -> s {reservedInstancesId = a} :: ReservedInstances)

-- | The recurring charge tag assigned to the resource.
reservedInstances_recurringCharges :: Lens.Lens' ReservedInstances (Prelude.Maybe [RecurringCharge])
reservedInstances_recurringCharges = Lens.lens (\ReservedInstances' {recurringCharges} -> recurringCharges) (\s@ReservedInstances' {} a -> s {recurringCharges = a} :: ReservedInstances) Prelude.. Lens.mapping Lens.coerced

-- | The tenancy of the instance.
reservedInstances_instanceTenancy :: Lens.Lens' ReservedInstances (Prelude.Maybe Tenancy)
reservedInstances_instanceTenancy = Lens.lens (\ReservedInstances' {instanceTenancy} -> instanceTenancy) (\s@ReservedInstances' {} a -> s {instanceTenancy = a} :: ReservedInstances)

-- | The date and time the Reserved Instance started.
reservedInstances_start :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.UTCTime)
reservedInstances_start = Lens.lens (\ReservedInstances' {start} -> start) (\s@ReservedInstances' {} a -> s {start = a} :: ReservedInstances) Prelude.. Lens.mapping Core._Time

-- | The state of the Reserved Instance purchase.
reservedInstances_state :: Lens.Lens' ReservedInstances (Prelude.Maybe ReservedInstanceState)
reservedInstances_state = Lens.lens (\ReservedInstances' {state} -> state) (\s@ReservedInstances' {} a -> s {state = a} :: ReservedInstances)

-- | The Reserved Instance offering type.
reservedInstances_offeringType :: Lens.Lens' ReservedInstances (Prelude.Maybe OfferingTypeValues)
reservedInstances_offeringType = Lens.lens (\ReservedInstances' {offeringType} -> offeringType) (\s@ReservedInstances' {} a -> s {offeringType = a} :: ReservedInstances)

-- | The Availability Zone in which the Reserved Instance can be used.
reservedInstances_availabilityZone :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Text)
reservedInstances_availabilityZone = Lens.lens (\ReservedInstances' {availabilityZone} -> availabilityZone) (\s@ReservedInstances' {} a -> s {availabilityZone = a} :: ReservedInstances)

-- | The instance type on which the Reserved Instance can be used.
reservedInstances_instanceType :: Lens.Lens' ReservedInstances (Prelude.Maybe InstanceType)
reservedInstances_instanceType = Lens.lens (\ReservedInstances' {instanceType} -> instanceType) (\s@ReservedInstances' {} a -> s {instanceType = a} :: ReservedInstances)

-- | The duration of the Reserved Instance, in seconds.
reservedInstances_duration :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Integer)
reservedInstances_duration = Lens.lens (\ReservedInstances' {duration} -> duration) (\s@ReservedInstances' {} a -> s {duration = a} :: ReservedInstances)

-- | The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
reservedInstances_currencyCode :: Lens.Lens' ReservedInstances (Prelude.Maybe CurrencyCodeValues)
reservedInstances_currencyCode = Lens.lens (\ReservedInstances' {currencyCode} -> currencyCode) (\s@ReservedInstances' {} a -> s {currencyCode = a} :: ReservedInstances)

-- | The time when the Reserved Instance expires.
reservedInstances_end :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.UTCTime)
reservedInstances_end = Lens.lens (\ReservedInstances' {end} -> end) (\s@ReservedInstances' {} a -> s {end = a} :: ReservedInstances) Prelude.. Lens.mapping Core._Time

-- | The scope of the Reserved Instance.
reservedInstances_scope :: Lens.Lens' ReservedInstances (Prelude.Maybe Scope)
reservedInstances_scope = Lens.lens (\ReservedInstances' {scope} -> scope) (\s@ReservedInstances' {} a -> s {scope = a} :: ReservedInstances)

-- | The number of reservations purchased.
reservedInstances_instanceCount :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Int)
reservedInstances_instanceCount = Lens.lens (\ReservedInstances' {instanceCount} -> instanceCount) (\s@ReservedInstances' {} a -> s {instanceCount = a} :: ReservedInstances)

-- | The Reserved Instance product platform description.
reservedInstances_productDescription :: Lens.Lens' ReservedInstances (Prelude.Maybe RIProductDescription)
reservedInstances_productDescription = Lens.lens (\ReservedInstances' {productDescription} -> productDescription) (\s@ReservedInstances' {} a -> s {productDescription = a} :: ReservedInstances)

-- | The purchase price of the Reserved Instance.
reservedInstances_fixedPrice :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Double)
reservedInstances_fixedPrice = Lens.lens (\ReservedInstances' {fixedPrice} -> fixedPrice) (\s@ReservedInstances' {} a -> s {fixedPrice = a} :: ReservedInstances)

-- | The usage price of the Reserved Instance, per hour.
reservedInstances_usagePrice :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Double)
reservedInstances_usagePrice = Lens.lens (\ReservedInstances' {usagePrice} -> usagePrice) (\s@ReservedInstances' {} a -> s {usagePrice = a} :: ReservedInstances)

instance Core.FromXML ReservedInstances where
  parseXML x =
    ReservedInstances'
      Prelude.<$> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "offeringClass")
      Prelude.<*> (x Core..@? "reservedInstancesId")
      Prelude.<*> ( x Core..@? "recurringCharges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "instanceTenancy")
      Prelude.<*> (x Core..@? "start")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "offeringType")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> (x Core..@? "duration")
      Prelude.<*> (x Core..@? "currencyCode")
      Prelude.<*> (x Core..@? "end")
      Prelude.<*> (x Core..@? "scope")
      Prelude.<*> (x Core..@? "instanceCount")
      Prelude.<*> (x Core..@? "productDescription")
      Prelude.<*> (x Core..@? "fixedPrice")
      Prelude.<*> (x Core..@? "usagePrice")

instance Prelude.Hashable ReservedInstances where
  hashWithSalt _salt ReservedInstances' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` offeringClass
      `Prelude.hashWithSalt` reservedInstancesId
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` instanceTenancy
      `Prelude.hashWithSalt` start
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedInstances where
  rnf ReservedInstances' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf offeringClass
      `Prelude.seq` Prelude.rnf reservedInstancesId
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf instanceTenancy
      `Prelude.seq` Prelude.rnf start
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf end
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf usagePrice
