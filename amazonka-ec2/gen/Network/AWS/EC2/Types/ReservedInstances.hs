{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.ReservedInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstances where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.OfferingClassType
import Network.AWS.EC2.Types.OfferingTypeValues
import Network.AWS.EC2.Types.RIProductDescription
import Network.AWS.EC2.Types.RecurringCharge
import Network.AWS.EC2.Types.ReservedInstanceState
import Network.AWS.EC2.Types.Scope
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Reserved Instance.
--
-- /See:/ 'newReservedInstances' smart constructor.
data ReservedInstances = ReservedInstances'
  { -- | The time when the Reserved Instance expires.
    end :: Prelude.Maybe Prelude.ISO8601,
    -- | The instance type on which the Reserved Instance can be used.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The duration of the Reserved Instance, in seconds.
    duration :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Prelude.Maybe Prelude.Text,
    -- | The tenancy of the instance.
    instanceTenancy :: Prelude.Maybe Tenancy,
    -- | The currency of the Reserved Instance. It\'s specified using ISO 4217
    -- standard currency codes. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The scope of the Reserved Instance.
    scope :: Prelude.Maybe Scope,
    -- | The state of the Reserved Instance purchase.
    state :: Prelude.Maybe ReservedInstanceState,
    -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag],
    -- | The offering class of the Reserved Instance.
    offeringClass :: Prelude.Maybe OfferingClassType,
    -- | The purchase price of the Reserved Instance.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The usage price of the Reserved Instance, per hour.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The Reserved Instance offering type.
    offeringType :: Prelude.Maybe OfferingTypeValues,
    -- | The date and time the Reserved Instance started.
    start :: Prelude.Maybe Prelude.ISO8601,
    -- | The recurring charge tag assigned to the resource.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The Reserved Instance product platform description.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The number of reservations purchased.
    instanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'end', 'reservedInstances_end' - The time when the Reserved Instance expires.
--
-- 'instanceType', 'reservedInstances_instanceType' - The instance type on which the Reserved Instance can be used.
--
-- 'duration', 'reservedInstances_duration' - The duration of the Reserved Instance, in seconds.
--
-- 'reservedInstancesId', 'reservedInstances_reservedInstancesId' - The ID of the Reserved Instance.
--
-- 'instanceTenancy', 'reservedInstances_instanceTenancy' - The tenancy of the instance.
--
-- 'currencyCode', 'reservedInstances_currencyCode' - The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
--
-- 'scope', 'reservedInstances_scope' - The scope of the Reserved Instance.
--
-- 'state', 'reservedInstances_state' - The state of the Reserved Instance purchase.
--
-- 'availabilityZone', 'reservedInstances_availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- 'tags', 'reservedInstances_tags' - Any tags assigned to the resource.
--
-- 'offeringClass', 'reservedInstances_offeringClass' - The offering class of the Reserved Instance.
--
-- 'fixedPrice', 'reservedInstances_fixedPrice' - The purchase price of the Reserved Instance.
--
-- 'usagePrice', 'reservedInstances_usagePrice' - The usage price of the Reserved Instance, per hour.
--
-- 'offeringType', 'reservedInstances_offeringType' - The Reserved Instance offering type.
--
-- 'start', 'reservedInstances_start' - The date and time the Reserved Instance started.
--
-- 'recurringCharges', 'reservedInstances_recurringCharges' - The recurring charge tag assigned to the resource.
--
-- 'productDescription', 'reservedInstances_productDescription' - The Reserved Instance product platform description.
--
-- 'instanceCount', 'reservedInstances_instanceCount' - The number of reservations purchased.
newReservedInstances ::
  ReservedInstances
newReservedInstances =
  ReservedInstances'
    { end = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      duration = Prelude.Nothing,
      reservedInstancesId = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      scope = Prelude.Nothing,
      state = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      tags = Prelude.Nothing,
      offeringClass = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      start = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      instanceCount = Prelude.Nothing
    }

-- | The time when the Reserved Instance expires.
reservedInstances_end :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.UTCTime)
reservedInstances_end = Lens.lens (\ReservedInstances' {end} -> end) (\s@ReservedInstances' {} a -> s {end = a} :: ReservedInstances) Prelude.. Lens.mapping Prelude._Time

-- | The instance type on which the Reserved Instance can be used.
reservedInstances_instanceType :: Lens.Lens' ReservedInstances (Prelude.Maybe InstanceType)
reservedInstances_instanceType = Lens.lens (\ReservedInstances' {instanceType} -> instanceType) (\s@ReservedInstances' {} a -> s {instanceType = a} :: ReservedInstances)

-- | The duration of the Reserved Instance, in seconds.
reservedInstances_duration :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Integer)
reservedInstances_duration = Lens.lens (\ReservedInstances' {duration} -> duration) (\s@ReservedInstances' {} a -> s {duration = a} :: ReservedInstances)

-- | The ID of the Reserved Instance.
reservedInstances_reservedInstancesId :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Text)
reservedInstances_reservedInstancesId = Lens.lens (\ReservedInstances' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstances' {} a -> s {reservedInstancesId = a} :: ReservedInstances)

-- | The tenancy of the instance.
reservedInstances_instanceTenancy :: Lens.Lens' ReservedInstances (Prelude.Maybe Tenancy)
reservedInstances_instanceTenancy = Lens.lens (\ReservedInstances' {instanceTenancy} -> instanceTenancy) (\s@ReservedInstances' {} a -> s {instanceTenancy = a} :: ReservedInstances)

-- | The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
reservedInstances_currencyCode :: Lens.Lens' ReservedInstances (Prelude.Maybe CurrencyCodeValues)
reservedInstances_currencyCode = Lens.lens (\ReservedInstances' {currencyCode} -> currencyCode) (\s@ReservedInstances' {} a -> s {currencyCode = a} :: ReservedInstances)

-- | The scope of the Reserved Instance.
reservedInstances_scope :: Lens.Lens' ReservedInstances (Prelude.Maybe Scope)
reservedInstances_scope = Lens.lens (\ReservedInstances' {scope} -> scope) (\s@ReservedInstances' {} a -> s {scope = a} :: ReservedInstances)

-- | The state of the Reserved Instance purchase.
reservedInstances_state :: Lens.Lens' ReservedInstances (Prelude.Maybe ReservedInstanceState)
reservedInstances_state = Lens.lens (\ReservedInstances' {state} -> state) (\s@ReservedInstances' {} a -> s {state = a} :: ReservedInstances)

-- | The Availability Zone in which the Reserved Instance can be used.
reservedInstances_availabilityZone :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Text)
reservedInstances_availabilityZone = Lens.lens (\ReservedInstances' {availabilityZone} -> availabilityZone) (\s@ReservedInstances' {} a -> s {availabilityZone = a} :: ReservedInstances)

-- | Any tags assigned to the resource.
reservedInstances_tags :: Lens.Lens' ReservedInstances (Prelude.Maybe [Tag])
reservedInstances_tags = Lens.lens (\ReservedInstances' {tags} -> tags) (\s@ReservedInstances' {} a -> s {tags = a} :: ReservedInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The offering class of the Reserved Instance.
reservedInstances_offeringClass :: Lens.Lens' ReservedInstances (Prelude.Maybe OfferingClassType)
reservedInstances_offeringClass = Lens.lens (\ReservedInstances' {offeringClass} -> offeringClass) (\s@ReservedInstances' {} a -> s {offeringClass = a} :: ReservedInstances)

-- | The purchase price of the Reserved Instance.
reservedInstances_fixedPrice :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Double)
reservedInstances_fixedPrice = Lens.lens (\ReservedInstances' {fixedPrice} -> fixedPrice) (\s@ReservedInstances' {} a -> s {fixedPrice = a} :: ReservedInstances)

-- | The usage price of the Reserved Instance, per hour.
reservedInstances_usagePrice :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Double)
reservedInstances_usagePrice = Lens.lens (\ReservedInstances' {usagePrice} -> usagePrice) (\s@ReservedInstances' {} a -> s {usagePrice = a} :: ReservedInstances)

-- | The Reserved Instance offering type.
reservedInstances_offeringType :: Lens.Lens' ReservedInstances (Prelude.Maybe OfferingTypeValues)
reservedInstances_offeringType = Lens.lens (\ReservedInstances' {offeringType} -> offeringType) (\s@ReservedInstances' {} a -> s {offeringType = a} :: ReservedInstances)

-- | The date and time the Reserved Instance started.
reservedInstances_start :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.UTCTime)
reservedInstances_start = Lens.lens (\ReservedInstances' {start} -> start) (\s@ReservedInstances' {} a -> s {start = a} :: ReservedInstances) Prelude.. Lens.mapping Prelude._Time

-- | The recurring charge tag assigned to the resource.
reservedInstances_recurringCharges :: Lens.Lens' ReservedInstances (Prelude.Maybe [RecurringCharge])
reservedInstances_recurringCharges = Lens.lens (\ReservedInstances' {recurringCharges} -> recurringCharges) (\s@ReservedInstances' {} a -> s {recurringCharges = a} :: ReservedInstances) Prelude.. Lens.mapping Prelude._Coerce

-- | The Reserved Instance product platform description.
reservedInstances_productDescription :: Lens.Lens' ReservedInstances (Prelude.Maybe RIProductDescription)
reservedInstances_productDescription = Lens.lens (\ReservedInstances' {productDescription} -> productDescription) (\s@ReservedInstances' {} a -> s {productDescription = a} :: ReservedInstances)

-- | The number of reservations purchased.
reservedInstances_instanceCount :: Lens.Lens' ReservedInstances (Prelude.Maybe Prelude.Int)
reservedInstances_instanceCount = Lens.lens (\ReservedInstances' {instanceCount} -> instanceCount) (\s@ReservedInstances' {} a -> s {instanceCount = a} :: ReservedInstances)

instance Prelude.FromXML ReservedInstances where
  parseXML x =
    ReservedInstances'
      Prelude.<$> (x Prelude..@? "end")
      Prelude.<*> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "duration")
      Prelude.<*> (x Prelude..@? "reservedInstancesId")
      Prelude.<*> (x Prelude..@? "instanceTenancy")
      Prelude.<*> (x Prelude..@? "currencyCode")
      Prelude.<*> (x Prelude..@? "scope")
      Prelude.<*> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "offeringClass")
      Prelude.<*> (x Prelude..@? "fixedPrice")
      Prelude.<*> (x Prelude..@? "usagePrice")
      Prelude.<*> (x Prelude..@? "offeringType")
      Prelude.<*> (x Prelude..@? "start")
      Prelude.<*> ( x Prelude..@? "recurringCharges"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "productDescription")
      Prelude.<*> (x Prelude..@? "instanceCount")

instance Prelude.Hashable ReservedInstances

instance Prelude.NFData ReservedInstances
