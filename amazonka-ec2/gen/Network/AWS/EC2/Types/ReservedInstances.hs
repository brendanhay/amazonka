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

import qualified Network.AWS.Core as Core
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

-- | Describes a Reserved Instance.
--
-- /See:/ 'newReservedInstances' smart constructor.
data ReservedInstances = ReservedInstances'
  { -- | The time when the Reserved Instance expires.
    end :: Core.Maybe Core.ISO8601,
    -- | The instance type on which the Reserved Instance can be used.
    instanceType :: Core.Maybe InstanceType,
    -- | The duration of the Reserved Instance, in seconds.
    duration :: Core.Maybe Core.Integer,
    -- | The ID of the Reserved Instance.
    reservedInstancesId :: Core.Maybe Core.Text,
    -- | The tenancy of the instance.
    instanceTenancy :: Core.Maybe Tenancy,
    -- | The currency of the Reserved Instance. It\'s specified using ISO 4217
    -- standard currency codes. At this time, the only supported currency is
    -- @USD@.
    currencyCode :: Core.Maybe CurrencyCodeValues,
    -- | The scope of the Reserved Instance.
    scope :: Core.Maybe Scope,
    -- | The state of the Reserved Instance purchase.
    state :: Core.Maybe ReservedInstanceState,
    -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Core.Maybe Core.Text,
    -- | Any tags assigned to the resource.
    tags :: Core.Maybe [Tag],
    -- | The offering class of the Reserved Instance.
    offeringClass :: Core.Maybe OfferingClassType,
    -- | The purchase price of the Reserved Instance.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The usage price of the Reserved Instance, per hour.
    usagePrice :: Core.Maybe Core.Double,
    -- | The Reserved Instance offering type.
    offeringType :: Core.Maybe OfferingTypeValues,
    -- | The date and time the Reserved Instance started.
    start :: Core.Maybe Core.ISO8601,
    -- | The recurring charge tag assigned to the resource.
    recurringCharges :: Core.Maybe [RecurringCharge],
    -- | The Reserved Instance product platform description.
    productDescription :: Core.Maybe RIProductDescription,
    -- | The number of reservations purchased.
    instanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { end = Core.Nothing,
      instanceType = Core.Nothing,
      duration = Core.Nothing,
      reservedInstancesId = Core.Nothing,
      instanceTenancy = Core.Nothing,
      currencyCode = Core.Nothing,
      scope = Core.Nothing,
      state = Core.Nothing,
      availabilityZone = Core.Nothing,
      tags = Core.Nothing,
      offeringClass = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      start = Core.Nothing,
      recurringCharges = Core.Nothing,
      productDescription = Core.Nothing,
      instanceCount = Core.Nothing
    }

-- | The time when the Reserved Instance expires.
reservedInstances_end :: Lens.Lens' ReservedInstances (Core.Maybe Core.UTCTime)
reservedInstances_end = Lens.lens (\ReservedInstances' {end} -> end) (\s@ReservedInstances' {} a -> s {end = a} :: ReservedInstances) Core.. Lens.mapping Core._Time

-- | The instance type on which the Reserved Instance can be used.
reservedInstances_instanceType :: Lens.Lens' ReservedInstances (Core.Maybe InstanceType)
reservedInstances_instanceType = Lens.lens (\ReservedInstances' {instanceType} -> instanceType) (\s@ReservedInstances' {} a -> s {instanceType = a} :: ReservedInstances)

-- | The duration of the Reserved Instance, in seconds.
reservedInstances_duration :: Lens.Lens' ReservedInstances (Core.Maybe Core.Integer)
reservedInstances_duration = Lens.lens (\ReservedInstances' {duration} -> duration) (\s@ReservedInstances' {} a -> s {duration = a} :: ReservedInstances)

-- | The ID of the Reserved Instance.
reservedInstances_reservedInstancesId :: Lens.Lens' ReservedInstances (Core.Maybe Core.Text)
reservedInstances_reservedInstancesId = Lens.lens (\ReservedInstances' {reservedInstancesId} -> reservedInstancesId) (\s@ReservedInstances' {} a -> s {reservedInstancesId = a} :: ReservedInstances)

-- | The tenancy of the instance.
reservedInstances_instanceTenancy :: Lens.Lens' ReservedInstances (Core.Maybe Tenancy)
reservedInstances_instanceTenancy = Lens.lens (\ReservedInstances' {instanceTenancy} -> instanceTenancy) (\s@ReservedInstances' {} a -> s {instanceTenancy = a} :: ReservedInstances)

-- | The currency of the Reserved Instance. It\'s specified using ISO 4217
-- standard currency codes. At this time, the only supported currency is
-- @USD@.
reservedInstances_currencyCode :: Lens.Lens' ReservedInstances (Core.Maybe CurrencyCodeValues)
reservedInstances_currencyCode = Lens.lens (\ReservedInstances' {currencyCode} -> currencyCode) (\s@ReservedInstances' {} a -> s {currencyCode = a} :: ReservedInstances)

-- | The scope of the Reserved Instance.
reservedInstances_scope :: Lens.Lens' ReservedInstances (Core.Maybe Scope)
reservedInstances_scope = Lens.lens (\ReservedInstances' {scope} -> scope) (\s@ReservedInstances' {} a -> s {scope = a} :: ReservedInstances)

-- | The state of the Reserved Instance purchase.
reservedInstances_state :: Lens.Lens' ReservedInstances (Core.Maybe ReservedInstanceState)
reservedInstances_state = Lens.lens (\ReservedInstances' {state} -> state) (\s@ReservedInstances' {} a -> s {state = a} :: ReservedInstances)

-- | The Availability Zone in which the Reserved Instance can be used.
reservedInstances_availabilityZone :: Lens.Lens' ReservedInstances (Core.Maybe Core.Text)
reservedInstances_availabilityZone = Lens.lens (\ReservedInstances' {availabilityZone} -> availabilityZone) (\s@ReservedInstances' {} a -> s {availabilityZone = a} :: ReservedInstances)

-- | Any tags assigned to the resource.
reservedInstances_tags :: Lens.Lens' ReservedInstances (Core.Maybe [Tag])
reservedInstances_tags = Lens.lens (\ReservedInstances' {tags} -> tags) (\s@ReservedInstances' {} a -> s {tags = a} :: ReservedInstances) Core.. Lens.mapping Lens._Coerce

-- | The offering class of the Reserved Instance.
reservedInstances_offeringClass :: Lens.Lens' ReservedInstances (Core.Maybe OfferingClassType)
reservedInstances_offeringClass = Lens.lens (\ReservedInstances' {offeringClass} -> offeringClass) (\s@ReservedInstances' {} a -> s {offeringClass = a} :: ReservedInstances)

-- | The purchase price of the Reserved Instance.
reservedInstances_fixedPrice :: Lens.Lens' ReservedInstances (Core.Maybe Core.Double)
reservedInstances_fixedPrice = Lens.lens (\ReservedInstances' {fixedPrice} -> fixedPrice) (\s@ReservedInstances' {} a -> s {fixedPrice = a} :: ReservedInstances)

-- | The usage price of the Reserved Instance, per hour.
reservedInstances_usagePrice :: Lens.Lens' ReservedInstances (Core.Maybe Core.Double)
reservedInstances_usagePrice = Lens.lens (\ReservedInstances' {usagePrice} -> usagePrice) (\s@ReservedInstances' {} a -> s {usagePrice = a} :: ReservedInstances)

-- | The Reserved Instance offering type.
reservedInstances_offeringType :: Lens.Lens' ReservedInstances (Core.Maybe OfferingTypeValues)
reservedInstances_offeringType = Lens.lens (\ReservedInstances' {offeringType} -> offeringType) (\s@ReservedInstances' {} a -> s {offeringType = a} :: ReservedInstances)

-- | The date and time the Reserved Instance started.
reservedInstances_start :: Lens.Lens' ReservedInstances (Core.Maybe Core.UTCTime)
reservedInstances_start = Lens.lens (\ReservedInstances' {start} -> start) (\s@ReservedInstances' {} a -> s {start = a} :: ReservedInstances) Core.. Lens.mapping Core._Time

-- | The recurring charge tag assigned to the resource.
reservedInstances_recurringCharges :: Lens.Lens' ReservedInstances (Core.Maybe [RecurringCharge])
reservedInstances_recurringCharges = Lens.lens (\ReservedInstances' {recurringCharges} -> recurringCharges) (\s@ReservedInstances' {} a -> s {recurringCharges = a} :: ReservedInstances) Core.. Lens.mapping Lens._Coerce

-- | The Reserved Instance product platform description.
reservedInstances_productDescription :: Lens.Lens' ReservedInstances (Core.Maybe RIProductDescription)
reservedInstances_productDescription = Lens.lens (\ReservedInstances' {productDescription} -> productDescription) (\s@ReservedInstances' {} a -> s {productDescription = a} :: ReservedInstances)

-- | The number of reservations purchased.
reservedInstances_instanceCount :: Lens.Lens' ReservedInstances (Core.Maybe Core.Int)
reservedInstances_instanceCount = Lens.lens (\ReservedInstances' {instanceCount} -> instanceCount) (\s@ReservedInstances' {} a -> s {instanceCount = a} :: ReservedInstances)

instance Core.FromXML ReservedInstances where
  parseXML x =
    ReservedInstances'
      Core.<$> (x Core..@? "end")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "duration")
      Core.<*> (x Core..@? "reservedInstancesId")
      Core.<*> (x Core..@? "instanceTenancy")
      Core.<*> (x Core..@? "currencyCode")
      Core.<*> (x Core..@? "scope")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "offeringClass")
      Core.<*> (x Core..@? "fixedPrice")
      Core.<*> (x Core..@? "usagePrice")
      Core.<*> (x Core..@? "offeringType")
      Core.<*> (x Core..@? "start")
      Core.<*> ( x Core..@? "recurringCharges" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "productDescription")
      Core.<*> (x Core..@? "instanceCount")

instance Core.Hashable ReservedInstances

instance Core.NFData ReservedInstances
