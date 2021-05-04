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
-- Module      : Network.AWS.EC2.Types.ReservedInstancesOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesOffering where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.OfferingClassType
import Network.AWS.EC2.Types.OfferingTypeValues
import Network.AWS.EC2.Types.PricingDetail
import Network.AWS.EC2.Types.RIProductDescription
import Network.AWS.EC2.Types.RecurringCharge
import Network.AWS.EC2.Types.Scope
import Network.AWS.EC2.Types.Tenancy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'newReservedInstancesOffering' smart constructor.
data ReservedInstancesOffering = ReservedInstancesOffering'
  { -- | The instance type on which the Reserved Instance can be used.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The duration of the Reserved Instance, in seconds.
    duration :: Prelude.Maybe Prelude.Integer,
    -- | The tenancy of the instance.
    instanceTenancy :: Prelude.Maybe Tenancy,
    -- | The ID of the Reserved Instance offering. This is the offering ID used
    -- in GetReservedInstancesExchangeQuote to confirm that an exchange can be
    -- made.
    reservedInstancesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The currency of the Reserved Instance offering you are purchasing. It\'s
    -- specified using ISO 4217 standard currency codes. At this time, the only
    -- supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | Whether the Reserved Instance is applied to instances in a Region or an
    -- Availability Zone.
    scope :: Prelude.Maybe Scope,
    -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the offering is available through the Reserved
    -- Instance Marketplace (resale) or AWS. If it\'s a Reserved Instance
    -- Marketplace offering, this is @true@.
    marketplace :: Prelude.Maybe Prelude.Bool,
    -- | If @convertible@ it can be exchanged for Reserved Instances of the same
    -- or higher monetary value, with different configurations. If @standard@,
    -- it is not possible to perform an exchange.
    offeringClass :: Prelude.Maybe OfferingClassType,
    -- | The purchase price of the Reserved Instance.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The usage price of the Reserved Instance, per hour.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The Reserved Instance offering type.
    offeringType :: Prelude.Maybe OfferingTypeValues,
    -- | The recurring charge tag assigned to the resource.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The Reserved Instance product platform description.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The pricing details of the Reserved Instance offering.
    pricingDetails :: Prelude.Maybe [PricingDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstancesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'reservedInstancesOffering_instanceType' - The instance type on which the Reserved Instance can be used.
--
-- 'duration', 'reservedInstancesOffering_duration' - The duration of the Reserved Instance, in seconds.
--
-- 'instanceTenancy', 'reservedInstancesOffering_instanceTenancy' - The tenancy of the instance.
--
-- 'reservedInstancesOfferingId', 'reservedInstancesOffering_reservedInstancesOfferingId' - The ID of the Reserved Instance offering. This is the offering ID used
-- in GetReservedInstancesExchangeQuote to confirm that an exchange can be
-- made.
--
-- 'currencyCode', 'reservedInstancesOffering_currencyCode' - The currency of the Reserved Instance offering you are purchasing. It\'s
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is @USD@.
--
-- 'scope', 'reservedInstancesOffering_scope' - Whether the Reserved Instance is applied to instances in a Region or an
-- Availability Zone.
--
-- 'availabilityZone', 'reservedInstancesOffering_availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- 'marketplace', 'reservedInstancesOffering_marketplace' - Indicates whether the offering is available through the Reserved
-- Instance Marketplace (resale) or AWS. If it\'s a Reserved Instance
-- Marketplace offering, this is @true@.
--
-- 'offeringClass', 'reservedInstancesOffering_offeringClass' - If @convertible@ it can be exchanged for Reserved Instances of the same
-- or higher monetary value, with different configurations. If @standard@,
-- it is not possible to perform an exchange.
--
-- 'fixedPrice', 'reservedInstancesOffering_fixedPrice' - The purchase price of the Reserved Instance.
--
-- 'usagePrice', 'reservedInstancesOffering_usagePrice' - The usage price of the Reserved Instance, per hour.
--
-- 'offeringType', 'reservedInstancesOffering_offeringType' - The Reserved Instance offering type.
--
-- 'recurringCharges', 'reservedInstancesOffering_recurringCharges' - The recurring charge tag assigned to the resource.
--
-- 'productDescription', 'reservedInstancesOffering_productDescription' - The Reserved Instance product platform description.
--
-- 'pricingDetails', 'reservedInstancesOffering_pricingDetails' - The pricing details of the Reserved Instance offering.
newReservedInstancesOffering ::
  ReservedInstancesOffering
newReservedInstancesOffering =
  ReservedInstancesOffering'
    { instanceType =
        Prelude.Nothing,
      duration = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      reservedInstancesOfferingId = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      scope = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      marketplace = Prelude.Nothing,
      offeringClass = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      pricingDetails = Prelude.Nothing
    }

-- | The instance type on which the Reserved Instance can be used.
reservedInstancesOffering_instanceType :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe InstanceType)
reservedInstancesOffering_instanceType = Lens.lens (\ReservedInstancesOffering' {instanceType} -> instanceType) (\s@ReservedInstancesOffering' {} a -> s {instanceType = a} :: ReservedInstancesOffering)

-- | The duration of the Reserved Instance, in seconds.
reservedInstancesOffering_duration :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Integer)
reservedInstancesOffering_duration = Lens.lens (\ReservedInstancesOffering' {duration} -> duration) (\s@ReservedInstancesOffering' {} a -> s {duration = a} :: ReservedInstancesOffering)

-- | The tenancy of the instance.
reservedInstancesOffering_instanceTenancy :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Tenancy)
reservedInstancesOffering_instanceTenancy = Lens.lens (\ReservedInstancesOffering' {instanceTenancy} -> instanceTenancy) (\s@ReservedInstancesOffering' {} a -> s {instanceTenancy = a} :: ReservedInstancesOffering)

-- | The ID of the Reserved Instance offering. This is the offering ID used
-- in GetReservedInstancesExchangeQuote to confirm that an exchange can be
-- made.
reservedInstancesOffering_reservedInstancesOfferingId :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Text)
reservedInstancesOffering_reservedInstancesOfferingId = Lens.lens (\ReservedInstancesOffering' {reservedInstancesOfferingId} -> reservedInstancesOfferingId) (\s@ReservedInstancesOffering' {} a -> s {reservedInstancesOfferingId = a} :: ReservedInstancesOffering)

-- | The currency of the Reserved Instance offering you are purchasing. It\'s
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is @USD@.
reservedInstancesOffering_currencyCode :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe CurrencyCodeValues)
reservedInstancesOffering_currencyCode = Lens.lens (\ReservedInstancesOffering' {currencyCode} -> currencyCode) (\s@ReservedInstancesOffering' {} a -> s {currencyCode = a} :: ReservedInstancesOffering)

-- | Whether the Reserved Instance is applied to instances in a Region or an
-- Availability Zone.
reservedInstancesOffering_scope :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Scope)
reservedInstancesOffering_scope = Lens.lens (\ReservedInstancesOffering' {scope} -> scope) (\s@ReservedInstancesOffering' {} a -> s {scope = a} :: ReservedInstancesOffering)

-- | The Availability Zone in which the Reserved Instance can be used.
reservedInstancesOffering_availabilityZone :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Text)
reservedInstancesOffering_availabilityZone = Lens.lens (\ReservedInstancesOffering' {availabilityZone} -> availabilityZone) (\s@ReservedInstancesOffering' {} a -> s {availabilityZone = a} :: ReservedInstancesOffering)

-- | Indicates whether the offering is available through the Reserved
-- Instance Marketplace (resale) or AWS. If it\'s a Reserved Instance
-- Marketplace offering, this is @true@.
reservedInstancesOffering_marketplace :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Bool)
reservedInstancesOffering_marketplace = Lens.lens (\ReservedInstancesOffering' {marketplace} -> marketplace) (\s@ReservedInstancesOffering' {} a -> s {marketplace = a} :: ReservedInstancesOffering)

-- | If @convertible@ it can be exchanged for Reserved Instances of the same
-- or higher monetary value, with different configurations. If @standard@,
-- it is not possible to perform an exchange.
reservedInstancesOffering_offeringClass :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe OfferingClassType)
reservedInstancesOffering_offeringClass = Lens.lens (\ReservedInstancesOffering' {offeringClass} -> offeringClass) (\s@ReservedInstancesOffering' {} a -> s {offeringClass = a} :: ReservedInstancesOffering)

-- | The purchase price of the Reserved Instance.
reservedInstancesOffering_fixedPrice :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Double)
reservedInstancesOffering_fixedPrice = Lens.lens (\ReservedInstancesOffering' {fixedPrice} -> fixedPrice) (\s@ReservedInstancesOffering' {} a -> s {fixedPrice = a} :: ReservedInstancesOffering)

-- | The usage price of the Reserved Instance, per hour.
reservedInstancesOffering_usagePrice :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Double)
reservedInstancesOffering_usagePrice = Lens.lens (\ReservedInstancesOffering' {usagePrice} -> usagePrice) (\s@ReservedInstancesOffering' {} a -> s {usagePrice = a} :: ReservedInstancesOffering)

-- | The Reserved Instance offering type.
reservedInstancesOffering_offeringType :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe OfferingTypeValues)
reservedInstancesOffering_offeringType = Lens.lens (\ReservedInstancesOffering' {offeringType} -> offeringType) (\s@ReservedInstancesOffering' {} a -> s {offeringType = a} :: ReservedInstancesOffering)

-- | The recurring charge tag assigned to the resource.
reservedInstancesOffering_recurringCharges :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe [RecurringCharge])
reservedInstancesOffering_recurringCharges = Lens.lens (\ReservedInstancesOffering' {recurringCharges} -> recurringCharges) (\s@ReservedInstancesOffering' {} a -> s {recurringCharges = a} :: ReservedInstancesOffering) Prelude.. Lens.mapping Prelude._Coerce

-- | The Reserved Instance product platform description.
reservedInstancesOffering_productDescription :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe RIProductDescription)
reservedInstancesOffering_productDescription = Lens.lens (\ReservedInstancesOffering' {productDescription} -> productDescription) (\s@ReservedInstancesOffering' {} a -> s {productDescription = a} :: ReservedInstancesOffering)

-- | The pricing details of the Reserved Instance offering.
reservedInstancesOffering_pricingDetails :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe [PricingDetail])
reservedInstancesOffering_pricingDetails = Lens.lens (\ReservedInstancesOffering' {pricingDetails} -> pricingDetails) (\s@ReservedInstancesOffering' {} a -> s {pricingDetails = a} :: ReservedInstancesOffering) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML ReservedInstancesOffering where
  parseXML x =
    ReservedInstancesOffering'
      Prelude.<$> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "duration")
      Prelude.<*> (x Prelude..@? "instanceTenancy")
      Prelude.<*> (x Prelude..@? "reservedInstancesOfferingId")
      Prelude.<*> (x Prelude..@? "currencyCode")
      Prelude.<*> (x Prelude..@? "scope")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "marketplace")
      Prelude.<*> (x Prelude..@? "offeringClass")
      Prelude.<*> (x Prelude..@? "fixedPrice")
      Prelude.<*> (x Prelude..@? "usagePrice")
      Prelude.<*> (x Prelude..@? "offeringType")
      Prelude.<*> ( x Prelude..@? "recurringCharges"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "productDescription")
      Prelude.<*> ( x Prelude..@? "pricingDetailsSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )

instance Prelude.Hashable ReservedInstancesOffering

instance Prelude.NFData ReservedInstancesOffering
