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
-- Module      : Amazonka.EC2.Types.ReservedInstancesOffering
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstancesOffering where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CurrencyCodeValues
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.OfferingClassType
import Amazonka.EC2.Types.OfferingTypeValues
import Amazonka.EC2.Types.PricingDetail
import Amazonka.EC2.Types.RIProductDescription
import Amazonka.EC2.Types.RecurringCharge
import Amazonka.EC2.Types.Scope
import Amazonka.EC2.Types.Tenancy
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'newReservedInstancesOffering' smart constructor.
data ReservedInstancesOffering = ReservedInstancesOffering'
  { -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The currency of the Reserved Instance offering you are purchasing. It\'s
    -- specified using ISO 4217 standard currency codes. At this time, the only
    -- supported currency is @USD@.
    currencyCode :: Prelude.Maybe CurrencyCodeValues,
    -- | The duration of the Reserved Instance, in seconds.
    duration :: Prelude.Maybe Prelude.Integer,
    -- | The purchase price of the Reserved Instance.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The tenancy of the instance.
    instanceTenancy :: Prelude.Maybe Tenancy,
    -- | The instance type on which the Reserved Instance can be used.
    instanceType :: Prelude.Maybe InstanceType,
    -- | Indicates whether the offering is available through the Reserved
    -- Instance Marketplace (resale) or Amazon Web Services. If it\'s a
    -- Reserved Instance Marketplace offering, this is @true@.
    marketplace :: Prelude.Maybe Prelude.Bool,
    -- | If @convertible@ it can be exchanged for Reserved Instances of the same
    -- or higher monetary value, with different configurations. If @standard@,
    -- it is not possible to perform an exchange.
    offeringClass :: Prelude.Maybe OfferingClassType,
    -- | The Reserved Instance offering type.
    offeringType :: Prelude.Maybe OfferingTypeValues,
    -- | The pricing details of the Reserved Instance offering.
    pricingDetails :: Prelude.Maybe [PricingDetail],
    -- | The Reserved Instance product platform description.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The recurring charge tag assigned to the resource.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The ID of the Reserved Instance offering. This is the offering ID used
    -- in GetReservedInstancesExchangeQuote to confirm that an exchange can be
    -- made.
    reservedInstancesOfferingId :: Prelude.Maybe Prelude.Text,
    -- | Whether the Reserved Instance is applied to instances in a Region or an
    -- Availability Zone.
    scope :: Prelude.Maybe Scope,
    -- | The usage price of the Reserved Instance, per hour.
    usagePrice :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstancesOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'reservedInstancesOffering_availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- 'currencyCode', 'reservedInstancesOffering_currencyCode' - The currency of the Reserved Instance offering you are purchasing. It\'s
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is @USD@.
--
-- 'duration', 'reservedInstancesOffering_duration' - The duration of the Reserved Instance, in seconds.
--
-- 'fixedPrice', 'reservedInstancesOffering_fixedPrice' - The purchase price of the Reserved Instance.
--
-- 'instanceTenancy', 'reservedInstancesOffering_instanceTenancy' - The tenancy of the instance.
--
-- 'instanceType', 'reservedInstancesOffering_instanceType' - The instance type on which the Reserved Instance can be used.
--
-- 'marketplace', 'reservedInstancesOffering_marketplace' - Indicates whether the offering is available through the Reserved
-- Instance Marketplace (resale) or Amazon Web Services. If it\'s a
-- Reserved Instance Marketplace offering, this is @true@.
--
-- 'offeringClass', 'reservedInstancesOffering_offeringClass' - If @convertible@ it can be exchanged for Reserved Instances of the same
-- or higher monetary value, with different configurations. If @standard@,
-- it is not possible to perform an exchange.
--
-- 'offeringType', 'reservedInstancesOffering_offeringType' - The Reserved Instance offering type.
--
-- 'pricingDetails', 'reservedInstancesOffering_pricingDetails' - The pricing details of the Reserved Instance offering.
--
-- 'productDescription', 'reservedInstancesOffering_productDescription' - The Reserved Instance product platform description.
--
-- 'recurringCharges', 'reservedInstancesOffering_recurringCharges' - The recurring charge tag assigned to the resource.
--
-- 'reservedInstancesOfferingId', 'reservedInstancesOffering_reservedInstancesOfferingId' - The ID of the Reserved Instance offering. This is the offering ID used
-- in GetReservedInstancesExchangeQuote to confirm that an exchange can be
-- made.
--
-- 'scope', 'reservedInstancesOffering_scope' - Whether the Reserved Instance is applied to instances in a Region or an
-- Availability Zone.
--
-- 'usagePrice', 'reservedInstancesOffering_usagePrice' - The usage price of the Reserved Instance, per hour.
newReservedInstancesOffering ::
  ReservedInstancesOffering
newReservedInstancesOffering =
  ReservedInstancesOffering'
    { availabilityZone =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      duration = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      instanceTenancy = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      marketplace = Prelude.Nothing,
      offeringClass = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      pricingDetails = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      reservedInstancesOfferingId = Prelude.Nothing,
      scope = Prelude.Nothing,
      usagePrice = Prelude.Nothing
    }

-- | The Availability Zone in which the Reserved Instance can be used.
reservedInstancesOffering_availabilityZone :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Text)
reservedInstancesOffering_availabilityZone = Lens.lens (\ReservedInstancesOffering' {availabilityZone} -> availabilityZone) (\s@ReservedInstancesOffering' {} a -> s {availabilityZone = a} :: ReservedInstancesOffering)

-- | The currency of the Reserved Instance offering you are purchasing. It\'s
-- specified using ISO 4217 standard currency codes. At this time, the only
-- supported currency is @USD@.
reservedInstancesOffering_currencyCode :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe CurrencyCodeValues)
reservedInstancesOffering_currencyCode = Lens.lens (\ReservedInstancesOffering' {currencyCode} -> currencyCode) (\s@ReservedInstancesOffering' {} a -> s {currencyCode = a} :: ReservedInstancesOffering)

-- | The duration of the Reserved Instance, in seconds.
reservedInstancesOffering_duration :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Integer)
reservedInstancesOffering_duration = Lens.lens (\ReservedInstancesOffering' {duration} -> duration) (\s@ReservedInstancesOffering' {} a -> s {duration = a} :: ReservedInstancesOffering)

-- | The purchase price of the Reserved Instance.
reservedInstancesOffering_fixedPrice :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Double)
reservedInstancesOffering_fixedPrice = Lens.lens (\ReservedInstancesOffering' {fixedPrice} -> fixedPrice) (\s@ReservedInstancesOffering' {} a -> s {fixedPrice = a} :: ReservedInstancesOffering)

-- | The tenancy of the instance.
reservedInstancesOffering_instanceTenancy :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Tenancy)
reservedInstancesOffering_instanceTenancy = Lens.lens (\ReservedInstancesOffering' {instanceTenancy} -> instanceTenancy) (\s@ReservedInstancesOffering' {} a -> s {instanceTenancy = a} :: ReservedInstancesOffering)

-- | The instance type on which the Reserved Instance can be used.
reservedInstancesOffering_instanceType :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe InstanceType)
reservedInstancesOffering_instanceType = Lens.lens (\ReservedInstancesOffering' {instanceType} -> instanceType) (\s@ReservedInstancesOffering' {} a -> s {instanceType = a} :: ReservedInstancesOffering)

-- | Indicates whether the offering is available through the Reserved
-- Instance Marketplace (resale) or Amazon Web Services. If it\'s a
-- Reserved Instance Marketplace offering, this is @true@.
reservedInstancesOffering_marketplace :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Bool)
reservedInstancesOffering_marketplace = Lens.lens (\ReservedInstancesOffering' {marketplace} -> marketplace) (\s@ReservedInstancesOffering' {} a -> s {marketplace = a} :: ReservedInstancesOffering)

-- | If @convertible@ it can be exchanged for Reserved Instances of the same
-- or higher monetary value, with different configurations. If @standard@,
-- it is not possible to perform an exchange.
reservedInstancesOffering_offeringClass :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe OfferingClassType)
reservedInstancesOffering_offeringClass = Lens.lens (\ReservedInstancesOffering' {offeringClass} -> offeringClass) (\s@ReservedInstancesOffering' {} a -> s {offeringClass = a} :: ReservedInstancesOffering)

-- | The Reserved Instance offering type.
reservedInstancesOffering_offeringType :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe OfferingTypeValues)
reservedInstancesOffering_offeringType = Lens.lens (\ReservedInstancesOffering' {offeringType} -> offeringType) (\s@ReservedInstancesOffering' {} a -> s {offeringType = a} :: ReservedInstancesOffering)

-- | The pricing details of the Reserved Instance offering.
reservedInstancesOffering_pricingDetails :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe [PricingDetail])
reservedInstancesOffering_pricingDetails = Lens.lens (\ReservedInstancesOffering' {pricingDetails} -> pricingDetails) (\s@ReservedInstancesOffering' {} a -> s {pricingDetails = a} :: ReservedInstancesOffering) Prelude.. Lens.mapping Lens.coerced

-- | The Reserved Instance product platform description.
reservedInstancesOffering_productDescription :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe RIProductDescription)
reservedInstancesOffering_productDescription = Lens.lens (\ReservedInstancesOffering' {productDescription} -> productDescription) (\s@ReservedInstancesOffering' {} a -> s {productDescription = a} :: ReservedInstancesOffering)

-- | The recurring charge tag assigned to the resource.
reservedInstancesOffering_recurringCharges :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe [RecurringCharge])
reservedInstancesOffering_recurringCharges = Lens.lens (\ReservedInstancesOffering' {recurringCharges} -> recurringCharges) (\s@ReservedInstancesOffering' {} a -> s {recurringCharges = a} :: ReservedInstancesOffering) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Reserved Instance offering. This is the offering ID used
-- in GetReservedInstancesExchangeQuote to confirm that an exchange can be
-- made.
reservedInstancesOffering_reservedInstancesOfferingId :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Text)
reservedInstancesOffering_reservedInstancesOfferingId = Lens.lens (\ReservedInstancesOffering' {reservedInstancesOfferingId} -> reservedInstancesOfferingId) (\s@ReservedInstancesOffering' {} a -> s {reservedInstancesOfferingId = a} :: ReservedInstancesOffering)

-- | Whether the Reserved Instance is applied to instances in a Region or an
-- Availability Zone.
reservedInstancesOffering_scope :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Scope)
reservedInstancesOffering_scope = Lens.lens (\ReservedInstancesOffering' {scope} -> scope) (\s@ReservedInstancesOffering' {} a -> s {scope = a} :: ReservedInstancesOffering)

-- | The usage price of the Reserved Instance, per hour.
reservedInstancesOffering_usagePrice :: Lens.Lens' ReservedInstancesOffering (Prelude.Maybe Prelude.Double)
reservedInstancesOffering_usagePrice = Lens.lens (\ReservedInstancesOffering' {usagePrice} -> usagePrice) (\s@ReservedInstancesOffering' {} a -> s {usagePrice = a} :: ReservedInstancesOffering)

instance Data.FromXML ReservedInstancesOffering where
  parseXML x =
    ReservedInstancesOffering'
      Prelude.<$> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "currencyCode")
      Prelude.<*> (x Data..@? "duration")
      Prelude.<*> (x Data..@? "fixedPrice")
      Prelude.<*> (x Data..@? "instanceTenancy")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "marketplace")
      Prelude.<*> (x Data..@? "offeringClass")
      Prelude.<*> (x Data..@? "offeringType")
      Prelude.<*> ( x Data..@? "pricingDetailsSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "productDescription")
      Prelude.<*> ( x Data..@? "recurringCharges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "reservedInstancesOfferingId")
      Prelude.<*> (x Data..@? "scope")
      Prelude.<*> (x Data..@? "usagePrice")

instance Prelude.Hashable ReservedInstancesOffering where
  hashWithSalt _salt ReservedInstancesOffering' {..} =
    _salt `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` instanceTenancy
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` marketplace
      `Prelude.hashWithSalt` offeringClass
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` pricingDetails
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` reservedInstancesOfferingId
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` usagePrice

instance Prelude.NFData ReservedInstancesOffering where
  rnf ReservedInstancesOffering' {..} =
    Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf instanceTenancy
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf marketplace
      `Prelude.seq` Prelude.rnf offeringClass
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf pricingDetails
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf reservedInstancesOfferingId
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf usagePrice
