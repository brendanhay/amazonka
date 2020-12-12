{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesOffering
  ( ReservedInstancesOffering (..),

    -- * Smart constructor
    mkReservedInstancesOffering,

    -- * Lenses
    rioMarketplace,
    rioCurrencyCode,
    rioProductDescription,
    rioInstanceType,
    rioAvailabilityZone,
    rioPricingDetails,
    rioScope,
    rioRecurringCharges,
    rioOfferingType,
    rioUsagePrice,
    rioFixedPrice,
    rioInstanceTenancy,
    rioReservedInstancesOfferingId,
    rioOfferingClass,
    rioDuration,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'mkReservedInstancesOffering' smart constructor.
data ReservedInstancesOffering = ReservedInstancesOffering'
  { marketplace ::
      Lude.Maybe Lude.Bool,
    currencyCode ::
      Lude.Maybe CurrencyCodeValues,
    productDescription ::
      Lude.Maybe RIProductDescription,
    instanceType :: Lude.Maybe InstanceType,
    availabilityZone ::
      Lude.Maybe Lude.Text,
    pricingDetails ::
      Lude.Maybe [PricingDetail],
    scope :: Lude.Maybe Scope,
    recurringCharges ::
      Lude.Maybe [RecurringCharge],
    offeringType ::
      Lude.Maybe OfferingTypeValues,
    usagePrice :: Lude.Maybe Lude.Double,
    fixedPrice :: Lude.Maybe Lude.Double,
    instanceTenancy :: Lude.Maybe Tenancy,
    reservedInstancesOfferingId ::
      Lude.Maybe Lude.Text,
    offeringClass ::
      Lude.Maybe OfferingClassType,
    duration :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstancesOffering' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
-- * 'currencyCode' - The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
-- * 'duration' - The duration of the Reserved Instance, in seconds.
-- * 'fixedPrice' - The purchase price of the Reserved Instance.
-- * 'instanceTenancy' - The tenancy of the instance.
-- * 'instanceType' - The instance type on which the Reserved Instance can be used.
-- * 'marketplace' - Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
-- * 'offeringClass' - If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
-- * 'offeringType' - The Reserved Instance offering type.
-- * 'pricingDetails' - The pricing details of the Reserved Instance offering.
-- * 'productDescription' - The Reserved Instance product platform description.
-- * 'recurringCharges' - The recurring charge tag assigned to the resource.
-- * 'reservedInstancesOfferingId' - The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
-- * 'scope' - Whether the Reserved Instance is applied to instances in a Region or an Availability Zone.
-- * 'usagePrice' - The usage price of the Reserved Instance, per hour.
mkReservedInstancesOffering ::
  ReservedInstancesOffering
mkReservedInstancesOffering =
  ReservedInstancesOffering'
    { marketplace = Lude.Nothing,
      currencyCode = Lude.Nothing,
      productDescription = Lude.Nothing,
      instanceType = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      pricingDetails = Lude.Nothing,
      scope = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      instanceTenancy = Lude.Nothing,
      reservedInstancesOfferingId = Lude.Nothing,
      offeringClass = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
--
-- /Note:/ Consider using 'marketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioMarketplace :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Lude.Bool)
rioMarketplace = Lens.lens (marketplace :: ReservedInstancesOffering -> Lude.Maybe Lude.Bool) (\s a -> s {marketplace = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioMarketplace "Use generic-lens or generic-optics with 'marketplace' instead." #-}

-- | The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioCurrencyCode :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe CurrencyCodeValues)
rioCurrencyCode = Lens.lens (currencyCode :: ReservedInstancesOffering -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The Reserved Instance product platform description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioProductDescription :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe RIProductDescription)
rioProductDescription = Lens.lens (productDescription :: ReservedInstancesOffering -> Lude.Maybe RIProductDescription) (\s a -> s {productDescription = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The instance type on which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioInstanceType :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe InstanceType)
rioInstanceType = Lens.lens (instanceType :: ReservedInstancesOffering -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The Availability Zone in which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioAvailabilityZone :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Lude.Text)
rioAvailabilityZone = Lens.lens (availabilityZone :: ReservedInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The pricing details of the Reserved Instance offering.
--
-- /Note:/ Consider using 'pricingDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioPricingDetails :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe [PricingDetail])
rioPricingDetails = Lens.lens (pricingDetails :: ReservedInstancesOffering -> Lude.Maybe [PricingDetail]) (\s a -> s {pricingDetails = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioPricingDetails "Use generic-lens or generic-optics with 'pricingDetails' instead." #-}

-- | Whether the Reserved Instance is applied to instances in a Region or an Availability Zone.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioScope :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Scope)
rioScope = Lens.lens (scope :: ReservedInstancesOffering -> Lude.Maybe Scope) (\s a -> s {scope = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | The recurring charge tag assigned to the resource.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioRecurringCharges :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe [RecurringCharge])
rioRecurringCharges = Lens.lens (recurringCharges :: ReservedInstancesOffering -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The Reserved Instance offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioOfferingType :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe OfferingTypeValues)
rioOfferingType = Lens.lens (offeringType :: ReservedInstancesOffering -> Lude.Maybe OfferingTypeValues) (\s a -> s {offeringType = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The usage price of the Reserved Instance, per hour.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioUsagePrice :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Lude.Double)
rioUsagePrice = Lens.lens (usagePrice :: ReservedInstancesOffering -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The purchase price of the Reserved Instance.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioFixedPrice :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Lude.Double)
rioFixedPrice = Lens.lens (fixedPrice :: ReservedInstancesOffering -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The tenancy of the instance.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioInstanceTenancy :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Tenancy)
rioInstanceTenancy = Lens.lens (instanceTenancy :: ReservedInstancesOffering -> Lude.Maybe Tenancy) (\s a -> s {instanceTenancy = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

-- | The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
--
-- /Note:/ Consider using 'reservedInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioReservedInstancesOfferingId :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Lude.Text)
rioReservedInstancesOfferingId = Lens.lens (reservedInstancesOfferingId :: ReservedInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesOfferingId = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioReservedInstancesOfferingId "Use generic-lens or generic-optics with 'reservedInstancesOfferingId' instead." #-}

-- | If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioOfferingClass :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe OfferingClassType)
rioOfferingClass = Lens.lens (offeringClass :: ReservedInstancesOffering -> Lude.Maybe OfferingClassType) (\s a -> s {offeringClass = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioOfferingClass "Use generic-lens or generic-optics with 'offeringClass' instead." #-}

-- | The duration of the Reserved Instance, in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioDuration :: Lens.Lens' ReservedInstancesOffering (Lude.Maybe Lude.Integer)
rioDuration = Lens.lens (duration :: ReservedInstancesOffering -> Lude.Maybe Lude.Integer) (\s a -> s {duration = a} :: ReservedInstancesOffering)
{-# DEPRECATED rioDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromXML ReservedInstancesOffering where
  parseXML x =
    ReservedInstancesOffering'
      Lude.<$> (x Lude..@? "marketplace")
      Lude.<*> (x Lude..@? "currencyCode")
      Lude.<*> (x Lude..@? "productDescription")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> ( x Lude..@? "pricingDetailsSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "scope")
      Lude.<*> ( x Lude..@? "recurringCharges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "offeringType")
      Lude.<*> (x Lude..@? "usagePrice")
      Lude.<*> (x Lude..@? "fixedPrice")
      Lude.<*> (x Lude..@? "instanceTenancy")
      Lude.<*> (x Lude..@? "reservedInstancesOfferingId")
      Lude.<*> (x Lude..@? "offeringClass")
      Lude.<*> (x Lude..@? "duration")
