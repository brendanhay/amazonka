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
    rioAvailabilityZone,
    rioCurrencyCode,
    rioDuration,
    rioFixedPrice,
    rioInstanceTenancy,
    rioInstanceType,
    rioMarketplace,
    rioOfferingClass,
    rioOfferingType,
    rioPricingDetails,
    rioProductDescription,
    rioRecurringCharges,
    rioReservedInstancesOfferingId,
    rioScope,
    rioUsagePrice,
  )
where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.OfferingClassType as Types
import qualified Network.AWS.EC2.Types.OfferingTypeValues as Types
import qualified Network.AWS.EC2.Types.PricingDetail as Types
import qualified Network.AWS.EC2.Types.RIProductDescription as Types
import qualified Network.AWS.EC2.Types.RecurringCharge as Types
import qualified Network.AWS.EC2.Types.Scope as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'mkReservedInstancesOffering' smart constructor.
data ReservedInstancesOffering = ReservedInstancesOffering'
  { -- | The Availability Zone in which the Reserved Instance can be used.
    availabilityZone :: Core.Maybe Types.String,
    -- | The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
    currencyCode :: Core.Maybe Types.CurrencyCodeValues,
    -- | The duration of the Reserved Instance, in seconds.
    duration :: Core.Maybe Core.Integer,
    -- | The purchase price of the Reserved Instance.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The tenancy of the instance.
    instanceTenancy :: Core.Maybe Types.Tenancy,
    -- | The instance type on which the Reserved Instance can be used.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
    marketplace :: Core.Maybe Core.Bool,
    -- | If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
    offeringClass :: Core.Maybe Types.OfferingClassType,
    -- | The Reserved Instance offering type.
    offeringType :: Core.Maybe Types.OfferingTypeValues,
    -- | The pricing details of the Reserved Instance offering.
    pricingDetails :: Core.Maybe [Types.PricingDetail],
    -- | The Reserved Instance product platform description.
    productDescription :: Core.Maybe Types.RIProductDescription,
    -- | The recurring charge tag assigned to the resource.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
    reservedInstancesOfferingId :: Core.Maybe Types.String,
    -- | Whether the Reserved Instance is applied to instances in a Region or an Availability Zone.
    scope :: Core.Maybe Types.Scope,
    -- | The usage price of the Reserved Instance, per hour.
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedInstancesOffering' value with any optional fields omitted.
mkReservedInstancesOffering ::
  ReservedInstancesOffering
mkReservedInstancesOffering =
  ReservedInstancesOffering'
    { availabilityZone = Core.Nothing,
      currencyCode = Core.Nothing,
      duration = Core.Nothing,
      fixedPrice = Core.Nothing,
      instanceTenancy = Core.Nothing,
      instanceType = Core.Nothing,
      marketplace = Core.Nothing,
      offeringClass = Core.Nothing,
      offeringType = Core.Nothing,
      pricingDetails = Core.Nothing,
      productDescription = Core.Nothing,
      recurringCharges = Core.Nothing,
      reservedInstancesOfferingId = Core.Nothing,
      scope = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | The Availability Zone in which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioAvailabilityZone :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.String)
rioAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED rioAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioCurrencyCode :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.CurrencyCodeValues)
rioCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rioCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The duration of the Reserved Instance, in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioDuration :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Core.Integer)
rioDuration = Lens.field @"duration"
{-# DEPRECATED rioDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The purchase price of the Reserved Instance.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioFixedPrice :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Core.Double)
rioFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED rioFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The tenancy of the instance.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioInstanceTenancy :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.Tenancy)
rioInstanceTenancy = Lens.field @"instanceTenancy"
{-# DEPRECATED rioInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

-- | The instance type on which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioInstanceType :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.InstanceType)
rioInstanceType = Lens.field @"instanceType"
{-# DEPRECATED rioInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
--
-- /Note:/ Consider using 'marketplace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioMarketplace :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Core.Bool)
rioMarketplace = Lens.field @"marketplace"
{-# DEPRECATED rioMarketplace "Use generic-lens or generic-optics with 'marketplace' instead." #-}

-- | If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioOfferingClass :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.OfferingClassType)
rioOfferingClass = Lens.field @"offeringClass"
{-# DEPRECATED rioOfferingClass "Use generic-lens or generic-optics with 'offeringClass' instead." #-}

-- | The Reserved Instance offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioOfferingType :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.OfferingTypeValues)
rioOfferingType = Lens.field @"offeringType"
{-# DEPRECATED rioOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The pricing details of the Reserved Instance offering.
--
-- /Note:/ Consider using 'pricingDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioPricingDetails :: Lens.Lens' ReservedInstancesOffering (Core.Maybe [Types.PricingDetail])
rioPricingDetails = Lens.field @"pricingDetails"
{-# DEPRECATED rioPricingDetails "Use generic-lens or generic-optics with 'pricingDetails' instead." #-}

-- | The Reserved Instance product platform description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioProductDescription :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.RIProductDescription)
rioProductDescription = Lens.field @"productDescription"
{-# DEPRECATED rioProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The recurring charge tag assigned to the resource.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioRecurringCharges :: Lens.Lens' ReservedInstancesOffering (Core.Maybe [Types.RecurringCharge])
rioRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED rioRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
--
-- /Note:/ Consider using 'reservedInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioReservedInstancesOfferingId :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.String)
rioReservedInstancesOfferingId = Lens.field @"reservedInstancesOfferingId"
{-# DEPRECATED rioReservedInstancesOfferingId "Use generic-lens or generic-optics with 'reservedInstancesOfferingId' instead." #-}

-- | Whether the Reserved Instance is applied to instances in a Region or an Availability Zone.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioScope :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Types.Scope)
rioScope = Lens.field @"scope"
{-# DEPRECATED rioScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | The usage price of the Reserved Instance, per hour.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rioUsagePrice :: Lens.Lens' ReservedInstancesOffering (Core.Maybe Core.Double)
rioUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED rioUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromXML ReservedInstancesOffering where
  parseXML x =
    ReservedInstancesOffering'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "currencyCode")
      Core.<*> (x Core..@? "duration")
      Core.<*> (x Core..@? "fixedPrice")
      Core.<*> (x Core..@? "instanceTenancy")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "marketplace")
      Core.<*> (x Core..@? "offeringClass")
      Core.<*> (x Core..@? "offeringType")
      Core.<*> (x Core..@? "pricingDetailsSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "productDescription")
      Core.<*> (x Core..@? "recurringCharges" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "reservedInstancesOfferingId")
      Core.<*> (x Core..@? "scope")
      Core.<*> (x Core..@? "usagePrice")
