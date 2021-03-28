{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservedInstances
  ( ReservedInstances (..)
  -- * Smart constructor
  , mkReservedInstances
  -- * Lenses
  , riAvailabilityZone
  , riCurrencyCode
  , riDuration
  , riEnd
  , riFixedPrice
  , riInstanceCount
  , riInstanceTenancy
  , riInstanceType
  , riOfferingClass
  , riOfferingType
  , riProductDescription
  , riRecurringCharges
  , riReservedInstancesId
  , riScope
  , riStart
  , riState
  , riTags
  , riUsagePrice
  ) where

import qualified Network.AWS.EC2.Types.CurrencyCodeValues as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.OfferingClassType as Types
import qualified Network.AWS.EC2.Types.OfferingTypeValues as Types
import qualified Network.AWS.EC2.Types.RIProductDescription as Types
import qualified Network.AWS.EC2.Types.RecurringCharge as Types
import qualified Network.AWS.EC2.Types.ReservedInstanceState as Types
import qualified Network.AWS.EC2.Types.Scope as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.Tenancy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a Reserved Instance.
--
-- /See:/ 'mkReservedInstances' smart constructor.
data ReservedInstances = ReservedInstances'
  { availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone in which the Reserved Instance can be used.
  , currencyCode :: Core.Maybe Types.CurrencyCodeValues
    -- ^ The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
  , duration :: Core.Maybe Core.Integer
    -- ^ The duration of the Reserved Instance, in seconds.
  , end :: Core.Maybe Core.UTCTime
    -- ^ The time when the Reserved Instance expires.
  , fixedPrice :: Core.Maybe Core.Double
    -- ^ The purchase price of the Reserved Instance.
  , instanceCount :: Core.Maybe Core.Int
    -- ^ The number of reservations purchased.
  , instanceTenancy :: Core.Maybe Types.Tenancy
    -- ^ The tenancy of the instance.
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type on which the Reserved Instance can be used.
  , offeringClass :: Core.Maybe Types.OfferingClassType
    -- ^ The offering class of the Reserved Instance.
  , offeringType :: Core.Maybe Types.OfferingTypeValues
    -- ^ The Reserved Instance offering type.
  , productDescription :: Core.Maybe Types.RIProductDescription
    -- ^ The Reserved Instance product platform description.
  , recurringCharges :: Core.Maybe [Types.RecurringCharge]
    -- ^ The recurring charge tag assigned to the resource.
  , reservedInstancesId :: Core.Maybe Core.Text
    -- ^ The ID of the Reserved Instance.
  , scope :: Core.Maybe Types.Scope
    -- ^ The scope of the Reserved Instance.
  , start :: Core.Maybe Core.UTCTime
    -- ^ The date and time the Reserved Instance started.
  , state :: Core.Maybe Types.ReservedInstanceState
    -- ^ The state of the Reserved Instance purchase.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the resource.
  , usagePrice :: Core.Maybe Core.Double
    -- ^ The usage price of the Reserved Instance, per hour.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReservedInstances' value with any optional fields omitted.
mkReservedInstances
    :: ReservedInstances
mkReservedInstances
  = ReservedInstances'{availabilityZone = Core.Nothing,
                       currencyCode = Core.Nothing, duration = Core.Nothing,
                       end = Core.Nothing, fixedPrice = Core.Nothing,
                       instanceCount = Core.Nothing, instanceTenancy = Core.Nothing,
                       instanceType = Core.Nothing, offeringClass = Core.Nothing,
                       offeringType = Core.Nothing, productDescription = Core.Nothing,
                       recurringCharges = Core.Nothing,
                       reservedInstancesId = Core.Nothing, scope = Core.Nothing,
                       start = Core.Nothing, state = Core.Nothing, tags = Core.Nothing,
                       usagePrice = Core.Nothing}

-- | The Availability Zone in which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAvailabilityZone :: Lens.Lens' ReservedInstances (Core.Maybe Core.Text)
riAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE riAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riCurrencyCode :: Lens.Lens' ReservedInstances (Core.Maybe Types.CurrencyCodeValues)
riCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE riCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The duration of the Reserved Instance, in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDuration :: Lens.Lens' ReservedInstances (Core.Maybe Core.Integer)
riDuration = Lens.field @"duration"
{-# INLINEABLE riDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The time when the Reserved Instance expires.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEnd :: Lens.Lens' ReservedInstances (Core.Maybe Core.UTCTime)
riEnd = Lens.field @"end"
{-# INLINEABLE riEnd #-}
{-# DEPRECATED end "Use generic-lens or generic-optics with 'end' instead"  #-}

-- | The purchase price of the Reserved Instance.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riFixedPrice :: Lens.Lens' ReservedInstances (Core.Maybe Core.Double)
riFixedPrice = Lens.field @"fixedPrice"
{-# INLINEABLE riFixedPrice #-}
{-# DEPRECATED fixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead"  #-}

-- | The number of reservations purchased.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceCount :: Lens.Lens' ReservedInstances (Core.Maybe Core.Int)
riInstanceCount = Lens.field @"instanceCount"
{-# INLINEABLE riInstanceCount #-}
{-# DEPRECATED instanceCount "Use generic-lens or generic-optics with 'instanceCount' instead"  #-}

-- | The tenancy of the instance.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceTenancy :: Lens.Lens' ReservedInstances (Core.Maybe Types.Tenancy)
riInstanceTenancy = Lens.field @"instanceTenancy"
{-# INLINEABLE riInstanceTenancy #-}
{-# DEPRECATED instanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead"  #-}

-- | The instance type on which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceType :: Lens.Lens' ReservedInstances (Core.Maybe Types.InstanceType)
riInstanceType = Lens.field @"instanceType"
{-# INLINEABLE riInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | The offering class of the Reserved Instance.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riOfferingClass :: Lens.Lens' ReservedInstances (Core.Maybe Types.OfferingClassType)
riOfferingClass = Lens.field @"offeringClass"
{-# INLINEABLE riOfferingClass #-}
{-# DEPRECATED offeringClass "Use generic-lens or generic-optics with 'offeringClass' instead"  #-}

-- | The Reserved Instance offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riOfferingType :: Lens.Lens' ReservedInstances (Core.Maybe Types.OfferingTypeValues)
riOfferingType = Lens.field @"offeringType"
{-# INLINEABLE riOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | The Reserved Instance product platform description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riProductDescription :: Lens.Lens' ReservedInstances (Core.Maybe Types.RIProductDescription)
riProductDescription = Lens.field @"productDescription"
{-# INLINEABLE riProductDescription #-}
{-# DEPRECATED productDescription "Use generic-lens or generic-optics with 'productDescription' instead"  #-}

-- | The recurring charge tag assigned to the resource.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRecurringCharges :: Lens.Lens' ReservedInstances (Core.Maybe [Types.RecurringCharge])
riRecurringCharges = Lens.field @"recurringCharges"
{-# INLINEABLE riRecurringCharges #-}
{-# DEPRECATED recurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead"  #-}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReservedInstancesId :: Lens.Lens' ReservedInstances (Core.Maybe Core.Text)
riReservedInstancesId = Lens.field @"reservedInstancesId"
{-# INLINEABLE riReservedInstancesId #-}
{-# DEPRECATED reservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead"  #-}

-- | The scope of the Reserved Instance.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riScope :: Lens.Lens' ReservedInstances (Core.Maybe Types.Scope)
riScope = Lens.field @"scope"
{-# INLINEABLE riScope #-}
{-# DEPRECATED scope "Use generic-lens or generic-optics with 'scope' instead"  #-}

-- | The date and time the Reserved Instance started.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riStart :: Lens.Lens' ReservedInstances (Core.Maybe Core.UTCTime)
riStart = Lens.field @"start"
{-# INLINEABLE riStart #-}
{-# DEPRECATED start "Use generic-lens or generic-optics with 'start' instead"  #-}

-- | The state of the Reserved Instance purchase.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riState :: Lens.Lens' ReservedInstances (Core.Maybe Types.ReservedInstanceState)
riState = Lens.field @"state"
{-# INLINEABLE riState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riTags :: Lens.Lens' ReservedInstances (Core.Maybe [Types.Tag])
riTags = Lens.field @"tags"
{-# INLINEABLE riTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The usage price of the Reserved Instance, per hour.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riUsagePrice :: Lens.Lens' ReservedInstances (Core.Maybe Core.Double)
riUsagePrice = Lens.field @"usagePrice"
{-# INLINEABLE riUsagePrice #-}
{-# DEPRECATED usagePrice "Use generic-lens or generic-optics with 'usagePrice' instead"  #-}

instance Core.FromXML ReservedInstances where
        parseXML x
          = ReservedInstances' Core.<$>
              (x Core..@? "availabilityZone") Core.<*> x Core..@? "currencyCode"
                Core.<*> x Core..@? "duration"
                Core.<*> x Core..@? "end"
                Core.<*> x Core..@? "fixedPrice"
                Core.<*> x Core..@? "instanceCount"
                Core.<*> x Core..@? "instanceTenancy"
                Core.<*> x Core..@? "instanceType"
                Core.<*> x Core..@? "offeringClass"
                Core.<*> x Core..@? "offeringType"
                Core.<*> x Core..@? "productDescription"
                Core.<*>
                x Core..@? "recurringCharges" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "reservedInstancesId"
                Core.<*> x Core..@? "scope"
                Core.<*> x Core..@? "start"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "usagePrice"
