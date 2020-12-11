-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstances
  ( ReservedInstances (..),

    -- * Smart constructor
    mkReservedInstances,

    -- * Lenses
    riState,
    riCurrencyCode,
    riInstanceCount,
    riProductDescription,
    riStart,
    riInstanceType,
    riEnd,
    riAvailabilityZone,
    riScope,
    riRecurringCharges,
    riOfferingType,
    riUsagePrice,
    riFixedPrice,
    riReservedInstancesId,
    riInstanceTenancy,
    riOfferingClass,
    riDuration,
    riTags,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance.
--
-- /See:/ 'mkReservedInstances' smart constructor.
data ReservedInstances = ReservedInstances'
  { state ::
      Lude.Maybe ReservedInstanceState,
    currencyCode :: Lude.Maybe CurrencyCodeValues,
    instanceCount :: Lude.Maybe Lude.Int,
    productDescription :: Lude.Maybe RIProductDescription,
    start :: Lude.Maybe Lude.ISO8601,
    instanceType :: Lude.Maybe InstanceType,
    end :: Lude.Maybe Lude.ISO8601,
    availabilityZone :: Lude.Maybe Lude.Text,
    scope :: Lude.Maybe Scope,
    recurringCharges :: Lude.Maybe [RecurringCharge],
    offeringType :: Lude.Maybe OfferingTypeValues,
    usagePrice :: Lude.Maybe Lude.Double,
    fixedPrice :: Lude.Maybe Lude.Double,
    reservedInstancesId :: Lude.Maybe Lude.Text,
    instanceTenancy :: Lude.Maybe Tenancy,
    offeringClass :: Lude.Maybe OfferingClassType,
    duration :: Lude.Maybe Lude.Integer,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstances' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in which the Reserved Instance can be used.
-- * 'currencyCode' - The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
-- * 'duration' - The duration of the Reserved Instance, in seconds.
-- * 'end' - The time when the Reserved Instance expires.
-- * 'fixedPrice' - The purchase price of the Reserved Instance.
-- * 'instanceCount' - The number of reservations purchased.
-- * 'instanceTenancy' - The tenancy of the instance.
-- * 'instanceType' - The instance type on which the Reserved Instance can be used.
-- * 'offeringClass' - The offering class of the Reserved Instance.
-- * 'offeringType' - The Reserved Instance offering type.
-- * 'productDescription' - The Reserved Instance product platform description.
-- * 'recurringCharges' - The recurring charge tag assigned to the resource.
-- * 'reservedInstancesId' - The ID of the Reserved Instance.
-- * 'scope' - The scope of the Reserved Instance.
-- * 'start' - The date and time the Reserved Instance started.
-- * 'state' - The state of the Reserved Instance purchase.
-- * 'tags' - Any tags assigned to the resource.
-- * 'usagePrice' - The usage price of the Reserved Instance, per hour.
mkReservedInstances ::
  ReservedInstances
mkReservedInstances =
  ReservedInstances'
    { state = Lude.Nothing,
      currencyCode = Lude.Nothing,
      instanceCount = Lude.Nothing,
      productDescription = Lude.Nothing,
      start = Lude.Nothing,
      instanceType = Lude.Nothing,
      end = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      scope = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      reservedInstancesId = Lude.Nothing,
      instanceTenancy = Lude.Nothing,
      offeringClass = Lude.Nothing,
      duration = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The state of the Reserved Instance purchase.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riState :: Lens.Lens' ReservedInstances (Lude.Maybe ReservedInstanceState)
riState = Lens.lens (state :: ReservedInstances -> Lude.Maybe ReservedInstanceState) (\s a -> s {state = a} :: ReservedInstances)
{-# DEPRECATED riState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riCurrencyCode :: Lens.Lens' ReservedInstances (Lude.Maybe CurrencyCodeValues)
riCurrencyCode = Lens.lens (currencyCode :: ReservedInstances -> Lude.Maybe CurrencyCodeValues) (\s a -> s {currencyCode = a} :: ReservedInstances)
{-# DEPRECATED riCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The number of reservations purchased.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceCount :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.Int)
riInstanceCount = Lens.lens (instanceCount :: ReservedInstances -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: ReservedInstances)
{-# DEPRECATED riInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The Reserved Instance product platform description.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riProductDescription :: Lens.Lens' ReservedInstances (Lude.Maybe RIProductDescription)
riProductDescription = Lens.lens (productDescription :: ReservedInstances -> Lude.Maybe RIProductDescription) (\s a -> s {productDescription = a} :: ReservedInstances)
{-# DEPRECATED riProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The date and time the Reserved Instance started.
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riStart :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.ISO8601)
riStart = Lens.lens (start :: ReservedInstances -> Lude.Maybe Lude.ISO8601) (\s a -> s {start = a} :: ReservedInstances)
{-# DEPRECATED riStart "Use generic-lens or generic-optics with 'start' instead." #-}

-- | The instance type on which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceType :: Lens.Lens' ReservedInstances (Lude.Maybe InstanceType)
riInstanceType = Lens.lens (instanceType :: ReservedInstances -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: ReservedInstances)
{-# DEPRECATED riInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The time when the Reserved Instance expires.
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEnd :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.ISO8601)
riEnd = Lens.lens (end :: ReservedInstances -> Lude.Maybe Lude.ISO8601) (\s a -> s {end = a} :: ReservedInstances)
{-# DEPRECATED riEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | The Availability Zone in which the Reserved Instance can be used.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riAvailabilityZone :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.Text)
riAvailabilityZone = Lens.lens (availabilityZone :: ReservedInstances -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: ReservedInstances)
{-# DEPRECATED riAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The scope of the Reserved Instance.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riScope :: Lens.Lens' ReservedInstances (Lude.Maybe Scope)
riScope = Lens.lens (scope :: ReservedInstances -> Lude.Maybe Scope) (\s a -> s {scope = a} :: ReservedInstances)
{-# DEPRECATED riScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | The recurring charge tag assigned to the resource.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRecurringCharges :: Lens.Lens' ReservedInstances (Lude.Maybe [RecurringCharge])
riRecurringCharges = Lens.lens (recurringCharges :: ReservedInstances -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedInstances)
{-# DEPRECATED riRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The Reserved Instance offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riOfferingType :: Lens.Lens' ReservedInstances (Lude.Maybe OfferingTypeValues)
riOfferingType = Lens.lens (offeringType :: ReservedInstances -> Lude.Maybe OfferingTypeValues) (\s a -> s {offeringType = a} :: ReservedInstances)
{-# DEPRECATED riOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The usage price of the Reserved Instance, per hour.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riUsagePrice :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.Double)
riUsagePrice = Lens.lens (usagePrice :: ReservedInstances -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedInstances)
{-# DEPRECATED riUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The purchase price of the Reserved Instance.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riFixedPrice :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.Double)
riFixedPrice = Lens.lens (fixedPrice :: ReservedInstances -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedInstances)
{-# DEPRECATED riFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The ID of the Reserved Instance.
--
-- /Note:/ Consider using 'reservedInstancesId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riReservedInstancesId :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.Text)
riReservedInstancesId = Lens.lens (reservedInstancesId :: ReservedInstances -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesId = a} :: ReservedInstances)
{-# DEPRECATED riReservedInstancesId "Use generic-lens or generic-optics with 'reservedInstancesId' instead." #-}

-- | The tenancy of the instance.
--
-- /Note:/ Consider using 'instanceTenancy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riInstanceTenancy :: Lens.Lens' ReservedInstances (Lude.Maybe Tenancy)
riInstanceTenancy = Lens.lens (instanceTenancy :: ReservedInstances -> Lude.Maybe Tenancy) (\s a -> s {instanceTenancy = a} :: ReservedInstances)
{-# DEPRECATED riInstanceTenancy "Use generic-lens or generic-optics with 'instanceTenancy' instead." #-}

-- | The offering class of the Reserved Instance.
--
-- /Note:/ Consider using 'offeringClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riOfferingClass :: Lens.Lens' ReservedInstances (Lude.Maybe OfferingClassType)
riOfferingClass = Lens.lens (offeringClass :: ReservedInstances -> Lude.Maybe OfferingClassType) (\s a -> s {offeringClass = a} :: ReservedInstances)
{-# DEPRECATED riOfferingClass "Use generic-lens or generic-optics with 'offeringClass' instead." #-}

-- | The duration of the Reserved Instance, in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDuration :: Lens.Lens' ReservedInstances (Lude.Maybe Lude.Integer)
riDuration = Lens.lens (duration :: ReservedInstances -> Lude.Maybe Lude.Integer) (\s a -> s {duration = a} :: ReservedInstances)
{-# DEPRECATED riDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | Any tags assigned to the resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riTags :: Lens.Lens' ReservedInstances (Lude.Maybe [Tag])
riTags = Lens.lens (tags :: ReservedInstances -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ReservedInstances)
{-# DEPRECATED riTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML ReservedInstances where
  parseXML x =
    ReservedInstances'
      Lude.<$> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "currencyCode")
      Lude.<*> (x Lude..@? "instanceCount")
      Lude.<*> (x Lude..@? "productDescription")
      Lude.<*> (x Lude..@? "start")
      Lude.<*> (x Lude..@? "instanceType")
      Lude.<*> (x Lude..@? "end")
      Lude.<*> (x Lude..@? "availabilityZone")
      Lude.<*> (x Lude..@? "scope")
      Lude.<*> ( x Lude..@? "recurringCharges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "offeringType")
      Lude.<*> (x Lude..@? "usagePrice")
      Lude.<*> (x Lude..@? "fixedPrice")
      Lude.<*> (x Lude..@? "reservedInstancesId")
      Lude.<*> (x Lude..@? "instanceTenancy")
      Lude.<*> (x Lude..@? "offeringClass")
      Lude.<*> (x Lude..@? "duration")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
