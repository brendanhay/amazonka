{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ReservedDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ReservedDBInstance
  ( ReservedDBInstance (..),

    -- * Smart constructor
    mkReservedDBInstance,

    -- * Lenses
    rdbiCurrencyCode,
    rdbiDBInstanceClass,
    rdbiDBInstanceCount,
    rdbiDuration,
    rdbiFixedPrice,
    rdbiLeaseId,
    rdbiMultiAZ,
    rdbiOfferingType,
    rdbiProductDescription,
    rdbiRecurringCharges,
    rdbiReservedDBInstanceArn,
    rdbiReservedDBInstanceId,
    rdbiReservedDBInstancesOfferingId,
    rdbiStartTime,
    rdbiState,
    rdbiUsagePrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.RecurringCharge as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type is used as a response element in the @DescribeReservedDBInstances@ and @PurchaseReservedDBInstancesOffering@ actions.
--
-- /See:/ 'mkReservedDBInstance' smart constructor.
data ReservedDBInstance = ReservedDBInstance'
  { -- | The currency code for the reserved DB instance.
    currencyCode :: Core.Maybe Types.String,
    -- | The DB instance class for the reserved DB instance.
    dBInstanceClass :: Core.Maybe Types.String,
    -- | The number of reserved DB instances.
    dBInstanceCount :: Core.Maybe Core.Int,
    -- | The duration of the reservation in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The fixed price charged for this reserved DB instance.
    fixedPrice :: Core.Maybe Core.Double,
    -- | The unique identifier for the lease associated with the reserved DB instance.
    leaseId :: Core.Maybe Types.String,
    -- | Indicates if the reservation applies to Multi-AZ deployments.
    multiAZ :: Core.Maybe Core.Bool,
    -- | The offering type of this reserved DB instance.
    offeringType :: Core.Maybe Types.String,
    -- | The description of the reserved DB instance.
    productDescription :: Core.Maybe Types.String,
    -- | The recurring price charged to run this reserved DB instance.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The Amazon Resource Name (ARN) for the reserved DB instance.
    reservedDBInstanceArn :: Core.Maybe Types.String,
    -- | The unique identifier for the reservation.
    reservedDBInstanceId :: Core.Maybe Types.String,
    -- | The offering identifier.
    reservedDBInstancesOfferingId :: Core.Maybe Types.String,
    -- | The time the reservation started.
    startTime :: Core.Maybe Core.UTCTime,
    -- | The state of the reserved DB instance.
    state :: Core.Maybe Types.String,
    -- | The hourly price charged for this reserved DB instance.
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReservedDBInstance' value with any optional fields omitted.
mkReservedDBInstance ::
  ReservedDBInstance
mkReservedDBInstance =
  ReservedDBInstance'
    { currencyCode = Core.Nothing,
      dBInstanceClass = Core.Nothing,
      dBInstanceCount = Core.Nothing,
      duration = Core.Nothing,
      fixedPrice = Core.Nothing,
      leaseId = Core.Nothing,
      multiAZ = Core.Nothing,
      offeringType = Core.Nothing,
      productDescription = Core.Nothing,
      recurringCharges = Core.Nothing,
      reservedDBInstanceArn = Core.Nothing,
      reservedDBInstanceId = Core.Nothing,
      reservedDBInstancesOfferingId = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | The currency code for the reserved DB instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiCurrencyCode :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rdbiCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The DB instance class for the reserved DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiDBInstanceClass :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED rdbiDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The number of reserved DB instances.
--
-- /Note:/ Consider using 'dBInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiDBInstanceCount :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Int)
rdbiDBInstanceCount = Lens.field @"dBInstanceCount"
{-# DEPRECATED rdbiDBInstanceCount "Use generic-lens or generic-optics with 'dBInstanceCount' instead." #-}

-- | The duration of the reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiDuration :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Int)
rdbiDuration = Lens.field @"duration"
{-# DEPRECATED rdbiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The fixed price charged for this reserved DB instance.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiFixedPrice :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Double)
rdbiFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED rdbiFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The unique identifier for the lease associated with the reserved DB instance.
--
-- /Note:/ Consider using 'leaseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiLeaseId :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiLeaseId = Lens.field @"leaseId"
{-# DEPRECATED rdbiLeaseId "Use generic-lens or generic-optics with 'leaseId' instead." #-}

-- | Indicates if the reservation applies to Multi-AZ deployments.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiMultiAZ :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Bool)
rdbiMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED rdbiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The offering type of this reserved DB instance.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiOfferingType :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiOfferingType = Lens.field @"offeringType"
{-# DEPRECATED rdbiOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The description of the reserved DB instance.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiProductDescription :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiProductDescription = Lens.field @"productDescription"
{-# DEPRECATED rdbiProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The recurring price charged to run this reserved DB instance.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiRecurringCharges :: Lens.Lens' ReservedDBInstance (Core.Maybe [Types.RecurringCharge])
rdbiRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED rdbiRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The Amazon Resource Name (ARN) for the reserved DB instance.
--
-- /Note:/ Consider using 'reservedDBInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiReservedDBInstanceArn :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiReservedDBInstanceArn = Lens.field @"reservedDBInstanceArn"
{-# DEPRECATED rdbiReservedDBInstanceArn "Use generic-lens or generic-optics with 'reservedDBInstanceArn' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedDBInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiReservedDBInstanceId :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiReservedDBInstanceId = Lens.field @"reservedDBInstanceId"
{-# DEPRECATED rdbiReservedDBInstanceId "Use generic-lens or generic-optics with 'reservedDBInstanceId' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiReservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiReservedDBInstancesOfferingId = Lens.field @"reservedDBInstancesOfferingId"
{-# DEPRECATED rdbiReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiStartTime :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.UTCTime)
rdbiStartTime = Lens.field @"startTime"
{-# DEPRECATED rdbiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The state of the reserved DB instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiState :: Lens.Lens' ReservedDBInstance (Core.Maybe Types.String)
rdbiState = Lens.field @"state"
{-# DEPRECATED rdbiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The hourly price charged for this reserved DB instance.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiUsagePrice :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Double)
rdbiUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED rdbiUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromXML ReservedDBInstance where
  parseXML x =
    ReservedDBInstance'
      Core.<$> (x Core..@? "CurrencyCode")
      Core.<*> (x Core..@? "DBInstanceClass")
      Core.<*> (x Core..@? "DBInstanceCount")
      Core.<*> (x Core..@? "Duration")
      Core.<*> (x Core..@? "FixedPrice")
      Core.<*> (x Core..@? "LeaseId")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "OfferingType")
      Core.<*> (x Core..@? "ProductDescription")
      Core.<*> ( x Core..@? "RecurringCharges"
                   Core..<@> Core.parseXMLList "RecurringCharge"
               )
      Core.<*> (x Core..@? "ReservedDBInstanceArn")
      Core.<*> (x Core..@? "ReservedDBInstanceId")
      Core.<*> (x Core..@? "ReservedDBInstancesOfferingId")
      Core.<*> (x Core..@? "StartTime")
      Core.<*> (x Core..@? "State")
      Core.<*> (x Core..@? "UsagePrice")
