{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ReservedDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ReservedDBInstance
  ( ReservedDBInstance (..)
  -- * Smart constructor
  , mkReservedDBInstance
  -- * Lenses
  , rdbiCurrencyCode
  , rdbiDBInstanceClass
  , rdbiDBInstanceCount
  , rdbiDuration
  , rdbiFixedPrice
  , rdbiLeaseId
  , rdbiMultiAZ
  , rdbiOfferingType
  , rdbiProductDescription
  , rdbiRecurringCharges
  , rdbiReservedDBInstanceArn
  , rdbiReservedDBInstanceId
  , rdbiReservedDBInstancesOfferingId
  , rdbiStartTime
  , rdbiState
  , rdbiUsagePrice
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.RecurringCharge as Types

-- | This data type is used as a response element in the @DescribeReservedDBInstances@ and @PurchaseReservedDBInstancesOffering@ actions. 
--
-- /See:/ 'mkReservedDBInstance' smart constructor.
data ReservedDBInstance = ReservedDBInstance'
  { currencyCode :: Core.Maybe Core.Text
    -- ^ The currency code for the reserved DB instance.
  , dBInstanceClass :: Core.Maybe Core.Text
    -- ^ The DB instance class for the reserved DB instance.
  , dBInstanceCount :: Core.Maybe Core.Int
    -- ^ The number of reserved DB instances.
  , duration :: Core.Maybe Core.Int
    -- ^ The duration of the reservation in seconds.
  , fixedPrice :: Core.Maybe Core.Double
    -- ^ The fixed price charged for this reserved DB instance.
  , leaseId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the lease associated with the reserved DB instance.
  , multiAZ :: Core.Maybe Core.Bool
    -- ^ Indicates if the reservation applies to Multi-AZ deployments.
  , offeringType :: Core.Maybe Core.Text
    -- ^ The offering type of this reserved DB instance.
  , productDescription :: Core.Maybe Core.Text
    -- ^ The description of the reserved DB instance.
  , recurringCharges :: Core.Maybe [Types.RecurringCharge]
    -- ^ The recurring price charged to run this reserved DB instance.
  , reservedDBInstanceArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the reserved DB instance.
  , reservedDBInstanceId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the reservation.
  , reservedDBInstancesOfferingId :: Core.Maybe Core.Text
    -- ^ The offering identifier.
  , startTime :: Core.Maybe Core.UTCTime
    -- ^ The time the reservation started.
  , state :: Core.Maybe Core.Text
    -- ^ The state of the reserved DB instance.
  , usagePrice :: Core.Maybe Core.Double
    -- ^ The hourly price charged for this reserved DB instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReservedDBInstance' value with any optional fields omitted.
mkReservedDBInstance
    :: ReservedDBInstance
mkReservedDBInstance
  = ReservedDBInstance'{currencyCode = Core.Nothing,
                        dBInstanceClass = Core.Nothing, dBInstanceCount = Core.Nothing,
                        duration = Core.Nothing, fixedPrice = Core.Nothing,
                        leaseId = Core.Nothing, multiAZ = Core.Nothing,
                        offeringType = Core.Nothing, productDescription = Core.Nothing,
                        recurringCharges = Core.Nothing,
                        reservedDBInstanceArn = Core.Nothing,
                        reservedDBInstanceId = Core.Nothing,
                        reservedDBInstancesOfferingId = Core.Nothing,
                        startTime = Core.Nothing, state = Core.Nothing,
                        usagePrice = Core.Nothing}

-- | The currency code for the reserved DB instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiCurrencyCode :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE rdbiCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The DB instance class for the reserved DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiDBInstanceClass :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE rdbiDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The number of reserved DB instances.
--
-- /Note:/ Consider using 'dBInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiDBInstanceCount :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Int)
rdbiDBInstanceCount = Lens.field @"dBInstanceCount"
{-# INLINEABLE rdbiDBInstanceCount #-}
{-# DEPRECATED dBInstanceCount "Use generic-lens or generic-optics with 'dBInstanceCount' instead"  #-}

-- | The duration of the reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiDuration :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Int)
rdbiDuration = Lens.field @"duration"
{-# INLINEABLE rdbiDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The fixed price charged for this reserved DB instance.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiFixedPrice :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Double)
rdbiFixedPrice = Lens.field @"fixedPrice"
{-# INLINEABLE rdbiFixedPrice #-}
{-# DEPRECATED fixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead"  #-}

-- | The unique identifier for the lease associated with the reserved DB instance.
--
-- /Note:/ Consider using 'leaseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiLeaseId :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiLeaseId = Lens.field @"leaseId"
{-# INLINEABLE rdbiLeaseId #-}
{-# DEPRECATED leaseId "Use generic-lens or generic-optics with 'leaseId' instead"  #-}

-- | Indicates if the reservation applies to Multi-AZ deployments.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiMultiAZ :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Bool)
rdbiMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE rdbiMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The offering type of this reserved DB instance.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiOfferingType :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiOfferingType = Lens.field @"offeringType"
{-# INLINEABLE rdbiOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | The description of the reserved DB instance.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiProductDescription :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiProductDescription = Lens.field @"productDescription"
{-# INLINEABLE rdbiProductDescription #-}
{-# DEPRECATED productDescription "Use generic-lens or generic-optics with 'productDescription' instead"  #-}

-- | The recurring price charged to run this reserved DB instance.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiRecurringCharges :: Lens.Lens' ReservedDBInstance (Core.Maybe [Types.RecurringCharge])
rdbiRecurringCharges = Lens.field @"recurringCharges"
{-# INLINEABLE rdbiRecurringCharges #-}
{-# DEPRECATED recurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead"  #-}

-- | The Amazon Resource Name (ARN) for the reserved DB instance.
--
-- /Note:/ Consider using 'reservedDBInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiReservedDBInstanceArn :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiReservedDBInstanceArn = Lens.field @"reservedDBInstanceArn"
{-# INLINEABLE rdbiReservedDBInstanceArn #-}
{-# DEPRECATED reservedDBInstanceArn "Use generic-lens or generic-optics with 'reservedDBInstanceArn' instead"  #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedDBInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiReservedDBInstanceId :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiReservedDBInstanceId = Lens.field @"reservedDBInstanceId"
{-# INLINEABLE rdbiReservedDBInstanceId #-}
{-# DEPRECATED reservedDBInstanceId "Use generic-lens or generic-optics with 'reservedDBInstanceId' instead"  #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiReservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiReservedDBInstancesOfferingId = Lens.field @"reservedDBInstancesOfferingId"
{-# INLINEABLE rdbiReservedDBInstancesOfferingId #-}
{-# DEPRECATED reservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead"  #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiStartTime :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.UTCTime)
rdbiStartTime = Lens.field @"startTime"
{-# INLINEABLE rdbiStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The state of the reserved DB instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiState :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Text)
rdbiState = Lens.field @"state"
{-# INLINEABLE rdbiState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The hourly price charged for this reserved DB instance.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbiUsagePrice :: Lens.Lens' ReservedDBInstance (Core.Maybe Core.Double)
rdbiUsagePrice = Lens.field @"usagePrice"
{-# INLINEABLE rdbiUsagePrice #-}
{-# DEPRECATED usagePrice "Use generic-lens or generic-optics with 'usagePrice' instead"  #-}

instance Core.FromXML ReservedDBInstance where
        parseXML x
          = ReservedDBInstance' Core.<$>
              (x Core..@? "CurrencyCode") Core.<*> x Core..@? "DBInstanceClass"
                Core.<*> x Core..@? "DBInstanceCount"
                Core.<*> x Core..@? "Duration"
                Core.<*> x Core..@? "FixedPrice"
                Core.<*> x Core..@? "LeaseId"
                Core.<*> x Core..@? "MultiAZ"
                Core.<*> x Core..@? "OfferingType"
                Core.<*> x Core..@? "ProductDescription"
                Core.<*>
                x Core..@? "RecurringCharges" Core..<@>
                  Core.parseXMLList "RecurringCharge"
                Core.<*> x Core..@? "ReservedDBInstanceArn"
                Core.<*> x Core..@? "ReservedDBInstanceId"
                Core.<*> x Core..@? "ReservedDBInstancesOfferingId"
                Core.<*> x Core..@? "StartTime"
                Core.<*> x Core..@? "State"
                Core.<*> x Core..@? "UsagePrice"
