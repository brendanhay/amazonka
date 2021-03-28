{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ReservedDBInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ReservedDBInstancesOffering
  ( ReservedDBInstancesOffering (..)
  -- * Smart constructor
  , mkReservedDBInstancesOffering
  -- * Lenses
  , rdbioCurrencyCode
  , rdbioDBInstanceClass
  , rdbioDuration
  , rdbioFixedPrice
  , rdbioMultiAZ
  , rdbioOfferingType
  , rdbioProductDescription
  , rdbioRecurringCharges
  , rdbioReservedDBInstancesOfferingId
  , rdbioUsagePrice
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.RecurringCharge as Types

-- | This data type is used as a response element in the @DescribeReservedDBInstancesOfferings@ action. 
--
-- /See:/ 'mkReservedDBInstancesOffering' smart constructor.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'
  { currencyCode :: Core.Maybe Core.Text
    -- ^ The currency code for the reserved DB instance offering.
  , dBInstanceClass :: Core.Maybe Core.Text
    -- ^ The DB instance class for the reserved DB instance.
  , duration :: Core.Maybe Core.Int
    -- ^ The duration of the offering in seconds.
  , fixedPrice :: Core.Maybe Core.Double
    -- ^ The fixed price charged for this offering.
  , multiAZ :: Core.Maybe Core.Bool
    -- ^ Indicates if the offering applies to Multi-AZ deployments.
  , offeringType :: Core.Maybe Core.Text
    -- ^ The offering type.
  , productDescription :: Core.Maybe Core.Text
    -- ^ The database engine used by the offering.
  , recurringCharges :: Core.Maybe [Types.RecurringCharge]
    -- ^ The recurring price charged to run this reserved DB instance.
  , reservedDBInstancesOfferingId :: Core.Maybe Core.Text
    -- ^ The offering identifier.
  , usagePrice :: Core.Maybe Core.Double
    -- ^ The hourly price charged for this offering.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedDBInstancesOffering' value with any optional fields omitted.
mkReservedDBInstancesOffering
    :: ReservedDBInstancesOffering
mkReservedDBInstancesOffering
  = ReservedDBInstancesOffering'{currencyCode = Core.Nothing,
                                 dBInstanceClass = Core.Nothing, duration = Core.Nothing,
                                 fixedPrice = Core.Nothing, multiAZ = Core.Nothing,
                                 offeringType = Core.Nothing, productDescription = Core.Nothing,
                                 recurringCharges = Core.Nothing,
                                 reservedDBInstancesOfferingId = Core.Nothing,
                                 usagePrice = Core.Nothing}

-- | The currency code for the reserved DB instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioCurrencyCode :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
rdbioCurrencyCode = Lens.field @"currencyCode"
{-# INLINEABLE rdbioCurrencyCode #-}
{-# DEPRECATED currencyCode "Use generic-lens or generic-optics with 'currencyCode' instead"  #-}

-- | The DB instance class for the reserved DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioDBInstanceClass :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
rdbioDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE rdbioDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The duration of the offering in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioDuration :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Int)
rdbioDuration = Lens.field @"duration"
{-# INLINEABLE rdbioDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The fixed price charged for this offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioFixedPrice :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Double)
rdbioFixedPrice = Lens.field @"fixedPrice"
{-# INLINEABLE rdbioFixedPrice #-}
{-# DEPRECATED fixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead"  #-}

-- | Indicates if the offering applies to Multi-AZ deployments.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioMultiAZ :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Bool)
rdbioMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE rdbioMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioOfferingType :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
rdbioOfferingType = Lens.field @"offeringType"
{-# INLINEABLE rdbioOfferingType #-}
{-# DEPRECATED offeringType "Use generic-lens or generic-optics with 'offeringType' instead"  #-}

-- | The database engine used by the offering.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioProductDescription :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
rdbioProductDescription = Lens.field @"productDescription"
{-# INLINEABLE rdbioProductDescription #-}
{-# DEPRECATED productDescription "Use generic-lens or generic-optics with 'productDescription' instead"  #-}

-- | The recurring price charged to run this reserved DB instance.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioRecurringCharges :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe [Types.RecurringCharge])
rdbioRecurringCharges = Lens.field @"recurringCharges"
{-# INLINEABLE rdbioRecurringCharges #-}
{-# DEPRECATED recurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead"  #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioReservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Text)
rdbioReservedDBInstancesOfferingId = Lens.field @"reservedDBInstancesOfferingId"
{-# INLINEABLE rdbioReservedDBInstancesOfferingId #-}
{-# DEPRECATED reservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead"  #-}

-- | The hourly price charged for this offering.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioUsagePrice :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Double)
rdbioUsagePrice = Lens.field @"usagePrice"
{-# INLINEABLE rdbioUsagePrice #-}
{-# DEPRECATED usagePrice "Use generic-lens or generic-optics with 'usagePrice' instead"  #-}

instance Core.FromXML ReservedDBInstancesOffering where
        parseXML x
          = ReservedDBInstancesOffering' Core.<$>
              (x Core..@? "CurrencyCode") Core.<*> x Core..@? "DBInstanceClass"
                Core.<*> x Core..@? "Duration"
                Core.<*> x Core..@? "FixedPrice"
                Core.<*> x Core..@? "MultiAZ"
                Core.<*> x Core..@? "OfferingType"
                Core.<*> x Core..@? "ProductDescription"
                Core.<*>
                x Core..@? "RecurringCharges" Core..<@>
                  Core.parseXMLList "RecurringCharge"
                Core.<*> x Core..@? "ReservedDBInstancesOfferingId"
                Core.<*> x Core..@? "UsagePrice"
