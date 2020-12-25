{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ReservedDBInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ReservedDBInstancesOffering
  ( ReservedDBInstancesOffering (..),

    -- * Smart constructor
    mkReservedDBInstancesOffering,

    -- * Lenses
    rdbioCurrencyCode,
    rdbioDBInstanceClass,
    rdbioDuration,
    rdbioFixedPrice,
    rdbioMultiAZ,
    rdbioOfferingType,
    rdbioProductDescription,
    rdbioRecurringCharges,
    rdbioReservedDBInstancesOfferingId,
    rdbioUsagePrice,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.RecurringCharge as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | This data type is used as a response element in the @DescribeReservedDBInstancesOfferings@ action.
--
-- /See:/ 'mkReservedDBInstancesOffering' smart constructor.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'
  { -- | The currency code for the reserved DB instance offering.
    currencyCode :: Core.Maybe Types.String,
    -- | The DB instance class for the reserved DB instance.
    dBInstanceClass :: Core.Maybe Types.String,
    -- | The duration of the offering in seconds.
    duration :: Core.Maybe Core.Int,
    -- | The fixed price charged for this offering.
    fixedPrice :: Core.Maybe Core.Double,
    -- | Indicates if the offering applies to Multi-AZ deployments.
    multiAZ :: Core.Maybe Core.Bool,
    -- | The offering type.
    offeringType :: Core.Maybe Types.String,
    -- | The database engine used by the offering.
    productDescription :: Core.Maybe Types.String,
    -- | The recurring price charged to run this reserved DB instance.
    recurringCharges :: Core.Maybe [Types.RecurringCharge],
    -- | The offering identifier.
    reservedDBInstancesOfferingId :: Core.Maybe Types.String,
    -- | The hourly price charged for this offering.
    usagePrice :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedDBInstancesOffering' value with any optional fields omitted.
mkReservedDBInstancesOffering ::
  ReservedDBInstancesOffering
mkReservedDBInstancesOffering =
  ReservedDBInstancesOffering'
    { currencyCode = Core.Nothing,
      dBInstanceClass = Core.Nothing,
      duration = Core.Nothing,
      fixedPrice = Core.Nothing,
      multiAZ = Core.Nothing,
      offeringType = Core.Nothing,
      productDescription = Core.Nothing,
      recurringCharges = Core.Nothing,
      reservedDBInstancesOfferingId = Core.Nothing,
      usagePrice = Core.Nothing
    }

-- | The currency code for the reserved DB instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioCurrencyCode :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Types.String)
rdbioCurrencyCode = Lens.field @"currencyCode"
{-# DEPRECATED rdbioCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The DB instance class for the reserved DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioDBInstanceClass :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Types.String)
rdbioDBInstanceClass = Lens.field @"dBInstanceClass"
{-# DEPRECATED rdbioDBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead." #-}

-- | The duration of the offering in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioDuration :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Int)
rdbioDuration = Lens.field @"duration"
{-# DEPRECATED rdbioDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The fixed price charged for this offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioFixedPrice :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Double)
rdbioFixedPrice = Lens.field @"fixedPrice"
{-# DEPRECATED rdbioFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Indicates if the offering applies to Multi-AZ deployments.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioMultiAZ :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Bool)
rdbioMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED rdbioMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioOfferingType :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Types.String)
rdbioOfferingType = Lens.field @"offeringType"
{-# DEPRECATED rdbioOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The database engine used by the offering.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioProductDescription :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Types.String)
rdbioProductDescription = Lens.field @"productDescription"
{-# DEPRECATED rdbioProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The recurring price charged to run this reserved DB instance.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioRecurringCharges :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe [Types.RecurringCharge])
rdbioRecurringCharges = Lens.field @"recurringCharges"
{-# DEPRECATED rdbioRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioReservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Types.String)
rdbioReservedDBInstancesOfferingId = Lens.field @"reservedDBInstancesOfferingId"
{-# DEPRECATED rdbioReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

-- | The hourly price charged for this offering.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdbioUsagePrice :: Lens.Lens' ReservedDBInstancesOffering (Core.Maybe Core.Double)
rdbioUsagePrice = Lens.field @"usagePrice"
{-# DEPRECATED rdbioUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

instance Core.FromXML ReservedDBInstancesOffering where
  parseXML x =
    ReservedDBInstancesOffering'
      Core.<$> (x Core..@? "CurrencyCode")
      Core.<*> (x Core..@? "DBInstanceClass")
      Core.<*> (x Core..@? "Duration")
      Core.<*> (x Core..@? "FixedPrice")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "OfferingType")
      Core.<*> (x Core..@? "ProductDescription")
      Core.<*> ( x Core..@? "RecurringCharges"
                   Core..<@> Core.parseXMLList "RecurringCharge"
               )
      Core.<*> (x Core..@? "ReservedDBInstancesOfferingId")
      Core.<*> (x Core..@? "UsagePrice")
