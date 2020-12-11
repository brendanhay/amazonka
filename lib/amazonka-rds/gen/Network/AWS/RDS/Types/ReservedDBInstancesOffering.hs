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
    rdioCurrencyCode,
    rdioProductDescription,
    rdioDBInstanceClass,
    rdioMultiAZ,
    rdioReservedDBInstancesOfferingId,
    rdioRecurringCharges,
    rdioOfferingType,
    rdioUsagePrice,
    rdioFixedPrice,
    rdioDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.RecurringCharge

-- | This data type is used as a response element in the @DescribeReservedDBInstancesOfferings@ action.
--
-- /See:/ 'mkReservedDBInstancesOffering' smart constructor.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'
  { currencyCode ::
      Lude.Maybe Lude.Text,
    productDescription ::
      Lude.Maybe Lude.Text,
    dbInstanceClass ::
      Lude.Maybe Lude.Text,
    multiAZ :: Lude.Maybe Lude.Bool,
    reservedDBInstancesOfferingId ::
      Lude.Maybe Lude.Text,
    recurringCharges ::
      Lude.Maybe [RecurringCharge],
    offeringType ::
      Lude.Maybe Lude.Text,
    usagePrice ::
      Lude.Maybe Lude.Double,
    fixedPrice ::
      Lude.Maybe Lude.Double,
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedDBInstancesOffering' with the minimum fields required to make a request.
--
-- * 'currencyCode' - The currency code for the reserved DB instance offering.
-- * 'dbInstanceClass' - The DB instance class for the reserved DB instance.
-- * 'duration' - The duration of the offering in seconds.
-- * 'fixedPrice' - The fixed price charged for this offering.
-- * 'multiAZ' - Indicates if the offering applies to Multi-AZ deployments.
-- * 'offeringType' - The offering type.
-- * 'productDescription' - The database engine used by the offering.
-- * 'recurringCharges' - The recurring price charged to run this reserved DB instance.
-- * 'reservedDBInstancesOfferingId' - The offering identifier.
-- * 'usagePrice' - The hourly price charged for this offering.
mkReservedDBInstancesOffering ::
  ReservedDBInstancesOffering
mkReservedDBInstancesOffering =
  ReservedDBInstancesOffering'
    { currencyCode = Lude.Nothing,
      productDescription = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      multiAZ = Lude.Nothing,
      reservedDBInstancesOfferingId = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | The currency code for the reserved DB instance offering.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioCurrencyCode :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Text)
rdioCurrencyCode = Lens.lens (currencyCode :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The database engine used by the offering.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioProductDescription :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Text)
rdioProductDescription = Lens.lens (productDescription :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The DB instance class for the reserved DB instance.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioDBInstanceClass :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Text)
rdioDBInstanceClass = Lens.lens (dbInstanceClass :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | Indicates if the offering applies to Multi-AZ deployments.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioMultiAZ :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Bool)
rdioMultiAZ = Lens.lens (multiAZ :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioReservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Text)
rdioReservedDBInstancesOfferingId = Lens.lens (reservedDBInstancesOfferingId :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstancesOfferingId = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

-- | The recurring price charged to run this reserved DB instance.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioRecurringCharges :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe [RecurringCharge])
rdioRecurringCharges = Lens.lens (recurringCharges :: ReservedDBInstancesOffering -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The offering type.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioOfferingType :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Text)
rdioOfferingType = Lens.lens (offeringType :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The hourly price charged for this offering.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioUsagePrice :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Double)
rdioUsagePrice = Lens.lens (usagePrice :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The fixed price charged for this offering.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioFixedPrice :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Double)
rdioFixedPrice = Lens.lens (fixedPrice :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration of the offering in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdioDuration :: Lens.Lens' ReservedDBInstancesOffering (Lude.Maybe Lude.Int)
rdioDuration = Lens.lens (duration :: ReservedDBInstancesOffering -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedDBInstancesOffering)
{-# DEPRECATED rdioDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromXML ReservedDBInstancesOffering where
  parseXML x =
    ReservedDBInstancesOffering'
      Lude.<$> (x Lude..@? "CurrencyCode")
      Lude.<*> (x Lude..@? "ProductDescription")
      Lude.<*> (x Lude..@? "DBInstanceClass")
      Lude.<*> (x Lude..@? "MultiAZ")
      Lude.<*> (x Lude..@? "ReservedDBInstancesOfferingId")
      Lude.<*> ( x Lude..@? "RecurringCharges" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "RecurringCharge")
               )
      Lude.<*> (x Lude..@? "OfferingType")
      Lude.<*> (x Lude..@? "UsagePrice")
      Lude.<*> (x Lude..@? "FixedPrice")
      Lude.<*> (x Lude..@? "Duration")
