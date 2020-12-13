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
    rdiDBInstanceCount,
    rdiState,
    rdiCurrencyCode,
    rdiStartTime,
    rdiProductDescription,
    rdiLeaseId,
    rdiReservedDBInstanceId,
    rdiReservedDBInstanceARN,
    rdiDBInstanceClass,
    rdiMultiAZ,
    rdiReservedDBInstancesOfferingId,
    rdiRecurringCharges,
    rdiOfferingType,
    rdiUsagePrice,
    rdiFixedPrice,
    rdiDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.RecurringCharge

-- | This data type is used as a response element in the @DescribeReservedDBInstances@ and @PurchaseReservedDBInstancesOffering@ actions.
--
-- /See:/ 'mkReservedDBInstance' smart constructor.
data ReservedDBInstance = ReservedDBInstance'
  { -- | The number of reserved DB instances.
    dbInstanceCount :: Lude.Maybe Lude.Int,
    -- | The state of the reserved DB instance.
    state :: Lude.Maybe Lude.Text,
    -- | The currency code for the reserved DB instance.
    currencyCode :: Lude.Maybe Lude.Text,
    -- | The time the reservation started.
    startTime :: Lude.Maybe Lude.DateTime,
    -- | The description of the reserved DB instance.
    productDescription :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the lease associated with the reserved DB instance.
    leaseId :: Lude.Maybe Lude.Text,
    -- | The unique identifier for the reservation.
    reservedDBInstanceId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the reserved DB instance.
    reservedDBInstanceARN :: Lude.Maybe Lude.Text,
    -- | The DB instance class for the reserved DB instance.
    dbInstanceClass :: Lude.Maybe Lude.Text,
    -- | Indicates if the reservation applies to Multi-AZ deployments.
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | The offering identifier.
    reservedDBInstancesOfferingId :: Lude.Maybe Lude.Text,
    -- | The recurring price charged to run this reserved DB instance.
    recurringCharges :: Lude.Maybe [RecurringCharge],
    -- | The offering type of this reserved DB instance.
    offeringType :: Lude.Maybe Lude.Text,
    -- | The hourly price charged for this reserved DB instance.
    usagePrice :: Lude.Maybe Lude.Double,
    -- | The fixed price charged for this reserved DB instance.
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | The duration of the reservation in seconds.
    duration :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedDBInstance' with the minimum fields required to make a request.
--
-- * 'dbInstanceCount' - The number of reserved DB instances.
-- * 'state' - The state of the reserved DB instance.
-- * 'currencyCode' - The currency code for the reserved DB instance.
-- * 'startTime' - The time the reservation started.
-- * 'productDescription' - The description of the reserved DB instance.
-- * 'leaseId' - The unique identifier for the lease associated with the reserved DB instance.
-- * 'reservedDBInstanceId' - The unique identifier for the reservation.
-- * 'reservedDBInstanceARN' - The Amazon Resource Name (ARN) for the reserved DB instance.
-- * 'dbInstanceClass' - The DB instance class for the reserved DB instance.
-- * 'multiAZ' - Indicates if the reservation applies to Multi-AZ deployments.
-- * 'reservedDBInstancesOfferingId' - The offering identifier.
-- * 'recurringCharges' - The recurring price charged to run this reserved DB instance.
-- * 'offeringType' - The offering type of this reserved DB instance.
-- * 'usagePrice' - The hourly price charged for this reserved DB instance.
-- * 'fixedPrice' - The fixed price charged for this reserved DB instance.
-- * 'duration' - The duration of the reservation in seconds.
mkReservedDBInstance ::
  ReservedDBInstance
mkReservedDBInstance =
  ReservedDBInstance'
    { dbInstanceCount = Lude.Nothing,
      state = Lude.Nothing,
      currencyCode = Lude.Nothing,
      startTime = Lude.Nothing,
      productDescription = Lude.Nothing,
      leaseId = Lude.Nothing,
      reservedDBInstanceId = Lude.Nothing,
      reservedDBInstanceARN = Lude.Nothing,
      dbInstanceClass = Lude.Nothing,
      multiAZ = Lude.Nothing,
      reservedDBInstancesOfferingId = Lude.Nothing,
      recurringCharges = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      duration = Lude.Nothing
    }

-- | The number of reserved DB instances.
--
-- /Note:/ Consider using 'dbInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDBInstanceCount :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Int)
rdiDBInstanceCount = Lens.lens (dbInstanceCount :: ReservedDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {dbInstanceCount = a} :: ReservedDBInstance)
{-# DEPRECATED rdiDBInstanceCount "Use generic-lens or generic-optics with 'dbInstanceCount' instead." #-}

-- | The state of the reserved DB instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiState :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiState = Lens.lens (state :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: ReservedDBInstance)
{-# DEPRECATED rdiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The currency code for the reserved DB instance.
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiCurrencyCode :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiCurrencyCode = Lens.lens (currencyCode :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: ReservedDBInstance)
{-# DEPRECATED rdiCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | The time the reservation started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiStartTime :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.DateTime)
rdiStartTime = Lens.lens (startTime :: ReservedDBInstance -> Lude.Maybe Lude.DateTime) (\s a -> s {startTime = a} :: ReservedDBInstance)
{-# DEPRECATED rdiStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The description of the reserved DB instance.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiProductDescription :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiProductDescription = Lens.lens (productDescription :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: ReservedDBInstance)
{-# DEPRECATED rdiProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The unique identifier for the lease associated with the reserved DB instance.
--
-- /Note:/ Consider using 'leaseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiLeaseId :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiLeaseId = Lens.lens (leaseId :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {leaseId = a} :: ReservedDBInstance)
{-# DEPRECATED rdiLeaseId "Use generic-lens or generic-optics with 'leaseId' instead." #-}

-- | The unique identifier for the reservation.
--
-- /Note:/ Consider using 'reservedDBInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiReservedDBInstanceId :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiReservedDBInstanceId = Lens.lens (reservedDBInstanceId :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstanceId = a} :: ReservedDBInstance)
{-# DEPRECATED rdiReservedDBInstanceId "Use generic-lens or generic-optics with 'reservedDBInstanceId' instead." #-}

-- | The Amazon Resource Name (ARN) for the reserved DB instance.
--
-- /Note:/ Consider using 'reservedDBInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiReservedDBInstanceARN :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiReservedDBInstanceARN = Lens.lens (reservedDBInstanceARN :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstanceARN = a} :: ReservedDBInstance)
{-# DEPRECATED rdiReservedDBInstanceARN "Use generic-lens or generic-optics with 'reservedDBInstanceARN' instead." #-}

-- | The DB instance class for the reserved DB instance.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDBInstanceClass :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiDBInstanceClass = Lens.lens (dbInstanceClass :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: ReservedDBInstance)
{-# DEPRECATED rdiDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | Indicates if the reservation applies to Multi-AZ deployments.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiMultiAZ :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Bool)
rdiMultiAZ = Lens.lens (multiAZ :: ReservedDBInstance -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: ReservedDBInstance)
{-# DEPRECATED rdiMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The offering identifier.
--
-- /Note:/ Consider using 'reservedDBInstancesOfferingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiReservedDBInstancesOfferingId :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiReservedDBInstancesOfferingId = Lens.lens (reservedDBInstancesOfferingId :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {reservedDBInstancesOfferingId = a} :: ReservedDBInstance)
{-# DEPRECATED rdiReservedDBInstancesOfferingId "Use generic-lens or generic-optics with 'reservedDBInstancesOfferingId' instead." #-}

-- | The recurring price charged to run this reserved DB instance.
--
-- /Note:/ Consider using 'recurringCharges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiRecurringCharges :: Lens.Lens' ReservedDBInstance (Lude.Maybe [RecurringCharge])
rdiRecurringCharges = Lens.lens (recurringCharges :: ReservedDBInstance -> Lude.Maybe [RecurringCharge]) (\s a -> s {recurringCharges = a} :: ReservedDBInstance)
{-# DEPRECATED rdiRecurringCharges "Use generic-lens or generic-optics with 'recurringCharges' instead." #-}

-- | The offering type of this reserved DB instance.
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiOfferingType :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Text)
rdiOfferingType = Lens.lens (offeringType :: ReservedDBInstance -> Lude.Maybe Lude.Text) (\s a -> s {offeringType = a} :: ReservedDBInstance)
{-# DEPRECATED rdiOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | The hourly price charged for this reserved DB instance.
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiUsagePrice :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Double)
rdiUsagePrice = Lens.lens (usagePrice :: ReservedDBInstance -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: ReservedDBInstance)
{-# DEPRECATED rdiUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | The fixed price charged for this reserved DB instance.
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiFixedPrice :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Double)
rdiFixedPrice = Lens.lens (fixedPrice :: ReservedDBInstance -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: ReservedDBInstance)
{-# DEPRECATED rdiFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | The duration of the reservation in seconds.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdiDuration :: Lens.Lens' ReservedDBInstance (Lude.Maybe Lude.Int)
rdiDuration = Lens.lens (duration :: ReservedDBInstance -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: ReservedDBInstance)
{-# DEPRECATED rdiDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Lude.FromXML ReservedDBInstance where
  parseXML x =
    ReservedDBInstance'
      Lude.<$> (x Lude..@? "DBInstanceCount")
      Lude.<*> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "CurrencyCode")
      Lude.<*> (x Lude..@? "StartTime")
      Lude.<*> (x Lude..@? "ProductDescription")
      Lude.<*> (x Lude..@? "LeaseId")
      Lude.<*> (x Lude..@? "ReservedDBInstanceId")
      Lude.<*> (x Lude..@? "ReservedDBInstanceArn")
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
