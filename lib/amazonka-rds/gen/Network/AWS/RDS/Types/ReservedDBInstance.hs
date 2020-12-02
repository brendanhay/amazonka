{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ReservedDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ReservedDBInstance where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.RecurringCharge

-- | This data type is used as a response element in the @DescribeReservedDBInstances@ and @PurchaseReservedDBInstancesOffering@ actions.
--
--
--
-- /See:/ 'reservedDBInstance' smart constructor.
data ReservedDBInstance = ReservedDBInstance'
  { _rdiDBInstanceCount ::
      !(Maybe Int),
    _rdiState :: !(Maybe Text),
    _rdiCurrencyCode :: !(Maybe Text),
    _rdiStartTime :: !(Maybe ISO8601),
    _rdiProductDescription :: !(Maybe Text),
    _rdiLeaseId :: !(Maybe Text),
    _rdiReservedDBInstanceId :: !(Maybe Text),
    _rdiReservedDBInstanceARN :: !(Maybe Text),
    _rdiDBInstanceClass :: !(Maybe Text),
    _rdiMultiAZ :: !(Maybe Bool),
    _rdiReservedDBInstancesOfferingId :: !(Maybe Text),
    _rdiRecurringCharges :: !(Maybe [RecurringCharge]),
    _rdiOfferingType :: !(Maybe Text),
    _rdiUsagePrice :: !(Maybe Double),
    _rdiFixedPrice :: !(Maybe Double),
    _rdiDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdiDBInstanceCount' - The number of reserved DB instances.
--
-- * 'rdiState' - The state of the reserved DB instance.
--
-- * 'rdiCurrencyCode' - The currency code for the reserved DB instance.
--
-- * 'rdiStartTime' - The time the reservation started.
--
-- * 'rdiProductDescription' - The description of the reserved DB instance.
--
-- * 'rdiLeaseId' - The unique identifier for the lease associated with the reserved DB instance.
--
-- * 'rdiReservedDBInstanceId' - The unique identifier for the reservation.
--
-- * 'rdiReservedDBInstanceARN' - The Amazon Resource Name (ARN) for the reserved DB instance.
--
-- * 'rdiDBInstanceClass' - The DB instance class for the reserved DB instance.
--
-- * 'rdiMultiAZ' - Indicates if the reservation applies to Multi-AZ deployments.
--
-- * 'rdiReservedDBInstancesOfferingId' - The offering identifier.
--
-- * 'rdiRecurringCharges' - The recurring price charged to run this reserved DB instance.
--
-- * 'rdiOfferingType' - The offering type of this reserved DB instance.
--
-- * 'rdiUsagePrice' - The hourly price charged for this reserved DB instance.
--
-- * 'rdiFixedPrice' - The fixed price charged for this reserved DB instance.
--
-- * 'rdiDuration' - The duration of the reservation in seconds.
reservedDBInstance ::
  ReservedDBInstance
reservedDBInstance =
  ReservedDBInstance'
    { _rdiDBInstanceCount = Nothing,
      _rdiState = Nothing,
      _rdiCurrencyCode = Nothing,
      _rdiStartTime = Nothing,
      _rdiProductDescription = Nothing,
      _rdiLeaseId = Nothing,
      _rdiReservedDBInstanceId = Nothing,
      _rdiReservedDBInstanceARN = Nothing,
      _rdiDBInstanceClass = Nothing,
      _rdiMultiAZ = Nothing,
      _rdiReservedDBInstancesOfferingId = Nothing,
      _rdiRecurringCharges = Nothing,
      _rdiOfferingType = Nothing,
      _rdiUsagePrice = Nothing,
      _rdiFixedPrice = Nothing,
      _rdiDuration = Nothing
    }

-- | The number of reserved DB instances.
rdiDBInstanceCount :: Lens' ReservedDBInstance (Maybe Int)
rdiDBInstanceCount = lens _rdiDBInstanceCount (\s a -> s {_rdiDBInstanceCount = a})

-- | The state of the reserved DB instance.
rdiState :: Lens' ReservedDBInstance (Maybe Text)
rdiState = lens _rdiState (\s a -> s {_rdiState = a})

-- | The currency code for the reserved DB instance.
rdiCurrencyCode :: Lens' ReservedDBInstance (Maybe Text)
rdiCurrencyCode = lens _rdiCurrencyCode (\s a -> s {_rdiCurrencyCode = a})

-- | The time the reservation started.
rdiStartTime :: Lens' ReservedDBInstance (Maybe UTCTime)
rdiStartTime = lens _rdiStartTime (\s a -> s {_rdiStartTime = a}) . mapping _Time

-- | The description of the reserved DB instance.
rdiProductDescription :: Lens' ReservedDBInstance (Maybe Text)
rdiProductDescription = lens _rdiProductDescription (\s a -> s {_rdiProductDescription = a})

-- | The unique identifier for the lease associated with the reserved DB instance.
rdiLeaseId :: Lens' ReservedDBInstance (Maybe Text)
rdiLeaseId = lens _rdiLeaseId (\s a -> s {_rdiLeaseId = a})

-- | The unique identifier for the reservation.
rdiReservedDBInstanceId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstanceId = lens _rdiReservedDBInstanceId (\s a -> s {_rdiReservedDBInstanceId = a})

-- | The Amazon Resource Name (ARN) for the reserved DB instance.
rdiReservedDBInstanceARN :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstanceARN = lens _rdiReservedDBInstanceARN (\s a -> s {_rdiReservedDBInstanceARN = a})

-- | The DB instance class for the reserved DB instance.
rdiDBInstanceClass :: Lens' ReservedDBInstance (Maybe Text)
rdiDBInstanceClass = lens _rdiDBInstanceClass (\s a -> s {_rdiDBInstanceClass = a})

-- | Indicates if the reservation applies to Multi-AZ deployments.
rdiMultiAZ :: Lens' ReservedDBInstance (Maybe Bool)
rdiMultiAZ = lens _rdiMultiAZ (\s a -> s {_rdiMultiAZ = a})

-- | The offering identifier.
rdiReservedDBInstancesOfferingId :: Lens' ReservedDBInstance (Maybe Text)
rdiReservedDBInstancesOfferingId = lens _rdiReservedDBInstancesOfferingId (\s a -> s {_rdiReservedDBInstancesOfferingId = a})

-- | The recurring price charged to run this reserved DB instance.
rdiRecurringCharges :: Lens' ReservedDBInstance [RecurringCharge]
rdiRecurringCharges = lens _rdiRecurringCharges (\s a -> s {_rdiRecurringCharges = a}) . _Default . _Coerce

-- | The offering type of this reserved DB instance.
rdiOfferingType :: Lens' ReservedDBInstance (Maybe Text)
rdiOfferingType = lens _rdiOfferingType (\s a -> s {_rdiOfferingType = a})

-- | The hourly price charged for this reserved DB instance.
rdiUsagePrice :: Lens' ReservedDBInstance (Maybe Double)
rdiUsagePrice = lens _rdiUsagePrice (\s a -> s {_rdiUsagePrice = a})

-- | The fixed price charged for this reserved DB instance.
rdiFixedPrice :: Lens' ReservedDBInstance (Maybe Double)
rdiFixedPrice = lens _rdiFixedPrice (\s a -> s {_rdiFixedPrice = a})

-- | The duration of the reservation in seconds.
rdiDuration :: Lens' ReservedDBInstance (Maybe Int)
rdiDuration = lens _rdiDuration (\s a -> s {_rdiDuration = a})

instance FromXML ReservedDBInstance where
  parseXML x =
    ReservedDBInstance'
      <$> (x .@? "DBInstanceCount")
      <*> (x .@? "State")
      <*> (x .@? "CurrencyCode")
      <*> (x .@? "StartTime")
      <*> (x .@? "ProductDescription")
      <*> (x .@? "LeaseId")
      <*> (x .@? "ReservedDBInstanceId")
      <*> (x .@? "ReservedDBInstanceArn")
      <*> (x .@? "DBInstanceClass")
      <*> (x .@? "MultiAZ")
      <*> (x .@? "ReservedDBInstancesOfferingId")
      <*> ( x .@? "RecurringCharges" .!@ mempty
              >>= may (parseXMLList "RecurringCharge")
          )
      <*> (x .@? "OfferingType")
      <*> (x .@? "UsagePrice")
      <*> (x .@? "FixedPrice")
      <*> (x .@? "Duration")

instance Hashable ReservedDBInstance

instance NFData ReservedDBInstance
