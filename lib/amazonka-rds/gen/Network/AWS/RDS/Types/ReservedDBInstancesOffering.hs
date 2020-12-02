{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ReservedDBInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ReservedDBInstancesOffering where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.RecurringCharge

-- | This data type is used as a response element in the @DescribeReservedDBInstancesOfferings@ action.
--
--
--
-- /See:/ 'reservedDBInstancesOffering' smart constructor.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering'
  { _rdioCurrencyCode ::
      !(Maybe Text),
    _rdioProductDescription ::
      !(Maybe Text),
    _rdioDBInstanceClass ::
      !(Maybe Text),
    _rdioMultiAZ :: !(Maybe Bool),
    _rdioReservedDBInstancesOfferingId ::
      !(Maybe Text),
    _rdioRecurringCharges ::
      !(Maybe [RecurringCharge]),
    _rdioOfferingType :: !(Maybe Text),
    _rdioUsagePrice :: !(Maybe Double),
    _rdioFixedPrice :: !(Maybe Double),
    _rdioDuration :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedDBInstancesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdioCurrencyCode' - The currency code for the reserved DB instance offering.
--
-- * 'rdioProductDescription' - The database engine used by the offering.
--
-- * 'rdioDBInstanceClass' - The DB instance class for the reserved DB instance.
--
-- * 'rdioMultiAZ' - Indicates if the offering applies to Multi-AZ deployments.
--
-- * 'rdioReservedDBInstancesOfferingId' - The offering identifier.
--
-- * 'rdioRecurringCharges' - The recurring price charged to run this reserved DB instance.
--
-- * 'rdioOfferingType' - The offering type.
--
-- * 'rdioUsagePrice' - The hourly price charged for this offering.
--
-- * 'rdioFixedPrice' - The fixed price charged for this offering.
--
-- * 'rdioDuration' - The duration of the offering in seconds.
reservedDBInstancesOffering ::
  ReservedDBInstancesOffering
reservedDBInstancesOffering =
  ReservedDBInstancesOffering'
    { _rdioCurrencyCode = Nothing,
      _rdioProductDescription = Nothing,
      _rdioDBInstanceClass = Nothing,
      _rdioMultiAZ = Nothing,
      _rdioReservedDBInstancesOfferingId = Nothing,
      _rdioRecurringCharges = Nothing,
      _rdioOfferingType = Nothing,
      _rdioUsagePrice = Nothing,
      _rdioFixedPrice = Nothing,
      _rdioDuration = Nothing
    }

-- | The currency code for the reserved DB instance offering.
rdioCurrencyCode :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioCurrencyCode = lens _rdioCurrencyCode (\s a -> s {_rdioCurrencyCode = a})

-- | The database engine used by the offering.
rdioProductDescription :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioProductDescription = lens _rdioProductDescription (\s a -> s {_rdioProductDescription = a})

-- | The DB instance class for the reserved DB instance.
rdioDBInstanceClass :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioDBInstanceClass = lens _rdioDBInstanceClass (\s a -> s {_rdioDBInstanceClass = a})

-- | Indicates if the offering applies to Multi-AZ deployments.
rdioMultiAZ :: Lens' ReservedDBInstancesOffering (Maybe Bool)
rdioMultiAZ = lens _rdioMultiAZ (\s a -> s {_rdioMultiAZ = a})

-- | The offering identifier.
rdioReservedDBInstancesOfferingId :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioReservedDBInstancesOfferingId = lens _rdioReservedDBInstancesOfferingId (\s a -> s {_rdioReservedDBInstancesOfferingId = a})

-- | The recurring price charged to run this reserved DB instance.
rdioRecurringCharges :: Lens' ReservedDBInstancesOffering [RecurringCharge]
rdioRecurringCharges = lens _rdioRecurringCharges (\s a -> s {_rdioRecurringCharges = a}) . _Default . _Coerce

-- | The offering type.
rdioOfferingType :: Lens' ReservedDBInstancesOffering (Maybe Text)
rdioOfferingType = lens _rdioOfferingType (\s a -> s {_rdioOfferingType = a})

-- | The hourly price charged for this offering.
rdioUsagePrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioUsagePrice = lens _rdioUsagePrice (\s a -> s {_rdioUsagePrice = a})

-- | The fixed price charged for this offering.
rdioFixedPrice :: Lens' ReservedDBInstancesOffering (Maybe Double)
rdioFixedPrice = lens _rdioFixedPrice (\s a -> s {_rdioFixedPrice = a})

-- | The duration of the offering in seconds.
rdioDuration :: Lens' ReservedDBInstancesOffering (Maybe Int)
rdioDuration = lens _rdioDuration (\s a -> s {_rdioDuration = a})

instance FromXML ReservedDBInstancesOffering where
  parseXML x =
    ReservedDBInstancesOffering'
      <$> (x .@? "CurrencyCode")
      <*> (x .@? "ProductDescription")
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

instance Hashable ReservedDBInstancesOffering

instance NFData ReservedDBInstancesOffering
