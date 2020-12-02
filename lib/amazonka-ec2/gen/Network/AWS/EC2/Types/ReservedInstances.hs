{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstances where

import Network.AWS.EC2.Internal
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Reserved Instance.
--
--
--
-- /See:/ 'reservedInstances' smart constructor.
data ReservedInstances = ReservedInstances'
  { _riState ::
      !(Maybe ReservedInstanceState),
    _riCurrencyCode :: !(Maybe CurrencyCodeValues),
    _riInstanceCount :: !(Maybe Int),
    _riProductDescription :: !(Maybe RIProductDescription),
    _riStart :: !(Maybe ISO8601),
    _riInstanceType :: !(Maybe InstanceType),
    _riEnd :: !(Maybe ISO8601),
    _riAvailabilityZone :: !(Maybe Text),
    _riScope :: !(Maybe Scope),
    _riRecurringCharges :: !(Maybe [RecurringCharge]),
    _riOfferingType :: !(Maybe OfferingTypeValues),
    _riUsagePrice :: !(Maybe Double),
    _riFixedPrice :: !(Maybe Double),
    _riReservedInstancesId :: !(Maybe Text),
    _riInstanceTenancy :: !(Maybe Tenancy),
    _riOfferingClass :: !(Maybe OfferingClassType),
    _riDuration :: !(Maybe Integer),
    _riTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'riState' - The state of the Reserved Instance purchase.
--
-- * 'riCurrencyCode' - The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- * 'riInstanceCount' - The number of reservations purchased.
--
-- * 'riProductDescription' - The Reserved Instance product platform description.
--
-- * 'riStart' - The date and time the Reserved Instance started.
--
-- * 'riInstanceType' - The instance type on which the Reserved Instance can be used.
--
-- * 'riEnd' - The time when the Reserved Instance expires.
--
-- * 'riAvailabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- * 'riScope' - The scope of the Reserved Instance.
--
-- * 'riRecurringCharges' - The recurring charge tag assigned to the resource.
--
-- * 'riOfferingType' - The Reserved Instance offering type.
--
-- * 'riUsagePrice' - The usage price of the Reserved Instance, per hour.
--
-- * 'riFixedPrice' - The purchase price of the Reserved Instance.
--
-- * 'riReservedInstancesId' - The ID of the Reserved Instance.
--
-- * 'riInstanceTenancy' - The tenancy of the instance.
--
-- * 'riOfferingClass' - The offering class of the Reserved Instance.
--
-- * 'riDuration' - The duration of the Reserved Instance, in seconds.
--
-- * 'riTags' - Any tags assigned to the resource.
reservedInstances ::
  ReservedInstances
reservedInstances =
  ReservedInstances'
    { _riState = Nothing,
      _riCurrencyCode = Nothing,
      _riInstanceCount = Nothing,
      _riProductDescription = Nothing,
      _riStart = Nothing,
      _riInstanceType = Nothing,
      _riEnd = Nothing,
      _riAvailabilityZone = Nothing,
      _riScope = Nothing,
      _riRecurringCharges = Nothing,
      _riOfferingType = Nothing,
      _riUsagePrice = Nothing,
      _riFixedPrice = Nothing,
      _riReservedInstancesId = Nothing,
      _riInstanceTenancy = Nothing,
      _riOfferingClass = Nothing,
      _riDuration = Nothing,
      _riTags = Nothing
    }

-- | The state of the Reserved Instance purchase.
riState :: Lens' ReservedInstances (Maybe ReservedInstanceState)
riState = lens _riState (\s a -> s {_riState = a})

-- | The currency of the Reserved Instance. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
riCurrencyCode :: Lens' ReservedInstances (Maybe CurrencyCodeValues)
riCurrencyCode = lens _riCurrencyCode (\s a -> s {_riCurrencyCode = a})

-- | The number of reservations purchased.
riInstanceCount :: Lens' ReservedInstances (Maybe Int)
riInstanceCount = lens _riInstanceCount (\s a -> s {_riInstanceCount = a})

-- | The Reserved Instance product platform description.
riProductDescription :: Lens' ReservedInstances (Maybe RIProductDescription)
riProductDescription = lens _riProductDescription (\s a -> s {_riProductDescription = a})

-- | The date and time the Reserved Instance started.
riStart :: Lens' ReservedInstances (Maybe UTCTime)
riStart = lens _riStart (\s a -> s {_riStart = a}) . mapping _Time

-- | The instance type on which the Reserved Instance can be used.
riInstanceType :: Lens' ReservedInstances (Maybe InstanceType)
riInstanceType = lens _riInstanceType (\s a -> s {_riInstanceType = a})

-- | The time when the Reserved Instance expires.
riEnd :: Lens' ReservedInstances (Maybe UTCTime)
riEnd = lens _riEnd (\s a -> s {_riEnd = a}) . mapping _Time

-- | The Availability Zone in which the Reserved Instance can be used.
riAvailabilityZone :: Lens' ReservedInstances (Maybe Text)
riAvailabilityZone = lens _riAvailabilityZone (\s a -> s {_riAvailabilityZone = a})

-- | The scope of the Reserved Instance.
riScope :: Lens' ReservedInstances (Maybe Scope)
riScope = lens _riScope (\s a -> s {_riScope = a})

-- | The recurring charge tag assigned to the resource.
riRecurringCharges :: Lens' ReservedInstances [RecurringCharge]
riRecurringCharges = lens _riRecurringCharges (\s a -> s {_riRecurringCharges = a}) . _Default . _Coerce

-- | The Reserved Instance offering type.
riOfferingType :: Lens' ReservedInstances (Maybe OfferingTypeValues)
riOfferingType = lens _riOfferingType (\s a -> s {_riOfferingType = a})

-- | The usage price of the Reserved Instance, per hour.
riUsagePrice :: Lens' ReservedInstances (Maybe Double)
riUsagePrice = lens _riUsagePrice (\s a -> s {_riUsagePrice = a})

-- | The purchase price of the Reserved Instance.
riFixedPrice :: Lens' ReservedInstances (Maybe Double)
riFixedPrice = lens _riFixedPrice (\s a -> s {_riFixedPrice = a})

-- | The ID of the Reserved Instance.
riReservedInstancesId :: Lens' ReservedInstances (Maybe Text)
riReservedInstancesId = lens _riReservedInstancesId (\s a -> s {_riReservedInstancesId = a})

-- | The tenancy of the instance.
riInstanceTenancy :: Lens' ReservedInstances (Maybe Tenancy)
riInstanceTenancy = lens _riInstanceTenancy (\s a -> s {_riInstanceTenancy = a})

-- | The offering class of the Reserved Instance.
riOfferingClass :: Lens' ReservedInstances (Maybe OfferingClassType)
riOfferingClass = lens _riOfferingClass (\s a -> s {_riOfferingClass = a})

-- | The duration of the Reserved Instance, in seconds.
riDuration :: Lens' ReservedInstances (Maybe Integer)
riDuration = lens _riDuration (\s a -> s {_riDuration = a})

-- | Any tags assigned to the resource.
riTags :: Lens' ReservedInstances [Tag]
riTags = lens _riTags (\s a -> s {_riTags = a}) . _Default . _Coerce

instance FromXML ReservedInstances where
  parseXML x =
    ReservedInstances'
      <$> (x .@? "state")
      <*> (x .@? "currencyCode")
      <*> (x .@? "instanceCount")
      <*> (x .@? "productDescription")
      <*> (x .@? "start")
      <*> (x .@? "instanceType")
      <*> (x .@? "end")
      <*> (x .@? "availabilityZone")
      <*> (x .@? "scope")
      <*> (x .@? "recurringCharges" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "offeringType")
      <*> (x .@? "usagePrice")
      <*> (x .@? "fixedPrice")
      <*> (x .@? "reservedInstancesId")
      <*> (x .@? "instanceTenancy")
      <*> (x .@? "offeringClass")
      <*> (x .@? "duration")
      <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ReservedInstances

instance NFData ReservedInstances
