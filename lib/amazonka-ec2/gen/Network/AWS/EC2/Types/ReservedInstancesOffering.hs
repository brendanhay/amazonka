{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesOffering where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CurrencyCodeValues
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.OfferingClassType
import Network.AWS.EC2.Types.OfferingTypeValues
import Network.AWS.EC2.Types.PricingDetail
import Network.AWS.EC2.Types.RIProductDescription
import Network.AWS.EC2.Types.RecurringCharge
import Network.AWS.EC2.Types.Scope
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a Reserved Instance offering.
--
--
--
-- /See:/ 'reservedInstancesOffering' smart constructor.
data ReservedInstancesOffering = ReservedInstancesOffering'
  { _rioMarketplace ::
      !(Maybe Bool),
    _rioCurrencyCode ::
      !(Maybe CurrencyCodeValues),
    _rioProductDescription ::
      !(Maybe RIProductDescription),
    _rioInstanceType ::
      !(Maybe InstanceType),
    _rioAvailabilityZone :: !(Maybe Text),
    _rioPricingDetails ::
      !(Maybe [PricingDetail]),
    _rioScope :: !(Maybe Scope),
    _rioRecurringCharges ::
      !(Maybe [RecurringCharge]),
    _rioOfferingType ::
      !(Maybe OfferingTypeValues),
    _rioUsagePrice :: !(Maybe Double),
    _rioFixedPrice :: !(Maybe Double),
    _rioInstanceTenancy :: !(Maybe Tenancy),
    _rioReservedInstancesOfferingId ::
      !(Maybe Text),
    _rioOfferingClass ::
      !(Maybe OfferingClassType),
    _rioDuration :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservedInstancesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rioMarketplace' - Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
--
-- * 'rioCurrencyCode' - The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
--
-- * 'rioProductDescription' - The Reserved Instance product platform description.
--
-- * 'rioInstanceType' - The instance type on which the Reserved Instance can be used.
--
-- * 'rioAvailabilityZone' - The Availability Zone in which the Reserved Instance can be used.
--
-- * 'rioPricingDetails' - The pricing details of the Reserved Instance offering.
--
-- * 'rioScope' - Whether the Reserved Instance is applied to instances in a Region or an Availability Zone.
--
-- * 'rioRecurringCharges' - The recurring charge tag assigned to the resource.
--
-- * 'rioOfferingType' - The Reserved Instance offering type.
--
-- * 'rioUsagePrice' - The usage price of the Reserved Instance, per hour.
--
-- * 'rioFixedPrice' - The purchase price of the Reserved Instance.
--
-- * 'rioInstanceTenancy' - The tenancy of the instance.
--
-- * 'rioReservedInstancesOfferingId' - The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
--
-- * 'rioOfferingClass' - If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
--
-- * 'rioDuration' - The duration of the Reserved Instance, in seconds.
reservedInstancesOffering ::
  ReservedInstancesOffering
reservedInstancesOffering =
  ReservedInstancesOffering'
    { _rioMarketplace = Nothing,
      _rioCurrencyCode = Nothing,
      _rioProductDescription = Nothing,
      _rioInstanceType = Nothing,
      _rioAvailabilityZone = Nothing,
      _rioPricingDetails = Nothing,
      _rioScope = Nothing,
      _rioRecurringCharges = Nothing,
      _rioOfferingType = Nothing,
      _rioUsagePrice = Nothing,
      _rioFixedPrice = Nothing,
      _rioInstanceTenancy = Nothing,
      _rioReservedInstancesOfferingId = Nothing,
      _rioOfferingClass = Nothing,
      _rioDuration = Nothing
    }

-- | Indicates whether the offering is available through the Reserved Instance Marketplace (resale) or AWS. If it's a Reserved Instance Marketplace offering, this is @true@ .
rioMarketplace :: Lens' ReservedInstancesOffering (Maybe Bool)
rioMarketplace = lens _rioMarketplace (\s a -> s {_rioMarketplace = a})

-- | The currency of the Reserved Instance offering you are purchasing. It's specified using ISO 4217 standard currency codes. At this time, the only supported currency is @USD@ .
rioCurrencyCode :: Lens' ReservedInstancesOffering (Maybe CurrencyCodeValues)
rioCurrencyCode = lens _rioCurrencyCode (\s a -> s {_rioCurrencyCode = a})

-- | The Reserved Instance product platform description.
rioProductDescription :: Lens' ReservedInstancesOffering (Maybe RIProductDescription)
rioProductDescription = lens _rioProductDescription (\s a -> s {_rioProductDescription = a})

-- | The instance type on which the Reserved Instance can be used.
rioInstanceType :: Lens' ReservedInstancesOffering (Maybe InstanceType)
rioInstanceType = lens _rioInstanceType (\s a -> s {_rioInstanceType = a})

-- | The Availability Zone in which the Reserved Instance can be used.
rioAvailabilityZone :: Lens' ReservedInstancesOffering (Maybe Text)
rioAvailabilityZone = lens _rioAvailabilityZone (\s a -> s {_rioAvailabilityZone = a})

-- | The pricing details of the Reserved Instance offering.
rioPricingDetails :: Lens' ReservedInstancesOffering [PricingDetail]
rioPricingDetails = lens _rioPricingDetails (\s a -> s {_rioPricingDetails = a}) . _Default . _Coerce

-- | Whether the Reserved Instance is applied to instances in a Region or an Availability Zone.
rioScope :: Lens' ReservedInstancesOffering (Maybe Scope)
rioScope = lens _rioScope (\s a -> s {_rioScope = a})

-- | The recurring charge tag assigned to the resource.
rioRecurringCharges :: Lens' ReservedInstancesOffering [RecurringCharge]
rioRecurringCharges = lens _rioRecurringCharges (\s a -> s {_rioRecurringCharges = a}) . _Default . _Coerce

-- | The Reserved Instance offering type.
rioOfferingType :: Lens' ReservedInstancesOffering (Maybe OfferingTypeValues)
rioOfferingType = lens _rioOfferingType (\s a -> s {_rioOfferingType = a})

-- | The usage price of the Reserved Instance, per hour.
rioUsagePrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioUsagePrice = lens _rioUsagePrice (\s a -> s {_rioUsagePrice = a})

-- | The purchase price of the Reserved Instance.
rioFixedPrice :: Lens' ReservedInstancesOffering (Maybe Double)
rioFixedPrice = lens _rioFixedPrice (\s a -> s {_rioFixedPrice = a})

-- | The tenancy of the instance.
rioInstanceTenancy :: Lens' ReservedInstancesOffering (Maybe Tenancy)
rioInstanceTenancy = lens _rioInstanceTenancy (\s a -> s {_rioInstanceTenancy = a})

-- | The ID of the Reserved Instance offering. This is the offering ID used in 'GetReservedInstancesExchangeQuote' to confirm that an exchange can be made.
rioReservedInstancesOfferingId :: Lens' ReservedInstancesOffering (Maybe Text)
rioReservedInstancesOfferingId = lens _rioReservedInstancesOfferingId (\s a -> s {_rioReservedInstancesOfferingId = a})

-- | If @convertible@ it can be exchanged for Reserved Instances of the same or higher monetary value, with different configurations. If @standard@ , it is not possible to perform an exchange.
rioOfferingClass :: Lens' ReservedInstancesOffering (Maybe OfferingClassType)
rioOfferingClass = lens _rioOfferingClass (\s a -> s {_rioOfferingClass = a})

-- | The duration of the Reserved Instance, in seconds.
rioDuration :: Lens' ReservedInstancesOffering (Maybe Integer)
rioDuration = lens _rioDuration (\s a -> s {_rioDuration = a})

instance FromXML ReservedInstancesOffering where
  parseXML x =
    ReservedInstancesOffering'
      <$> (x .@? "marketplace")
      <*> (x .@? "currencyCode")
      <*> (x .@? "productDescription")
      <*> (x .@? "instanceType")
      <*> (x .@? "availabilityZone")
      <*> ( x .@? "pricingDetailsSet" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "scope")
      <*> (x .@? "recurringCharges" .!@ mempty >>= may (parseXMLList "item"))
      <*> (x .@? "offeringType")
      <*> (x .@? "usagePrice")
      <*> (x .@? "fixedPrice")
      <*> (x .@? "instanceTenancy")
      <*> (x .@? "reservedInstancesOfferingId")
      <*> (x .@? "offeringClass")
      <*> (x .@? "duration")

instance Hashable ReservedInstancesOffering

instance NFData ReservedInstancesOffering
