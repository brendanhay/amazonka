{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageRecord where

import Network.AWS.Lens
import Network.AWS.MarketplaceMetering.Types.UsageAllocation
import Network.AWS.Prelude

-- | A UsageRecord indicates a quantity of usage for a given product, customer, dimension and time.
--
--
-- Multiple requests with the same UsageRecords as input will be deduplicated to prevent double charges.
--
--
-- /See:/ 'usageRecord' smart constructor.
data UsageRecord = UsageRecord'
  { _urQuantity :: !(Maybe Nat),
    _urUsageAllocations :: !(Maybe (List1 UsageAllocation)),
    _urTimestamp :: !POSIX,
    _urCustomerIdentifier :: !Text,
    _urDimension :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsageRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urQuantity' - The quantity of usage consumed by the customer for the given dimension and time. Defaults to @0@ if not specified.
--
-- * 'urUsageAllocations' - The set of UsageAllocations to submit. The sum of all UsageAllocation quantities must equal the Quantity of the UsageRecord.
--
-- * 'urTimestamp' - Timestamp, in UTC, for which the usage is being reported. Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
--
-- * 'urCustomerIdentifier' - The CustomerIdentifier is obtained through the ResolveCustomer operation and represents an individual buyer in your application.
--
-- * 'urDimension' - During the process of registering a product on AWS Marketplace, up to eight dimensions are specified. These represent different units of value in your application.
usageRecord ::
  -- | 'urTimestamp'
  UTCTime ->
  -- | 'urCustomerIdentifier'
  Text ->
  -- | 'urDimension'
  Text ->
  UsageRecord
usageRecord pTimestamp_ pCustomerIdentifier_ pDimension_ =
  UsageRecord'
    { _urQuantity = Nothing,
      _urUsageAllocations = Nothing,
      _urTimestamp = _Time # pTimestamp_,
      _urCustomerIdentifier = pCustomerIdentifier_,
      _urDimension = pDimension_
    }

-- | The quantity of usage consumed by the customer for the given dimension and time. Defaults to @0@ if not specified.
urQuantity :: Lens' UsageRecord (Maybe Natural)
urQuantity = lens _urQuantity (\s a -> s {_urQuantity = a}) . mapping _Nat

-- | The set of UsageAllocations to submit. The sum of all UsageAllocation quantities must equal the Quantity of the UsageRecord.
urUsageAllocations :: Lens' UsageRecord (Maybe (NonEmpty UsageAllocation))
urUsageAllocations = lens _urUsageAllocations (\s a -> s {_urUsageAllocations = a}) . mapping _List1

-- | Timestamp, in UTC, for which the usage is being reported. Your application can meter usage for up to one hour in the past. Make sure the timestamp value is not before the start of the software usage.
urTimestamp :: Lens' UsageRecord UTCTime
urTimestamp = lens _urTimestamp (\s a -> s {_urTimestamp = a}) . _Time

-- | The CustomerIdentifier is obtained through the ResolveCustomer operation and represents an individual buyer in your application.
urCustomerIdentifier :: Lens' UsageRecord Text
urCustomerIdentifier = lens _urCustomerIdentifier (\s a -> s {_urCustomerIdentifier = a})

-- | During the process of registering a product on AWS Marketplace, up to eight dimensions are specified. These represent different units of value in your application.
urDimension :: Lens' UsageRecord Text
urDimension = lens _urDimension (\s a -> s {_urDimension = a})

instance FromJSON UsageRecord where
  parseJSON =
    withObject
      "UsageRecord"
      ( \x ->
          UsageRecord'
            <$> (x .:? "Quantity")
            <*> (x .:? "UsageAllocations")
            <*> (x .: "Timestamp")
            <*> (x .: "CustomerIdentifier")
            <*> (x .: "Dimension")
      )

instance Hashable UsageRecord

instance NFData UsageRecord

instance ToJSON UsageRecord where
  toJSON UsageRecord' {..} =
    object
      ( catMaybes
          [ ("Quantity" .=) <$> _urQuantity,
            ("UsageAllocations" .=) <$> _urUsageAllocations,
            Just ("Timestamp" .= _urTimestamp),
            Just ("CustomerIdentifier" .= _urCustomerIdentifier),
            Just ("Dimension" .= _urDimension)
          ]
      )
