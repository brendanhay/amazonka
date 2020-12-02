{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types.EntitlementValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceEntitlement.Types.EntitlementValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
--
--
--
-- /See:/ 'entitlementValue' smart constructor.
data EntitlementValue = EntitlementValue'
  { _evIntegerValue ::
      !(Maybe Int),
    _evDoubleValue :: !(Maybe Double),
    _evStringValue :: !(Maybe Text),
    _evBooleanValue :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntitlementValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'evIntegerValue' - The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
--
-- * 'evDoubleValue' - The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
--
-- * 'evStringValue' - The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
--
-- * 'evBooleanValue' - The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
entitlementValue ::
  EntitlementValue
entitlementValue =
  EntitlementValue'
    { _evIntegerValue = Nothing,
      _evDoubleValue = Nothing,
      _evStringValue = Nothing,
      _evBooleanValue = Nothing
    }

-- | The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
evIntegerValue :: Lens' EntitlementValue (Maybe Int)
evIntegerValue = lens _evIntegerValue (\s a -> s {_evIntegerValue = a})

-- | The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
evDoubleValue :: Lens' EntitlementValue (Maybe Double)
evDoubleValue = lens _evDoubleValue (\s a -> s {_evDoubleValue = a})

-- | The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
evStringValue :: Lens' EntitlementValue (Maybe Text)
evStringValue = lens _evStringValue (\s a -> s {_evStringValue = a})

-- | The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
evBooleanValue :: Lens' EntitlementValue (Maybe Bool)
evBooleanValue = lens _evBooleanValue (\s a -> s {_evBooleanValue = a})

instance FromJSON EntitlementValue where
  parseJSON =
    withObject
      "EntitlementValue"
      ( \x ->
          EntitlementValue'
            <$> (x .:? "IntegerValue")
            <*> (x .:? "DoubleValue")
            <*> (x .:? "StringValue")
            <*> (x .:? "BooleanValue")
      )

instance Hashable EntitlementValue

instance NFData EntitlementValue
