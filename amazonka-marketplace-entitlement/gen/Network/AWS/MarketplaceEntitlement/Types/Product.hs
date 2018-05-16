{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceEntitlement.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceEntitlement.Types.Product where

import Network.AWS.Lens
import Network.AWS.MarketplaceEntitlement.Types.Sum
import Network.AWS.Prelude

-- | An entitlement represents capacity in a product owned by the customer. For example, a customer might own some number of users or seats in an SaaS application or some amount of data capacity in a multi-tenant database.
--
--
--
-- /See:/ 'entitlement' smart constructor.
data Entitlement = Entitlement'
  { _eDimension          :: !(Maybe Text)
  , _eValue              :: !(Maybe EntitlementValue)
  , _eExpirationDate     :: !(Maybe POSIX)
  , _eCustomerIdentifier :: !(Maybe Text)
  , _eProductCode        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Entitlement' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eDimension' - The dimension for which the given entitlement applies. Dimensions represent categories of capacity in a product and are specified when the product is listed in AWS Marketplace.
--
-- * 'eValue' - The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
--
-- * 'eExpirationDate' - The expiration date represents the minimum date through which this entitlement is expected to remain valid. For contractual products listed on AWS Marketplace, the expiration date is the date at which the customer will renew or cancel their contract. Customers who are opting to renew their contract will still have entitlements with an expiration date.
--
-- * 'eCustomerIdentifier' - The customer identifier is a handle to each unique customer in an application. Customer identifiers are obtained through the ResolveCustomer operation in AWS Marketplace Metering Service.
--
-- * 'eProductCode' - The product code for which the given entitlement applies. Product codes are provided by AWS Marketplace when the product listing is created.
entitlement
    :: Entitlement
entitlement =
  Entitlement'
    { _eDimension = Nothing
    , _eValue = Nothing
    , _eExpirationDate = Nothing
    , _eCustomerIdentifier = Nothing
    , _eProductCode = Nothing
    }


-- | The dimension for which the given entitlement applies. Dimensions represent categories of capacity in a product and are specified when the product is listed in AWS Marketplace.
eDimension :: Lens' Entitlement (Maybe Text)
eDimension = lens _eDimension (\ s a -> s{_eDimension = a})

-- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
eValue :: Lens' Entitlement (Maybe EntitlementValue)
eValue = lens _eValue (\ s a -> s{_eValue = a})

-- | The expiration date represents the minimum date through which this entitlement is expected to remain valid. For contractual products listed on AWS Marketplace, the expiration date is the date at which the customer will renew or cancel their contract. Customers who are opting to renew their contract will still have entitlements with an expiration date.
eExpirationDate :: Lens' Entitlement (Maybe UTCTime)
eExpirationDate = lens _eExpirationDate (\ s a -> s{_eExpirationDate = a}) . mapping _Time

-- | The customer identifier is a handle to each unique customer in an application. Customer identifiers are obtained through the ResolveCustomer operation in AWS Marketplace Metering Service.
eCustomerIdentifier :: Lens' Entitlement (Maybe Text)
eCustomerIdentifier = lens _eCustomerIdentifier (\ s a -> s{_eCustomerIdentifier = a})

-- | The product code for which the given entitlement applies. Product codes are provided by AWS Marketplace when the product listing is created.
eProductCode :: Lens' Entitlement (Maybe Text)
eProductCode = lens _eProductCode (\ s a -> s{_eProductCode = a})

instance FromJSON Entitlement where
        parseJSON
          = withObject "Entitlement"
              (\ x ->
                 Entitlement' <$>
                   (x .:? "Dimension") <*> (x .:? "Value") <*>
                     (x .:? "ExpirationDate")
                     <*> (x .:? "CustomerIdentifier")
                     <*> (x .:? "ProductCode"))

instance Hashable Entitlement where

instance NFData Entitlement where

-- | The EntitlementValue represents the amount of capacity that the customer is entitled to for the product.
--
--
--
-- /See:/ 'entitlementValue' smart constructor.
data EntitlementValue = EntitlementValue'
  { _evIntegerValue :: !(Maybe Int)
  , _evDoubleValue  :: !(Maybe Double)
  , _evStringValue  :: !(Maybe Text)
  , _evBooleanValue :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
entitlementValue
    :: EntitlementValue
entitlementValue =
  EntitlementValue'
    { _evIntegerValue = Nothing
    , _evDoubleValue = Nothing
    , _evStringValue = Nothing
    , _evBooleanValue = Nothing
    }


-- | The IntegerValue field will be populated with an integer value when the entitlement is an integer type. Otherwise, the field will not be set.
evIntegerValue :: Lens' EntitlementValue (Maybe Int)
evIntegerValue = lens _evIntegerValue (\ s a -> s{_evIntegerValue = a})

-- | The DoubleValue field will be populated with a double value when the entitlement is a double type. Otherwise, the field will not be set.
evDoubleValue :: Lens' EntitlementValue (Maybe Double)
evDoubleValue = lens _evDoubleValue (\ s a -> s{_evDoubleValue = a})

-- | The StringValue field will be populated with a string value when the entitlement is a string type. Otherwise, the field will not be set.
evStringValue :: Lens' EntitlementValue (Maybe Text)
evStringValue = lens _evStringValue (\ s a -> s{_evStringValue = a})

-- | The BooleanValue field will be populated with a boolean value when the entitlement is a boolean type. Otherwise, the field will not be set.
evBooleanValue :: Lens' EntitlementValue (Maybe Bool)
evBooleanValue = lens _evBooleanValue (\ s a -> s{_evBooleanValue = a})

instance FromJSON EntitlementValue where
        parseJSON
          = withObject "EntitlementValue"
              (\ x ->
                 EntitlementValue' <$>
                   (x .:? "IntegerValue") <*> (x .:? "DoubleValue") <*>
                     (x .:? "StringValue")
                     <*> (x .:? "BooleanValue"))

instance Hashable EntitlementValue where

instance NFData EntitlementValue where
