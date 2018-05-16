{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pricing.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Pricing.Types.Sum

-- | The values of a given attribute, such as @Throughput Optimized HDD@ or @Provisioned IOPS@ for the @Amazon EC2@ @volumeType@ attribute.
--
--
--
-- /See:/ 'attributeValue' smart constructor.
newtype AttributeValue = AttributeValue'
  { _avValue :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avValue' - The specific value of an @attributeName@ .
attributeValue
    :: AttributeValue
attributeValue = AttributeValue' {_avValue = Nothing}


-- | The specific value of an @attributeName@ .
avValue :: Lens' AttributeValue (Maybe Text)
avValue = lens _avValue (\ s a -> s{_avValue = a})

instance FromJSON AttributeValue where
        parseJSON
          = withObject "AttributeValue"
              (\ x -> AttributeValue' <$> (x .:? "Value"))

instance Hashable AttributeValue where

instance NFData AttributeValue where

-- | The constraints that you want all returned products to match.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'
  { _fType  :: !FilterType
  , _fField :: !Text
  , _fValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fType' - The type of filter that you want to use. Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
--
-- * 'fField' - The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields. Valid values include: @ServiceCode@ , and all attribute names For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
--
-- * 'fValue' - The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
filter'
    :: FilterType -- ^ 'fType'
    -> Text -- ^ 'fField'
    -> Text -- ^ 'fValue'
    -> Filter
filter' pType_ pField_ pValue_ =
  Filter' {_fType = pType_, _fField = pField_, _fValue = pValue_}


-- | The type of filter that you want to use. Valid values are: @TERM_MATCH@ . @TERM_MATCH@ returns only products that match both the given filter field and the given value.
fType :: Lens' Filter FilterType
fType = lens _fType (\ s a -> s{_fType = a})

-- | The product metadata field that you want to filter on. You can filter by just the service code to see all products for a specific service, filter by just the attribute name to see a specific attribute for multiple services, or use both a service code and an attribute name to retrieve only products that match both fields. Valid values include: @ServiceCode@ , and all attribute names For example, you can filter by the @AmazonEC2@ service code and the @volumeType@ attribute name to get the prices for only Amazon EC2 volumes.
fField :: Lens' Filter Text
fField = lens _fField (\ s a -> s{_fField = a})

-- | The service code or attribute value that you want to filter by. If you are filtering by service code this is the actual service code, such as @AmazonEC2@ . If you are filtering by attribute name, this is the attribute value that you want the returned products to match, such as a @Provisioned IOPS@ volume.
fValue :: Lens' Filter Text
fValue = lens _fValue (\ s a -> s{_fValue = a})

instance Hashable Filter where

instance NFData Filter where

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [Just ("Type" .= _fType), Just ("Field" .= _fField),
                  Just ("Value" .= _fValue)])

-- | The metadata for a service, such as the service code and available attribute names.
--
--
--
-- /See:/ 'pricingService' smart constructor.
data PricingService = PricingService'
  { _psAttributeNames :: !(Maybe [Text])
  , _psServiceCode    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PricingService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psAttributeNames' - The attributes that are available for this service.
--
-- * 'psServiceCode' - The code for the AWS service.
pricingService
    :: PricingService
pricingService =
  PricingService' {_psAttributeNames = Nothing, _psServiceCode = Nothing}


-- | The attributes that are available for this service.
psAttributeNames :: Lens' PricingService [Text]
psAttributeNames = lens _psAttributeNames (\ s a -> s{_psAttributeNames = a}) . _Default . _Coerce

-- | The code for the AWS service.
psServiceCode :: Lens' PricingService (Maybe Text)
psServiceCode = lens _psServiceCode (\ s a -> s{_psServiceCode = a})

instance FromJSON PricingService where
        parseJSON
          = withObject "PricingService"
              (\ x ->
                 PricingService' <$>
                   (x .:? "AttributeNames" .!= mempty) <*>
                     (x .:? "ServiceCode"))

instance Hashable PricingService where

instance NFData PricingService where
