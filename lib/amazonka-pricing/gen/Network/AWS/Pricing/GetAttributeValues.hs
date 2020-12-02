{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pricing.GetAttributeValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of attribute values. Attibutes are similar to the details in a Price List API offer file. For a list of available attributes, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/reading-an-offer.html#pps-defs Offer File Definitions> in the <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/billing-what-is.html AWS Billing and Cost Management User Guide> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Pricing.GetAttributeValues
    (
    -- * Creating a Request
      getAttributeValues
    , GetAttributeValues
    -- * Request Lenses
    , gavNextToken
    , gavMaxResults
    , gavServiceCode
    , gavAttributeName

    -- * Destructuring the Response
    , getAttributeValuesResponse
    , GetAttributeValuesResponse
    -- * Response Lenses
    , gavrsAttributeValues
    , gavrsNextToken
    , gavrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Pricing.Types
import Network.AWS.Pricing.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAttributeValues' smart constructor.
data GetAttributeValues = GetAttributeValues'
  { _gavNextToken     :: !(Maybe Text)
  , _gavMaxResults    :: !(Maybe Nat)
  , _gavServiceCode   :: !Text
  , _gavAttributeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAttributeValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gavNextToken' - The pagination token that indicates the next set of results that you want to retrieve.
--
-- * 'gavMaxResults' - The maximum number of results to return in response.
--
-- * 'gavServiceCode' - The service code for the service whose attributes you want to retrieve. For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@ .
--
-- * 'gavAttributeName' - The name of the attribute that you want to retrieve the values for, such as @volumeType@ .
getAttributeValues
    :: Text -- ^ 'gavServiceCode'
    -> Text -- ^ 'gavAttributeName'
    -> GetAttributeValues
getAttributeValues pServiceCode_ pAttributeName_ =
  GetAttributeValues'
    { _gavNextToken = Nothing
    , _gavMaxResults = Nothing
    , _gavServiceCode = pServiceCode_
    , _gavAttributeName = pAttributeName_
    }


-- | The pagination token that indicates the next set of results that you want to retrieve.
gavNextToken :: Lens' GetAttributeValues (Maybe Text)
gavNextToken = lens _gavNextToken (\ s a -> s{_gavNextToken = a})

-- | The maximum number of results to return in response.
gavMaxResults :: Lens' GetAttributeValues (Maybe Natural)
gavMaxResults = lens _gavMaxResults (\ s a -> s{_gavMaxResults = a}) . mapping _Nat

-- | The service code for the service whose attributes you want to retrieve. For example, if you want the retrieve an EC2 attribute, use @AmazonEC2@ .
gavServiceCode :: Lens' GetAttributeValues Text
gavServiceCode = lens _gavServiceCode (\ s a -> s{_gavServiceCode = a})

-- | The name of the attribute that you want to retrieve the values for, such as @volumeType@ .
gavAttributeName :: Lens' GetAttributeValues Text
gavAttributeName = lens _gavAttributeName (\ s a -> s{_gavAttributeName = a})

instance AWSPager GetAttributeValues where
        page rq rs
          | stop (rs ^. gavrsNextToken) = Nothing
          | stop (rs ^. gavrsAttributeValues) = Nothing
          | otherwise =
            Just $ rq & gavNextToken .~ rs ^. gavrsNextToken

instance AWSRequest GetAttributeValues where
        type Rs GetAttributeValues =
             GetAttributeValuesResponse
        request = postJSON pricing
        response
          = receiveJSON
              (\ s h x ->
                 GetAttributeValuesResponse' <$>
                   (x .?> "AttributeValues" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetAttributeValues where

instance NFData GetAttributeValues where

instance ToHeaders GetAttributeValues where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPriceListService.GetAttributeValues" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAttributeValues where
        toJSON GetAttributeValues'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gavNextToken,
                  ("MaxResults" .=) <$> _gavMaxResults,
                  Just ("ServiceCode" .= _gavServiceCode),
                  Just ("AttributeName" .= _gavAttributeName)])

instance ToPath GetAttributeValues where
        toPath = const "/"

instance ToQuery GetAttributeValues where
        toQuery = const mempty

-- | /See:/ 'getAttributeValuesResponse' smart constructor.
data GetAttributeValuesResponse = GetAttributeValuesResponse'
  { _gavrsAttributeValues :: !(Maybe [AttributeValue])
  , _gavrsNextToken       :: !(Maybe Text)
  , _gavrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAttributeValuesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gavrsAttributeValues' - The list of values for an attribute. For example, @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available values for the @AmazonEC2@ @volumeType@ .
--
-- * 'gavrsNextToken' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'gavrsResponseStatus' - -- | The response status code.
getAttributeValuesResponse
    :: Int -- ^ 'gavrsResponseStatus'
    -> GetAttributeValuesResponse
getAttributeValuesResponse pResponseStatus_ =
  GetAttributeValuesResponse'
    { _gavrsAttributeValues = Nothing
    , _gavrsNextToken = Nothing
    , _gavrsResponseStatus = pResponseStatus_
    }


-- | The list of values for an attribute. For example, @Throughput Optimized HDD@ and @Provisioned IOPS@ are two available values for the @AmazonEC2@ @volumeType@ .
gavrsAttributeValues :: Lens' GetAttributeValuesResponse [AttributeValue]
gavrsAttributeValues = lens _gavrsAttributeValues (\ s a -> s{_gavrsAttributeValues = a}) . _Default . _Coerce

-- | The pagination token that indicates the next set of results to retrieve.
gavrsNextToken :: Lens' GetAttributeValuesResponse (Maybe Text)
gavrsNextToken = lens _gavrsNextToken (\ s a -> s{_gavrsNextToken = a})

-- | -- | The response status code.
gavrsResponseStatus :: Lens' GetAttributeValuesResponse Int
gavrsResponseStatus = lens _gavrsResponseStatus (\ s a -> s{_gavrsResponseStatus = a})

instance NFData GetAttributeValuesResponse where
