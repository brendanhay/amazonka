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
-- Module      : Network.AWS.Pricing.DescribeServices
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the metadata for one service or a list of the metadata for all services. Use this without a service code to get the service codes for all services. Use it with a service code, such as @AmazonEC2@ , to get information specific to that service, such as the attribute names available for that service. For example, some of the attribute names available for EC2 are @volumeType@ , @maxIopsVolume@ , @operation@ , @locationType@ , and @instanceCapacity10xlarge@ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Pricing.DescribeServices
    (
    -- * Creating a Request
      describeServices
    , DescribeServices
    -- * Request Lenses
    , dsFormatVersion
    , dsNextToken
    , dsServiceCode
    , dsMaxResults

    -- * Destructuring the Response
    , describeServicesResponse
    , DescribeServicesResponse
    -- * Response Lenses
    , dsrsFormatVersion
    , dsrsNextToken
    , dsrsServices
    , dsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Pricing.Types
import Network.AWS.Pricing.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeServices' smart constructor.
data DescribeServices = DescribeServices'
  { _dsFormatVersion :: !(Maybe Text)
  , _dsNextToken     :: !(Maybe Text)
  , _dsServiceCode   :: !(Maybe Text)
  , _dsMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsFormatVersion' - The format version that you want the response to be in. Valid values are: @aws_v1@
--
-- * 'dsNextToken' - The pagination token that indicates the next set of results that you want to retrieve.
--
-- * 'dsServiceCode' - The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
--
-- * 'dsMaxResults' - The maximum number of results that you want returned in the response.
describeServices
    :: DescribeServices
describeServices =
  DescribeServices'
    { _dsFormatVersion = Nothing
    , _dsNextToken = Nothing
    , _dsServiceCode = Nothing
    , _dsMaxResults = Nothing
    }


-- | The format version that you want the response to be in. Valid values are: @aws_v1@
dsFormatVersion :: Lens' DescribeServices (Maybe Text)
dsFormatVersion = lens _dsFormatVersion (\ s a -> s{_dsFormatVersion = a})

-- | The pagination token that indicates the next set of results that you want to retrieve.
dsNextToken :: Lens' DescribeServices (Maybe Text)
dsNextToken = lens _dsNextToken (\ s a -> s{_dsNextToken = a})

-- | The code for the service whose information you want to retrieve, such as @AmazonEC2@ . You can use the @ServiceCode@ to filter the results in a @GetProducts@ call. To retrieve a list of all services, leave this blank.
dsServiceCode :: Lens' DescribeServices (Maybe Text)
dsServiceCode = lens _dsServiceCode (\ s a -> s{_dsServiceCode = a})

-- | The maximum number of results that you want returned in the response.
dsMaxResults :: Lens' DescribeServices (Maybe Natural)
dsMaxResults = lens _dsMaxResults (\ s a -> s{_dsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeServices where
        page rq rs
          | stop (rs ^. dsrsNextToken) = Nothing
          | stop (rs ^. dsrsServices) = Nothing
          | otherwise =
            Just $ rq & dsNextToken .~ rs ^. dsrsNextToken

instance AWSRequest DescribeServices where
        type Rs DescribeServices = DescribeServicesResponse
        request = postJSON pricing
        response
          = receiveJSON
              (\ s h x ->
                 DescribeServicesResponse' <$>
                   (x .?> "FormatVersion") <*> (x .?> "NextToken") <*>
                     (x .?> "Services" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeServices where

instance NFData DescribeServices where

instance ToHeaders DescribeServices where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPriceListService.DescribeServices" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeServices where
        toJSON DescribeServices'{..}
          = object
              (catMaybes
                 [("FormatVersion" .=) <$> _dsFormatVersion,
                  ("NextToken" .=) <$> _dsNextToken,
                  ("ServiceCode" .=) <$> _dsServiceCode,
                  ("MaxResults" .=) <$> _dsMaxResults])

instance ToPath DescribeServices where
        toPath = const "/"

instance ToQuery DescribeServices where
        toQuery = const mempty

-- | /See:/ 'describeServicesResponse' smart constructor.
data DescribeServicesResponse = DescribeServicesResponse'
  { _dsrsFormatVersion  :: !(Maybe Text)
  , _dsrsNextToken      :: !(Maybe Text)
  , _dsrsServices       :: !(Maybe [PricingService])
  , _dsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeServicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsFormatVersion' - The format version of the response. For example, @aws_v1@ .
--
-- * 'dsrsNextToken' - The pagination token for the next set of retreivable results.
--
-- * 'dsrsServices' - The service metadata for the service or services in the response.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeServicesResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DescribeServicesResponse
describeServicesResponse pResponseStatus_ =
  DescribeServicesResponse'
    { _dsrsFormatVersion = Nothing
    , _dsrsNextToken = Nothing
    , _dsrsServices = Nothing
    , _dsrsResponseStatus = pResponseStatus_
    }


-- | The format version of the response. For example, @aws_v1@ .
dsrsFormatVersion :: Lens' DescribeServicesResponse (Maybe Text)
dsrsFormatVersion = lens _dsrsFormatVersion (\ s a -> s{_dsrsFormatVersion = a})

-- | The pagination token for the next set of retreivable results.
dsrsNextToken :: Lens' DescribeServicesResponse (Maybe Text)
dsrsNextToken = lens _dsrsNextToken (\ s a -> s{_dsrsNextToken = a})

-- | The service metadata for the service or services in the response.
dsrsServices :: Lens' DescribeServicesResponse [PricingService]
dsrsServices = lens _dsrsServices (\ s a -> s{_dsrsServices = a}) . _Default . _Coerce

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeServicesResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DescribeServicesResponse where
