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
-- Module      : Network.AWS.EC2.DescribeMovingAddresses
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Elastic IP addresses that are being moved to the EC2-VPC platform, or that are being restored to the EC2-Classic platform. This request does not return information about any other Elastic IP addresses in your account.
--
--
module Network.AWS.EC2.DescribeMovingAddresses
    (
    -- * Creating a Request
      describeMovingAddresses
    , DescribeMovingAddresses
    -- * Request Lenses
    , dmaFilters
    , dmaPublicIPs
    , dmaNextToken
    , dmaDryRun
    , dmaMaxResults

    -- * Destructuring the Response
    , describeMovingAddressesResponse
    , DescribeMovingAddressesResponse
    -- * Response Lenses
    , dmarsMovingAddressStatuses
    , dmarsNextToken
    , dmarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeMovingAddresses.
--
--
--
-- /See:/ 'describeMovingAddresses' smart constructor.
data DescribeMovingAddresses = DescribeMovingAddresses'
  { _dmaFilters    :: !(Maybe [Filter])
  , _dmaPublicIPs  :: !(Maybe [Text])
  , _dmaNextToken  :: !(Maybe Text)
  , _dmaDryRun     :: !(Maybe Bool)
  , _dmaMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMovingAddresses' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmaFilters' - One or more filters.     * @moving-status@ - The status of the Elastic IP address (@MovingToVpc@ | @RestoringToClassic@ ).
--
-- * 'dmaPublicIPs' - One or more Elastic IP addresses.
--
-- * 'dmaNextToken' - The token to use to retrieve the next page of results.
--
-- * 'dmaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dmaMaxResults' - The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value outside of this range, an error is returned. Default: If no value is provided, the default is 1000.
describeMovingAddresses
    :: DescribeMovingAddresses
describeMovingAddresses =
  DescribeMovingAddresses'
    { _dmaFilters = Nothing
    , _dmaPublicIPs = Nothing
    , _dmaNextToken = Nothing
    , _dmaDryRun = Nothing
    , _dmaMaxResults = Nothing
    }


-- | One or more filters.     * @moving-status@ - The status of the Elastic IP address (@MovingToVpc@ | @RestoringToClassic@ ).
dmaFilters :: Lens' DescribeMovingAddresses [Filter]
dmaFilters = lens _dmaFilters (\ s a -> s{_dmaFilters = a}) . _Default . _Coerce

-- | One or more Elastic IP addresses.
dmaPublicIPs :: Lens' DescribeMovingAddresses [Text]
dmaPublicIPs = lens _dmaPublicIPs (\ s a -> s{_dmaPublicIPs = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results.
dmaNextToken :: Lens' DescribeMovingAddresses (Maybe Text)
dmaNextToken = lens _dmaNextToken (\ s a -> s{_dmaNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dmaDryRun :: Lens' DescribeMovingAddresses (Maybe Bool)
dmaDryRun = lens _dmaDryRun (\ s a -> s{_dmaDryRun = a})

-- | The maximum number of results to return for the request in a single page. The remaining results of the initial request can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value outside of this range, an error is returned. Default: If no value is provided, the default is 1000.
dmaMaxResults :: Lens' DescribeMovingAddresses (Maybe Int)
dmaMaxResults = lens _dmaMaxResults (\ s a -> s{_dmaMaxResults = a})

instance AWSRequest DescribeMovingAddresses where
        type Rs DescribeMovingAddresses =
             DescribeMovingAddressesResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeMovingAddressesResponse' <$>
                   (x .@? "movingAddressStatusSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMovingAddresses where

instance NFData DescribeMovingAddresses where

instance ToHeaders DescribeMovingAddresses where
        toHeaders = const mempty

instance ToPath DescribeMovingAddresses where
        toPath = const "/"

instance ToQuery DescribeMovingAddresses where
        toQuery DescribeMovingAddresses'{..}
          = mconcat
              ["Action" =:
                 ("DescribeMovingAddresses" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dmaFilters),
               toQuery (toQueryList "PublicIp" <$> _dmaPublicIPs),
               "NextToken" =: _dmaNextToken, "DryRun" =: _dmaDryRun,
               "MaxResults" =: _dmaMaxResults]

-- | Contains the output of DescribeMovingAddresses.
--
--
--
-- /See:/ 'describeMovingAddressesResponse' smart constructor.
data DescribeMovingAddressesResponse = DescribeMovingAddressesResponse'
  { _dmarsMovingAddressStatuses :: !(Maybe [MovingAddressStatus])
  , _dmarsNextToken             :: !(Maybe Text)
  , _dmarsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMovingAddressesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmarsMovingAddressStatuses' - The status for each Elastic IP address.
--
-- * 'dmarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dmarsResponseStatus' - -- | The response status code.
describeMovingAddressesResponse
    :: Int -- ^ 'dmarsResponseStatus'
    -> DescribeMovingAddressesResponse
describeMovingAddressesResponse pResponseStatus_ =
  DescribeMovingAddressesResponse'
    { _dmarsMovingAddressStatuses = Nothing
    , _dmarsNextToken = Nothing
    , _dmarsResponseStatus = pResponseStatus_
    }


-- | The status for each Elastic IP address.
dmarsMovingAddressStatuses :: Lens' DescribeMovingAddressesResponse [MovingAddressStatus]
dmarsMovingAddressStatuses = lens _dmarsMovingAddressStatuses (\ s a -> s{_dmarsMovingAddressStatuses = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dmarsNextToken :: Lens' DescribeMovingAddressesResponse (Maybe Text)
dmarsNextToken = lens _dmarsNextToken (\ s a -> s{_dmarsNextToken = a})

-- | -- | The response status code.
dmarsResponseStatus :: Lens' DescribeMovingAddressesResponse Int
dmarsResponseStatus = lens _dmarsResponseStatus (\ s a -> s{_dmarsResponseStatus = a})

instance NFData DescribeMovingAddressesResponse where
