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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Elastic IP addresses that are being moved to the EC2-VPC
-- platform, or that are being restored to the EC2-Classic platform. This
-- request does not return information about any other Elastic IP addresses
-- in your account.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeMovingAddresses.html AWS API Reference> for DescribeMovingAddresses.
module Network.AWS.EC2.DescribeMovingAddresses
    (
    -- * Creating a Request
      DescribeMovingAddresses
    , describeMovingAddresses
    -- * Request Lenses
    , dmaPublicIPs
    , dmaFilters
    , dmaNextToken
    , dmaDryRun
    , dmaMaxResults

    -- * Destructuring the Response
    , DescribeMovingAddressesResponse
    , describeMovingAddressesResponse
    -- * Response Lenses
    , dmarsMovingAddressStatuses
    , dmarsNextToken
    , dmarsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMovingAddresses' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmaPublicIPs'
--
-- * 'dmaFilters'
--
-- * 'dmaNextToken'
--
-- * 'dmaDryRun'
--
-- * 'dmaMaxResults'
data DescribeMovingAddresses = DescribeMovingAddresses'
    { _dmaPublicIPs  :: !(Maybe [Text])
    , _dmaFilters    :: !(Maybe [Filter])
    , _dmaNextToken  :: !(Maybe Text)
    , _dmaDryRun     :: !(Maybe Bool)
    , _dmaMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMovingAddresses' smart constructor.
describeMovingAddresses :: DescribeMovingAddresses
describeMovingAddresses =
    DescribeMovingAddresses'
    { _dmaPublicIPs = Nothing
    , _dmaFilters = Nothing
    , _dmaNextToken = Nothing
    , _dmaDryRun = Nothing
    , _dmaMaxResults = Nothing
    }

-- | One or more Elastic IP addresses.
dmaPublicIPs :: Lens' DescribeMovingAddresses [Text]
dmaPublicIPs = lens _dmaPublicIPs (\ s a -> s{_dmaPublicIPs = a}) . _Default . _Coerce;

-- | One or more filters.
--
-- -   @moving-status@ - The status of the Elastic IP address
--     (@MovingToVpc@ | @RestoringToClassic@).
--
dmaFilters :: Lens' DescribeMovingAddresses [Filter]
dmaFilters = lens _dmaFilters (\ s a -> s{_dmaFilters = a}) . _Default . _Coerce;

-- | The token to use to retrieve the next page of results.
dmaNextToken :: Lens' DescribeMovingAddresses (Maybe Text)
dmaNextToken = lens _dmaNextToken (\ s a -> s{_dmaNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dmaDryRun :: Lens' DescribeMovingAddresses (Maybe Bool)
dmaDryRun = lens _dmaDryRun (\ s a -> s{_dmaDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value outside of
-- this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
dmaMaxResults :: Lens' DescribeMovingAddresses (Maybe Int)
dmaMaxResults = lens _dmaMaxResults (\ s a -> s{_dmaMaxResults = a});

instance AWSRequest DescribeMovingAddresses where
        type Sv DescribeMovingAddresses = EC2
        type Rs DescribeMovingAddresses =
             DescribeMovingAddressesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeMovingAddressesResponse' <$>
                   (x .@? "movingAddressStatusSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeMovingAddresses where
        toHeaders = const mempty

instance ToPath DescribeMovingAddresses where
        toPath = const "/"

instance ToQuery DescribeMovingAddresses where
        toQuery DescribeMovingAddresses'{..}
          = mconcat
              ["Action" =:
                 ("DescribeMovingAddresses" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "item" <$> _dmaPublicIPs),
               toQuery (toQueryList "Filter" <$> _dmaFilters),
               "NextToken" =: _dmaNextToken, "DryRun" =: _dmaDryRun,
               "MaxResults" =: _dmaMaxResults]

-- | /See:/ 'describeMovingAddressesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmarsMovingAddressStatuses'
--
-- * 'dmarsNextToken'
--
-- * 'dmarsStatus'
data DescribeMovingAddressesResponse = DescribeMovingAddressesResponse'
    { _dmarsMovingAddressStatuses :: !(Maybe [MovingAddressStatus])
    , _dmarsNextToken             :: !(Maybe Text)
    , _dmarsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeMovingAddressesResponse' smart constructor.
describeMovingAddressesResponse :: Int -> DescribeMovingAddressesResponse
describeMovingAddressesResponse pStatus_ =
    DescribeMovingAddressesResponse'
    { _dmarsMovingAddressStatuses = Nothing
    , _dmarsNextToken = Nothing
    , _dmarsStatus = pStatus_
    }

-- | The status for each Elastic IP address.
dmarsMovingAddressStatuses :: Lens' DescribeMovingAddressesResponse [MovingAddressStatus]
dmarsMovingAddressStatuses = lens _dmarsMovingAddressStatuses (\ s a -> s{_dmarsMovingAddressStatuses = a}) . _Default . _Coerce;

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
dmarsNextToken :: Lens' DescribeMovingAddressesResponse (Maybe Text)
dmarsNextToken = lens _dmarsNextToken (\ s a -> s{_dmarsNextToken = a});

-- | Undocumented member.
dmarsStatus :: Lens' DescribeMovingAddressesResponse Int
dmarsStatus = lens _dmarsStatus (\ s a -> s{_dmarsStatus = a});
