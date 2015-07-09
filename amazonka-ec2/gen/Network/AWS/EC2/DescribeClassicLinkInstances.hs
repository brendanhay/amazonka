{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeClassicLinkInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Describes one or more of your linked EC2-Classic instances. This request
-- only returns information about EC2-Classic instances linked to a VPC
-- through ClassicLink; you cannot use this request to return information
-- about other instances.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeClassicLinkInstances.html>
module Network.AWS.EC2.DescribeClassicLinkInstances
    (
    -- * Request
      DescribeClassicLinkInstances
    -- ** Request constructor
    , describeClassicLinkInstances
    -- ** Request lenses
    , dcliFilters
    , dcliNextToken
    , dcliInstanceIds
    , dcliDryRun
    , dcliMaxResults

    -- * Response
    , DescribeClassicLinkInstancesResponse
    -- ** Response constructor
    , describeClassicLinkInstancesResponse
    -- ** Response lenses
    , dclirNextToken
    , dclirInstances
    , dclirStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeClassicLinkInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcliFilters'
--
-- * 'dcliNextToken'
--
-- * 'dcliInstanceIds'
--
-- * 'dcliDryRun'
--
-- * 'dcliMaxResults'
data DescribeClassicLinkInstances = DescribeClassicLinkInstances'
    { _dcliFilters     :: !(Maybe [Filter])
    , _dcliNextToken   :: !(Maybe Text)
    , _dcliInstanceIds :: !(Maybe [Text])
    , _dcliDryRun      :: !(Maybe Bool)
    , _dcliMaxResults  :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClassicLinkInstances' smart constructor.
describeClassicLinkInstances :: DescribeClassicLinkInstances
describeClassicLinkInstances =
    DescribeClassicLinkInstances'
    { _dcliFilters = Nothing
    , _dcliNextToken = Nothing
    , _dcliInstanceIds = Nothing
    , _dcliDryRun = Nothing
    , _dcliMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   @group-id@ - The ID of a VPC security group that\'s associated with
--     the instance.
--
-- -   @instance-id@ - The ID of the instance.
--
-- -   @tag@:/key/=/value/ - The key\/value combination of a tag assigned
--     to the resource.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. This filter
--     is independent of the @tag-value@ filter. For example, if you use
--     both the filter \"tag-key=Purpose\" and the filter \"tag-value=X\",
--     you get any resources assigned both the tag key Purpose (regardless
--     of what the tag\'s value is), and the tag value X (regardless of
--     what the tag\'s key is). If you want to list only resources where
--     Purpose is X, see the @tag@:/key/=/value/ filter.
--
-- -   @tag-value@ - The value of a tag assigned to the resource. This
--     filter is independent of the @tag-key@ filter.
--
-- -   @vpc-id@ - The ID of the VPC that the instance is linked to.
--
dcliFilters :: Lens' DescribeClassicLinkInstances [Filter]
dcliFilters = lens _dcliFilters (\ s a -> s{_dcliFilters = a}) . _Default;

-- | The token to retrieve the next page of results.
dcliNextToken :: Lens' DescribeClassicLinkInstances (Maybe Text)
dcliNextToken = lens _dcliNextToken (\ s a -> s{_dcliNextToken = a});

-- | One or more instance IDs. Must be instances linked to a VPC through
-- ClassicLink.
dcliInstanceIds :: Lens' DescribeClassicLinkInstances [Text]
dcliInstanceIds = lens _dcliInstanceIds (\ s a -> s{_dcliInstanceIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dcliDryRun :: Lens' DescribeClassicLinkInstances (Maybe Bool)
dcliDryRun = lens _dcliDryRun (\ s a -> s{_dcliDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value larger than
-- 1000, only 1000 results are returned. You cannot specify this parameter
-- and the instance IDs parameter in the same request.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
dcliMaxResults :: Lens' DescribeClassicLinkInstances (Maybe Int)
dcliMaxResults = lens _dcliMaxResults (\ s a -> s{_dcliMaxResults = a});

instance AWSRequest DescribeClassicLinkInstances
         where
        type Sv DescribeClassicLinkInstances = EC2
        type Rs DescribeClassicLinkInstances =
             DescribeClassicLinkInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeClassicLinkInstancesResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "instancesSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeClassicLinkInstances where
        toHeaders = const mempty

instance ToPath DescribeClassicLinkInstances where
        toPath = const "/"

instance ToQuery DescribeClassicLinkInstances where
        toQuery DescribeClassicLinkInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClassicLinkInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dcliFilters),
               "NextToken" =: _dcliNextToken,
               toQuery
                 (toQueryList "InstanceId" <$> _dcliInstanceIds),
               "DryRun" =: _dcliDryRun,
               "MaxResults" =: _dcliMaxResults]

-- | /See:/ 'describeClassicLinkInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dclirNextToken'
--
-- * 'dclirInstances'
--
-- * 'dclirStatus'
data DescribeClassicLinkInstancesResponse = DescribeClassicLinkInstancesResponse'
    { _dclirNextToken :: !(Maybe Text)
    , _dclirInstances :: !(Maybe [ClassicLinkInstance])
    , _dclirStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeClassicLinkInstancesResponse' smart constructor.
describeClassicLinkInstancesResponse :: Int -> DescribeClassicLinkInstancesResponse
describeClassicLinkInstancesResponse pStatus =
    DescribeClassicLinkInstancesResponse'
    { _dclirNextToken = Nothing
    , _dclirInstances = Nothing
    , _dclirStatus = pStatus
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
dclirNextToken :: Lens' DescribeClassicLinkInstancesResponse (Maybe Text)
dclirNextToken = lens _dclirNextToken (\ s a -> s{_dclirNextToken = a});

-- | Information about one or more linked EC2-Classic instances.
dclirInstances :: Lens' DescribeClassicLinkInstancesResponse [ClassicLinkInstance]
dclirInstances = lens _dclirInstances (\ s a -> s{_dclirInstances = a}) . _Default;

-- | FIXME: Undocumented member.
dclirStatus :: Lens' DescribeClassicLinkInstancesResponse Int
dclirStatus = lens _dclirStatus (\ s a -> s{_dclirStatus = a});
