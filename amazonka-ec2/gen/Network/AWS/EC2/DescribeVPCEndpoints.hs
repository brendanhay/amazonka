{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVPCEndpoints
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC endpoints.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVPCEndpoints.html>
module Network.AWS.EC2.DescribeVPCEndpoints
    (
    -- * Request
      DescribeVPCEndpoints
    -- ** Request constructor
    , describeVPCEndpoints
    -- ** Request lenses
    , dvpcerqFilters
    , dvpcerqNextToken
    , dvpcerqVPCEndpointIds
    , dvpcerqDryRun
    , dvpcerqMaxResults

    -- * Response
    , DescribeVPCEndpointsResponse
    -- ** Response constructor
    , describeVPCEndpointsResponse
    -- ** Response lenses
    , dvpcersNextToken
    , dvpcersVPCEndpoints
    , dvpcersStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeVPCEndpoints' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcerqFilters'
--
-- * 'dvpcerqNextToken'
--
-- * 'dvpcerqVPCEndpointIds'
--
-- * 'dvpcerqDryRun'
--
-- * 'dvpcerqMaxResults'
data DescribeVPCEndpoints = DescribeVPCEndpoints'
    { _dvpcerqFilters        :: !(Maybe [Filter])
    , _dvpcerqNextToken      :: !(Maybe Text)
    , _dvpcerqVPCEndpointIds :: !(Maybe [Text])
    , _dvpcerqDryRun         :: !(Maybe Bool)
    , _dvpcerqMaxResults     :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCEndpoints' smart constructor.
describeVPCEndpoints :: DescribeVPCEndpoints
describeVPCEndpoints =
    DescribeVPCEndpoints'
    { _dvpcerqFilters = Nothing
    , _dvpcerqNextToken = Nothing
    , _dvpcerqVPCEndpointIds = Nothing
    , _dvpcerqDryRun = Nothing
    , _dvpcerqMaxResults = Nothing
    }

-- | One or more filters.
--
-- -   @service-name@: The name of the AWS service.
--
-- -   @vpc-id@: The ID of the VPC in which the endpoint resides.
--
-- -   @vpc-endpoint-id@: The ID of the endpoint.
--
-- -   @vpc-endpoint-state@: The state of the endpoint. (@pending@ |
--     @available@ | @deleting@ | @deleted@)
--
dvpcerqFilters :: Lens' DescribeVPCEndpoints [Filter]
dvpcerqFilters = lens _dvpcerqFilters (\ s a -> s{_dvpcerqFilters = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a prior call.)
dvpcerqNextToken :: Lens' DescribeVPCEndpoints (Maybe Text)
dvpcerqNextToken = lens _dvpcerqNextToken (\ s a -> s{_dvpcerqNextToken = a});

-- | One or more endpoint IDs.
dvpcerqVPCEndpointIds :: Lens' DescribeVPCEndpoints [Text]
dvpcerqVPCEndpointIds = lens _dvpcerqVPCEndpointIds (\ s a -> s{_dvpcerqVPCEndpointIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvpcerqDryRun :: Lens' DescribeVPCEndpoints (Maybe Bool)
dvpcerqDryRun = lens _dvpcerqDryRun (\ s a -> s{_dvpcerqDryRun = a});

-- | The maximum number of items to return for this request. The request
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- Constraint: If the value is greater than 1000, we return only 1000
-- items.
dvpcerqMaxResults :: Lens' DescribeVPCEndpoints (Maybe Int)
dvpcerqMaxResults = lens _dvpcerqMaxResults (\ s a -> s{_dvpcerqMaxResults = a});

instance AWSRequest DescribeVPCEndpoints where
        type Sv DescribeVPCEndpoints = EC2
        type Rs DescribeVPCEndpoints =
             DescribeVPCEndpointsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "vpcEndpointSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeVPCEndpoints where
        toHeaders = const mempty

instance ToPath DescribeVPCEndpoints where
        toPath = const "/"

instance ToQuery DescribeVPCEndpoints where
        toQuery DescribeVPCEndpoints'{..}
          = mconcat
              ["Action" =: ("DescribeVPCEndpoints" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvpcerqFilters),
               "NextToken" =: _dvpcerqNextToken,
               toQuery
                 (toQueryList "item" <$> _dvpcerqVPCEndpointIds),
               "DryRun" =: _dvpcerqDryRun,
               "MaxResults" =: _dvpcerqMaxResults]

-- | /See:/ 'describeVPCEndpointsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcersNextToken'
--
-- * 'dvpcersVPCEndpoints'
--
-- * 'dvpcersStatus'
data DescribeVPCEndpointsResponse = DescribeVPCEndpointsResponse'
    { _dvpcersNextToken    :: !(Maybe Text)
    , _dvpcersVPCEndpoints :: !(Maybe [VPCEndpoint])
    , _dvpcersStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeVPCEndpointsResponse' smart constructor.
describeVPCEndpointsResponse :: Int -> DescribeVPCEndpointsResponse
describeVPCEndpointsResponse pStatus_ =
    DescribeVPCEndpointsResponse'
    { _dvpcersNextToken = Nothing
    , _dvpcersVPCEndpoints = Nothing
    , _dvpcersStatus = pStatus_
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dvpcersNextToken :: Lens' DescribeVPCEndpointsResponse (Maybe Text)
dvpcersNextToken = lens _dvpcersNextToken (\ s a -> s{_dvpcersNextToken = a});

-- | Information about the endpoints.
dvpcersVPCEndpoints :: Lens' DescribeVPCEndpointsResponse [VPCEndpoint]
dvpcersVPCEndpoints = lens _dvpcersVPCEndpoints (\ s a -> s{_dvpcersVPCEndpoints = a}) . _Default;

-- | FIXME: Undocumented member.
dvpcersStatus :: Lens' DescribeVPCEndpointsResponse Int
dvpcersStatus = lens _dvpcersStatus (\ s a -> s{_dvpcersStatus = a});
