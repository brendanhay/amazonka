{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotFleetRequests
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes your Spot fleet requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetRequests.html>
module Network.AWS.EC2.DescribeSpotFleetRequests
    (
    -- * Request
      DescribeSpotFleetRequests
    -- ** Request constructor
    , describeSpotFleetRequests
    -- ** Request lenses
    , dsfrSpotFleetRequestIds
    , dsfrNextToken
    , dsfrDryRun
    , dsfrMaxResults

    -- * Response
    , DescribeSpotFleetRequestsResponse
    -- ** Response constructor
    , describeSpotFleetRequestsResponse
    -- ** Response lenses
    , dsfrrsNextToken
    , dsfrrsStatus
    , dsfrrsSpotFleetRequestConfigs
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotFleetRequests.
--
-- /See:/ 'describeSpotFleetRequests' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrSpotFleetRequestIds'
--
-- * 'dsfrNextToken'
--
-- * 'dsfrDryRun'
--
-- * 'dsfrMaxResults'
data DescribeSpotFleetRequests = DescribeSpotFleetRequests'
    { _dsfrSpotFleetRequestIds :: !(Maybe [Text])
    , _dsfrNextToken           :: !(Maybe Text)
    , _dsfrDryRun              :: !(Maybe Bool)
    , _dsfrMaxResults          :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotFleetRequests' smart constructor.
describeSpotFleetRequests :: DescribeSpotFleetRequests
describeSpotFleetRequests =
    DescribeSpotFleetRequests'
    { _dsfrSpotFleetRequestIds = Nothing
    , _dsfrNextToken = Nothing
    , _dsfrDryRun = Nothing
    , _dsfrMaxResults = Nothing
    }

-- | The IDs of the Spot fleet requests.
dsfrSpotFleetRequestIds :: Lens' DescribeSpotFleetRequests [Text]
dsfrSpotFleetRequestIds = lens _dsfrSpotFleetRequestIds (\ s a -> s{_dsfrSpotFleetRequestIds = a}) . _Default;

-- | The token for the next set of results.
dsfrNextToken :: Lens' DescribeSpotFleetRequests (Maybe Text)
dsfrNextToken = lens _dsfrNextToken (\ s a -> s{_dsfrNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsfrDryRun :: Lens' DescribeSpotFleetRequests (Maybe Bool)
dsfrDryRun = lens _dsfrDryRun (\ s a -> s{_dsfrDryRun = a});

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
dsfrMaxResults :: Lens' DescribeSpotFleetRequests (Maybe Int)
dsfrMaxResults = lens _dsfrMaxResults (\ s a -> s{_dsfrMaxResults = a});

instance AWSRequest DescribeSpotFleetRequests where
        type Sv DescribeSpotFleetRequests = EC2
        type Rs DescribeSpotFleetRequests =
             DescribeSpotFleetRequestsResponse
        request = post "DescribeSpotFleetRequests"
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotFleetRequestsResponse' <$>
                   (x .@? "nextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "spotFleetRequestConfigSet" .!@ mempty >>=
                        parseXMLList "item"))

instance ToHeaders DescribeSpotFleetRequests where
        toHeaders = const mempty

instance ToPath DescribeSpotFleetRequests where
        toPath = const "/"

instance ToQuery DescribeSpotFleetRequests where
        toQuery DescribeSpotFleetRequests'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotFleetRequests" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               toQuery
                 (toQueryList "item" <$> _dsfrSpotFleetRequestIds),
               "NextToken" =: _dsfrNextToken,
               "DryRun" =: _dsfrDryRun,
               "MaxResults" =: _dsfrMaxResults]

-- | Contains the output of DescribeSpotFleetRequests.
--
-- /See:/ 'describeSpotFleetRequestsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfrrsNextToken'
--
-- * 'dsfrrsStatus'
--
-- * 'dsfrrsSpotFleetRequestConfigs'
data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse'
    { _dsfrrsNextToken               :: !(Maybe Text)
    , _dsfrrsStatus                  :: !Int
    , _dsfrrsSpotFleetRequestConfigs :: ![SpotFleetRequestConfig]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotFleetRequestsResponse' smart constructor.
describeSpotFleetRequestsResponse :: Int -> DescribeSpotFleetRequestsResponse
describeSpotFleetRequestsResponse pStatus_ =
    DescribeSpotFleetRequestsResponse'
    { _dsfrrsNextToken = Nothing
    , _dsfrrsStatus = pStatus_
    , _dsfrrsSpotFleetRequestConfigs = mempty
    }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
dsfrrsNextToken :: Lens' DescribeSpotFleetRequestsResponse (Maybe Text)
dsfrrsNextToken = lens _dsfrrsNextToken (\ s a -> s{_dsfrrsNextToken = a});

-- | FIXME: Undocumented member.
dsfrrsStatus :: Lens' DescribeSpotFleetRequestsResponse Int
dsfrrsStatus = lens _dsfrrsStatus (\ s a -> s{_dsfrrsStatus = a});

-- | Information about the configuration of your Spot fleet.
dsfrrsSpotFleetRequestConfigs :: Lens' DescribeSpotFleetRequestsResponse [SpotFleetRequestConfig]
dsfrrsSpotFleetRequestConfigs = lens _dsfrrsSpotFleetRequestConfigs (\ s a -> s{_dsfrrsSpotFleetRequestConfigs = a});
