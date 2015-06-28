{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeSpotFleetRequests
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes your Spot fleet requests.
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
    , dsfrrNextToken
    , dsfrrSpotFleetRequestConfigs
    , dsfrrStatus
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
    } deriving (Eq,Read,Show)

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
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotFleetRequestsResponse' <$>
                   (x .@? "nextToken") <*> (parseXMLList "item" x) <*>
                     (pure s))

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
-- * 'dsfrrNextToken'
--
-- * 'dsfrrSpotFleetRequestConfigs'
--
-- * 'dsfrrStatus'
data DescribeSpotFleetRequestsResponse = DescribeSpotFleetRequestsResponse'
    { _dsfrrNextToken               :: !(Maybe Text)
    , _dsfrrSpotFleetRequestConfigs :: ![SpotFleetRequestConfig]
    , _dsfrrStatus                  :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeSpotFleetRequestsResponse' smart constructor.
describeSpotFleetRequestsResponse :: Status -> DescribeSpotFleetRequestsResponse
describeSpotFleetRequestsResponse pStatus =
    DescribeSpotFleetRequestsResponse'
    { _dsfrrNextToken = Nothing
    , _dsfrrSpotFleetRequestConfigs = mempty
    , _dsfrrStatus = pStatus
    }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
dsfrrNextToken :: Lens' DescribeSpotFleetRequestsResponse (Maybe Text)
dsfrrNextToken = lens _dsfrrNextToken (\ s a -> s{_dsfrrNextToken = a});

-- | Information about the configuration of your Spot fleet.
dsfrrSpotFleetRequestConfigs :: Lens' DescribeSpotFleetRequestsResponse [SpotFleetRequestConfig]
dsfrrSpotFleetRequestConfigs = lens _dsfrrSpotFleetRequestConfigs (\ s a -> s{_dsfrrSpotFleetRequestConfigs = a});

-- | FIXME: Undocumented member.
dsfrrStatus :: Lens' DescribeSpotFleetRequestsResponse Status
dsfrrStatus = lens _dsfrrStatus (\ s a -> s{_dsfrrStatus = a});
