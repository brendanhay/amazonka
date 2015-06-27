{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeSpotFleetInstances
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

-- | Describes the running instances for the specified Spot fleet.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotFleetInstances.html>
module Network.AWS.EC2.DescribeSpotFleetInstances
    (
    -- * Request
      DescribeSpotFleetInstances
    -- ** Request constructor
    , describeSpotFleetInstances
    -- ** Request lenses
    , dsfiNextToken
    , dsfiDryRun
    , dsfiMaxResults
    , dsfiSpotFleetRequestId

    -- * Response
    , DescribeSpotFleetInstancesResponse
    -- ** Response constructor
    , describeSpotFleetInstancesResponse
    -- ** Response lenses
    , dsfirNextToken
    , dsfirSpotFleetRequestId
    , dsfirActiveInstances
    , dsfirStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotFleetInstances.
--
-- /See:/ 'describeSpotFleetInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfiNextToken'
--
-- * 'dsfiDryRun'
--
-- * 'dsfiMaxResults'
--
-- * 'dsfiSpotFleetRequestId'
data DescribeSpotFleetInstances = DescribeSpotFleetInstances'
    { _dsfiNextToken          :: !(Maybe Text)
    , _dsfiDryRun             :: !(Maybe Bool)
    , _dsfiMaxResults         :: !(Maybe Int)
    , _dsfiSpotFleetRequestId :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeSpotFleetInstances' smart constructor.
describeSpotFleetInstances :: Text -> DescribeSpotFleetInstances
describeSpotFleetInstances pSpotFleetRequestId =
    DescribeSpotFleetInstances'
    { _dsfiNextToken = Nothing
    , _dsfiDryRun = Nothing
    , _dsfiMaxResults = Nothing
    , _dsfiSpotFleetRequestId = pSpotFleetRequestId
    }

-- | The token for the next set of results.
dsfiNextToken :: Lens' DescribeSpotFleetInstances (Maybe Text)
dsfiNextToken = lens _dsfiNextToken (\ s a -> s{_dsfiNextToken = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsfiDryRun :: Lens' DescribeSpotFleetInstances (Maybe Bool)
dsfiDryRun = lens _dsfiDryRun (\ s a -> s{_dsfiDryRun = a});

-- | The maximum number of results to return in a single call. Specify a
-- value between 1 and 1000. The default value is 1000. To retrieve the
-- remaining results, make another call with the returned @NextToken@
-- value.
dsfiMaxResults :: Lens' DescribeSpotFleetInstances (Maybe Int)
dsfiMaxResults = lens _dsfiMaxResults (\ s a -> s{_dsfiMaxResults = a});

-- | The ID of the Spot fleet request.
dsfiSpotFleetRequestId :: Lens' DescribeSpotFleetInstances Text
dsfiSpotFleetRequestId = lens _dsfiSpotFleetRequestId (\ s a -> s{_dsfiSpotFleetRequestId = a});

instance AWSRequest DescribeSpotFleetInstances where
        type Sv DescribeSpotFleetInstances = EC2
        type Rs DescribeSpotFleetInstances =
             DescribeSpotFleetInstancesResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotFleetInstancesResponse' <$>
                   (x .@? "nextToken") <*> (x .@ "spotFleetRequestId")
                     <*> (parseXMLList "item" x)
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeSpotFleetInstances where
        toHeaders = const mempty

instance ToPath DescribeSpotFleetInstances where
        toPath = const "/"

instance ToQuery DescribeSpotFleetInstances where
        toQuery DescribeSpotFleetInstances'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotFleetInstances" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NextToken" =: _dsfiNextToken,
               "DryRun" =: _dsfiDryRun,
               "MaxResults" =: _dsfiMaxResults,
               "SpotFleetRequestId" =: _dsfiSpotFleetRequestId]

-- | Contains the output of DescribeSpotFleetInstances.
--
-- /See:/ 'describeSpotFleetInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsfirNextToken'
--
-- * 'dsfirSpotFleetRequestId'
--
-- * 'dsfirActiveInstances'
--
-- * 'dsfirStatus'
data DescribeSpotFleetInstancesResponse = DescribeSpotFleetInstancesResponse'
    { _dsfirNextToken          :: !(Maybe Text)
    , _dsfirSpotFleetRequestId :: !Text
    , _dsfirActiveInstances    :: ![ActiveInstance]
    , _dsfirStatus             :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeSpotFleetInstancesResponse' smart constructor.
describeSpotFleetInstancesResponse :: Text -> Int -> DescribeSpotFleetInstancesResponse
describeSpotFleetInstancesResponse pSpotFleetRequestId pStatus =
    DescribeSpotFleetInstancesResponse'
    { _dsfirNextToken = Nothing
    , _dsfirSpotFleetRequestId = pSpotFleetRequestId
    , _dsfirActiveInstances = mempty
    , _dsfirStatus = pStatus
    }

-- | The token required to retrieve the next set of results. This value is
-- @null@ when there are no more results to return.
dsfirNextToken :: Lens' DescribeSpotFleetInstancesResponse (Maybe Text)
dsfirNextToken = lens _dsfirNextToken (\ s a -> s{_dsfirNextToken = a});

-- | The ID of the Spot fleet request.
dsfirSpotFleetRequestId :: Lens' DescribeSpotFleetInstancesResponse Text
dsfirSpotFleetRequestId = lens _dsfirSpotFleetRequestId (\ s a -> s{_dsfirSpotFleetRequestId = a});

-- | The running instances. Note that this list is refreshed periodically and
-- might be out of date.
dsfirActiveInstances :: Lens' DescribeSpotFleetInstancesResponse [ActiveInstance]
dsfirActiveInstances = lens _dsfirActiveInstances (\ s a -> s{_dsfirActiveInstances = a});

-- | FIXME: Undocumented member.
dsfirStatus :: Lens' DescribeSpotFleetInstancesResponse Int
dsfirStatus = lens _dsfirStatus (\ s a -> s{_dsfirStatus = a});
