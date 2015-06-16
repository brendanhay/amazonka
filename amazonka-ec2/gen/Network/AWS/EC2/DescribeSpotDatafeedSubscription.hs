{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
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

-- | Describes the data feed for Spot Instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance Data Feed>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotDatafeedSubscription.html>
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
    (
    -- * Request
      DescribeSpotDatafeedSubscription
    -- ** Request constructor
    , describeSpotDatafeedSubscription
    -- ** Request lenses
    , dsdsDryRun

    -- * Response
    , DescribeSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , describeSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , dsdsrSpotDatafeedSubscription
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'describeSpotDatafeedSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsDryRun'
newtype DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription'{_dsdsDryRun :: Maybe Bool} deriving (Eq, Read, Show)

-- | 'DescribeSpotDatafeedSubscription' smart constructor.
describeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription
describeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription'{_dsdsDryRun = Nothing};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsdsDryRun :: Lens' DescribeSpotDatafeedSubscription (Maybe Bool)
dsdsDryRun = lens _dsdsDryRun (\ s a -> s{_dsdsDryRun = a});

instance AWSRequest DescribeSpotDatafeedSubscription
         where
        type Sv DescribeSpotDatafeedSubscription = EC2
        type Rs DescribeSpotDatafeedSubscription =
             DescribeSpotDatafeedSubscriptionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotDatafeedSubscriptionResponse' <$>
                   (x .@? "spotDatafeedSubscription"))

instance ToHeaders DescribeSpotDatafeedSubscription
         where
        toHeaders = const mempty

instance ToPath DescribeSpotDatafeedSubscription
         where
        toPath = const "/"

instance ToQuery DescribeSpotDatafeedSubscription
         where
        toQuery DescribeSpotDatafeedSubscription'{..}
          = mconcat
              ["Action" =:
                 ("DescribeSpotDatafeedSubscription" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dsdsDryRun]

-- | /See:/ 'describeSpotDatafeedSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsrSpotDatafeedSubscription'
newtype DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse'{_dsdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription} deriving (Eq, Read, Show)

-- | 'DescribeSpotDatafeedSubscriptionResponse' smart constructor.
describeSpotDatafeedSubscriptionResponse :: DescribeSpotDatafeedSubscriptionResponse
describeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse'{_dsdsrSpotDatafeedSubscription = Nothing};

-- | The Spot Instance data feed subscription.
dsdsrSpotDatafeedSubscription :: Lens' DescribeSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
dsdsrSpotDatafeedSubscription = lens _dsdsrSpotDatafeedSubscription (\ s a -> s{_dsdsrSpotDatafeedSubscription = a});
