{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the data feed for Spot Instances. For more information, see
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
    , dsdsrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DescribeSpotDatafeedSubscription.
--
-- /See:/ 'describeSpotDatafeedSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsDryRun'
newtype DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription'
    { _dsdsDryRun :: Maybe Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotDatafeedSubscription' smart constructor.
describeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription
describeSpotDatafeedSubscription =
    DescribeSpotDatafeedSubscription'
    { _dsdsDryRun = Nothing
    }

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
                   (x .@? "spotDatafeedSubscription") <*>
                     (pure (fromEnum s)))

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

-- | Contains the output of DescribeSpotDatafeedSubscription.
--
-- /See:/ 'describeSpotDatafeedSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsrSpotDatafeedSubscription'
--
-- * 'dsdsrStatus'
data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse'
    { _dsdsrSpotDatafeedSubscription :: !(Maybe SpotDatafeedSubscription)
    , _dsdsrStatus                   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSpotDatafeedSubscriptionResponse' smart constructor.
describeSpotDatafeedSubscriptionResponse :: Int -> DescribeSpotDatafeedSubscriptionResponse
describeSpotDatafeedSubscriptionResponse pStatus =
    DescribeSpotDatafeedSubscriptionResponse'
    { _dsdsrSpotDatafeedSubscription = Nothing
    , _dsdsrStatus = pStatus
    }

-- | The Spot Instance data feed subscription.
dsdsrSpotDatafeedSubscription :: Lens' DescribeSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
dsdsrSpotDatafeedSubscription = lens _dsdsrSpotDatafeedSubscription (\ s a -> s{_dsdsrSpotDatafeedSubscription = a});

-- | FIXME: Undocumented member.
dsdsrStatus :: Lens' DescribeSpotDatafeedSubscriptionResponse Int
dsdsrStatus = lens _dsdsrStatus (\ s a -> s{_dsdsrStatus = a});
