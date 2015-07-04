{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.EC2.CreateSpotDatafeedSubscription
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a data feed for Spot Instances, enabling you to view Spot
-- Instance usage logs. You can create one data feed per AWS account. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance Data Feed>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSpotDatafeedSubscription.html>
module Network.AWS.EC2.CreateSpotDatafeedSubscription
    (
    -- * Request
      CreateSpotDatafeedSubscription
    -- ** Request constructor
    , createSpotDatafeedSubscription
    -- ** Request lenses
    , csdsPrefix
    , csdsDryRun
    , csdsBucket

    -- * Response
    , CreateSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , createSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , csdsrSpotDatafeedSubscription
    , csdsrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreateSpotDatafeedSubscription.
--
-- /See:/ 'createSpotDatafeedSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdsPrefix'
--
-- * 'csdsDryRun'
--
-- * 'csdsBucket'
data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription'
    { _csdsPrefix :: !(Maybe Text)
    , _csdsDryRun :: !(Maybe Bool)
    , _csdsBucket :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSpotDatafeedSubscription' smart constructor.
createSpotDatafeedSubscription :: Text -> CreateSpotDatafeedSubscription
createSpotDatafeedSubscription pBucket =
    CreateSpotDatafeedSubscription'
    { _csdsPrefix = Nothing
    , _csdsDryRun = Nothing
    , _csdsBucket = pBucket
    }

-- | A prefix for the data feed file names.
csdsPrefix :: Lens' CreateSpotDatafeedSubscription (Maybe Text)
csdsPrefix = lens _csdsPrefix (\ s a -> s{_csdsPrefix = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csdsDryRun :: Lens' CreateSpotDatafeedSubscription (Maybe Bool)
csdsDryRun = lens _csdsDryRun (\ s a -> s{_csdsDryRun = a});

-- | The Amazon S3 bucket in which to store the Spot Instance data feed.
csdsBucket :: Lens' CreateSpotDatafeedSubscription Text
csdsBucket = lens _csdsBucket (\ s a -> s{_csdsBucket = a});

instance AWSRequest CreateSpotDatafeedSubscription
         where
        type Sv CreateSpotDatafeedSubscription = EC2
        type Rs CreateSpotDatafeedSubscription =
             CreateSpotDatafeedSubscriptionResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CreateSpotDatafeedSubscriptionResponse' <$>
                   (x .@? "spotDatafeedSubscription") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreateSpotDatafeedSubscription
         where
        toHeaders = const mempty

instance ToPath CreateSpotDatafeedSubscription where
        toPath = const "/"

instance ToQuery CreateSpotDatafeedSubscription where
        toQuery CreateSpotDatafeedSubscription'{..}
          = mconcat
              ["Action" =:
                 ("CreateSpotDatafeedSubscription" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Prefix" =: _csdsPrefix, "DryRun" =: _csdsDryRun,
               "Bucket" =: _csdsBucket]

-- | Contains the output of CreateSpotDatafeedSubscription.
--
-- /See:/ 'createSpotDatafeedSubscriptionResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdsrSpotDatafeedSubscription'
--
-- * 'csdsrStatus'
data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse'
    { _csdsrSpotDatafeedSubscription :: !(Maybe SpotDatafeedSubscription)
    , _csdsrStatus                   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSpotDatafeedSubscriptionResponse' smart constructor.
createSpotDatafeedSubscriptionResponse :: Int -> CreateSpotDatafeedSubscriptionResponse
createSpotDatafeedSubscriptionResponse pStatus =
    CreateSpotDatafeedSubscriptionResponse'
    { _csdsrSpotDatafeedSubscription = Nothing
    , _csdsrStatus = pStatus
    }

-- | The Spot Instance data feed subscription.
csdsrSpotDatafeedSubscription :: Lens' CreateSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
csdsrSpotDatafeedSubscription = lens _csdsrSpotDatafeedSubscription (\ s a -> s{_csdsrSpotDatafeedSubscription = a});

-- | FIXME: Undocumented member.
csdsrStatus :: Lens' CreateSpotDatafeedSubscriptionResponse Int
csdsrStatus = lens _csdsrStatus (\ s a -> s{_csdsrStatus = a});
