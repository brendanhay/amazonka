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
-- Module      : Network.AWS.EC2.CreateSpotDatafeedSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data feed for Spot Instances, enabling you to view Spot Instance usage logs. You can create one data feed per AWS account. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance Data Feed> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
module Network.AWS.EC2.CreateSpotDatafeedSubscription
    (
    -- * Creating a Request
      createSpotDatafeedSubscription
    , CreateSpotDatafeedSubscription
    -- * Request Lenses
    , csdsPrefix
    , csdsDryRun
    , csdsBucket

    -- * Destructuring the Response
    , createSpotDatafeedSubscriptionResponse
    , CreateSpotDatafeedSubscriptionResponse
    -- * Response Lenses
    , csdsrsSpotDatafeedSubscription
    , csdsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateSpotDatafeedSubscription.
--
--
--
-- /See:/ 'createSpotDatafeedSubscription' smart constructor.
data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription'
  { _csdsPrefix :: !(Maybe Text)
  , _csdsDryRun :: !(Maybe Bool)
  , _csdsBucket :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdsPrefix' - A prefix for the data feed file names.
--
-- * 'csdsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'csdsBucket' - The Amazon S3 bucket in which to store the Spot Instance data feed.
createSpotDatafeedSubscription
    :: Text -- ^ 'csdsBucket'
    -> CreateSpotDatafeedSubscription
createSpotDatafeedSubscription pBucket_ =
  CreateSpotDatafeedSubscription'
    {_csdsPrefix = Nothing, _csdsDryRun = Nothing, _csdsBucket = pBucket_}


-- | A prefix for the data feed file names.
csdsPrefix :: Lens' CreateSpotDatafeedSubscription (Maybe Text)
csdsPrefix = lens _csdsPrefix (\ s a -> s{_csdsPrefix = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
csdsDryRun :: Lens' CreateSpotDatafeedSubscription (Maybe Bool)
csdsDryRun = lens _csdsDryRun (\ s a -> s{_csdsDryRun = a})

-- | The Amazon S3 bucket in which to store the Spot Instance data feed.
csdsBucket :: Lens' CreateSpotDatafeedSubscription Text
csdsBucket = lens _csdsBucket (\ s a -> s{_csdsBucket = a})

instance AWSRequest CreateSpotDatafeedSubscription
         where
        type Rs CreateSpotDatafeedSubscription =
             CreateSpotDatafeedSubscriptionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateSpotDatafeedSubscriptionResponse' <$>
                   (x .@? "spotDatafeedSubscription") <*>
                     (pure (fromEnum s)))

instance Hashable CreateSpotDatafeedSubscription
         where

instance NFData CreateSpotDatafeedSubscription where

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
               "Version" =: ("2016-11-15" :: ByteString),
               "Prefix" =: _csdsPrefix, "DryRun" =: _csdsDryRun,
               "Bucket" =: _csdsBucket]

-- | Contains the output of CreateSpotDatafeedSubscription.
--
--
--
-- /See:/ 'createSpotDatafeedSubscriptionResponse' smart constructor.
data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse'
  { _csdsrsSpotDatafeedSubscription :: !(Maybe SpotDatafeedSubscription)
  , _csdsrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSpotDatafeedSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csdsrsSpotDatafeedSubscription' - The Spot Instance data feed subscription.
--
-- * 'csdsrsResponseStatus' - -- | The response status code.
createSpotDatafeedSubscriptionResponse
    :: Int -- ^ 'csdsrsResponseStatus'
    -> CreateSpotDatafeedSubscriptionResponse
createSpotDatafeedSubscriptionResponse pResponseStatus_ =
  CreateSpotDatafeedSubscriptionResponse'
    { _csdsrsSpotDatafeedSubscription = Nothing
    , _csdsrsResponseStatus = pResponseStatus_
    }


-- | The Spot Instance data feed subscription.
csdsrsSpotDatafeedSubscription :: Lens' CreateSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
csdsrsSpotDatafeedSubscription = lens _csdsrsSpotDatafeedSubscription (\ s a -> s{_csdsrsSpotDatafeedSubscription = a})

-- | -- | The response status code.
csdsrsResponseStatus :: Lens' CreateSpotDatafeedSubscriptionResponse Int
csdsrsResponseStatus = lens _csdsrsResponseStatus (\ s a -> s{_csdsrsResponseStatus = a})

instance NFData
           CreateSpotDatafeedSubscriptionResponse
         where
