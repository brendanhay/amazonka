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
-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the data feed for Spot Instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance Data Feed> in the /Amazon EC2 User Guide for Linux Instances/ .
--
--
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
    (
    -- * Creating a Request
      describeSpotDatafeedSubscription
    , DescribeSpotDatafeedSubscription
    -- * Request Lenses
    , dsdsDryRun

    -- * Destructuring the Response
    , describeSpotDatafeedSubscriptionResponse
    , DescribeSpotDatafeedSubscriptionResponse
    -- * Response Lenses
    , dsdsrsSpotDatafeedSubscription
    , dsdsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeSpotDatafeedSubscription.
--
--
--
-- /See:/ 'describeSpotDatafeedSubscription' smart constructor.
newtype DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription'
  { _dsdsDryRun :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
describeSpotDatafeedSubscription
    :: DescribeSpotDatafeedSubscription
describeSpotDatafeedSubscription =
  DescribeSpotDatafeedSubscription' {_dsdsDryRun = Nothing}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsdsDryRun :: Lens' DescribeSpotDatafeedSubscription (Maybe Bool)
dsdsDryRun = lens _dsdsDryRun (\ s a -> s{_dsdsDryRun = a})

instance AWSRequest DescribeSpotDatafeedSubscription
         where
        type Rs DescribeSpotDatafeedSubscription =
             DescribeSpotDatafeedSubscriptionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeSpotDatafeedSubscriptionResponse' <$>
                   (x .@? "spotDatafeedSubscription") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeSpotDatafeedSubscription
         where

instance NFData DescribeSpotDatafeedSubscription
         where

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
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dsdsDryRun]

-- | Contains the output of DescribeSpotDatafeedSubscription.
--
--
--
-- /See:/ 'describeSpotDatafeedSubscriptionResponse' smart constructor.
data DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse'
  { _dsdsrsSpotDatafeedSubscription :: !(Maybe SpotDatafeedSubscription)
  , _dsdsrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSpotDatafeedSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdsrsSpotDatafeedSubscription' - The Spot Instance data feed subscription.
--
-- * 'dsdsrsResponseStatus' - -- | The response status code.
describeSpotDatafeedSubscriptionResponse
    :: Int -- ^ 'dsdsrsResponseStatus'
    -> DescribeSpotDatafeedSubscriptionResponse
describeSpotDatafeedSubscriptionResponse pResponseStatus_ =
  DescribeSpotDatafeedSubscriptionResponse'
    { _dsdsrsSpotDatafeedSubscription = Nothing
    , _dsdsrsResponseStatus = pResponseStatus_
    }


-- | The Spot Instance data feed subscription.
dsdsrsSpotDatafeedSubscription :: Lens' DescribeSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
dsdsrsSpotDatafeedSubscription = lens _dsdsrsSpotDatafeedSubscription (\ s a -> s{_dsdsrsSpotDatafeedSubscription = a})

-- | -- | The response status code.
dsdsrsResponseStatus :: Lens' DescribeSpotDatafeedSubscriptionResponse Int
dsdsrsResponseStatus = lens _dsdsrsResponseStatus (\ s a -> s{_dsdsrsResponseStatus = a})

instance NFData
           DescribeSpotDatafeedSubscriptionResponse
         where
