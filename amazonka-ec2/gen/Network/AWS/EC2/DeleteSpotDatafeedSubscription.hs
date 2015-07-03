{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
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

-- | Deletes the data feed for Spot Instances. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance Data Feed>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSpotDatafeedSubscription.html>
module Network.AWS.EC2.DeleteSpotDatafeedSubscription
    (
    -- * Request
      DeleteSpotDatafeedSubscription
    -- ** Request constructor
    , deleteSpotDatafeedSubscription
    -- ** Request lenses
    , dsds1DryRun

    -- * Response
    , DeleteSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , deleteSpotDatafeedSubscriptionResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for DeleteSpotDatafeedSubscription.
--
-- /See:/ 'deleteSpotDatafeedSubscription' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsds1DryRun'
newtype DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription'
    { _dsds1DryRun :: Maybe Bool
    } deriving (Eq,Read,Show)

-- | 'DeleteSpotDatafeedSubscription' smart constructor.
deleteSpotDatafeedSubscription :: DeleteSpotDatafeedSubscription
deleteSpotDatafeedSubscription =
    DeleteSpotDatafeedSubscription'
    { _dsds1DryRun = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dsds1DryRun :: Lens' DeleteSpotDatafeedSubscription (Maybe Bool)
dsds1DryRun = lens _dsds1DryRun (\ s a -> s{_dsds1DryRun = a});

instance AWSRequest DeleteSpotDatafeedSubscription
         where
        type Sv DeleteSpotDatafeedSubscription = EC2
        type Rs DeleteSpotDatafeedSubscription =
             DeleteSpotDatafeedSubscriptionResponse
        request = post
        response
          = receiveNull DeleteSpotDatafeedSubscriptionResponse'

instance ToHeaders DeleteSpotDatafeedSubscription
         where
        toHeaders = const mempty

instance ToPath DeleteSpotDatafeedSubscription where
        toPath = const "/"

instance ToQuery DeleteSpotDatafeedSubscription where
        toQuery DeleteSpotDatafeedSubscription'{..}
          = mconcat
              ["Action" =:
                 ("DeleteSpotDatafeedSubscription" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dsds1DryRun]

-- | /See:/ 'deleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse =
    DeleteSpotDatafeedSubscriptionResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteSpotDatafeedSubscriptionResponse' smart constructor.
deleteSpotDatafeedSubscriptionResponse :: DeleteSpotDatafeedSubscriptionResponse
deleteSpotDatafeedSubscriptionResponse =
    DeleteSpotDatafeedSubscriptionResponse'
