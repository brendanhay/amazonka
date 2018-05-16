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
-- Module      : Network.AWS.EC2.DeleteSpotDatafeedSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the data feed for Spot Instances.
--
--
module Network.AWS.EC2.DeleteSpotDatafeedSubscription
    (
    -- * Creating a Request
      deleteSpotDatafeedSubscription
    , DeleteSpotDatafeedSubscription
    -- * Request Lenses
    , dsdssDryRun

    -- * Destructuring the Response
    , deleteSpotDatafeedSubscriptionResponse
    , DeleteSpotDatafeedSubscriptionResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteSpotDatafeedSubscription.
--
--
--
-- /See:/ 'deleteSpotDatafeedSubscription' smart constructor.
newtype DeleteSpotDatafeedSubscription = DeleteSpotDatafeedSubscription'
  { _dsdssDryRun :: Maybe Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSpotDatafeedSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdssDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deleteSpotDatafeedSubscription
    :: DeleteSpotDatafeedSubscription
deleteSpotDatafeedSubscription =
  DeleteSpotDatafeedSubscription' {_dsdssDryRun = Nothing}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dsdssDryRun :: Lens' DeleteSpotDatafeedSubscription (Maybe Bool)
dsdssDryRun = lens _dsdssDryRun (\ s a -> s{_dsdssDryRun = a})

instance AWSRequest DeleteSpotDatafeedSubscription
         where
        type Rs DeleteSpotDatafeedSubscription =
             DeleteSpotDatafeedSubscriptionResponse
        request = postQuery ec2
        response
          = receiveNull DeleteSpotDatafeedSubscriptionResponse'

instance Hashable DeleteSpotDatafeedSubscription
         where

instance NFData DeleteSpotDatafeedSubscription where

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
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dsdssDryRun]

-- | /See:/ 'deleteSpotDatafeedSubscriptionResponse' smart constructor.
data DeleteSpotDatafeedSubscriptionResponse =
  DeleteSpotDatafeedSubscriptionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSpotDatafeedSubscriptionResponse' with the minimum fields required to make a request.
--
deleteSpotDatafeedSubscriptionResponse
    :: DeleteSpotDatafeedSubscriptionResponse
deleteSpotDatafeedSubscriptionResponse = DeleteSpotDatafeedSubscriptionResponse'


instance NFData
           DeleteSpotDatafeedSubscriptionResponse
         where
