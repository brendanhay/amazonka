{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Config.DeleteDeliveryChannel
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

-- | Deletes the specified delivery channel.
--
-- The delivery channel cannot be deleted if it is the only delivery
-- channel and the configuration recorder is still running. To delete the
-- delivery channel, stop the running configuration recorder using the
-- StopConfigurationRecorder action.
--
-- <http://docs.aws.amazon.com/config/latest/APIReference/API_DeleteDeliveryChannel.html>
module Network.AWS.Config.DeleteDeliveryChannel
    (
    -- * Request
      DeleteDeliveryChannel
    -- ** Request constructor
    , deleteDeliveryChannel
    -- ** Request lenses
    , ddcDeliveryChannelName

    -- * Response
    , DeleteDeliveryChannelResponse
    -- ** Response constructor
    , deleteDeliveryChannelResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDeliveryChannel' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDeliveryChannelName'
newtype DeleteDeliveryChannel = DeleteDeliveryChannel'{_ddcDeliveryChannelName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDeliveryChannel' smart constructor.
deleteDeliveryChannel :: Text -> DeleteDeliveryChannel
deleteDeliveryChannel pDeliveryChannelName = DeleteDeliveryChannel'{_ddcDeliveryChannelName = pDeliveryChannelName};

-- | The name of the delivery channel to delete.
ddcDeliveryChannelName :: Lens' DeleteDeliveryChannel Text
ddcDeliveryChannelName = lens _ddcDeliveryChannelName (\ s a -> s{_ddcDeliveryChannelName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteDeliveryChannel where
        type Sv DeleteDeliveryChannel = Config
        type Rs DeleteDeliveryChannel =
             DeleteDeliveryChannelResponse
        request = postJSON
        response = receiveNull DeleteDeliveryChannelResponse'

instance ToHeaders DeleteDeliveryChannel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteDeliveryChannel" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDeliveryChannel where
        toJSON DeleteDeliveryChannel'{..}
          = object
              ["DeliveryChannelName" .= _ddcDeliveryChannelName]

instance ToPath DeleteDeliveryChannel where
        toPath = const "/"

instance ToQuery DeleteDeliveryChannel where
        toQuery = const mempty

-- | /See:/ 'deleteDeliveryChannelResponse' smart constructor.
data DeleteDeliveryChannelResponse = DeleteDeliveryChannelResponse' deriving (Eq, Read, Show)

-- | 'DeleteDeliveryChannelResponse' smart constructor.
deleteDeliveryChannelResponse :: DeleteDeliveryChannelResponse
deleteDeliveryChannelResponse = DeleteDeliveryChannelResponse';
