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
-- Module      : Network.AWS.Config.DeleteDeliveryChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the delivery channel.
--
--
-- Before you can delete the delivery channel, you must stop the configuration recorder by using the 'StopConfigurationRecorder' action.
--
module Network.AWS.Config.DeleteDeliveryChannel
    (
    -- * Creating a Request
      deleteDeliveryChannel
    , DeleteDeliveryChannel
    -- * Request Lenses
    , ddcDeliveryChannelName

    -- * Destructuring the Response
    , deleteDeliveryChannelResponse
    , DeleteDeliveryChannelResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DeleteDeliveryChannel' action. The action accepts the following data, in JSON format.
--
--
--
-- /See:/ 'deleteDeliveryChannel' smart constructor.
newtype DeleteDeliveryChannel = DeleteDeliveryChannel'
  { _ddcDeliveryChannelName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeliveryChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcDeliveryChannelName' - The name of the delivery channel to delete.
deleteDeliveryChannel
    :: Text -- ^ 'ddcDeliveryChannelName'
    -> DeleteDeliveryChannel
deleteDeliveryChannel pDeliveryChannelName_ =
  DeleteDeliveryChannel' {_ddcDeliveryChannelName = pDeliveryChannelName_}


-- | The name of the delivery channel to delete.
ddcDeliveryChannelName :: Lens' DeleteDeliveryChannel Text
ddcDeliveryChannelName = lens _ddcDeliveryChannelName (\ s a -> s{_ddcDeliveryChannelName = a})

instance AWSRequest DeleteDeliveryChannel where
        type Rs DeleteDeliveryChannel =
             DeleteDeliveryChannelResponse
        request = postJSON config
        response = receiveNull DeleteDeliveryChannelResponse'

instance Hashable DeleteDeliveryChannel where

instance NFData DeleteDeliveryChannel where

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
              (catMaybes
                 [Just
                    ("DeliveryChannelName" .= _ddcDeliveryChannelName)])

instance ToPath DeleteDeliveryChannel where
        toPath = const "/"

instance ToQuery DeleteDeliveryChannel where
        toQuery = const mempty

-- | /See:/ 'deleteDeliveryChannelResponse' smart constructor.
data DeleteDeliveryChannelResponse =
  DeleteDeliveryChannelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDeliveryChannelResponse' with the minimum fields required to make a request.
--
deleteDeliveryChannelResponse
    :: DeleteDeliveryChannelResponse
deleteDeliveryChannelResponse = DeleteDeliveryChannelResponse'


instance NFData DeleteDeliveryChannelResponse where
