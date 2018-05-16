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
-- Module      : Network.AWS.Pinpoint.DeleteAPNSVoipChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an APNS VoIP channel
module Network.AWS.Pinpoint.DeleteAPNSVoipChannel
    (
    -- * Creating a Request
      deleteAPNSVoipChannel
    , DeleteAPNSVoipChannel
    -- * Request Lenses
    , davcApplicationId

    -- * Destructuring the Response
    , deleteAPNSVoipChannelResponse
    , DeleteAPNSVoipChannelResponse
    -- * Response Lenses
    , davcrsResponseStatus
    , davcrsAPNSVoipChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAPNSVoipChannel' smart constructor.
newtype DeleteAPNSVoipChannel = DeleteAPNSVoipChannel'
  { _davcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSVoipChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davcApplicationId' - Undocumented member.
deleteAPNSVoipChannel
    :: Text -- ^ 'davcApplicationId'
    -> DeleteAPNSVoipChannel
deleteAPNSVoipChannel pApplicationId_ =
  DeleteAPNSVoipChannel' {_davcApplicationId = pApplicationId_}


-- | Undocumented member.
davcApplicationId :: Lens' DeleteAPNSVoipChannel Text
davcApplicationId = lens _davcApplicationId (\ s a -> s{_davcApplicationId = a})

instance AWSRequest DeleteAPNSVoipChannel where
        type Rs DeleteAPNSVoipChannel =
             DeleteAPNSVoipChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAPNSVoipChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteAPNSVoipChannel where

instance NFData DeleteAPNSVoipChannel where

instance ToHeaders DeleteAPNSVoipChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAPNSVoipChannel where
        toPath DeleteAPNSVoipChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _davcApplicationId,
               "/channels/apns_voip"]

instance ToQuery DeleteAPNSVoipChannel where
        toQuery = const mempty

-- | /See:/ 'deleteAPNSVoipChannelResponse' smart constructor.
data DeleteAPNSVoipChannelResponse = DeleteAPNSVoipChannelResponse'
  { _davcrsResponseStatus          :: !Int
  , _davcrsAPNSVoipChannelResponse :: !APNSVoipChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'davcrsResponseStatus' - -- | The response status code.
--
-- * 'davcrsAPNSVoipChannelResponse' - Undocumented member.
deleteAPNSVoipChannelResponse
    :: Int -- ^ 'davcrsResponseStatus'
    -> APNSVoipChannelResponse -- ^ 'davcrsAPNSVoipChannelResponse'
    -> DeleteAPNSVoipChannelResponse
deleteAPNSVoipChannelResponse pResponseStatus_ pAPNSVoipChannelResponse_ =
  DeleteAPNSVoipChannelResponse'
    { _davcrsResponseStatus = pResponseStatus_
    , _davcrsAPNSVoipChannelResponse = pAPNSVoipChannelResponse_
    }


-- | -- | The response status code.
davcrsResponseStatus :: Lens' DeleteAPNSVoipChannelResponse Int
davcrsResponseStatus = lens _davcrsResponseStatus (\ s a -> s{_davcrsResponseStatus = a})

-- | Undocumented member.
davcrsAPNSVoipChannelResponse :: Lens' DeleteAPNSVoipChannelResponse APNSVoipChannelResponse
davcrsAPNSVoipChannelResponse = lens _davcrsAPNSVoipChannelResponse (\ s a -> s{_davcrsAPNSVoipChannelResponse = a})

instance NFData DeleteAPNSVoipChannelResponse where
