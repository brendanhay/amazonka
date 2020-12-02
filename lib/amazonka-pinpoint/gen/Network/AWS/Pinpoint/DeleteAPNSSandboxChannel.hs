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
-- Module      : Network.AWS.Pinpoint.DeleteAPNSSandboxChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an APNS sandbox channel
module Network.AWS.Pinpoint.DeleteAPNSSandboxChannel
    (
    -- * Creating a Request
      deleteAPNSSandboxChannel
    , DeleteAPNSSandboxChannel
    -- * Request Lenses
    , dascApplicationId

    -- * Destructuring the Response
    , deleteAPNSSandboxChannelResponse
    , DeleteAPNSSandboxChannelResponse
    -- * Response Lenses
    , dascrsResponseStatus
    , dascrsAPNSSandboxChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAPNSSandboxChannel' smart constructor.
newtype DeleteAPNSSandboxChannel = DeleteAPNSSandboxChannel'
  { _dascApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSSandboxChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dascApplicationId' - Undocumented member.
deleteAPNSSandboxChannel
    :: Text -- ^ 'dascApplicationId'
    -> DeleteAPNSSandboxChannel
deleteAPNSSandboxChannel pApplicationId_ =
  DeleteAPNSSandboxChannel' {_dascApplicationId = pApplicationId_}


-- | Undocumented member.
dascApplicationId :: Lens' DeleteAPNSSandboxChannel Text
dascApplicationId = lens _dascApplicationId (\ s a -> s{_dascApplicationId = a})

instance AWSRequest DeleteAPNSSandboxChannel where
        type Rs DeleteAPNSSandboxChannel =
             DeleteAPNSSandboxChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteAPNSSandboxChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteAPNSSandboxChannel where

instance NFData DeleteAPNSSandboxChannel where

instance ToHeaders DeleteAPNSSandboxChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteAPNSSandboxChannel where
        toPath DeleteAPNSSandboxChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _dascApplicationId,
               "/channels/apns_sandbox"]

instance ToQuery DeleteAPNSSandboxChannel where
        toQuery = const mempty

-- | /See:/ 'deleteAPNSSandboxChannelResponse' smart constructor.
data DeleteAPNSSandboxChannelResponse = DeleteAPNSSandboxChannelResponse'
  { _dascrsResponseStatus             :: !Int
  , _dascrsAPNSSandboxChannelResponse :: !APNSSandboxChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAPNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dascrsResponseStatus' - -- | The response status code.
--
-- * 'dascrsAPNSSandboxChannelResponse' - Undocumented member.
deleteAPNSSandboxChannelResponse
    :: Int -- ^ 'dascrsResponseStatus'
    -> APNSSandboxChannelResponse -- ^ 'dascrsAPNSSandboxChannelResponse'
    -> DeleteAPNSSandboxChannelResponse
deleteAPNSSandboxChannelResponse pResponseStatus_ pAPNSSandboxChannelResponse_ =
  DeleteAPNSSandboxChannelResponse'
    { _dascrsResponseStatus = pResponseStatus_
    , _dascrsAPNSSandboxChannelResponse = pAPNSSandboxChannelResponse_
    }


-- | -- | The response status code.
dascrsResponseStatus :: Lens' DeleteAPNSSandboxChannelResponse Int
dascrsResponseStatus = lens _dascrsResponseStatus (\ s a -> s{_dascrsResponseStatus = a})

-- | Undocumented member.
dascrsAPNSSandboxChannelResponse :: Lens' DeleteAPNSSandboxChannelResponse APNSSandboxChannelResponse
dascrsAPNSSandboxChannelResponse = lens _dascrsAPNSSandboxChannelResponse (\ s a -> s{_dascrsAPNSSandboxChannelResponse = a})

instance NFData DeleteAPNSSandboxChannelResponse
         where
