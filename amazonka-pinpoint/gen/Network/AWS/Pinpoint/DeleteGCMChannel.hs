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
-- Module      : Network.AWS.Pinpoint.DeleteGCMChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the GCM channel for an app.
module Network.AWS.Pinpoint.DeleteGCMChannel
    (
    -- * Creating a Request
      deleteGCMChannel
    , DeleteGCMChannel
    -- * Request Lenses
    , dgcApplicationId

    -- * Destructuring the Response
    , deleteGCMChannelResponse
    , DeleteGCMChannelResponse
    -- * Response Lenses
    , dgcrsResponseStatus
    , dgcrsGCMChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGCMChannel' smart constructor.
newtype DeleteGCMChannel = DeleteGCMChannel'
  { _dgcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGCMChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgcApplicationId' - Undocumented member.
deleteGCMChannel
    :: Text -- ^ 'dgcApplicationId'
    -> DeleteGCMChannel
deleteGCMChannel pApplicationId_ =
  DeleteGCMChannel' {_dgcApplicationId = pApplicationId_}


-- | Undocumented member.
dgcApplicationId :: Lens' DeleteGCMChannel Text
dgcApplicationId = lens _dgcApplicationId (\ s a -> s{_dgcApplicationId = a})

instance AWSRequest DeleteGCMChannel where
        type Rs DeleteGCMChannel = DeleteGCMChannelResponse
        request = delete pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 DeleteGCMChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable DeleteGCMChannel where

instance NFData DeleteGCMChannel where

instance ToHeaders DeleteGCMChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteGCMChannel where
        toPath DeleteGCMChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _dgcApplicationId,
               "/channels/gcm"]

instance ToQuery DeleteGCMChannel where
        toQuery = const mempty

-- | /See:/ 'deleteGCMChannelResponse' smart constructor.
data DeleteGCMChannelResponse = DeleteGCMChannelResponse'
  { _dgcrsResponseStatus     :: !Int
  , _dgcrsGCMChannelResponse :: !GCMChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGCMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgcrsResponseStatus' - -- | The response status code.
--
-- * 'dgcrsGCMChannelResponse' - Undocumented member.
deleteGCMChannelResponse
    :: Int -- ^ 'dgcrsResponseStatus'
    -> GCMChannelResponse -- ^ 'dgcrsGCMChannelResponse'
    -> DeleteGCMChannelResponse
deleteGCMChannelResponse pResponseStatus_ pGCMChannelResponse_ =
  DeleteGCMChannelResponse'
    { _dgcrsResponseStatus = pResponseStatus_
    , _dgcrsGCMChannelResponse = pGCMChannelResponse_
    }


-- | -- | The response status code.
dgcrsResponseStatus :: Lens' DeleteGCMChannelResponse Int
dgcrsResponseStatus = lens _dgcrsResponseStatus (\ s a -> s{_dgcrsResponseStatus = a})

-- | Undocumented member.
dgcrsGCMChannelResponse :: Lens' DeleteGCMChannelResponse GCMChannelResponse
dgcrsGCMChannelResponse = lens _dgcrsGCMChannelResponse (\ s a -> s{_dgcrsGCMChannelResponse = a})

instance NFData DeleteGCMChannelResponse where
