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
-- Module      : Network.AWS.Pinpoint.UpdateEmailChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an email channel
module Network.AWS.Pinpoint.UpdateEmailChannel
    (
    -- * Creating a Request
      updateEmailChannel
    , UpdateEmailChannel
    -- * Request Lenses
    , uecApplicationId
    , uecEmailChannelRequest

    -- * Destructuring the Response
    , updateEmailChannelResponse
    , UpdateEmailChannelResponse
    -- * Response Lenses
    , uecrsResponseStatus
    , uecrsEmailChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEmailChannel' smart constructor.
data UpdateEmailChannel = UpdateEmailChannel'
  { _uecApplicationId       :: !Text
  , _uecEmailChannelRequest :: !EmailChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEmailChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uecApplicationId' - Undocumented member.
--
-- * 'uecEmailChannelRequest' - Undocumented member.
updateEmailChannel
    :: Text -- ^ 'uecApplicationId'
    -> EmailChannelRequest -- ^ 'uecEmailChannelRequest'
    -> UpdateEmailChannel
updateEmailChannel pApplicationId_ pEmailChannelRequest_ =
  UpdateEmailChannel'
    { _uecApplicationId = pApplicationId_
    , _uecEmailChannelRequest = pEmailChannelRequest_
    }


-- | Undocumented member.
uecApplicationId :: Lens' UpdateEmailChannel Text
uecApplicationId = lens _uecApplicationId (\ s a -> s{_uecApplicationId = a})

-- | Undocumented member.
uecEmailChannelRequest :: Lens' UpdateEmailChannel EmailChannelRequest
uecEmailChannelRequest = lens _uecEmailChannelRequest (\ s a -> s{_uecEmailChannelRequest = a})

instance AWSRequest UpdateEmailChannel where
        type Rs UpdateEmailChannel =
             UpdateEmailChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateEmailChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateEmailChannel where

instance NFData UpdateEmailChannel where

instance ToHeaders UpdateEmailChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateEmailChannel where
        toJSON UpdateEmailChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("EmailChannelRequest" .= _uecEmailChannelRequest)])

instance ToPath UpdateEmailChannel where
        toPath UpdateEmailChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _uecApplicationId,
               "/channels/email"]

instance ToQuery UpdateEmailChannel where
        toQuery = const mempty

-- | /See:/ 'updateEmailChannelResponse' smart constructor.
data UpdateEmailChannelResponse = UpdateEmailChannelResponse'
  { _uecrsResponseStatus       :: !Int
  , _uecrsEmailChannelResponse :: !EmailChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEmailChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uecrsResponseStatus' - -- | The response status code.
--
-- * 'uecrsEmailChannelResponse' - Undocumented member.
updateEmailChannelResponse
    :: Int -- ^ 'uecrsResponseStatus'
    -> EmailChannelResponse -- ^ 'uecrsEmailChannelResponse'
    -> UpdateEmailChannelResponse
updateEmailChannelResponse pResponseStatus_ pEmailChannelResponse_ =
  UpdateEmailChannelResponse'
    { _uecrsResponseStatus = pResponseStatus_
    , _uecrsEmailChannelResponse = pEmailChannelResponse_
    }


-- | -- | The response status code.
uecrsResponseStatus :: Lens' UpdateEmailChannelResponse Int
uecrsResponseStatus = lens _uecrsResponseStatus (\ s a -> s{_uecrsResponseStatus = a})

-- | Undocumented member.
uecrsEmailChannelResponse :: Lens' UpdateEmailChannelResponse EmailChannelResponse
uecrsEmailChannelResponse = lens _uecrsEmailChannelResponse (\ s a -> s{_uecrsEmailChannelResponse = a})

instance NFData UpdateEmailChannelResponse where
