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
-- Module      : Network.AWS.Pinpoint.UpdateAPNSChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use to update the APNs channel for an app.
module Network.AWS.Pinpoint.UpdateAPNSChannel
    (
    -- * Creating a Request
      updateAPNSChannel
    , UpdateAPNSChannel
    -- * Request Lenses
    , uacApplicationId
    , uacAPNSChannelRequest

    -- * Destructuring the Response
    , updateAPNSChannelResponse
    , UpdateAPNSChannelResponse
    -- * Response Lenses
    , uacrsResponseStatus
    , uacrsAPNSChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAPNSChannel' smart constructor.
data UpdateAPNSChannel = UpdateAPNSChannel'
  { _uacApplicationId      :: !Text
  , _uacAPNSChannelRequest :: !APNSChannelRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPNSChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uacApplicationId' - Undocumented member.
--
-- * 'uacAPNSChannelRequest' - Undocumented member.
updateAPNSChannel
    :: Text -- ^ 'uacApplicationId'
    -> APNSChannelRequest -- ^ 'uacAPNSChannelRequest'
    -> UpdateAPNSChannel
updateAPNSChannel pApplicationId_ pAPNSChannelRequest_ =
  UpdateAPNSChannel'
    { _uacApplicationId = pApplicationId_
    , _uacAPNSChannelRequest = pAPNSChannelRequest_
    }


-- | Undocumented member.
uacApplicationId :: Lens' UpdateAPNSChannel Text
uacApplicationId = lens _uacApplicationId (\ s a -> s{_uacApplicationId = a})

-- | Undocumented member.
uacAPNSChannelRequest :: Lens' UpdateAPNSChannel APNSChannelRequest
uacAPNSChannelRequest = lens _uacAPNSChannelRequest (\ s a -> s{_uacAPNSChannelRequest = a})

instance AWSRequest UpdateAPNSChannel where
        type Rs UpdateAPNSChannel = UpdateAPNSChannelResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateAPNSChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateAPNSChannel where

instance NFData UpdateAPNSChannel where

instance ToHeaders UpdateAPNSChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateAPNSChannel where
        toJSON UpdateAPNSChannel'{..}
          = object
              (catMaybes
                 [Just
                    ("APNSChannelRequest" .= _uacAPNSChannelRequest)])

instance ToPath UpdateAPNSChannel where
        toPath UpdateAPNSChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _uacApplicationId,
               "/channels/apns"]

instance ToQuery UpdateAPNSChannel where
        toQuery = const mempty

-- | /See:/ 'updateAPNSChannelResponse' smart constructor.
data UpdateAPNSChannelResponse = UpdateAPNSChannelResponse'
  { _uacrsResponseStatus      :: !Int
  , _uacrsAPNSChannelResponse :: !APNSChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAPNSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uacrsResponseStatus' - -- | The response status code.
--
-- * 'uacrsAPNSChannelResponse' - Undocumented member.
updateAPNSChannelResponse
    :: Int -- ^ 'uacrsResponseStatus'
    -> APNSChannelResponse -- ^ 'uacrsAPNSChannelResponse'
    -> UpdateAPNSChannelResponse
updateAPNSChannelResponse pResponseStatus_ pAPNSChannelResponse_ =
  UpdateAPNSChannelResponse'
    { _uacrsResponseStatus = pResponseStatus_
    , _uacrsAPNSChannelResponse = pAPNSChannelResponse_
    }


-- | -- | The response status code.
uacrsResponseStatus :: Lens' UpdateAPNSChannelResponse Int
uacrsResponseStatus = lens _uacrsResponseStatus (\ s a -> s{_uacrsResponseStatus = a})

-- | Undocumented member.
uacrsAPNSChannelResponse :: Lens' UpdateAPNSChannelResponse APNSChannelResponse
uacrsAPNSChannelResponse = lens _uacrsAPNSChannelResponse (\ s a -> s{_uacrsAPNSChannelResponse = a})

instance NFData UpdateAPNSChannelResponse where
