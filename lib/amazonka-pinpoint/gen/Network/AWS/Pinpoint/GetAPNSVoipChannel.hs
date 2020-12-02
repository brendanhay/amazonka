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
-- Module      : Network.AWS.Pinpoint.GetAPNSVoipChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an APNS VoIP channel
module Network.AWS.Pinpoint.GetAPNSVoipChannel
    (
    -- * Creating a Request
      getAPNSVoipChannel
    , GetAPNSVoipChannel
    -- * Request Lenses
    , gavcApplicationId

    -- * Destructuring the Response
    , getAPNSVoipChannelResponse
    , GetAPNSVoipChannelResponse
    -- * Response Lenses
    , gavcrsResponseStatus
    , gavcrsAPNSVoipChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPNSVoipChannel' smart constructor.
newtype GetAPNSVoipChannel = GetAPNSVoipChannel'
  { _gavcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPNSVoipChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gavcApplicationId' - Undocumented member.
getAPNSVoipChannel
    :: Text -- ^ 'gavcApplicationId'
    -> GetAPNSVoipChannel
getAPNSVoipChannel pApplicationId_ =
  GetAPNSVoipChannel' {_gavcApplicationId = pApplicationId_}


-- | Undocumented member.
gavcApplicationId :: Lens' GetAPNSVoipChannel Text
gavcApplicationId = lens _gavcApplicationId (\ s a -> s{_gavcApplicationId = a})

instance AWSRequest GetAPNSVoipChannel where
        type Rs GetAPNSVoipChannel =
             GetAPNSVoipChannelResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetAPNSVoipChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetAPNSVoipChannel where

instance NFData GetAPNSVoipChannel where

instance ToHeaders GetAPNSVoipChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAPNSVoipChannel where
        toPath GetAPNSVoipChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _gavcApplicationId,
               "/channels/apns_voip"]

instance ToQuery GetAPNSVoipChannel where
        toQuery = const mempty

-- | /See:/ 'getAPNSVoipChannelResponse' smart constructor.
data GetAPNSVoipChannelResponse = GetAPNSVoipChannelResponse'
  { _gavcrsResponseStatus          :: !Int
  , _gavcrsAPNSVoipChannelResponse :: !APNSVoipChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gavcrsResponseStatus' - -- | The response status code.
--
-- * 'gavcrsAPNSVoipChannelResponse' - Undocumented member.
getAPNSVoipChannelResponse
    :: Int -- ^ 'gavcrsResponseStatus'
    -> APNSVoipChannelResponse -- ^ 'gavcrsAPNSVoipChannelResponse'
    -> GetAPNSVoipChannelResponse
getAPNSVoipChannelResponse pResponseStatus_ pAPNSVoipChannelResponse_ =
  GetAPNSVoipChannelResponse'
    { _gavcrsResponseStatus = pResponseStatus_
    , _gavcrsAPNSVoipChannelResponse = pAPNSVoipChannelResponse_
    }


-- | -- | The response status code.
gavcrsResponseStatus :: Lens' GetAPNSVoipChannelResponse Int
gavcrsResponseStatus = lens _gavcrsResponseStatus (\ s a -> s{_gavcrsResponseStatus = a})

-- | Undocumented member.
gavcrsAPNSVoipChannelResponse :: Lens' GetAPNSVoipChannelResponse APNSVoipChannelResponse
gavcrsAPNSVoipChannelResponse = lens _gavcrsAPNSVoipChannelResponse (\ s a -> s{_gavcrsAPNSVoipChannelResponse = a})

instance NFData GetAPNSVoipChannelResponse where
