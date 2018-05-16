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
-- Module      : Network.AWS.Pinpoint.GetAPNSSandboxChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an APNS sandbox channel
module Network.AWS.Pinpoint.GetAPNSSandboxChannel
    (
    -- * Creating a Request
      getAPNSSandboxChannel
    , GetAPNSSandboxChannel
    -- * Request Lenses
    , gascApplicationId

    -- * Destructuring the Response
    , getAPNSSandboxChannelResponse
    , GetAPNSSandboxChannelResponse
    -- * Response Lenses
    , gascrsResponseStatus
    , gascrsAPNSSandboxChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPNSSandboxChannel' smart constructor.
newtype GetAPNSSandboxChannel = GetAPNSSandboxChannel'
  { _gascApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPNSSandboxChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gascApplicationId' - Undocumented member.
getAPNSSandboxChannel
    :: Text -- ^ 'gascApplicationId'
    -> GetAPNSSandboxChannel
getAPNSSandboxChannel pApplicationId_ =
  GetAPNSSandboxChannel' {_gascApplicationId = pApplicationId_}


-- | Undocumented member.
gascApplicationId :: Lens' GetAPNSSandboxChannel Text
gascApplicationId = lens _gascApplicationId (\ s a -> s{_gascApplicationId = a})

instance AWSRequest GetAPNSSandboxChannel where
        type Rs GetAPNSSandboxChannel =
             GetAPNSSandboxChannelResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetAPNSSandboxChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetAPNSSandboxChannel where

instance NFData GetAPNSSandboxChannel where

instance ToHeaders GetAPNSSandboxChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAPNSSandboxChannel where
        toPath GetAPNSSandboxChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _gascApplicationId,
               "/channels/apns_sandbox"]

instance ToQuery GetAPNSSandboxChannel where
        toQuery = const mempty

-- | /See:/ 'getAPNSSandboxChannelResponse' smart constructor.
data GetAPNSSandboxChannelResponse = GetAPNSSandboxChannelResponse'
  { _gascrsResponseStatus             :: !Int
  , _gascrsAPNSSandboxChannelResponse :: !APNSSandboxChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gascrsResponseStatus' - -- | The response status code.
--
-- * 'gascrsAPNSSandboxChannelResponse' - Undocumented member.
getAPNSSandboxChannelResponse
    :: Int -- ^ 'gascrsResponseStatus'
    -> APNSSandboxChannelResponse -- ^ 'gascrsAPNSSandboxChannelResponse'
    -> GetAPNSSandboxChannelResponse
getAPNSSandboxChannelResponse pResponseStatus_ pAPNSSandboxChannelResponse_ =
  GetAPNSSandboxChannelResponse'
    { _gascrsResponseStatus = pResponseStatus_
    , _gascrsAPNSSandboxChannelResponse = pAPNSSandboxChannelResponse_
    }


-- | -- | The response status code.
gascrsResponseStatus :: Lens' GetAPNSSandboxChannelResponse Int
gascrsResponseStatus = lens _gascrsResponseStatus (\ s a -> s{_gascrsResponseStatus = a})

-- | Undocumented member.
gascrsAPNSSandboxChannelResponse :: Lens' GetAPNSSandboxChannelResponse APNSSandboxChannelResponse
gascrsAPNSSandboxChannelResponse = lens _gascrsAPNSSandboxChannelResponse (\ s a -> s{_gascrsAPNSSandboxChannelResponse = a})

instance NFData GetAPNSSandboxChannelResponse where
