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
-- Module      : Network.AWS.Pinpoint.GetAPNSChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the APNs channel for an app.
module Network.AWS.Pinpoint.GetAPNSChannel
    (
    -- * Creating a Request
      getAPNSChannel
    , GetAPNSChannel
    -- * Request Lenses
    , gacApplicationId

    -- * Destructuring the Response
    , getAPNSChannelResponse
    , GetAPNSChannelResponse
    -- * Response Lenses
    , gacrsResponseStatus
    , gacrsAPNSChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPNSChannel' smart constructor.
newtype GetAPNSChannel = GetAPNSChannel'
  { _gacApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPNSChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gacApplicationId' - Undocumented member.
getAPNSChannel
    :: Text -- ^ 'gacApplicationId'
    -> GetAPNSChannel
getAPNSChannel pApplicationId_ =
  GetAPNSChannel' {_gacApplicationId = pApplicationId_}


-- | Undocumented member.
gacApplicationId :: Lens' GetAPNSChannel Text
gacApplicationId = lens _gacApplicationId (\ s a -> s{_gacApplicationId = a})

instance AWSRequest GetAPNSChannel where
        type Rs GetAPNSChannel = GetAPNSChannelResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetAPNSChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetAPNSChannel where

instance NFData GetAPNSChannel where

instance ToHeaders GetAPNSChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAPNSChannel where
        toPath GetAPNSChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _gacApplicationId,
               "/channels/apns"]

instance ToQuery GetAPNSChannel where
        toQuery = const mempty

-- | /See:/ 'getAPNSChannelResponse' smart constructor.
data GetAPNSChannelResponse = GetAPNSChannelResponse'
  { _gacrsResponseStatus      :: !Int
  , _gacrsAPNSChannelResponse :: !APNSChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAPNSChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gacrsResponseStatus' - -- | The response status code.
--
-- * 'gacrsAPNSChannelResponse' - Undocumented member.
getAPNSChannelResponse
    :: Int -- ^ 'gacrsResponseStatus'
    -> APNSChannelResponse -- ^ 'gacrsAPNSChannelResponse'
    -> GetAPNSChannelResponse
getAPNSChannelResponse pResponseStatus_ pAPNSChannelResponse_ =
  GetAPNSChannelResponse'
    { _gacrsResponseStatus = pResponseStatus_
    , _gacrsAPNSChannelResponse = pAPNSChannelResponse_
    }


-- | -- | The response status code.
gacrsResponseStatus :: Lens' GetAPNSChannelResponse Int
gacrsResponseStatus = lens _gacrsResponseStatus (\ s a -> s{_gacrsResponseStatus = a})

-- | Undocumented member.
gacrsAPNSChannelResponse :: Lens' GetAPNSChannelResponse APNSChannelResponse
gacrsAPNSChannelResponse = lens _gacrsAPNSChannelResponse (\ s a -> s{_gacrsAPNSChannelResponse = a})

instance NFData GetAPNSChannelResponse where
