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
-- Module      : Network.AWS.Pinpoint.GetADMChannel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an ADM channel
module Network.AWS.Pinpoint.GetADMChannel
    (
    -- * Creating a Request
      getADMChannel
    , GetADMChannel
    -- * Request Lenses
    , gadmcApplicationId

    -- * Destructuring the Response
    , getADMChannelResponse
    , GetADMChannelResponse
    -- * Response Lenses
    , gadmcrsResponseStatus
    , gadmcrsADMChannelResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getADMChannel' smart constructor.
newtype GetADMChannel = GetADMChannel'
  { _gadmcApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetADMChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gadmcApplicationId' - Undocumented member.
getADMChannel
    :: Text -- ^ 'gadmcApplicationId'
    -> GetADMChannel
getADMChannel pApplicationId_ =
  GetADMChannel' {_gadmcApplicationId = pApplicationId_}


-- | Undocumented member.
gadmcApplicationId :: Lens' GetADMChannel Text
gadmcApplicationId = lens _gadmcApplicationId (\ s a -> s{_gadmcApplicationId = a})

instance AWSRequest GetADMChannel where
        type Rs GetADMChannel = GetADMChannelResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetADMChannelResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetADMChannel where

instance NFData GetADMChannel where

instance ToHeaders GetADMChannel where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetADMChannel where
        toPath GetADMChannel'{..}
          = mconcat
              ["/v1/apps/", toBS _gadmcApplicationId,
               "/channels/adm"]

instance ToQuery GetADMChannel where
        toQuery = const mempty

-- | /See:/ 'getADMChannelResponse' smart constructor.
data GetADMChannelResponse = GetADMChannelResponse'
  { _gadmcrsResponseStatus     :: !Int
  , _gadmcrsADMChannelResponse :: !ADMChannelResponse
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetADMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gadmcrsResponseStatus' - -- | The response status code.
--
-- * 'gadmcrsADMChannelResponse' - Undocumented member.
getADMChannelResponse
    :: Int -- ^ 'gadmcrsResponseStatus'
    -> ADMChannelResponse -- ^ 'gadmcrsADMChannelResponse'
    -> GetADMChannelResponse
getADMChannelResponse pResponseStatus_ pADMChannelResponse_ =
  GetADMChannelResponse'
    { _gadmcrsResponseStatus = pResponseStatus_
    , _gadmcrsADMChannelResponse = pADMChannelResponse_
    }


-- | -- | The response status code.
gadmcrsResponseStatus :: Lens' GetADMChannelResponse Int
gadmcrsResponseStatus = lens _gadmcrsResponseStatus (\ s a -> s{_gadmcrsResponseStatus = a})

-- | Undocumented member.
gadmcrsADMChannelResponse :: Lens' GetADMChannelResponse ADMChannelResponse
gadmcrsADMChannelResponse = lens _gadmcrsADMChannelResponse (\ s a -> s{_gadmcrsADMChannelResponse = a})

instance NFData GetADMChannelResponse where
