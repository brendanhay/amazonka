{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetVoiceChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the voice channel for an application.
module Network.AWS.Pinpoint.GetVoiceChannel
  ( -- * Creating a Request
    getVoiceChannel,
    GetVoiceChannel,

    -- * Request Lenses
    gvcApplicationId,

    -- * Destructuring the Response
    getVoiceChannelResponse,
    GetVoiceChannelResponse,

    -- * Response Lenses
    gvcrsResponseStatus,
    gvcrsVoiceChannelResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getVoiceChannel' smart constructor.
newtype GetVoiceChannel = GetVoiceChannel'
  { _gvcApplicationId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetVoiceChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvcApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getVoiceChannel ::
  -- | 'gvcApplicationId'
  Text ->
  GetVoiceChannel
getVoiceChannel pApplicationId_ =
  GetVoiceChannel' {_gvcApplicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gvcApplicationId :: Lens' GetVoiceChannel Text
gvcApplicationId = lens _gvcApplicationId (\s a -> s {_gvcApplicationId = a})

instance AWSRequest GetVoiceChannel where
  type Rs GetVoiceChannel = GetVoiceChannelResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetVoiceChannelResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetVoiceChannel

instance NFData GetVoiceChannel

instance ToHeaders GetVoiceChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetVoiceChannel where
  toPath GetVoiceChannel' {..} =
    mconcat ["/v1/apps/", toBS _gvcApplicationId, "/channels/voice"]

instance ToQuery GetVoiceChannel where
  toQuery = const mempty

-- | /See:/ 'getVoiceChannelResponse' smart constructor.
data GetVoiceChannelResponse = GetVoiceChannelResponse'
  { _gvcrsResponseStatus ::
      !Int,
    _gvcrsVoiceChannelResponse ::
      !VoiceChannelResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetVoiceChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvcrsResponseStatus' - -- | The response status code.
--
-- * 'gvcrsVoiceChannelResponse' - Undocumented member.
getVoiceChannelResponse ::
  -- | 'gvcrsResponseStatus'
  Int ->
  -- | 'gvcrsVoiceChannelResponse'
  VoiceChannelResponse ->
  GetVoiceChannelResponse
getVoiceChannelResponse pResponseStatus_ pVoiceChannelResponse_ =
  GetVoiceChannelResponse'
    { _gvcrsResponseStatus = pResponseStatus_,
      _gvcrsVoiceChannelResponse = pVoiceChannelResponse_
    }

-- | -- | The response status code.
gvcrsResponseStatus :: Lens' GetVoiceChannelResponse Int
gvcrsResponseStatus = lens _gvcrsResponseStatus (\s a -> s {_gvcrsResponseStatus = a})

-- | Undocumented member.
gvcrsVoiceChannelResponse :: Lens' GetVoiceChannelResponse VoiceChannelResponse
gvcrsVoiceChannelResponse = lens _gvcrsVoiceChannelResponse (\s a -> s {_gvcrsVoiceChannelResponse = a})

instance NFData GetVoiceChannelResponse
