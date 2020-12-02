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
-- Module      : Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status and settings of the APNs VoIP sandbox channel for an application.
module Network.AWS.Pinpoint.GetAPNSVoipSandboxChannel
  ( -- * Creating a Request
    getAPNSVoipSandboxChannel,
    GetAPNSVoipSandboxChannel,

    -- * Request Lenses
    gavscApplicationId,

    -- * Destructuring the Response
    getAPNSVoipSandboxChannelResponse,
    GetAPNSVoipSandboxChannelResponse,

    -- * Response Lenses
    gavscrsResponseStatus,
    gavscrsAPNSVoipSandboxChannelResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAPNSVoipSandboxChannel' smart constructor.
newtype GetAPNSVoipSandboxChannel = GetAPNSVoipSandboxChannel'
  { _gavscApplicationId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAPNSVoipSandboxChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gavscApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getAPNSVoipSandboxChannel ::
  -- | 'gavscApplicationId'
  Text ->
  GetAPNSVoipSandboxChannel
getAPNSVoipSandboxChannel pApplicationId_ =
  GetAPNSVoipSandboxChannel' {_gavscApplicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gavscApplicationId :: Lens' GetAPNSVoipSandboxChannel Text
gavscApplicationId = lens _gavscApplicationId (\s a -> s {_gavscApplicationId = a})

instance AWSRequest GetAPNSVoipSandboxChannel where
  type
    Rs GetAPNSVoipSandboxChannel =
      GetAPNSVoipSandboxChannelResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetAPNSVoipSandboxChannelResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetAPNSVoipSandboxChannel

instance NFData GetAPNSVoipSandboxChannel

instance ToHeaders GetAPNSVoipSandboxChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetAPNSVoipSandboxChannel where
  toPath GetAPNSVoipSandboxChannel' {..} =
    mconcat
      [ "/v1/apps/",
        toBS _gavscApplicationId,
        "/channels/apns_voip_sandbox"
      ]

instance ToQuery GetAPNSVoipSandboxChannel where
  toQuery = const mempty

-- | /See:/ 'getAPNSVoipSandboxChannelResponse' smart constructor.
data GetAPNSVoipSandboxChannelResponse = GetAPNSVoipSandboxChannelResponse'
  { _gavscrsResponseStatus ::
      !Int,
    _gavscrsAPNSVoipSandboxChannelResponse ::
      !APNSVoipSandboxChannelResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetAPNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gavscrsResponseStatus' - -- | The response status code.
--
-- * 'gavscrsAPNSVoipSandboxChannelResponse' - Undocumented member.
getAPNSVoipSandboxChannelResponse ::
  -- | 'gavscrsResponseStatus'
  Int ->
  -- | 'gavscrsAPNSVoipSandboxChannelResponse'
  APNSVoipSandboxChannelResponse ->
  GetAPNSVoipSandboxChannelResponse
getAPNSVoipSandboxChannelResponse
  pResponseStatus_
  pAPNSVoipSandboxChannelResponse_ =
    GetAPNSVoipSandboxChannelResponse'
      { _gavscrsResponseStatus =
          pResponseStatus_,
        _gavscrsAPNSVoipSandboxChannelResponse =
          pAPNSVoipSandboxChannelResponse_
      }

-- | -- | The response status code.
gavscrsResponseStatus :: Lens' GetAPNSVoipSandboxChannelResponse Int
gavscrsResponseStatus = lens _gavscrsResponseStatus (\s a -> s {_gavscrsResponseStatus = a})

-- | Undocumented member.
gavscrsAPNSVoipSandboxChannelResponse :: Lens' GetAPNSVoipSandboxChannelResponse APNSVoipSandboxChannelResponse
gavscrsAPNSVoipSandboxChannelResponse = lens _gavscrsAPNSVoipSandboxChannelResponse (\s a -> s {_gavscrsAPNSVoipSandboxChannelResponse = a})

instance NFData GetAPNSVoipSandboxChannelResponse
