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
-- Module      : Network.AWS.Pinpoint.UpdateAPNSSandboxChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the APNs sandbox channel for an application or updates the status and settings of the APNs sandbox channel for an application.
module Network.AWS.Pinpoint.UpdateAPNSSandboxChannel
  ( -- * Creating a Request
    updateAPNSSandboxChannel,
    UpdateAPNSSandboxChannel,

    -- * Request Lenses
    uascApplicationId,
    uascAPNSSandboxChannelRequest,

    -- * Destructuring the Response
    updateAPNSSandboxChannelResponse,
    UpdateAPNSSandboxChannelResponse,

    -- * Response Lenses
    uascrsResponseStatus,
    uascrsAPNSSandboxChannelResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAPNSSandboxChannel' smart constructor.
data UpdateAPNSSandboxChannel = UpdateAPNSSandboxChannel'
  { _uascApplicationId ::
      !Text,
    _uascAPNSSandboxChannelRequest ::
      !APNSSandboxChannelRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAPNSSandboxChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uascApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'uascAPNSSandboxChannelRequest' - Undocumented member.
updateAPNSSandboxChannel ::
  -- | 'uascApplicationId'
  Text ->
  -- | 'uascAPNSSandboxChannelRequest'
  APNSSandboxChannelRequest ->
  UpdateAPNSSandboxChannel
updateAPNSSandboxChannel
  pApplicationId_
  pAPNSSandboxChannelRequest_ =
    UpdateAPNSSandboxChannel'
      { _uascApplicationId = pApplicationId_,
        _uascAPNSSandboxChannelRequest = pAPNSSandboxChannelRequest_
      }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
uascApplicationId :: Lens' UpdateAPNSSandboxChannel Text
uascApplicationId = lens _uascApplicationId (\s a -> s {_uascApplicationId = a})

-- | Undocumented member.
uascAPNSSandboxChannelRequest :: Lens' UpdateAPNSSandboxChannel APNSSandboxChannelRequest
uascAPNSSandboxChannelRequest = lens _uascAPNSSandboxChannelRequest (\s a -> s {_uascAPNSSandboxChannelRequest = a})

instance AWSRequest UpdateAPNSSandboxChannel where
  type Rs UpdateAPNSSandboxChannel = UpdateAPNSSandboxChannelResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateAPNSSandboxChannelResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateAPNSSandboxChannel

instance NFData UpdateAPNSSandboxChannel

instance ToHeaders UpdateAPNSSandboxChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateAPNSSandboxChannel where
  toJSON UpdateAPNSSandboxChannel' {..} =
    object
      ( catMaybes
          [ Just
              ("APNSSandboxChannelRequest" .= _uascAPNSSandboxChannelRequest)
          ]
      )

instance ToPath UpdateAPNSSandboxChannel where
  toPath UpdateAPNSSandboxChannel' {..} =
    mconcat
      ["/v1/apps/", toBS _uascApplicationId, "/channels/apns_sandbox"]

instance ToQuery UpdateAPNSSandboxChannel where
  toQuery = const mempty

-- | /See:/ 'updateAPNSSandboxChannelResponse' smart constructor.
data UpdateAPNSSandboxChannelResponse = UpdateAPNSSandboxChannelResponse'
  { _uascrsResponseStatus ::
      !Int,
    _uascrsAPNSSandboxChannelResponse ::
      !APNSSandboxChannelResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateAPNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uascrsResponseStatus' - -- | The response status code.
--
-- * 'uascrsAPNSSandboxChannelResponse' - Undocumented member.
updateAPNSSandboxChannelResponse ::
  -- | 'uascrsResponseStatus'
  Int ->
  -- | 'uascrsAPNSSandboxChannelResponse'
  APNSSandboxChannelResponse ->
  UpdateAPNSSandboxChannelResponse
updateAPNSSandboxChannelResponse
  pResponseStatus_
  pAPNSSandboxChannelResponse_ =
    UpdateAPNSSandboxChannelResponse'
      { _uascrsResponseStatus =
          pResponseStatus_,
        _uascrsAPNSSandboxChannelResponse =
          pAPNSSandboxChannelResponse_
      }

-- | -- | The response status code.
uascrsResponseStatus :: Lens' UpdateAPNSSandboxChannelResponse Int
uascrsResponseStatus = lens _uascrsResponseStatus (\s a -> s {_uascrsResponseStatus = a})

-- | Undocumented member.
uascrsAPNSSandboxChannelResponse :: Lens' UpdateAPNSSandboxChannelResponse APNSSandboxChannelResponse
uascrsAPNSSandboxChannelResponse = lens _uascrsAPNSSandboxChannelResponse (\s a -> s {_uascrsAPNSSandboxChannelResponse = a})

instance NFData UpdateAPNSSandboxChannelResponse
