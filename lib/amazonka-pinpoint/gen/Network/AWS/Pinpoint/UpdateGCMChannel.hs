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
-- Module      : Network.AWS.Pinpoint.UpdateGCMChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the GCM channel for an application or updates the status and settings of the GCM channel for an application.
module Network.AWS.Pinpoint.UpdateGCMChannel
  ( -- * Creating a Request
    updateGCMChannel,
    UpdateGCMChannel,

    -- * Request Lenses
    ugcApplicationId,
    ugcGCMChannelRequest,

    -- * Destructuring the Response
    updateGCMChannelResponse,
    UpdateGCMChannelResponse,

    -- * Response Lenses
    ugcrsResponseStatus,
    ugcrsGCMChannelResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateGCMChannel' smart constructor.
data UpdateGCMChannel = UpdateGCMChannel'
  { _ugcApplicationId ::
      !Text,
    _ugcGCMChannelRequest :: !GCMChannelRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGCMChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugcApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- * 'ugcGCMChannelRequest' - Undocumented member.
updateGCMChannel ::
  -- | 'ugcApplicationId'
  Text ->
  -- | 'ugcGCMChannelRequest'
  GCMChannelRequest ->
  UpdateGCMChannel
updateGCMChannel pApplicationId_ pGCMChannelRequest_ =
  UpdateGCMChannel'
    { _ugcApplicationId = pApplicationId_,
      _ugcGCMChannelRequest = pGCMChannelRequest_
    }

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
ugcApplicationId :: Lens' UpdateGCMChannel Text
ugcApplicationId = lens _ugcApplicationId (\s a -> s {_ugcApplicationId = a})

-- | Undocumented member.
ugcGCMChannelRequest :: Lens' UpdateGCMChannel GCMChannelRequest
ugcGCMChannelRequest = lens _ugcGCMChannelRequest (\s a -> s {_ugcGCMChannelRequest = a})

instance AWSRequest UpdateGCMChannel where
  type Rs UpdateGCMChannel = UpdateGCMChannelResponse
  request = putJSON pinpoint
  response =
    receiveJSON
      ( \s h x ->
          UpdateGCMChannelResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable UpdateGCMChannel

instance NFData UpdateGCMChannel

instance ToHeaders UpdateGCMChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateGCMChannel where
  toJSON UpdateGCMChannel' {..} =
    object
      (catMaybes [Just ("GCMChannelRequest" .= _ugcGCMChannelRequest)])

instance ToPath UpdateGCMChannel where
  toPath UpdateGCMChannel' {..} =
    mconcat ["/v1/apps/", toBS _ugcApplicationId, "/channels/gcm"]

instance ToQuery UpdateGCMChannel where
  toQuery = const mempty

-- | /See:/ 'updateGCMChannelResponse' smart constructor.
data UpdateGCMChannelResponse = UpdateGCMChannelResponse'
  { _ugcrsResponseStatus ::
      !Int,
    _ugcrsGCMChannelResponse ::
      !GCMChannelResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateGCMChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugcrsResponseStatus' - -- | The response status code.
--
-- * 'ugcrsGCMChannelResponse' - Undocumented member.
updateGCMChannelResponse ::
  -- | 'ugcrsResponseStatus'
  Int ->
  -- | 'ugcrsGCMChannelResponse'
  GCMChannelResponse ->
  UpdateGCMChannelResponse
updateGCMChannelResponse pResponseStatus_ pGCMChannelResponse_ =
  UpdateGCMChannelResponse'
    { _ugcrsResponseStatus =
        pResponseStatus_,
      _ugcrsGCMChannelResponse = pGCMChannelResponse_
    }

-- | -- | The response status code.
ugcrsResponseStatus :: Lens' UpdateGCMChannelResponse Int
ugcrsResponseStatus = lens _ugcrsResponseStatus (\s a -> s {_ugcrsResponseStatus = a})

-- | Undocumented member.
ugcrsGCMChannelResponse :: Lens' UpdateGCMChannelResponse GCMChannelResponse
ugcrsGCMChannelResponse = lens _ugcrsGCMChannelResponse (\s a -> s {_ugcrsGCMChannelResponse = a})

instance NFData UpdateGCMChannelResponse
