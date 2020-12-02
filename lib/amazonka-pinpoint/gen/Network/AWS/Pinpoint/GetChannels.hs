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
-- Module      : Network.AWS.Pinpoint.GetChannels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the history and status of each channel for an application.
module Network.AWS.Pinpoint.GetChannels
  ( -- * Creating a Request
    getChannels,
    GetChannels,

    -- * Request Lenses
    gcsApplicationId,

    -- * Destructuring the Response
    getChannelsResponse,
    GetChannelsResponse,

    -- * Response Lenses
    gcsrsResponseStatus,
    gcsrsChannelsResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getChannels' smart constructor.
newtype GetChannels = GetChannels' {_gcsApplicationId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetChannels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsApplicationId' - The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
getChannels ::
  -- | 'gcsApplicationId'
  Text ->
  GetChannels
getChannels pApplicationId_ =
  GetChannels' {_gcsApplicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
gcsApplicationId :: Lens' GetChannels Text
gcsApplicationId = lens _gcsApplicationId (\s a -> s {_gcsApplicationId = a})

instance AWSRequest GetChannels where
  type Rs GetChannels = GetChannelsResponse
  request = get pinpoint
  response =
    receiveJSON
      ( \s h x ->
          GetChannelsResponse'
            <$> (pure (fromEnum s)) <*> (eitherParseJSON x)
      )

instance Hashable GetChannels

instance NFData GetChannels

instance ToHeaders GetChannels where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath GetChannels where
  toPath GetChannels' {..} =
    mconcat ["/v1/apps/", toBS _gcsApplicationId, "/channels"]

instance ToQuery GetChannels where
  toQuery = const mempty

-- | /See:/ 'getChannelsResponse' smart constructor.
data GetChannelsResponse = GetChannelsResponse'
  { _gcsrsResponseStatus ::
      !Int,
    _gcsrsChannelsResponse :: !ChannelsResponse
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetChannelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsrsResponseStatus' - -- | The response status code.
--
-- * 'gcsrsChannelsResponse' - Undocumented member.
getChannelsResponse ::
  -- | 'gcsrsResponseStatus'
  Int ->
  -- | 'gcsrsChannelsResponse'
  ChannelsResponse ->
  GetChannelsResponse
getChannelsResponse pResponseStatus_ pChannelsResponse_ =
  GetChannelsResponse'
    { _gcsrsResponseStatus = pResponseStatus_,
      _gcsrsChannelsResponse = pChannelsResponse_
    }

-- | -- | The response status code.
gcsrsResponseStatus :: Lens' GetChannelsResponse Int
gcsrsResponseStatus = lens _gcsrsResponseStatus (\s a -> s {_gcsrsResponseStatus = a})

-- | Undocumented member.
gcsrsChannelsResponse :: Lens' GetChannelsResponse ChannelsResponse
gcsrsChannelsResponse = lens _gcsrsChannelsResponse (\s a -> s {_gcsrsChannelsResponse = a})

instance NFData GetChannelsResponse
