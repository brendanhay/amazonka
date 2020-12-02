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
-- Module      : Network.AWS.MediaLive.UpdateChannelClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the class of the channel.
module Network.AWS.MediaLive.UpdateChannelClass
  ( -- * Creating a Request
    updateChannelClass,
    UpdateChannelClass,

    -- * Request Lenses
    uccDestinations,
    uccChannelId,
    uccChannelClass,

    -- * Destructuring the Response
    updateChannelClassResponse,
    UpdateChannelClassResponse,

    -- * Response Lenses
    uccrsChannel,
    uccrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Channel class that the channel should be updated to.
--
-- /See:/ 'updateChannelClass' smart constructor.
data UpdateChannelClass = UpdateChannelClass'
  { _uccDestinations ::
      !(Maybe [OutputDestination]),
    _uccChannelId :: !Text,
    _uccChannelClass :: !ChannelClass
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateChannelClass' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccDestinations' - A list of output destinations for this channel.
--
-- * 'uccChannelId' - Channel Id of the channel whose class should be updated.
--
-- * 'uccChannelClass' - The channel class that you wish to update this channel to use.
updateChannelClass ::
  -- | 'uccChannelId'
  Text ->
  -- | 'uccChannelClass'
  ChannelClass ->
  UpdateChannelClass
updateChannelClass pChannelId_ pChannelClass_ =
  UpdateChannelClass'
    { _uccDestinations = Nothing,
      _uccChannelId = pChannelId_,
      _uccChannelClass = pChannelClass_
    }

-- | A list of output destinations for this channel.
uccDestinations :: Lens' UpdateChannelClass [OutputDestination]
uccDestinations = lens _uccDestinations (\s a -> s {_uccDestinations = a}) . _Default . _Coerce

-- | Channel Id of the channel whose class should be updated.
uccChannelId :: Lens' UpdateChannelClass Text
uccChannelId = lens _uccChannelId (\s a -> s {_uccChannelId = a})

-- | The channel class that you wish to update this channel to use.
uccChannelClass :: Lens' UpdateChannelClass ChannelClass
uccChannelClass = lens _uccChannelClass (\s a -> s {_uccChannelClass = a})

instance AWSRequest UpdateChannelClass where
  type Rs UpdateChannelClass = UpdateChannelClassResponse
  request = putJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          UpdateChannelClassResponse'
            <$> (x .?> "channel") <*> (pure (fromEnum s))
      )

instance Hashable UpdateChannelClass

instance NFData UpdateChannelClass

instance ToHeaders UpdateChannelClass where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateChannelClass where
  toJSON UpdateChannelClass' {..} =
    object
      ( catMaybes
          [ ("destinations" .=) <$> _uccDestinations,
            Just ("channelClass" .= _uccChannelClass)
          ]
      )

instance ToPath UpdateChannelClass where
  toPath UpdateChannelClass' {..} =
    mconcat ["/prod/channels/", toBS _uccChannelId, "/channelClass"]

instance ToQuery UpdateChannelClass where
  toQuery = const mempty

-- | Placeholder documentation for UpdateChannelClassResponse
--
-- /See:/ 'updateChannelClassResponse' smart constructor.
data UpdateChannelClassResponse = UpdateChannelClassResponse'
  { _uccrsChannel ::
      !(Maybe Channel),
    _uccrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateChannelClassResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uccrsChannel' - Undocumented member.
--
-- * 'uccrsResponseStatus' - -- | The response status code.
updateChannelClassResponse ::
  -- | 'uccrsResponseStatus'
  Int ->
  UpdateChannelClassResponse
updateChannelClassResponse pResponseStatus_ =
  UpdateChannelClassResponse'
    { _uccrsChannel = Nothing,
      _uccrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
uccrsChannel :: Lens' UpdateChannelClassResponse (Maybe Channel)
uccrsChannel = lens _uccrsChannel (\s a -> s {_uccrsChannel = a})

-- | -- | The response status code.
uccrsResponseStatus :: Lens' UpdateChannelClassResponse Int
uccrsResponseStatus = lens _uccrsResponseStatus (\s a -> s {_uccrsResponseStatus = a})

instance NFData UpdateChannelClassResponse
