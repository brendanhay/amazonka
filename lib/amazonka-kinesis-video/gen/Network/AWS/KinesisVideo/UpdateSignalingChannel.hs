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
-- Module      : Network.AWS.KinesisVideo.UpdateSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the existing signaling channel. This is an asynchronous operation and takes time to complete.
--
--
-- If the @MessageTtlSeconds@ value is updated (either increased or reduced), it only applies to new messages sent via this channel after it's been updated. Existing messages are still expired as per the previous @MessageTtlSeconds@ value.
module Network.AWS.KinesisVideo.UpdateSignalingChannel
  ( -- * Creating a Request
    updateSignalingChannel,
    UpdateSignalingChannel,

    -- * Request Lenses
    uscSingleMasterConfiguration,
    uscChannelARN,
    uscCurrentVersion,

    -- * Destructuring the Response
    updateSignalingChannelResponse,
    UpdateSignalingChannelResponse,

    -- * Response Lenses
    uscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSignalingChannel' smart constructor.
data UpdateSignalingChannel = UpdateSignalingChannel'
  { _uscSingleMasterConfiguration ::
      !(Maybe SingleMasterConfiguration),
    _uscChannelARN :: !Text,
    _uscCurrentVersion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSignalingChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscSingleMasterConfiguration' - The structure containing the configuration for the @SINGLE_MASTER@ type of the signaling channel that you want to update.
--
-- * 'uscChannelARN' - The Amazon Resource Name (ARN) of the signaling channel that you want to update.
--
-- * 'uscCurrentVersion' - The current version of the signaling channel that you want to update.
updateSignalingChannel ::
  -- | 'uscChannelARN'
  Text ->
  -- | 'uscCurrentVersion'
  Text ->
  UpdateSignalingChannel
updateSignalingChannel pChannelARN_ pCurrentVersion_ =
  UpdateSignalingChannel'
    { _uscSingleMasterConfiguration = Nothing,
      _uscChannelARN = pChannelARN_,
      _uscCurrentVersion = pCurrentVersion_
    }

-- | The structure containing the configuration for the @SINGLE_MASTER@ type of the signaling channel that you want to update.
uscSingleMasterConfiguration :: Lens' UpdateSignalingChannel (Maybe SingleMasterConfiguration)
uscSingleMasterConfiguration = lens _uscSingleMasterConfiguration (\s a -> s {_uscSingleMasterConfiguration = a})

-- | The Amazon Resource Name (ARN) of the signaling channel that you want to update.
uscChannelARN :: Lens' UpdateSignalingChannel Text
uscChannelARN = lens _uscChannelARN (\s a -> s {_uscChannelARN = a})

-- | The current version of the signaling channel that you want to update.
uscCurrentVersion :: Lens' UpdateSignalingChannel Text
uscCurrentVersion = lens _uscCurrentVersion (\s a -> s {_uscCurrentVersion = a})

instance AWSRequest UpdateSignalingChannel where
  type Rs UpdateSignalingChannel = UpdateSignalingChannelResponse
  request = postJSON kinesisVideo
  response =
    receiveEmpty
      ( \s h x ->
          UpdateSignalingChannelResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateSignalingChannel

instance NFData UpdateSignalingChannel

instance ToHeaders UpdateSignalingChannel where
  toHeaders = const mempty

instance ToJSON UpdateSignalingChannel where
  toJSON UpdateSignalingChannel' {..} =
    object
      ( catMaybes
          [ ("SingleMasterConfiguration" .=)
              <$> _uscSingleMasterConfiguration,
            Just ("ChannelARN" .= _uscChannelARN),
            Just ("CurrentVersion" .= _uscCurrentVersion)
          ]
      )

instance ToPath UpdateSignalingChannel where
  toPath = const "/updateSignalingChannel"

instance ToQuery UpdateSignalingChannel where
  toQuery = const mempty

-- | /See:/ 'updateSignalingChannelResponse' smart constructor.
newtype UpdateSignalingChannelResponse = UpdateSignalingChannelResponse'
  { _uscrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSignalingChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscrsResponseStatus' - -- | The response status code.
updateSignalingChannelResponse ::
  -- | 'uscrsResponseStatus'
  Int ->
  UpdateSignalingChannelResponse
updateSignalingChannelResponse pResponseStatus_ =
  UpdateSignalingChannelResponse'
    { _uscrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uscrsResponseStatus :: Lens' UpdateSignalingChannelResponse Int
uscrsResponseStatus = lens _uscrsResponseStatus (\s a -> s {_uscrsResponseStatus = a})

instance NFData UpdateSignalingChannelResponse
