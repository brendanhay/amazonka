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
-- Module      : Network.AWS.KinesisVideo.CreateSignalingChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signaling channel.
--
--
-- @CreateSignalingChannel@ is an asynchronous operation.
module Network.AWS.KinesisVideo.CreateSignalingChannel
  ( -- * Creating a Request
    createSignalingChannel,
    CreateSignalingChannel,

    -- * Request Lenses
    cscSingleMasterConfiguration,
    cscChannelType,
    cscTags,
    cscChannelName,

    -- * Destructuring the Response
    createSignalingChannelResponse,
    CreateSignalingChannelResponse,

    -- * Response Lenses
    cscrsChannelARN,
    cscrsResponseStatus,
  )
where

import Network.AWS.KinesisVideo.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createSignalingChannel' smart constructor.
data CreateSignalingChannel = CreateSignalingChannel'
  { _cscSingleMasterConfiguration ::
      !(Maybe SingleMasterConfiguration),
    _cscChannelType :: !(Maybe ChannelType),
    _cscTags :: !(Maybe [Tag]),
    _cscChannelName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSignalingChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscSingleMasterConfiguration' - A structure containing the configuration for the @SINGLE_MASTER@ channel type.
--
-- * 'cscChannelType' - A type of the signaling channel that you are creating. Currently, @SINGLE_MASTER@ is the only supported channel type.
--
-- * 'cscTags' - A set of tags (key-value pairs) that you want to associate with this channel.
--
-- * 'cscChannelName' - A name for the signaling channel that you are creating. It must be unique for each AWS account and AWS Region.
createSignalingChannel ::
  -- | 'cscChannelName'
  Text ->
  CreateSignalingChannel
createSignalingChannel pChannelName_ =
  CreateSignalingChannel'
    { _cscSingleMasterConfiguration = Nothing,
      _cscChannelType = Nothing,
      _cscTags = Nothing,
      _cscChannelName = pChannelName_
    }

-- | A structure containing the configuration for the @SINGLE_MASTER@ channel type.
cscSingleMasterConfiguration :: Lens' CreateSignalingChannel (Maybe SingleMasterConfiguration)
cscSingleMasterConfiguration = lens _cscSingleMasterConfiguration (\s a -> s {_cscSingleMasterConfiguration = a})

-- | A type of the signaling channel that you are creating. Currently, @SINGLE_MASTER@ is the only supported channel type.
cscChannelType :: Lens' CreateSignalingChannel (Maybe ChannelType)
cscChannelType = lens _cscChannelType (\s a -> s {_cscChannelType = a})

-- | A set of tags (key-value pairs) that you want to associate with this channel.
cscTags :: Lens' CreateSignalingChannel [Tag]
cscTags = lens _cscTags (\s a -> s {_cscTags = a}) . _Default . _Coerce

-- | A name for the signaling channel that you are creating. It must be unique for each AWS account and AWS Region.
cscChannelName :: Lens' CreateSignalingChannel Text
cscChannelName = lens _cscChannelName (\s a -> s {_cscChannelName = a})

instance AWSRequest CreateSignalingChannel where
  type Rs CreateSignalingChannel = CreateSignalingChannelResponse
  request = postJSON kinesisVideo
  response =
    receiveJSON
      ( \s h x ->
          CreateSignalingChannelResponse'
            <$> (x .?> "ChannelARN") <*> (pure (fromEnum s))
      )

instance Hashable CreateSignalingChannel

instance NFData CreateSignalingChannel

instance ToHeaders CreateSignalingChannel where
  toHeaders = const mempty

instance ToJSON CreateSignalingChannel where
  toJSON CreateSignalingChannel' {..} =
    object
      ( catMaybes
          [ ("SingleMasterConfiguration" .=)
              <$> _cscSingleMasterConfiguration,
            ("ChannelType" .=) <$> _cscChannelType,
            ("Tags" .=) <$> _cscTags,
            Just ("ChannelName" .= _cscChannelName)
          ]
      )

instance ToPath CreateSignalingChannel where
  toPath = const "/createSignalingChannel"

instance ToQuery CreateSignalingChannel where
  toQuery = const mempty

-- | /See:/ 'createSignalingChannelResponse' smart constructor.
data CreateSignalingChannelResponse = CreateSignalingChannelResponse'
  { _cscrsChannelARN ::
      !(Maybe Text),
    _cscrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateSignalingChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscrsChannelARN' - The Amazon Resource Name (ARN) of the created channel.
--
-- * 'cscrsResponseStatus' - -- | The response status code.
createSignalingChannelResponse ::
  -- | 'cscrsResponseStatus'
  Int ->
  CreateSignalingChannelResponse
createSignalingChannelResponse pResponseStatus_ =
  CreateSignalingChannelResponse'
    { _cscrsChannelARN = Nothing,
      _cscrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the created channel.
cscrsChannelARN :: Lens' CreateSignalingChannelResponse (Maybe Text)
cscrsChannelARN = lens _cscrsChannelARN (\s a -> s {_cscrsChannelARN = a})

-- | -- | The response status code.
cscrsResponseStatus :: Lens' CreateSignalingChannelResponse Int
cscrsResponseStatus = lens _cscrsResponseStatus (\s a -> s {_cscrsResponseStatus = a})

instance NFData CreateSignalingChannelResponse
