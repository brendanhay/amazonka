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
-- Module      : Network.AWS.IoTAnalytics.CreateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a channel. A channel collects data from an MQTT topic and archives the raw, unprocessed messages before publishing the data to a pipeline.
module Network.AWS.IoTAnalytics.CreateChannel
  ( -- * Creating a Request
    createChannel,
    CreateChannel,

    -- * Request Lenses
    ccRetentionPeriod,
    ccChannelStorage,
    ccTags,
    ccChannelName,

    -- * Destructuring the Response
    createChannelResponse,
    CreateChannelResponse,

    -- * Response Lenses
    ccrsChannelARN,
    ccrsRetentionPeriod,
    ccrsChannelName,
    ccrsResponseStatus,
  )
where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createChannel' smart constructor.
data CreateChannel = CreateChannel'
  { _ccRetentionPeriod ::
      !(Maybe RetentionPeriod),
    _ccChannelStorage :: !(Maybe ChannelStorage),
    _ccTags :: !(Maybe (List1 Tag)),
    _ccChannelName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccRetentionPeriod' - How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
--
-- * 'ccChannelStorage' - Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
--
-- * 'ccTags' - Metadata which can be used to manage the channel.
--
-- * 'ccChannelName' - The name of the channel.
createChannel ::
  -- | 'ccChannelName'
  Text ->
  CreateChannel
createChannel pChannelName_ =
  CreateChannel'
    { _ccRetentionPeriod = Nothing,
      _ccChannelStorage = Nothing,
      _ccTags = Nothing,
      _ccChannelName = pChannelName_
    }

-- | How long, in days, message data is kept for the channel. When @customerManagedS3@ storage is selected, this parameter is ignored.
ccRetentionPeriod :: Lens' CreateChannel (Maybe RetentionPeriod)
ccRetentionPeriod = lens _ccRetentionPeriod (\s a -> s {_ccRetentionPeriod = a})

-- | Where channel data is stored. You can choose one of @serviceManagedS3@ or @customerManagedS3@ storage. If not specified, the default is @serviceManagedS3@ . You cannot change this storage option after the channel is created.
ccChannelStorage :: Lens' CreateChannel (Maybe ChannelStorage)
ccChannelStorage = lens _ccChannelStorage (\s a -> s {_ccChannelStorage = a})

-- | Metadata which can be used to manage the channel.
ccTags :: Lens' CreateChannel (Maybe (NonEmpty Tag))
ccTags = lens _ccTags (\s a -> s {_ccTags = a}) . mapping _List1

-- | The name of the channel.
ccChannelName :: Lens' CreateChannel Text
ccChannelName = lens _ccChannelName (\s a -> s {_ccChannelName = a})

instance AWSRequest CreateChannel where
  type Rs CreateChannel = CreateChannelResponse
  request = postJSON ioTAnalytics
  response =
    receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            <$> (x .?> "channelArn")
            <*> (x .?> "retentionPeriod")
            <*> (x .?> "channelName")
            <*> (pure (fromEnum s))
      )

instance Hashable CreateChannel

instance NFData CreateChannel

instance ToHeaders CreateChannel where
  toHeaders = const mempty

instance ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    object
      ( catMaybes
          [ ("retentionPeriod" .=) <$> _ccRetentionPeriod,
            ("channelStorage" .=) <$> _ccChannelStorage,
            ("tags" .=) <$> _ccTags,
            Just ("channelName" .= _ccChannelName)
          ]
      )

instance ToPath CreateChannel where
  toPath = const "/channels"

instance ToQuery CreateChannel where
  toQuery = const mempty

-- | /See:/ 'createChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { _ccrsChannelARN ::
      !(Maybe Text),
    _ccrsRetentionPeriod ::
      !(Maybe RetentionPeriod),
    _ccrsChannelName :: !(Maybe Text),
    _ccrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsChannelARN' - The ARN of the channel.
--
-- * 'ccrsRetentionPeriod' - How long, in days, message data is kept for the channel.
--
-- * 'ccrsChannelName' - The name of the channel.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createChannelResponse ::
  -- | 'ccrsResponseStatus'
  Int ->
  CreateChannelResponse
createChannelResponse pResponseStatus_ =
  CreateChannelResponse'
    { _ccrsChannelARN = Nothing,
      _ccrsRetentionPeriod = Nothing,
      _ccrsChannelName = Nothing,
      _ccrsResponseStatus = pResponseStatus_
    }

-- | The ARN of the channel.
ccrsChannelARN :: Lens' CreateChannelResponse (Maybe Text)
ccrsChannelARN = lens _ccrsChannelARN (\s a -> s {_ccrsChannelARN = a})

-- | How long, in days, message data is kept for the channel.
ccrsRetentionPeriod :: Lens' CreateChannelResponse (Maybe RetentionPeriod)
ccrsRetentionPeriod = lens _ccrsRetentionPeriod (\s a -> s {_ccrsRetentionPeriod = a})

-- | The name of the channel.
ccrsChannelName :: Lens' CreateChannelResponse (Maybe Text)
ccrsChannelName = lens _ccrsChannelName (\s a -> s {_ccrsChannelName = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateChannelResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\s a -> s {_ccrsResponseStatus = a})

instance NFData CreateChannelResponse
