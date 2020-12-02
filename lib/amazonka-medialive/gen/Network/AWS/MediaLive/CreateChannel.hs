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
-- Module      : Network.AWS.MediaLive.CreateChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new channel
module Network.AWS.MediaLive.CreateChannel
  ( -- * Creating a Request
    createChannel,
    CreateChannel,

    -- * Request Lenses
    ccRequestId,
    ccLogLevel,
    ccInputSpecification,
    ccInputAttachments,
    ccReserved,
    ccDestinations,
    ccName,
    ccCdiInputSpecification,
    ccChannelClass,
    ccTags,
    ccEncoderSettings,
    ccRoleARN,

    -- * Destructuring the Response
    createChannelResponse,
    CreateChannelResponse,

    -- * Response Lenses
    ccrsChannel,
    ccrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | A request to create a channel
--
-- /See:/ 'createChannel' smart constructor.
data CreateChannel = CreateChannel'
  { _ccRequestId :: !(Maybe Text),
    _ccLogLevel :: !(Maybe LogLevel),
    _ccInputSpecification :: !(Maybe InputSpecification),
    _ccInputAttachments :: !(Maybe [InputAttachment]),
    _ccReserved :: !(Maybe Text),
    _ccDestinations :: !(Maybe [OutputDestination]),
    _ccName :: !(Maybe Text),
    _ccCdiInputSpecification :: !(Maybe CdiInputSpecification),
    _ccChannelClass :: !(Maybe ChannelClass),
    _ccTags :: !(Maybe (Map Text (Text))),
    _ccEncoderSettings :: !(Maybe EncoderSettings),
    _ccRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccRequestId' - Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
--
-- * 'ccLogLevel' - The log level to write to CloudWatch Logs.
--
-- * 'ccInputSpecification' - Specification of network and file inputs for this channel
--
-- * 'ccInputAttachments' - List of input attachments for channel.
--
-- * 'ccReserved' - Deprecated field that's only usable by whitelisted customers.
--
-- * 'ccDestinations' - Undocumented member.
--
-- * 'ccName' - Name of channel.
--
-- * 'ccCdiInputSpecification' - Specification of CDI inputs for this channel
--
-- * 'ccChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'ccTags' - A collection of key-value pairs.
--
-- * 'ccEncoderSettings' - Undocumented member.
--
-- * 'ccRoleARN' - An optional Amazon Resource Name (ARN) of the role to assume when running the Channel.
createChannel ::
  CreateChannel
createChannel =
  CreateChannel'
    { _ccRequestId = Nothing,
      _ccLogLevel = Nothing,
      _ccInputSpecification = Nothing,
      _ccInputAttachments = Nothing,
      _ccReserved = Nothing,
      _ccDestinations = Nothing,
      _ccName = Nothing,
      _ccCdiInputSpecification = Nothing,
      _ccChannelClass = Nothing,
      _ccTags = Nothing,
      _ccEncoderSettings = Nothing,
      _ccRoleARN = Nothing
    }

-- | Unique request ID to be specified. This is needed to prevent retries from creating multiple resources.
ccRequestId :: Lens' CreateChannel (Maybe Text)
ccRequestId = lens _ccRequestId (\s a -> s {_ccRequestId = a})

-- | The log level to write to CloudWatch Logs.
ccLogLevel :: Lens' CreateChannel (Maybe LogLevel)
ccLogLevel = lens _ccLogLevel (\s a -> s {_ccLogLevel = a})

-- | Specification of network and file inputs for this channel
ccInputSpecification :: Lens' CreateChannel (Maybe InputSpecification)
ccInputSpecification = lens _ccInputSpecification (\s a -> s {_ccInputSpecification = a})

-- | List of input attachments for channel.
ccInputAttachments :: Lens' CreateChannel [InputAttachment]
ccInputAttachments = lens _ccInputAttachments (\s a -> s {_ccInputAttachments = a}) . _Default . _Coerce

-- | Deprecated field that's only usable by whitelisted customers.
ccReserved :: Lens' CreateChannel (Maybe Text)
ccReserved = lens _ccReserved (\s a -> s {_ccReserved = a})

-- | Undocumented member.
ccDestinations :: Lens' CreateChannel [OutputDestination]
ccDestinations = lens _ccDestinations (\s a -> s {_ccDestinations = a}) . _Default . _Coerce

-- | Name of channel.
ccName :: Lens' CreateChannel (Maybe Text)
ccName = lens _ccName (\s a -> s {_ccName = a})

-- | Specification of CDI inputs for this channel
ccCdiInputSpecification :: Lens' CreateChannel (Maybe CdiInputSpecification)
ccCdiInputSpecification = lens _ccCdiInputSpecification (\s a -> s {_ccCdiInputSpecification = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
ccChannelClass :: Lens' CreateChannel (Maybe ChannelClass)
ccChannelClass = lens _ccChannelClass (\s a -> s {_ccChannelClass = a})

-- | A collection of key-value pairs.
ccTags :: Lens' CreateChannel (HashMap Text (Text))
ccTags = lens _ccTags (\s a -> s {_ccTags = a}) . _Default . _Map

-- | Undocumented member.
ccEncoderSettings :: Lens' CreateChannel (Maybe EncoderSettings)
ccEncoderSettings = lens _ccEncoderSettings (\s a -> s {_ccEncoderSettings = a})

-- | An optional Amazon Resource Name (ARN) of the role to assume when running the Channel.
ccRoleARN :: Lens' CreateChannel (Maybe Text)
ccRoleARN = lens _ccRoleARN (\s a -> s {_ccRoleARN = a})

instance AWSRequest CreateChannel where
  type Rs CreateChannel = CreateChannelResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            <$> (x .?> "channel") <*> (pure (fromEnum s))
      )

instance Hashable CreateChannel

instance NFData CreateChannel

instance ToHeaders CreateChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON CreateChannel where
  toJSON CreateChannel' {..} =
    object
      ( catMaybes
          [ ("requestId" .=) <$> _ccRequestId,
            ("logLevel" .=) <$> _ccLogLevel,
            ("inputSpecification" .=) <$> _ccInputSpecification,
            ("inputAttachments" .=) <$> _ccInputAttachments,
            ("reserved" .=) <$> _ccReserved,
            ("destinations" .=) <$> _ccDestinations,
            ("name" .=) <$> _ccName,
            ("cdiInputSpecification" .=) <$> _ccCdiInputSpecification,
            ("channelClass" .=) <$> _ccChannelClass,
            ("tags" .=) <$> _ccTags,
            ("encoderSettings" .=) <$> _ccEncoderSettings,
            ("roleArn" .=) <$> _ccRoleARN
          ]
      )

instance ToPath CreateChannel where
  toPath = const "/prod/channels"

instance ToQuery CreateChannel where
  toQuery = const mempty

-- | Placeholder documentation for CreateChannelResponse
--
-- /See:/ 'createChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { _ccrsChannel ::
      !(Maybe Channel),
    _ccrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsChannel' - Undocumented member.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createChannelResponse ::
  -- | 'ccrsResponseStatus'
  Int ->
  CreateChannelResponse
createChannelResponse pResponseStatus_ =
  CreateChannelResponse'
    { _ccrsChannel = Nothing,
      _ccrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ccrsChannel :: Lens' CreateChannelResponse (Maybe Channel)
ccrsChannel = lens _ccrsChannel (\s a -> s {_ccrsChannel = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateChannelResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\s a -> s {_ccrsResponseStatus = a})

instance NFData CreateChannelResponse
