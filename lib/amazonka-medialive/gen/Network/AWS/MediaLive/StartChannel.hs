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
-- Module      : Network.AWS.MediaLive.StartChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing channel
module Network.AWS.MediaLive.StartChannel
  ( -- * Creating a Request
    startChannel,
    StartChannel,

    -- * Request Lenses
    scChannelId,

    -- * Destructuring the Response
    startChannelResponse,
    StartChannelResponse,

    -- * Response Lenses
    scrsState,
    scrsLogLevel,
    scrsARN,
    scrsPipelinesRunningCount,
    scrsPipelineDetails,
    scrsInputSpecification,
    scrsInputAttachments,
    scrsDestinations,
    scrsName,
    scrsCdiInputSpecification,
    scrsId,
    scrsChannelClass,
    scrsEgressEndpoints,
    scrsTags,
    scrsEncoderSettings,
    scrsRoleARN,
    scrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for StartChannelRequest
--
-- /See:/ 'startChannel' smart constructor.
newtype StartChannel = StartChannel' {_scChannelId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scChannelId' - A request to start a channel
startChannel ::
  -- | 'scChannelId'
  Text ->
  StartChannel
startChannel pChannelId_ =
  StartChannel' {_scChannelId = pChannelId_}

-- | A request to start a channel
scChannelId :: Lens' StartChannel Text
scChannelId = lens _scChannelId (\s a -> s {_scChannelId = a})

instance AWSRequest StartChannel where
  type Rs StartChannel = StartChannelResponse
  request = postJSON mediaLive
  response =
    receiveJSON
      ( \s h x ->
          StartChannelResponse'
            <$> (x .?> "state")
            <*> (x .?> "logLevel")
            <*> (x .?> "arn")
            <*> (x .?> "pipelinesRunningCount")
            <*> (x .?> "pipelineDetails" .!@ mempty)
            <*> (x .?> "inputSpecification")
            <*> (x .?> "inputAttachments" .!@ mempty)
            <*> (x .?> "destinations" .!@ mempty)
            <*> (x .?> "name")
            <*> (x .?> "cdiInputSpecification")
            <*> (x .?> "id")
            <*> (x .?> "channelClass")
            <*> (x .?> "egressEndpoints" .!@ mempty)
            <*> (x .?> "tags" .!@ mempty)
            <*> (x .?> "encoderSettings")
            <*> (x .?> "roleArn")
            <*> (pure (fromEnum s))
      )

instance Hashable StartChannel

instance NFData StartChannel

instance ToHeaders StartChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON StartChannel where
  toJSON = const (Object mempty)

instance ToPath StartChannel where
  toPath StartChannel' {..} =
    mconcat ["/prod/channels/", toBS _scChannelId, "/start"]

instance ToQuery StartChannel where
  toQuery = const mempty

-- | Placeholder documentation for StartChannelResponse
--
-- /See:/ 'startChannelResponse' smart constructor.
data StartChannelResponse = StartChannelResponse'
  { _scrsState ::
      !(Maybe ChannelState),
    _scrsLogLevel :: !(Maybe LogLevel),
    _scrsARN :: !(Maybe Text),
    _scrsPipelinesRunningCount :: !(Maybe Int),
    _scrsPipelineDetails :: !(Maybe [PipelineDetail]),
    _scrsInputSpecification ::
      !(Maybe InputSpecification),
    _scrsInputAttachments ::
      !(Maybe [InputAttachment]),
    _scrsDestinations :: !(Maybe [OutputDestination]),
    _scrsName :: !(Maybe Text),
    _scrsCdiInputSpecification ::
      !(Maybe CdiInputSpecification),
    _scrsId :: !(Maybe Text),
    _scrsChannelClass :: !(Maybe ChannelClass),
    _scrsEgressEndpoints ::
      !(Maybe [ChannelEgressEndpoint]),
    _scrsTags :: !(Maybe (Map Text (Text))),
    _scrsEncoderSettings :: !(Maybe EncoderSettings),
    _scrsRoleARN :: !(Maybe Text),
    _scrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrsState' - Undocumented member.
--
-- * 'scrsLogLevel' - The log level being written to CloudWatch Logs.
--
-- * 'scrsARN' - The unique arn of the channel.
--
-- * 'scrsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'scrsPipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- * 'scrsInputSpecification' - Specification of network and file inputs for this channel
--
-- * 'scrsInputAttachments' - List of input attachments for channel.
--
-- * 'scrsDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'scrsName' - The name of the channel. (user-mutable)
--
-- * 'scrsCdiInputSpecification' - Specification of CDI inputs for this channel
--
-- * 'scrsId' - The unique id of the channel.
--
-- * 'scrsChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'scrsEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'scrsTags' - A collection of key-value pairs.
--
-- * 'scrsEncoderSettings' - Undocumented member.
--
-- * 'scrsRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- * 'scrsResponseStatus' - -- | The response status code.
startChannelResponse ::
  -- | 'scrsResponseStatus'
  Int ->
  StartChannelResponse
startChannelResponse pResponseStatus_ =
  StartChannelResponse'
    { _scrsState = Nothing,
      _scrsLogLevel = Nothing,
      _scrsARN = Nothing,
      _scrsPipelinesRunningCount = Nothing,
      _scrsPipelineDetails = Nothing,
      _scrsInputSpecification = Nothing,
      _scrsInputAttachments = Nothing,
      _scrsDestinations = Nothing,
      _scrsName = Nothing,
      _scrsCdiInputSpecification = Nothing,
      _scrsId = Nothing,
      _scrsChannelClass = Nothing,
      _scrsEgressEndpoints = Nothing,
      _scrsTags = Nothing,
      _scrsEncoderSettings = Nothing,
      _scrsRoleARN = Nothing,
      _scrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
scrsState :: Lens' StartChannelResponse (Maybe ChannelState)
scrsState = lens _scrsState (\s a -> s {_scrsState = a})

-- | The log level being written to CloudWatch Logs.
scrsLogLevel :: Lens' StartChannelResponse (Maybe LogLevel)
scrsLogLevel = lens _scrsLogLevel (\s a -> s {_scrsLogLevel = a})

-- | The unique arn of the channel.
scrsARN :: Lens' StartChannelResponse (Maybe Text)
scrsARN = lens _scrsARN (\s a -> s {_scrsARN = a})

-- | The number of currently healthy pipelines.
scrsPipelinesRunningCount :: Lens' StartChannelResponse (Maybe Int)
scrsPipelinesRunningCount = lens _scrsPipelinesRunningCount (\s a -> s {_scrsPipelinesRunningCount = a})

-- | Runtime details for the pipelines of a running channel.
scrsPipelineDetails :: Lens' StartChannelResponse [PipelineDetail]
scrsPipelineDetails = lens _scrsPipelineDetails (\s a -> s {_scrsPipelineDetails = a}) . _Default . _Coerce

-- | Specification of network and file inputs for this channel
scrsInputSpecification :: Lens' StartChannelResponse (Maybe InputSpecification)
scrsInputSpecification = lens _scrsInputSpecification (\s a -> s {_scrsInputSpecification = a})

-- | List of input attachments for channel.
scrsInputAttachments :: Lens' StartChannelResponse [InputAttachment]
scrsInputAttachments = lens _scrsInputAttachments (\s a -> s {_scrsInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
scrsDestinations :: Lens' StartChannelResponse [OutputDestination]
scrsDestinations = lens _scrsDestinations (\s a -> s {_scrsDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
scrsName :: Lens' StartChannelResponse (Maybe Text)
scrsName = lens _scrsName (\s a -> s {_scrsName = a})

-- | Specification of CDI inputs for this channel
scrsCdiInputSpecification :: Lens' StartChannelResponse (Maybe CdiInputSpecification)
scrsCdiInputSpecification = lens _scrsCdiInputSpecification (\s a -> s {_scrsCdiInputSpecification = a})

-- | The unique id of the channel.
scrsId :: Lens' StartChannelResponse (Maybe Text)
scrsId = lens _scrsId (\s a -> s {_scrsId = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
scrsChannelClass :: Lens' StartChannelResponse (Maybe ChannelClass)
scrsChannelClass = lens _scrsChannelClass (\s a -> s {_scrsChannelClass = a})

-- | The endpoints where outgoing connections initiate from
scrsEgressEndpoints :: Lens' StartChannelResponse [ChannelEgressEndpoint]
scrsEgressEndpoints = lens _scrsEgressEndpoints (\s a -> s {_scrsEgressEndpoints = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
scrsTags :: Lens' StartChannelResponse (HashMap Text (Text))
scrsTags = lens _scrsTags (\s a -> s {_scrsTags = a}) . _Default . _Map

-- | Undocumented member.
scrsEncoderSettings :: Lens' StartChannelResponse (Maybe EncoderSettings)
scrsEncoderSettings = lens _scrsEncoderSettings (\s a -> s {_scrsEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
scrsRoleARN :: Lens' StartChannelResponse (Maybe Text)
scrsRoleARN = lens _scrsRoleARN (\s a -> s {_scrsRoleARN = a})

-- | -- | The response status code.
scrsResponseStatus :: Lens' StartChannelResponse Int
scrsResponseStatus = lens _scrsResponseStatus (\s a -> s {_scrsResponseStatus = a})

instance NFData StartChannelResponse
