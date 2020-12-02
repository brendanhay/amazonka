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
-- Module      : Network.AWS.MediaLive.DeleteChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts deletion of channel. The associated outputs are also deleted.
module Network.AWS.MediaLive.DeleteChannel
  ( -- * Creating a Request
    deleteChannel,
    DeleteChannel,

    -- * Request Lenses
    dcChannelId,

    -- * Destructuring the Response
    deleteChannelResponse,
    DeleteChannelResponse,

    -- * Response Lenses
    drsState,
    drsLogLevel,
    drsARN,
    drsPipelinesRunningCount,
    drsPipelineDetails,
    drsInputSpecification,
    drsInputAttachments,
    drsDestinations,
    drsName,
    drsCdiInputSpecification,
    drsId,
    drsChannelClass,
    drsEgressEndpoints,
    drsTags,
    drsEncoderSettings,
    drsRoleARN,
    drsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DeleteChannelRequest
--
-- /See:/ 'deleteChannel' smart constructor.
newtype DeleteChannel = DeleteChannel' {_dcChannelId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcChannelId' - Unique ID of the channel.
deleteChannel ::
  -- | 'dcChannelId'
  Text ->
  DeleteChannel
deleteChannel pChannelId_ =
  DeleteChannel' {_dcChannelId = pChannelId_}

-- | Unique ID of the channel.
dcChannelId :: Lens' DeleteChannel Text
dcChannelId = lens _dcChannelId (\s a -> s {_dcChannelId = a})

instance AWSRequest DeleteChannel where
  type Rs DeleteChannel = DeleteChannelResponse
  request = delete mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DeleteChannelResponse'
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

instance Hashable DeleteChannel

instance NFData DeleteChannel

instance ToHeaders DeleteChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    mconcat ["/prod/channels/", toBS _dcChannelId]

instance ToQuery DeleteChannel where
  toQuery = const mempty

-- | Placeholder documentation for DeleteChannelResponse
--
-- /See:/ 'deleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
  { _drsState ::
      !(Maybe ChannelState),
    _drsLogLevel :: !(Maybe LogLevel),
    _drsARN :: !(Maybe Text),
    _drsPipelinesRunningCount :: !(Maybe Int),
    _drsPipelineDetails ::
      !(Maybe [PipelineDetail]),
    _drsInputSpecification ::
      !(Maybe InputSpecification),
    _drsInputAttachments ::
      !(Maybe [InputAttachment]),
    _drsDestinations ::
      !(Maybe [OutputDestination]),
    _drsName :: !(Maybe Text),
    _drsCdiInputSpecification ::
      !(Maybe CdiInputSpecification),
    _drsId :: !(Maybe Text),
    _drsChannelClass :: !(Maybe ChannelClass),
    _drsEgressEndpoints ::
      !(Maybe [ChannelEgressEndpoint]),
    _drsTags :: !(Maybe (Map Text (Text))),
    _drsEncoderSettings :: !(Maybe EncoderSettings),
    _drsRoleARN :: !(Maybe Text),
    _drsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsState' - Undocumented member.
--
-- * 'drsLogLevel' - The log level being written to CloudWatch Logs.
--
-- * 'drsARN' - The unique arn of the channel.
--
-- * 'drsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'drsPipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- * 'drsInputSpecification' - Specification of network and file inputs for this channel
--
-- * 'drsInputAttachments' - List of input attachments for channel.
--
-- * 'drsDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'drsName' - The name of the channel. (user-mutable)
--
-- * 'drsCdiInputSpecification' - Specification of CDI inputs for this channel
--
-- * 'drsId' - The unique id of the channel.
--
-- * 'drsChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'drsEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'drsTags' - A collection of key-value pairs.
--
-- * 'drsEncoderSettings' - Undocumented member.
--
-- * 'drsRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteChannelResponse ::
  -- | 'drsResponseStatus'
  Int ->
  DeleteChannelResponse
deleteChannelResponse pResponseStatus_ =
  DeleteChannelResponse'
    { _drsState = Nothing,
      _drsLogLevel = Nothing,
      _drsARN = Nothing,
      _drsPipelinesRunningCount = Nothing,
      _drsPipelineDetails = Nothing,
      _drsInputSpecification = Nothing,
      _drsInputAttachments = Nothing,
      _drsDestinations = Nothing,
      _drsName = Nothing,
      _drsCdiInputSpecification = Nothing,
      _drsId = Nothing,
      _drsChannelClass = Nothing,
      _drsEgressEndpoints = Nothing,
      _drsTags = Nothing,
      _drsEncoderSettings = Nothing,
      _drsRoleARN = Nothing,
      _drsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
drsState :: Lens' DeleteChannelResponse (Maybe ChannelState)
drsState = lens _drsState (\s a -> s {_drsState = a})

-- | The log level being written to CloudWatch Logs.
drsLogLevel :: Lens' DeleteChannelResponse (Maybe LogLevel)
drsLogLevel = lens _drsLogLevel (\s a -> s {_drsLogLevel = a})

-- | The unique arn of the channel.
drsARN :: Lens' DeleteChannelResponse (Maybe Text)
drsARN = lens _drsARN (\s a -> s {_drsARN = a})

-- | The number of currently healthy pipelines.
drsPipelinesRunningCount :: Lens' DeleteChannelResponse (Maybe Int)
drsPipelinesRunningCount = lens _drsPipelinesRunningCount (\s a -> s {_drsPipelinesRunningCount = a})

-- | Runtime details for the pipelines of a running channel.
drsPipelineDetails :: Lens' DeleteChannelResponse [PipelineDetail]
drsPipelineDetails = lens _drsPipelineDetails (\s a -> s {_drsPipelineDetails = a}) . _Default . _Coerce

-- | Specification of network and file inputs for this channel
drsInputSpecification :: Lens' DeleteChannelResponse (Maybe InputSpecification)
drsInputSpecification = lens _drsInputSpecification (\s a -> s {_drsInputSpecification = a})

-- | List of input attachments for channel.
drsInputAttachments :: Lens' DeleteChannelResponse [InputAttachment]
drsInputAttachments = lens _drsInputAttachments (\s a -> s {_drsInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
drsDestinations :: Lens' DeleteChannelResponse [OutputDestination]
drsDestinations = lens _drsDestinations (\s a -> s {_drsDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
drsName :: Lens' DeleteChannelResponse (Maybe Text)
drsName = lens _drsName (\s a -> s {_drsName = a})

-- | Specification of CDI inputs for this channel
drsCdiInputSpecification :: Lens' DeleteChannelResponse (Maybe CdiInputSpecification)
drsCdiInputSpecification = lens _drsCdiInputSpecification (\s a -> s {_drsCdiInputSpecification = a})

-- | The unique id of the channel.
drsId :: Lens' DeleteChannelResponse (Maybe Text)
drsId = lens _drsId (\s a -> s {_drsId = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
drsChannelClass :: Lens' DeleteChannelResponse (Maybe ChannelClass)
drsChannelClass = lens _drsChannelClass (\s a -> s {_drsChannelClass = a})

-- | The endpoints where outgoing connections initiate from
drsEgressEndpoints :: Lens' DeleteChannelResponse [ChannelEgressEndpoint]
drsEgressEndpoints = lens _drsEgressEndpoints (\s a -> s {_drsEgressEndpoints = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
drsTags :: Lens' DeleteChannelResponse (HashMap Text (Text))
drsTags = lens _drsTags (\s a -> s {_drsTags = a}) . _Default . _Map

-- | Undocumented member.
drsEncoderSettings :: Lens' DeleteChannelResponse (Maybe EncoderSettings)
drsEncoderSettings = lens _drsEncoderSettings (\s a -> s {_drsEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
drsRoleARN :: Lens' DeleteChannelResponse (Maybe Text)
drsRoleARN = lens _drsRoleARN (\s a -> s {_drsRoleARN = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteChannelResponse Int
drsResponseStatus = lens _drsResponseStatus (\s a -> s {_drsResponseStatus = a})

instance NFData DeleteChannelResponse
