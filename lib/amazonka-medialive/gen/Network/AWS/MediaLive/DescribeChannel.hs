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
-- Module      : Network.AWS.MediaLive.DescribeChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a channel
module Network.AWS.MediaLive.DescribeChannel
  ( -- * Creating a Request
    describeChannel,
    DescribeChannel,

    -- * Request Lenses
    desChannelId,

    -- * Destructuring the Response
    describeChannelResponse,
    DescribeChannelResponse,

    -- * Response Lenses
    dcrsState,
    dcrsLogLevel,
    dcrsARN,
    dcrsPipelinesRunningCount,
    dcrsPipelineDetails,
    dcrsInputSpecification,
    dcrsInputAttachments,
    dcrsDestinations,
    dcrsName,
    dcrsCdiInputSpecification,
    dcrsId,
    dcrsChannelClass,
    dcrsEgressEndpoints,
    dcrsTags,
    dcrsEncoderSettings,
    dcrsRoleARN,
    dcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for DescribeChannelRequest
--
-- /See:/ 'describeChannel' smart constructor.
newtype DescribeChannel = DescribeChannel' {_desChannelId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChannel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desChannelId' - channel ID
describeChannel ::
  -- | 'desChannelId'
  Text ->
  DescribeChannel
describeChannel pChannelId_ =
  DescribeChannel' {_desChannelId = pChannelId_}

-- | channel ID
desChannelId :: Lens' DescribeChannel Text
desChannelId = lens _desChannelId (\s a -> s {_desChannelId = a})

instance AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
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

instance Hashable DescribeChannel

instance NFData DescribeChannel

instance ToHeaders DescribeChannel where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    mconcat ["/prod/channels/", toBS _desChannelId]

instance ToQuery DescribeChannel where
  toQuery = const mempty

-- | Placeholder documentation for DescribeChannelResponse
--
-- /See:/ 'describeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { _dcrsState ::
      !(Maybe ChannelState),
    _dcrsLogLevel :: !(Maybe LogLevel),
    _dcrsARN :: !(Maybe Text),
    _dcrsPipelinesRunningCount :: !(Maybe Int),
    _dcrsPipelineDetails ::
      !(Maybe [PipelineDetail]),
    _dcrsInputSpecification ::
      !(Maybe InputSpecification),
    _dcrsInputAttachments ::
      !(Maybe [InputAttachment]),
    _dcrsDestinations ::
      !(Maybe [OutputDestination]),
    _dcrsName :: !(Maybe Text),
    _dcrsCdiInputSpecification ::
      !(Maybe CdiInputSpecification),
    _dcrsId :: !(Maybe Text),
    _dcrsChannelClass :: !(Maybe ChannelClass),
    _dcrsEgressEndpoints ::
      !(Maybe [ChannelEgressEndpoint]),
    _dcrsTags :: !(Maybe (Map Text (Text))),
    _dcrsEncoderSettings ::
      !(Maybe EncoderSettings),
    _dcrsRoleARN :: !(Maybe Text),
    _dcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeChannelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsState' - Undocumented member.
--
-- * 'dcrsLogLevel' - The log level being written to CloudWatch Logs.
--
-- * 'dcrsARN' - The unique arn of the channel.
--
-- * 'dcrsPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'dcrsPipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- * 'dcrsInputSpecification' - Specification of network and file inputs for this channel
--
-- * 'dcrsInputAttachments' - List of input attachments for channel.
--
-- * 'dcrsDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'dcrsName' - The name of the channel. (user-mutable)
--
-- * 'dcrsCdiInputSpecification' - Specification of CDI inputs for this channel
--
-- * 'dcrsId' - The unique id of the channel.
--
-- * 'dcrsChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'dcrsEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'dcrsTags' - A collection of key-value pairs.
--
-- * 'dcrsEncoderSettings' - Undocumented member.
--
-- * 'dcrsRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeChannelResponse ::
  -- | 'dcrsResponseStatus'
  Int ->
  DescribeChannelResponse
describeChannelResponse pResponseStatus_ =
  DescribeChannelResponse'
    { _dcrsState = Nothing,
      _dcrsLogLevel = Nothing,
      _dcrsARN = Nothing,
      _dcrsPipelinesRunningCount = Nothing,
      _dcrsPipelineDetails = Nothing,
      _dcrsInputSpecification = Nothing,
      _dcrsInputAttachments = Nothing,
      _dcrsDestinations = Nothing,
      _dcrsName = Nothing,
      _dcrsCdiInputSpecification = Nothing,
      _dcrsId = Nothing,
      _dcrsChannelClass = Nothing,
      _dcrsEgressEndpoints = Nothing,
      _dcrsTags = Nothing,
      _dcrsEncoderSettings = Nothing,
      _dcrsRoleARN = Nothing,
      _dcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dcrsState :: Lens' DescribeChannelResponse (Maybe ChannelState)
dcrsState = lens _dcrsState (\s a -> s {_dcrsState = a})

-- | The log level being written to CloudWatch Logs.
dcrsLogLevel :: Lens' DescribeChannelResponse (Maybe LogLevel)
dcrsLogLevel = lens _dcrsLogLevel (\s a -> s {_dcrsLogLevel = a})

-- | The unique arn of the channel.
dcrsARN :: Lens' DescribeChannelResponse (Maybe Text)
dcrsARN = lens _dcrsARN (\s a -> s {_dcrsARN = a})

-- | The number of currently healthy pipelines.
dcrsPipelinesRunningCount :: Lens' DescribeChannelResponse (Maybe Int)
dcrsPipelinesRunningCount = lens _dcrsPipelinesRunningCount (\s a -> s {_dcrsPipelinesRunningCount = a})

-- | Runtime details for the pipelines of a running channel.
dcrsPipelineDetails :: Lens' DescribeChannelResponse [PipelineDetail]
dcrsPipelineDetails = lens _dcrsPipelineDetails (\s a -> s {_dcrsPipelineDetails = a}) . _Default . _Coerce

-- | Specification of network and file inputs for this channel
dcrsInputSpecification :: Lens' DescribeChannelResponse (Maybe InputSpecification)
dcrsInputSpecification = lens _dcrsInputSpecification (\s a -> s {_dcrsInputSpecification = a})

-- | List of input attachments for channel.
dcrsInputAttachments :: Lens' DescribeChannelResponse [InputAttachment]
dcrsInputAttachments = lens _dcrsInputAttachments (\s a -> s {_dcrsInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
dcrsDestinations :: Lens' DescribeChannelResponse [OutputDestination]
dcrsDestinations = lens _dcrsDestinations (\s a -> s {_dcrsDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
dcrsName :: Lens' DescribeChannelResponse (Maybe Text)
dcrsName = lens _dcrsName (\s a -> s {_dcrsName = a})

-- | Specification of CDI inputs for this channel
dcrsCdiInputSpecification :: Lens' DescribeChannelResponse (Maybe CdiInputSpecification)
dcrsCdiInputSpecification = lens _dcrsCdiInputSpecification (\s a -> s {_dcrsCdiInputSpecification = a})

-- | The unique id of the channel.
dcrsId :: Lens' DescribeChannelResponse (Maybe Text)
dcrsId = lens _dcrsId (\s a -> s {_dcrsId = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
dcrsChannelClass :: Lens' DescribeChannelResponse (Maybe ChannelClass)
dcrsChannelClass = lens _dcrsChannelClass (\s a -> s {_dcrsChannelClass = a})

-- | The endpoints where outgoing connections initiate from
dcrsEgressEndpoints :: Lens' DescribeChannelResponse [ChannelEgressEndpoint]
dcrsEgressEndpoints = lens _dcrsEgressEndpoints (\s a -> s {_dcrsEgressEndpoints = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
dcrsTags :: Lens' DescribeChannelResponse (HashMap Text (Text))
dcrsTags = lens _dcrsTags (\s a -> s {_dcrsTags = a}) . _Default . _Map

-- | Undocumented member.
dcrsEncoderSettings :: Lens' DescribeChannelResponse (Maybe EncoderSettings)
dcrsEncoderSettings = lens _dcrsEncoderSettings (\s a -> s {_dcrsEncoderSettings = a})

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
dcrsRoleARN :: Lens' DescribeChannelResponse (Maybe Text)
dcrsRoleARN = lens _dcrsRoleARN (\s a -> s {_dcrsRoleARN = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeChannelResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\s a -> s {_dcrsResponseStatus = a})

instance NFData DescribeChannelResponse
