{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelSummary where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.CdiInputSpecification
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.LogLevel
import Network.AWS.MediaLive.Types.OutputDestination
import Network.AWS.Prelude

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'channelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { _csState ::
      !(Maybe ChannelState),
    _csLogLevel :: !(Maybe LogLevel),
    _csARN :: !(Maybe Text),
    _csPipelinesRunningCount :: !(Maybe Int),
    _csInputSpecification :: !(Maybe InputSpecification),
    _csInputAttachments :: !(Maybe [InputAttachment]),
    _csDestinations :: !(Maybe [OutputDestination]),
    _csName :: !(Maybe Text),
    _csCdiInputSpecification :: !(Maybe CdiInputSpecification),
    _csId :: !(Maybe Text),
    _csChannelClass :: !(Maybe ChannelClass),
    _csEgressEndpoints :: !(Maybe [ChannelEgressEndpoint]),
    _csTags :: !(Maybe (Map Text (Text))),
    _csRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ChannelSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csState' - Undocumented member.
--
-- * 'csLogLevel' - The log level being written to CloudWatch Logs.
--
-- * 'csARN' - The unique arn of the channel.
--
-- * 'csPipelinesRunningCount' - The number of currently healthy pipelines.
--
-- * 'csInputSpecification' - Specification of network and file inputs for this channel
--
-- * 'csInputAttachments' - List of input attachments for channel.
--
-- * 'csDestinations' - A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
--
-- * 'csName' - The name of the channel. (user-mutable)
--
-- * 'csCdiInputSpecification' - Specification of CDI inputs for this channel
--
-- * 'csId' - The unique id of the channel.
--
-- * 'csChannelClass' - The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
--
-- * 'csEgressEndpoints' - The endpoints where outgoing connections initiate from
--
-- * 'csTags' - A collection of key-value pairs.
--
-- * 'csRoleARN' - The Amazon Resource Name (ARN) of the role assumed when running the Channel.
channelSummary ::
  ChannelSummary
channelSummary =
  ChannelSummary'
    { _csState = Nothing,
      _csLogLevel = Nothing,
      _csARN = Nothing,
      _csPipelinesRunningCount = Nothing,
      _csInputSpecification = Nothing,
      _csInputAttachments = Nothing,
      _csDestinations = Nothing,
      _csName = Nothing,
      _csCdiInputSpecification = Nothing,
      _csId = Nothing,
      _csChannelClass = Nothing,
      _csEgressEndpoints = Nothing,
      _csTags = Nothing,
      _csRoleARN = Nothing
    }

-- | Undocumented member.
csState :: Lens' ChannelSummary (Maybe ChannelState)
csState = lens _csState (\s a -> s {_csState = a})

-- | The log level being written to CloudWatch Logs.
csLogLevel :: Lens' ChannelSummary (Maybe LogLevel)
csLogLevel = lens _csLogLevel (\s a -> s {_csLogLevel = a})

-- | The unique arn of the channel.
csARN :: Lens' ChannelSummary (Maybe Text)
csARN = lens _csARN (\s a -> s {_csARN = a})

-- | The number of currently healthy pipelines.
csPipelinesRunningCount :: Lens' ChannelSummary (Maybe Int)
csPipelinesRunningCount = lens _csPipelinesRunningCount (\s a -> s {_csPipelinesRunningCount = a})

-- | Specification of network and file inputs for this channel
csInputSpecification :: Lens' ChannelSummary (Maybe InputSpecification)
csInputSpecification = lens _csInputSpecification (\s a -> s {_csInputSpecification = a})

-- | List of input attachments for channel.
csInputAttachments :: Lens' ChannelSummary [InputAttachment]
csInputAttachments = lens _csInputAttachments (\s a -> s {_csInputAttachments = a}) . _Default . _Coerce

-- | A list of destinations of the channel. For UDP outputs, there is one destination per output. For other types (HLS, for example), there is one destination per packager.
csDestinations :: Lens' ChannelSummary [OutputDestination]
csDestinations = lens _csDestinations (\s a -> s {_csDestinations = a}) . _Default . _Coerce

-- | The name of the channel. (user-mutable)
csName :: Lens' ChannelSummary (Maybe Text)
csName = lens _csName (\s a -> s {_csName = a})

-- | Specification of CDI inputs for this channel
csCdiInputSpecification :: Lens' ChannelSummary (Maybe CdiInputSpecification)
csCdiInputSpecification = lens _csCdiInputSpecification (\s a -> s {_csCdiInputSpecification = a})

-- | The unique id of the channel.
csId :: Lens' ChannelSummary (Maybe Text)
csId = lens _csId (\s a -> s {_csId = a})

-- | The class for this channel. STANDARD for a channel with two pipelines or SINGLE_PIPELINE for a channel with one pipeline.
csChannelClass :: Lens' ChannelSummary (Maybe ChannelClass)
csChannelClass = lens _csChannelClass (\s a -> s {_csChannelClass = a})

-- | The endpoints where outgoing connections initiate from
csEgressEndpoints :: Lens' ChannelSummary [ChannelEgressEndpoint]
csEgressEndpoints = lens _csEgressEndpoints (\s a -> s {_csEgressEndpoints = a}) . _Default . _Coerce

-- | A collection of key-value pairs.
csTags :: Lens' ChannelSummary (HashMap Text (Text))
csTags = lens _csTags (\s a -> s {_csTags = a}) . _Default . _Map

-- | The Amazon Resource Name (ARN) of the role assumed when running the Channel.
csRoleARN :: Lens' ChannelSummary (Maybe Text)
csRoleARN = lens _csRoleARN (\s a -> s {_csRoleARN = a})

instance FromJSON ChannelSummary where
  parseJSON =
    withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            <$> (x .:? "state")
            <*> (x .:? "logLevel")
            <*> (x .:? "arn")
            <*> (x .:? "pipelinesRunningCount")
            <*> (x .:? "inputSpecification")
            <*> (x .:? "inputAttachments" .!= mempty)
            <*> (x .:? "destinations" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "cdiInputSpecification")
            <*> (x .:? "id")
            <*> (x .:? "channelClass")
            <*> (x .:? "egressEndpoints" .!= mempty)
            <*> (x .:? "tags" .!= mempty)
            <*> (x .:? "roleArn")
      )

instance Hashable ChannelSummary

instance NFData ChannelSummary
