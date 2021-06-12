{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Channel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Channel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CdiInputSpecification
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.EncoderSettings
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.LogLevel
import Network.AWS.MediaLive.Types.OutputDestination
import Network.AWS.MediaLive.Types.PipelineDetail
import Network.AWS.MediaLive.Types.VpcOutputSettings

-- | Placeholder documentation for Channel
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { encoderSettings :: Core.Maybe EncoderSettings,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Core.Maybe Core.Text,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Core.Maybe InputSpecification,
    -- | The unique arn of the channel.
    arn :: Core.Maybe Core.Text,
    -- | The unique id of the channel.
    id :: Core.Maybe Core.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Core.Maybe Core.Int,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Core.Maybe ChannelClass,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Core.Maybe LogLevel,
    -- | A list of destinations of the channel. For UDP outputs, there is one
    -- destination per output. For other types (HLS, for example), there is one
    -- destination per packager.
    destinations :: Core.Maybe [OutputDestination],
    state :: Core.Maybe ChannelState,
    -- | The name of the channel. (user-mutable)
    name :: Core.Maybe Core.Text,
    -- | List of input attachments for channel.
    inputAttachments :: Core.Maybe [InputAttachment],
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Core.Maybe [PipelineDetail],
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Core.Maybe [ChannelEgressEndpoint],
    -- | Settings for VPC output
    vpc :: Core.Maybe VpcOutputSettings,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Core.Maybe CdiInputSpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoderSettings', 'channel_encoderSettings' - Undocumented member.
--
-- 'roleArn', 'channel_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'inputSpecification', 'channel_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'arn', 'channel_arn' - The unique arn of the channel.
--
-- 'id', 'channel_id' - The unique id of the channel.
--
-- 'pipelinesRunningCount', 'channel_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'channelClass', 'channel_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'logLevel', 'channel_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'destinations', 'channel_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'state', 'channel_state' - Undocumented member.
--
-- 'name', 'channel_name' - The name of the channel. (user-mutable)
--
-- 'inputAttachments', 'channel_inputAttachments' - List of input attachments for channel.
--
-- 'tags', 'channel_tags' - A collection of key-value pairs.
--
-- 'pipelineDetails', 'channel_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'egressEndpoints', 'channel_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'vpc', 'channel_vpc' - Settings for VPC output
--
-- 'cdiInputSpecification', 'channel_cdiInputSpecification' - Specification of CDI inputs for this channel
newChannel ::
  Channel
newChannel =
  Channel'
    { encoderSettings = Core.Nothing,
      roleArn = Core.Nothing,
      inputSpecification = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      pipelinesRunningCount = Core.Nothing,
      channelClass = Core.Nothing,
      logLevel = Core.Nothing,
      destinations = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      inputAttachments = Core.Nothing,
      tags = Core.Nothing,
      pipelineDetails = Core.Nothing,
      egressEndpoints = Core.Nothing,
      vpc = Core.Nothing,
      cdiInputSpecification = Core.Nothing
    }

-- | Undocumented member.
channel_encoderSettings :: Lens.Lens' Channel (Core.Maybe EncoderSettings)
channel_encoderSettings = Lens.lens (\Channel' {encoderSettings} -> encoderSettings) (\s@Channel' {} a -> s {encoderSettings = a} :: Channel)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
channel_roleArn :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_roleArn = Lens.lens (\Channel' {roleArn} -> roleArn) (\s@Channel' {} a -> s {roleArn = a} :: Channel)

-- | Specification of network and file inputs for this channel
channel_inputSpecification :: Lens.Lens' Channel (Core.Maybe InputSpecification)
channel_inputSpecification = Lens.lens (\Channel' {inputSpecification} -> inputSpecification) (\s@Channel' {} a -> s {inputSpecification = a} :: Channel)

-- | The unique arn of the channel.
channel_arn :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The unique id of the channel.
channel_id :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_id = Lens.lens (\Channel' {id} -> id) (\s@Channel' {} a -> s {id = a} :: Channel)

-- | The number of currently healthy pipelines.
channel_pipelinesRunningCount :: Lens.Lens' Channel (Core.Maybe Core.Int)
channel_pipelinesRunningCount = Lens.lens (\Channel' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@Channel' {} a -> s {pipelinesRunningCount = a} :: Channel)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
channel_channelClass :: Lens.Lens' Channel (Core.Maybe ChannelClass)
channel_channelClass = Lens.lens (\Channel' {channelClass} -> channelClass) (\s@Channel' {} a -> s {channelClass = a} :: Channel)

-- | The log level being written to CloudWatch Logs.
channel_logLevel :: Lens.Lens' Channel (Core.Maybe LogLevel)
channel_logLevel = Lens.lens (\Channel' {logLevel} -> logLevel) (\s@Channel' {} a -> s {logLevel = a} :: Channel)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
channel_destinations :: Lens.Lens' Channel (Core.Maybe [OutputDestination])
channel_destinations = Lens.lens (\Channel' {destinations} -> destinations) (\s@Channel' {} a -> s {destinations = a} :: Channel) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
channel_state :: Lens.Lens' Channel (Core.Maybe ChannelState)
channel_state = Lens.lens (\Channel' {state} -> state) (\s@Channel' {} a -> s {state = a} :: Channel)

-- | The name of the channel. (user-mutable)
channel_name :: Lens.Lens' Channel (Core.Maybe Core.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel)

-- | List of input attachments for channel.
channel_inputAttachments :: Lens.Lens' Channel (Core.Maybe [InputAttachment])
channel_inputAttachments = Lens.lens (\Channel' {inputAttachments} -> inputAttachments) (\s@Channel' {} a -> s {inputAttachments = a} :: Channel) Core.. Lens.mapping Lens._Coerce

-- | A collection of key-value pairs.
channel_tags :: Lens.Lens' Channel (Core.Maybe (Core.HashMap Core.Text Core.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Core.. Lens.mapping Lens._Coerce

-- | Runtime details for the pipelines of a running channel.
channel_pipelineDetails :: Lens.Lens' Channel (Core.Maybe [PipelineDetail])
channel_pipelineDetails = Lens.lens (\Channel' {pipelineDetails} -> pipelineDetails) (\s@Channel' {} a -> s {pipelineDetails = a} :: Channel) Core.. Lens.mapping Lens._Coerce

-- | The endpoints where outgoing connections initiate from
channel_egressEndpoints :: Lens.Lens' Channel (Core.Maybe [ChannelEgressEndpoint])
channel_egressEndpoints = Lens.lens (\Channel' {egressEndpoints} -> egressEndpoints) (\s@Channel' {} a -> s {egressEndpoints = a} :: Channel) Core.. Lens.mapping Lens._Coerce

-- | Settings for VPC output
channel_vpc :: Lens.Lens' Channel (Core.Maybe VpcOutputSettings)
channel_vpc = Lens.lens (\Channel' {vpc} -> vpc) (\s@Channel' {} a -> s {vpc = a} :: Channel)

-- | Specification of CDI inputs for this channel
channel_cdiInputSpecification :: Lens.Lens' Channel (Core.Maybe CdiInputSpecification)
channel_cdiInputSpecification = Lens.lens (\Channel' {cdiInputSpecification} -> cdiInputSpecification) (\s@Channel' {} a -> s {cdiInputSpecification = a} :: Channel)

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Core.<$> (x Core..:? "encoderSettings")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "inputSpecification")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "pipelinesRunningCount")
            Core.<*> (x Core..:? "channelClass")
            Core.<*> (x Core..:? "logLevel")
            Core.<*> (x Core..:? "destinations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "inputAttachments" Core..!= Core.mempty)
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "pipelineDetails" Core..!= Core.mempty)
            Core.<*> (x Core..:? "egressEndpoints" Core..!= Core.mempty)
            Core.<*> (x Core..:? "vpc")
            Core.<*> (x Core..:? "cdiInputSpecification")
      )

instance Core.Hashable Channel

instance Core.NFData Channel
