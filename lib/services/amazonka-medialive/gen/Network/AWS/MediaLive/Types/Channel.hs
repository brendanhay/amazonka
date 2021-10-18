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
import Network.AWS.MediaLive.Types.VpcOutputSettingsDescription
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for Channel
--
-- /See:/ 'newChannel' smart constructor.
data Channel = Channel'
  { -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    encoderSettings :: Prelude.Maybe EncoderSettings,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The unique arn of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique id of the channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Prelude.Maybe ChannelClass,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | The name of the channel. (user-mutable)
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of destinations of the channel. For UDP outputs, there is one
    -- destination per output. For other types (HLS, for example), there is one
    -- destination per packager.
    destinations :: Prelude.Maybe [OutputDestination],
    state :: Prelude.Maybe ChannelState,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Prelude.Maybe [PipelineDetail],
    -- | Settings for VPC output
    vpc :: Prelude.Maybe VpcOutputSettingsDescription,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Prelude.Maybe [ChannelEgressEndpoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Channel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'channel_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'encoderSettings', 'channel_encoderSettings' - Undocumented member.
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
-- 'name', 'channel_name' - The name of the channel. (user-mutable)
--
-- 'destinations', 'channel_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'state', 'channel_state' - Undocumented member.
--
-- 'inputAttachments', 'channel_inputAttachments' - List of input attachments for channel.
--
-- 'tags', 'channel_tags' - A collection of key-value pairs.
--
-- 'pipelineDetails', 'channel_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'vpc', 'channel_vpc' - Settings for VPC output
--
-- 'cdiInputSpecification', 'channel_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'egressEndpoints', 'channel_egressEndpoints' - The endpoints where outgoing connections initiate from
newChannel ::
  Channel
newChannel =
  Channel'
    { roleArn = Prelude.Nothing,
      encoderSettings = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      name = Prelude.Nothing,
      destinations = Prelude.Nothing,
      state = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      tags = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing,
      vpc = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
channel_roleArn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_roleArn = Lens.lens (\Channel' {roleArn} -> roleArn) (\s@Channel' {} a -> s {roleArn = a} :: Channel)

-- | Undocumented member.
channel_encoderSettings :: Lens.Lens' Channel (Prelude.Maybe EncoderSettings)
channel_encoderSettings = Lens.lens (\Channel' {encoderSettings} -> encoderSettings) (\s@Channel' {} a -> s {encoderSettings = a} :: Channel)

-- | Specification of network and file inputs for this channel
channel_inputSpecification :: Lens.Lens' Channel (Prelude.Maybe InputSpecification)
channel_inputSpecification = Lens.lens (\Channel' {inputSpecification} -> inputSpecification) (\s@Channel' {} a -> s {inputSpecification = a} :: Channel)

-- | The unique arn of the channel.
channel_arn :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_arn = Lens.lens (\Channel' {arn} -> arn) (\s@Channel' {} a -> s {arn = a} :: Channel)

-- | The unique id of the channel.
channel_id :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_id = Lens.lens (\Channel' {id} -> id) (\s@Channel' {} a -> s {id = a} :: Channel)

-- | The number of currently healthy pipelines.
channel_pipelinesRunningCount :: Lens.Lens' Channel (Prelude.Maybe Prelude.Int)
channel_pipelinesRunningCount = Lens.lens (\Channel' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@Channel' {} a -> s {pipelinesRunningCount = a} :: Channel)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
channel_channelClass :: Lens.Lens' Channel (Prelude.Maybe ChannelClass)
channel_channelClass = Lens.lens (\Channel' {channelClass} -> channelClass) (\s@Channel' {} a -> s {channelClass = a} :: Channel)

-- | The log level being written to CloudWatch Logs.
channel_logLevel :: Lens.Lens' Channel (Prelude.Maybe LogLevel)
channel_logLevel = Lens.lens (\Channel' {logLevel} -> logLevel) (\s@Channel' {} a -> s {logLevel = a} :: Channel)

-- | The name of the channel. (user-mutable)
channel_name :: Lens.Lens' Channel (Prelude.Maybe Prelude.Text)
channel_name = Lens.lens (\Channel' {name} -> name) (\s@Channel' {} a -> s {name = a} :: Channel)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
channel_destinations :: Lens.Lens' Channel (Prelude.Maybe [OutputDestination])
channel_destinations = Lens.lens (\Channel' {destinations} -> destinations) (\s@Channel' {} a -> s {destinations = a} :: Channel) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
channel_state :: Lens.Lens' Channel (Prelude.Maybe ChannelState)
channel_state = Lens.lens (\Channel' {state} -> state) (\s@Channel' {} a -> s {state = a} :: Channel)

-- | List of input attachments for channel.
channel_inputAttachments :: Lens.Lens' Channel (Prelude.Maybe [InputAttachment])
channel_inputAttachments = Lens.lens (\Channel' {inputAttachments} -> inputAttachments) (\s@Channel' {} a -> s {inputAttachments = a} :: Channel) Prelude.. Lens.mapping Lens._Coerce

-- | A collection of key-value pairs.
channel_tags :: Lens.Lens' Channel (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channel_tags = Lens.lens (\Channel' {tags} -> tags) (\s@Channel' {} a -> s {tags = a} :: Channel) Prelude.. Lens.mapping Lens._Coerce

-- | Runtime details for the pipelines of a running channel.
channel_pipelineDetails :: Lens.Lens' Channel (Prelude.Maybe [PipelineDetail])
channel_pipelineDetails = Lens.lens (\Channel' {pipelineDetails} -> pipelineDetails) (\s@Channel' {} a -> s {pipelineDetails = a} :: Channel) Prelude.. Lens.mapping Lens._Coerce

-- | Settings for VPC output
channel_vpc :: Lens.Lens' Channel (Prelude.Maybe VpcOutputSettingsDescription)
channel_vpc = Lens.lens (\Channel' {vpc} -> vpc) (\s@Channel' {} a -> s {vpc = a} :: Channel)

-- | Specification of CDI inputs for this channel
channel_cdiInputSpecification :: Lens.Lens' Channel (Prelude.Maybe CdiInputSpecification)
channel_cdiInputSpecification = Lens.lens (\Channel' {cdiInputSpecification} -> cdiInputSpecification) (\s@Channel' {} a -> s {cdiInputSpecification = a} :: Channel)

-- | The endpoints where outgoing connections initiate from
channel_egressEndpoints :: Lens.Lens' Channel (Prelude.Maybe [ChannelEgressEndpoint])
channel_egressEndpoints = Lens.lens (\Channel' {egressEndpoints} -> egressEndpoints) (\s@Channel' {} a -> s {egressEndpoints = a} :: Channel) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON Channel where
  parseJSON =
    Core.withObject
      "Channel"
      ( \x ->
          Channel'
            Prelude.<$> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "encoderSettings")
            Prelude.<*> (x Core..:? "inputSpecification")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "pipelinesRunningCount")
            Prelude.<*> (x Core..:? "channelClass")
            Prelude.<*> (x Core..:? "logLevel")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "destinations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> ( x Core..:? "inputAttachments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "pipelineDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "vpc")
            Prelude.<*> (x Core..:? "cdiInputSpecification")
            Prelude.<*> ( x Core..:? "egressEndpoints"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Channel

instance Prelude.NFData Channel
