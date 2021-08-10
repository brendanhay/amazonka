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
-- Module      : Network.AWS.MediaLive.Types.ChannelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.CdiInputSpecification
import Network.AWS.MediaLive.Types.ChannelClass
import Network.AWS.MediaLive.Types.ChannelEgressEndpoint
import Network.AWS.MediaLive.Types.ChannelState
import Network.AWS.MediaLive.Types.InputAttachment
import Network.AWS.MediaLive.Types.InputSpecification
import Network.AWS.MediaLive.Types.LogLevel
import Network.AWS.MediaLive.Types.OutputDestination
import Network.AWS.MediaLive.Types.VpcOutputSettings
import qualified Network.AWS.Prelude as Prelude

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
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
    -- | A list of destinations of the channel. For UDP outputs, there is one
    -- destination per output. For other types (HLS, for example), there is one
    -- destination per packager.
    destinations :: Prelude.Maybe [OutputDestination],
    state :: Prelude.Maybe ChannelState,
    -- | The name of the channel. (user-mutable)
    name :: Prelude.Maybe Prelude.Text,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Prelude.Maybe [ChannelEgressEndpoint],
    -- | Settings for VPC output
    vpc :: Prelude.Maybe VpcOutputSettings,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'channelSummary_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'inputSpecification', 'channelSummary_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'arn', 'channelSummary_arn' - The unique arn of the channel.
--
-- 'id', 'channelSummary_id' - The unique id of the channel.
--
-- 'pipelinesRunningCount', 'channelSummary_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'channelClass', 'channelSummary_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'logLevel', 'channelSummary_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'destinations', 'channelSummary_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'state', 'channelSummary_state' - Undocumented member.
--
-- 'name', 'channelSummary_name' - The name of the channel. (user-mutable)
--
-- 'inputAttachments', 'channelSummary_inputAttachments' - List of input attachments for channel.
--
-- 'tags', 'channelSummary_tags' - A collection of key-value pairs.
--
-- 'egressEndpoints', 'channelSummary_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'vpc', 'channelSummary_vpc' - Settings for VPC output
--
-- 'cdiInputSpecification', 'channelSummary_cdiInputSpecification' - Specification of CDI inputs for this channel
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { roleArn = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      destinations = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      tags = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing,
      vpc = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
channelSummary_roleArn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_roleArn = Lens.lens (\ChannelSummary' {roleArn} -> roleArn) (\s@ChannelSummary' {} a -> s {roleArn = a} :: ChannelSummary)

-- | Specification of network and file inputs for this channel
channelSummary_inputSpecification :: Lens.Lens' ChannelSummary (Prelude.Maybe InputSpecification)
channelSummary_inputSpecification = Lens.lens (\ChannelSummary' {inputSpecification} -> inputSpecification) (\s@ChannelSummary' {} a -> s {inputSpecification = a} :: ChannelSummary)

-- | The unique arn of the channel.
channelSummary_arn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_arn = Lens.lens (\ChannelSummary' {arn} -> arn) (\s@ChannelSummary' {} a -> s {arn = a} :: ChannelSummary)

-- | The unique id of the channel.
channelSummary_id :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_id = Lens.lens (\ChannelSummary' {id} -> id) (\s@ChannelSummary' {} a -> s {id = a} :: ChannelSummary)

-- | The number of currently healthy pipelines.
channelSummary_pipelinesRunningCount :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Int)
channelSummary_pipelinesRunningCount = Lens.lens (\ChannelSummary' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@ChannelSummary' {} a -> s {pipelinesRunningCount = a} :: ChannelSummary)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
channelSummary_channelClass :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelClass)
channelSummary_channelClass = Lens.lens (\ChannelSummary' {channelClass} -> channelClass) (\s@ChannelSummary' {} a -> s {channelClass = a} :: ChannelSummary)

-- | The log level being written to CloudWatch Logs.
channelSummary_logLevel :: Lens.Lens' ChannelSummary (Prelude.Maybe LogLevel)
channelSummary_logLevel = Lens.lens (\ChannelSummary' {logLevel} -> logLevel) (\s@ChannelSummary' {} a -> s {logLevel = a} :: ChannelSummary)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
channelSummary_destinations :: Lens.Lens' ChannelSummary (Prelude.Maybe [OutputDestination])
channelSummary_destinations = Lens.lens (\ChannelSummary' {destinations} -> destinations) (\s@ChannelSummary' {} a -> s {destinations = a} :: ChannelSummary) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
channelSummary_state :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelState)
channelSummary_state = Lens.lens (\ChannelSummary' {state} -> state) (\s@ChannelSummary' {} a -> s {state = a} :: ChannelSummary)

-- | The name of the channel. (user-mutable)
channelSummary_name :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary)

-- | List of input attachments for channel.
channelSummary_inputAttachments :: Lens.Lens' ChannelSummary (Prelude.Maybe [InputAttachment])
channelSummary_inputAttachments = Lens.lens (\ChannelSummary' {inputAttachments} -> inputAttachments) (\s@ChannelSummary' {} a -> s {inputAttachments = a} :: ChannelSummary) Prelude.. Lens.mapping Lens._Coerce

-- | A collection of key-value pairs.
channelSummary_tags :: Lens.Lens' ChannelSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channelSummary_tags = Lens.lens (\ChannelSummary' {tags} -> tags) (\s@ChannelSummary' {} a -> s {tags = a} :: ChannelSummary) Prelude.. Lens.mapping Lens._Coerce

-- | The endpoints where outgoing connections initiate from
channelSummary_egressEndpoints :: Lens.Lens' ChannelSummary (Prelude.Maybe [ChannelEgressEndpoint])
channelSummary_egressEndpoints = Lens.lens (\ChannelSummary' {egressEndpoints} -> egressEndpoints) (\s@ChannelSummary' {} a -> s {egressEndpoints = a} :: ChannelSummary) Prelude.. Lens.mapping Lens._Coerce

-- | Settings for VPC output
channelSummary_vpc :: Lens.Lens' ChannelSummary (Prelude.Maybe VpcOutputSettings)
channelSummary_vpc = Lens.lens (\ChannelSummary' {vpc} -> vpc) (\s@ChannelSummary' {} a -> s {vpc = a} :: ChannelSummary)

-- | Specification of CDI inputs for this channel
channelSummary_cdiInputSpecification :: Lens.Lens' ChannelSummary (Prelude.Maybe CdiInputSpecification)
channelSummary_cdiInputSpecification = Lens.lens (\ChannelSummary' {cdiInputSpecification} -> cdiInputSpecification) (\s@ChannelSummary' {} a -> s {cdiInputSpecification = a} :: ChannelSummary)

instance Core.FromJSON ChannelSummary where
  parseJSON =
    Core.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Core..:? "roleArn")
            Prelude.<*> (x Core..:? "inputSpecification")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "pipelinesRunningCount")
            Prelude.<*> (x Core..:? "channelClass")
            Prelude.<*> (x Core..:? "logLevel")
            Prelude.<*> (x Core..:? "destinations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> ( x Core..:? "inputAttachments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "egressEndpoints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "vpc")
            Prelude.<*> (x Core..:? "cdiInputSpecification")
      )

instance Prelude.Hashable ChannelSummary

instance Prelude.NFData ChannelSummary
