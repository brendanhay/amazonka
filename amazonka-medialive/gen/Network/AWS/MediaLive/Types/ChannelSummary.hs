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

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | The Amazon Resource Name (ARN) of the role assumed when running the
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
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Core.Maybe [ChannelEgressEndpoint],
    -- | Settings for VPC output
    vpc :: Core.Maybe VpcOutputSettings,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Core.Maybe CdiInputSpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { roleArn = Core.Nothing,
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
      egressEndpoints = Core.Nothing,
      vpc = Core.Nothing,
      cdiInputSpecification = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
channelSummary_roleArn :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
channelSummary_roleArn = Lens.lens (\ChannelSummary' {roleArn} -> roleArn) (\s@ChannelSummary' {} a -> s {roleArn = a} :: ChannelSummary)

-- | Specification of network and file inputs for this channel
channelSummary_inputSpecification :: Lens.Lens' ChannelSummary (Core.Maybe InputSpecification)
channelSummary_inputSpecification = Lens.lens (\ChannelSummary' {inputSpecification} -> inputSpecification) (\s@ChannelSummary' {} a -> s {inputSpecification = a} :: ChannelSummary)

-- | The unique arn of the channel.
channelSummary_arn :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
channelSummary_arn = Lens.lens (\ChannelSummary' {arn} -> arn) (\s@ChannelSummary' {} a -> s {arn = a} :: ChannelSummary)

-- | The unique id of the channel.
channelSummary_id :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
channelSummary_id = Lens.lens (\ChannelSummary' {id} -> id) (\s@ChannelSummary' {} a -> s {id = a} :: ChannelSummary)

-- | The number of currently healthy pipelines.
channelSummary_pipelinesRunningCount :: Lens.Lens' ChannelSummary (Core.Maybe Core.Int)
channelSummary_pipelinesRunningCount = Lens.lens (\ChannelSummary' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@ChannelSummary' {} a -> s {pipelinesRunningCount = a} :: ChannelSummary)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
channelSummary_channelClass :: Lens.Lens' ChannelSummary (Core.Maybe ChannelClass)
channelSummary_channelClass = Lens.lens (\ChannelSummary' {channelClass} -> channelClass) (\s@ChannelSummary' {} a -> s {channelClass = a} :: ChannelSummary)

-- | The log level being written to CloudWatch Logs.
channelSummary_logLevel :: Lens.Lens' ChannelSummary (Core.Maybe LogLevel)
channelSummary_logLevel = Lens.lens (\ChannelSummary' {logLevel} -> logLevel) (\s@ChannelSummary' {} a -> s {logLevel = a} :: ChannelSummary)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
channelSummary_destinations :: Lens.Lens' ChannelSummary (Core.Maybe [OutputDestination])
channelSummary_destinations = Lens.lens (\ChannelSummary' {destinations} -> destinations) (\s@ChannelSummary' {} a -> s {destinations = a} :: ChannelSummary) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
channelSummary_state :: Lens.Lens' ChannelSummary (Core.Maybe ChannelState)
channelSummary_state = Lens.lens (\ChannelSummary' {state} -> state) (\s@ChannelSummary' {} a -> s {state = a} :: ChannelSummary)

-- | The name of the channel. (user-mutable)
channelSummary_name :: Lens.Lens' ChannelSummary (Core.Maybe Core.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary)

-- | List of input attachments for channel.
channelSummary_inputAttachments :: Lens.Lens' ChannelSummary (Core.Maybe [InputAttachment])
channelSummary_inputAttachments = Lens.lens (\ChannelSummary' {inputAttachments} -> inputAttachments) (\s@ChannelSummary' {} a -> s {inputAttachments = a} :: ChannelSummary) Core.. Lens.mapping Lens._Coerce

-- | A collection of key-value pairs.
channelSummary_tags :: Lens.Lens' ChannelSummary (Core.Maybe (Core.HashMap Core.Text Core.Text))
channelSummary_tags = Lens.lens (\ChannelSummary' {tags} -> tags) (\s@ChannelSummary' {} a -> s {tags = a} :: ChannelSummary) Core.. Lens.mapping Lens._Coerce

-- | The endpoints where outgoing connections initiate from
channelSummary_egressEndpoints :: Lens.Lens' ChannelSummary (Core.Maybe [ChannelEgressEndpoint])
channelSummary_egressEndpoints = Lens.lens (\ChannelSummary' {egressEndpoints} -> egressEndpoints) (\s@ChannelSummary' {} a -> s {egressEndpoints = a} :: ChannelSummary) Core.. Lens.mapping Lens._Coerce

-- | Settings for VPC output
channelSummary_vpc :: Lens.Lens' ChannelSummary (Core.Maybe VpcOutputSettings)
channelSummary_vpc = Lens.lens (\ChannelSummary' {vpc} -> vpc) (\s@ChannelSummary' {} a -> s {vpc = a} :: ChannelSummary)

-- | Specification of CDI inputs for this channel
channelSummary_cdiInputSpecification :: Lens.Lens' ChannelSummary (Core.Maybe CdiInputSpecification)
channelSummary_cdiInputSpecification = Lens.lens (\ChannelSummary' {cdiInputSpecification} -> cdiInputSpecification) (\s@ChannelSummary' {} a -> s {cdiInputSpecification = a} :: ChannelSummary)

instance Core.FromJSON ChannelSummary where
  parseJSON =
    Core.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Core.<$> (x Core..:? "roleArn")
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
            Core.<*> (x Core..:? "egressEndpoints" Core..!= Core.mempty)
            Core.<*> (x Core..:? "vpc")
            Core.<*> (x Core..:? "cdiInputSpecification")
      )

instance Core.Hashable ChannelSummary

instance Core.NFData ChannelSummary
