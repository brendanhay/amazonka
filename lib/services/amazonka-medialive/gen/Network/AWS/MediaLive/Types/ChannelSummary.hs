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
-- Module      : Amazonka.MediaLive.Types.ChannelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ChannelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.CdiInputSpecification
import Amazonka.MediaLive.Types.ChannelClass
import Amazonka.MediaLive.Types.ChannelEgressEndpoint
import Amazonka.MediaLive.Types.ChannelState
import Amazonka.MediaLive.Types.InputAttachment
import Amazonka.MediaLive.Types.InputSpecification
import Amazonka.MediaLive.Types.LogLevel
import Amazonka.MediaLive.Types.OutputDestination
import Amazonka.MediaLive.Types.VpcOutputSettingsDescription
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { state :: Prelude.Maybe ChannelState,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | The unique arn of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | A list of destinations of the channel. For UDP outputs, there is one
    -- destination per output. For other types (HLS, for example), there is one
    -- destination per packager.
    destinations :: Prelude.Maybe [OutputDestination],
    -- | The name of the channel. (user-mutable)
    name :: Prelude.Maybe Prelude.Text,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | The unique id of the channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Prelude.Maybe ChannelClass,
    -- | Settings for any VPC outputs.
    vpc :: Prelude.Maybe VpcOutputSettingsDescription,
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Prelude.Maybe [ChannelEgressEndpoint],
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text
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
-- 'state', 'channelSummary_state' - Undocumented member.
--
-- 'logLevel', 'channelSummary_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'arn', 'channelSummary_arn' - The unique arn of the channel.
--
-- 'pipelinesRunningCount', 'channelSummary_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'inputSpecification', 'channelSummary_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'inputAttachments', 'channelSummary_inputAttachments' - List of input attachments for channel.
--
-- 'destinations', 'channelSummary_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'name', 'channelSummary_name' - The name of the channel. (user-mutable)
--
-- 'cdiInputSpecification', 'channelSummary_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'id', 'channelSummary_id' - The unique id of the channel.
--
-- 'channelClass', 'channelSummary_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'vpc', 'channelSummary_vpc' - Settings for any VPC outputs.
--
-- 'egressEndpoints', 'channelSummary_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'tags', 'channelSummary_tags' - A collection of key-value pairs.
--
-- 'roleArn', 'channelSummary_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { state = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      arn = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      id = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      vpc = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing,
      tags = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | Undocumented member.
channelSummary_state :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelState)
channelSummary_state = Lens.lens (\ChannelSummary' {state} -> state) (\s@ChannelSummary' {} a -> s {state = a} :: ChannelSummary)

-- | The log level being written to CloudWatch Logs.
channelSummary_logLevel :: Lens.Lens' ChannelSummary (Prelude.Maybe LogLevel)
channelSummary_logLevel = Lens.lens (\ChannelSummary' {logLevel} -> logLevel) (\s@ChannelSummary' {} a -> s {logLevel = a} :: ChannelSummary)

-- | The unique arn of the channel.
channelSummary_arn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_arn = Lens.lens (\ChannelSummary' {arn} -> arn) (\s@ChannelSummary' {} a -> s {arn = a} :: ChannelSummary)

-- | The number of currently healthy pipelines.
channelSummary_pipelinesRunningCount :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Int)
channelSummary_pipelinesRunningCount = Lens.lens (\ChannelSummary' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@ChannelSummary' {} a -> s {pipelinesRunningCount = a} :: ChannelSummary)

-- | Specification of network and file inputs for this channel
channelSummary_inputSpecification :: Lens.Lens' ChannelSummary (Prelude.Maybe InputSpecification)
channelSummary_inputSpecification = Lens.lens (\ChannelSummary' {inputSpecification} -> inputSpecification) (\s@ChannelSummary' {} a -> s {inputSpecification = a} :: ChannelSummary)

-- | List of input attachments for channel.
channelSummary_inputAttachments :: Lens.Lens' ChannelSummary (Prelude.Maybe [InputAttachment])
channelSummary_inputAttachments = Lens.lens (\ChannelSummary' {inputAttachments} -> inputAttachments) (\s@ChannelSummary' {} a -> s {inputAttachments = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
channelSummary_destinations :: Lens.Lens' ChannelSummary (Prelude.Maybe [OutputDestination])
channelSummary_destinations = Lens.lens (\ChannelSummary' {destinations} -> destinations) (\s@ChannelSummary' {} a -> s {destinations = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel. (user-mutable)
channelSummary_name :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary)

-- | Specification of CDI inputs for this channel
channelSummary_cdiInputSpecification :: Lens.Lens' ChannelSummary (Prelude.Maybe CdiInputSpecification)
channelSummary_cdiInputSpecification = Lens.lens (\ChannelSummary' {cdiInputSpecification} -> cdiInputSpecification) (\s@ChannelSummary' {} a -> s {cdiInputSpecification = a} :: ChannelSummary)

-- | The unique id of the channel.
channelSummary_id :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_id = Lens.lens (\ChannelSummary' {id} -> id) (\s@ChannelSummary' {} a -> s {id = a} :: ChannelSummary)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
channelSummary_channelClass :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelClass)
channelSummary_channelClass = Lens.lens (\ChannelSummary' {channelClass} -> channelClass) (\s@ChannelSummary' {} a -> s {channelClass = a} :: ChannelSummary)

-- | Settings for any VPC outputs.
channelSummary_vpc :: Lens.Lens' ChannelSummary (Prelude.Maybe VpcOutputSettingsDescription)
channelSummary_vpc = Lens.lens (\ChannelSummary' {vpc} -> vpc) (\s@ChannelSummary' {} a -> s {vpc = a} :: ChannelSummary)

-- | The endpoints where outgoing connections initiate from
channelSummary_egressEndpoints :: Lens.Lens' ChannelSummary (Prelude.Maybe [ChannelEgressEndpoint])
channelSummary_egressEndpoints = Lens.lens (\ChannelSummary' {egressEndpoints} -> egressEndpoints) (\s@ChannelSummary' {} a -> s {egressEndpoints = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | A collection of key-value pairs.
channelSummary_tags :: Lens.Lens' ChannelSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channelSummary_tags = Lens.lens (\ChannelSummary' {tags} -> tags) (\s@ChannelSummary' {} a -> s {tags = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
channelSummary_roleArn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_roleArn = Lens.lens (\ChannelSummary' {roleArn} -> roleArn) (\s@ChannelSummary' {} a -> s {roleArn = a} :: ChannelSummary)

instance Core.FromJSON ChannelSummary where
  parseJSON =
    Core.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "logLevel")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "pipelinesRunningCount")
            Prelude.<*> (x Core..:? "inputSpecification")
            Prelude.<*> ( x Core..:? "inputAttachments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "destinations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "cdiInputSpecification")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "channelClass")
            Prelude.<*> (x Core..:? "vpc")
            Prelude.<*> ( x Core..:? "egressEndpoints"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable ChannelSummary

instance Prelude.NFData ChannelSummary
