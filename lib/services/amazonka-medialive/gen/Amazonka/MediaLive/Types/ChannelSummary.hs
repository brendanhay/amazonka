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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ChannelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.CdiInputSpecification
import Amazonka.MediaLive.Types.ChannelClass
import Amazonka.MediaLive.Types.ChannelEgressEndpoint
import Amazonka.MediaLive.Types.ChannelState
import Amazonka.MediaLive.Types.InputAttachment
import Amazonka.MediaLive.Types.InputSpecification
import Amazonka.MediaLive.Types.LogLevel
import Amazonka.MediaLive.Types.MaintenanceStatus
import Amazonka.MediaLive.Types.OutputDestination
import Amazonka.MediaLive.Types.VpcOutputSettingsDescription
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for ChannelSummary
--
-- /See:/ 'newChannelSummary' smart constructor.
data ChannelSummary = ChannelSummary'
  { -- | The unique arn of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Prelude.Maybe ChannelClass,
    -- | A list of destinations of the channel. For UDP outputs, there is one
    -- destination per output. For other types (HLS, for example), there is one
    -- destination per packager.
    destinations :: Prelude.Maybe [OutputDestination],
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Prelude.Maybe [ChannelEgressEndpoint],
    -- | The unique id of the channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | Maintenance settings for this channel.
    maintenance :: Prelude.Maybe MaintenanceStatus,
    -- | The name of the channel. (user-mutable)
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe ChannelState,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Settings for any VPC outputs.
    vpc :: Prelude.Maybe VpcOutputSettingsDescription
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
-- 'arn', 'channelSummary_arn' - The unique arn of the channel.
--
-- 'cdiInputSpecification', 'channelSummary_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'channelClass', 'channelSummary_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'destinations', 'channelSummary_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'egressEndpoints', 'channelSummary_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'id', 'channelSummary_id' - The unique id of the channel.
--
-- 'inputAttachments', 'channelSummary_inputAttachments' - List of input attachments for channel.
--
-- 'inputSpecification', 'channelSummary_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'logLevel', 'channelSummary_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'maintenance', 'channelSummary_maintenance' - Maintenance settings for this channel.
--
-- 'name', 'channelSummary_name' - The name of the channel. (user-mutable)
--
-- 'pipelinesRunningCount', 'channelSummary_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'roleArn', 'channelSummary_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'state', 'channelSummary_state' - Undocumented member.
--
-- 'tags', 'channelSummary_tags' - A collection of key-value pairs.
--
-- 'vpc', 'channelSummary_vpc' - Settings for any VPC outputs.
newChannelSummary ::
  ChannelSummary
newChannelSummary =
  ChannelSummary'
    { arn = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      destinations = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing,
      id = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      maintenance = Prelude.Nothing,
      name = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | The unique arn of the channel.
channelSummary_arn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_arn = Lens.lens (\ChannelSummary' {arn} -> arn) (\s@ChannelSummary' {} a -> s {arn = a} :: ChannelSummary)

-- | Specification of CDI inputs for this channel
channelSummary_cdiInputSpecification :: Lens.Lens' ChannelSummary (Prelude.Maybe CdiInputSpecification)
channelSummary_cdiInputSpecification = Lens.lens (\ChannelSummary' {cdiInputSpecification} -> cdiInputSpecification) (\s@ChannelSummary' {} a -> s {cdiInputSpecification = a} :: ChannelSummary)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
channelSummary_channelClass :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelClass)
channelSummary_channelClass = Lens.lens (\ChannelSummary' {channelClass} -> channelClass) (\s@ChannelSummary' {} a -> s {channelClass = a} :: ChannelSummary)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
channelSummary_destinations :: Lens.Lens' ChannelSummary (Prelude.Maybe [OutputDestination])
channelSummary_destinations = Lens.lens (\ChannelSummary' {destinations} -> destinations) (\s@ChannelSummary' {} a -> s {destinations = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | The endpoints where outgoing connections initiate from
channelSummary_egressEndpoints :: Lens.Lens' ChannelSummary (Prelude.Maybe [ChannelEgressEndpoint])
channelSummary_egressEndpoints = Lens.lens (\ChannelSummary' {egressEndpoints} -> egressEndpoints) (\s@ChannelSummary' {} a -> s {egressEndpoints = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | The unique id of the channel.
channelSummary_id :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_id = Lens.lens (\ChannelSummary' {id} -> id) (\s@ChannelSummary' {} a -> s {id = a} :: ChannelSummary)

-- | List of input attachments for channel.
channelSummary_inputAttachments :: Lens.Lens' ChannelSummary (Prelude.Maybe [InputAttachment])
channelSummary_inputAttachments = Lens.lens (\ChannelSummary' {inputAttachments} -> inputAttachments) (\s@ChannelSummary' {} a -> s {inputAttachments = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | Specification of network and file inputs for this channel
channelSummary_inputSpecification :: Lens.Lens' ChannelSummary (Prelude.Maybe InputSpecification)
channelSummary_inputSpecification = Lens.lens (\ChannelSummary' {inputSpecification} -> inputSpecification) (\s@ChannelSummary' {} a -> s {inputSpecification = a} :: ChannelSummary)

-- | The log level being written to CloudWatch Logs.
channelSummary_logLevel :: Lens.Lens' ChannelSummary (Prelude.Maybe LogLevel)
channelSummary_logLevel = Lens.lens (\ChannelSummary' {logLevel} -> logLevel) (\s@ChannelSummary' {} a -> s {logLevel = a} :: ChannelSummary)

-- | Maintenance settings for this channel.
channelSummary_maintenance :: Lens.Lens' ChannelSummary (Prelude.Maybe MaintenanceStatus)
channelSummary_maintenance = Lens.lens (\ChannelSummary' {maintenance} -> maintenance) (\s@ChannelSummary' {} a -> s {maintenance = a} :: ChannelSummary)

-- | The name of the channel. (user-mutable)
channelSummary_name :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_name = Lens.lens (\ChannelSummary' {name} -> name) (\s@ChannelSummary' {} a -> s {name = a} :: ChannelSummary)

-- | The number of currently healthy pipelines.
channelSummary_pipelinesRunningCount :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Int)
channelSummary_pipelinesRunningCount = Lens.lens (\ChannelSummary' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@ChannelSummary' {} a -> s {pipelinesRunningCount = a} :: ChannelSummary)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
channelSummary_roleArn :: Lens.Lens' ChannelSummary (Prelude.Maybe Prelude.Text)
channelSummary_roleArn = Lens.lens (\ChannelSummary' {roleArn} -> roleArn) (\s@ChannelSummary' {} a -> s {roleArn = a} :: ChannelSummary)

-- | Undocumented member.
channelSummary_state :: Lens.Lens' ChannelSummary (Prelude.Maybe ChannelState)
channelSummary_state = Lens.lens (\ChannelSummary' {state} -> state) (\s@ChannelSummary' {} a -> s {state = a} :: ChannelSummary)

-- | A collection of key-value pairs.
channelSummary_tags :: Lens.Lens' ChannelSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
channelSummary_tags = Lens.lens (\ChannelSummary' {tags} -> tags) (\s@ChannelSummary' {} a -> s {tags = a} :: ChannelSummary) Prelude.. Lens.mapping Lens.coerced

-- | Settings for any VPC outputs.
channelSummary_vpc :: Lens.Lens' ChannelSummary (Prelude.Maybe VpcOutputSettingsDescription)
channelSummary_vpc = Lens.lens (\ChannelSummary' {vpc} -> vpc) (\s@ChannelSummary' {} a -> s {vpc = a} :: ChannelSummary)

instance Data.FromJSON ChannelSummary where
  parseJSON =
    Data.withObject
      "ChannelSummary"
      ( \x ->
          ChannelSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "cdiInputSpecification")
            Prelude.<*> (x Data..:? "channelClass")
            Prelude.<*> (x Data..:? "destinations" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "egressEndpoints"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> ( x
                            Data..:? "inputAttachments"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "inputSpecification")
            Prelude.<*> (x Data..:? "logLevel")
            Prelude.<*> (x Data..:? "maintenance")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "pipelinesRunningCount")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpc")
      )

instance Prelude.Hashable ChannelSummary where
  hashWithSalt _salt ChannelSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` cdiInputSpecification
      `Prelude.hashWithSalt` channelClass
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` egressEndpoints
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` inputAttachments
      `Prelude.hashWithSalt` inputSpecification
      `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` maintenance
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` pipelinesRunningCount
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData ChannelSummary where
  rnf ChannelSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf cdiInputSpecification `Prelude.seq`
        Prelude.rnf channelClass `Prelude.seq`
          Prelude.rnf destinations `Prelude.seq`
            Prelude.rnf egressEndpoints `Prelude.seq`
              Prelude.rnf id `Prelude.seq`
                Prelude.rnf inputAttachments `Prelude.seq`
                  Prelude.rnf inputSpecification `Prelude.seq`
                    Prelude.rnf logLevel `Prelude.seq`
                      Prelude.rnf maintenance `Prelude.seq`
                        Prelude.rnf name `Prelude.seq`
                          Prelude.rnf pipelinesRunningCount `Prelude.seq`
                            Prelude.rnf roleArn `Prelude.seq`
                              Prelude.rnf state `Prelude.seq`
                                Prelude.rnf tags `Prelude.seq`
                                  Prelude.rnf vpc
