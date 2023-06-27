{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.StartChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing channel
module Amazonka.MediaLive.StartChannel
  ( -- * Creating a Request
    StartChannel (..),
    newStartChannel,

    -- * Request Lenses
    startChannel_channelId,

    -- * Destructuring the Response
    StartChannelResponse (..),
    newStartChannelResponse,

    -- * Response Lenses
    startChannelResponse_arn,
    startChannelResponse_cdiInputSpecification,
    startChannelResponse_channelClass,
    startChannelResponse_destinations,
    startChannelResponse_egressEndpoints,
    startChannelResponse_encoderSettings,
    startChannelResponse_id,
    startChannelResponse_inputAttachments,
    startChannelResponse_inputSpecification,
    startChannelResponse_logLevel,
    startChannelResponse_maintenance,
    startChannelResponse_name,
    startChannelResponse_pipelineDetails,
    startChannelResponse_pipelinesRunningCount,
    startChannelResponse_roleArn,
    startChannelResponse_state,
    startChannelResponse_tags,
    startChannelResponse_vpc,
    startChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for StartChannelRequest
--
-- /See:/ 'newStartChannel' smart constructor.
data StartChannel = StartChannel'
  { -- | A request to start a channel
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'startChannel_channelId' - A request to start a channel
newStartChannel ::
  -- | 'channelId'
  Prelude.Text ->
  StartChannel
newStartChannel pChannelId_ =
  StartChannel' {channelId = pChannelId_}

-- | A request to start a channel
startChannel_channelId :: Lens.Lens' StartChannel Prelude.Text
startChannel_channelId = Lens.lens (\StartChannel' {channelId} -> channelId) (\s@StartChannel' {} a -> s {channelId = a} :: StartChannel)

instance Core.AWSRequest StartChannel where
  type AWSResponse StartChannel = StartChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChannelResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "cdiInputSpecification")
            Prelude.<*> (x Data..?> "channelClass")
            Prelude.<*> (x Data..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "egressEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "encoderSettings")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> ( x
                            Data..?> "inputAttachments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "inputSpecification")
            Prelude.<*> (x Data..?> "logLevel")
            Prelude.<*> (x Data..?> "maintenance")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> ( x
                            Data..?> "pipelineDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "pipelinesRunningCount")
            Prelude.<*> (x Data..?> "roleArn")
            Prelude.<*> (x Data..?> "state")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "vpc")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartChannel where
  hashWithSalt _salt StartChannel' {..} =
    _salt `Prelude.hashWithSalt` channelId

instance Prelude.NFData StartChannel where
  rnf StartChannel' {..} = Prelude.rnf channelId

instance Data.ToHeaders StartChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartChannel where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartChannel where
  toPath StartChannel' {..} =
    Prelude.mconcat
      ["/prod/channels/", Data.toBS channelId, "/start"]

instance Data.ToQuery StartChannel where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for StartChannelResponse
--
-- /See:/ 'newStartChannelResponse' smart constructor.
data StartChannelResponse = StartChannelResponse'
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
    encoderSettings :: Prelude.Maybe EncoderSettings,
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
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Prelude.Maybe [PipelineDetail],
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe ChannelState,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Settings for VPC output
    vpc :: Prelude.Maybe VpcOutputSettingsDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'startChannelResponse_arn' - The unique arn of the channel.
--
-- 'cdiInputSpecification', 'startChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'channelClass', 'startChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'destinations', 'startChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'egressEndpoints', 'startChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'encoderSettings', 'startChannelResponse_encoderSettings' - Undocumented member.
--
-- 'id', 'startChannelResponse_id' - The unique id of the channel.
--
-- 'inputAttachments', 'startChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'inputSpecification', 'startChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'logLevel', 'startChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'maintenance', 'startChannelResponse_maintenance' - Maintenance settings for this channel.
--
-- 'name', 'startChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'pipelineDetails', 'startChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'pipelinesRunningCount', 'startChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'roleArn', 'startChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'state', 'startChannelResponse_state' - Undocumented member.
--
-- 'tags', 'startChannelResponse_tags' - A collection of key-value pairs.
--
-- 'vpc', 'startChannelResponse_vpc' - Settings for VPC output
--
-- 'httpStatus', 'startChannelResponse_httpStatus' - The response's http status code.
newStartChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartChannelResponse
newStartChannelResponse pHttpStatus_ =
  StartChannelResponse'
    { arn = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      destinations = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing,
      encoderSettings = Prelude.Nothing,
      id = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      maintenance = Prelude.Nothing,
      name = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpc = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique arn of the channel.
startChannelResponse_arn :: Lens.Lens' StartChannelResponse (Prelude.Maybe Prelude.Text)
startChannelResponse_arn = Lens.lens (\StartChannelResponse' {arn} -> arn) (\s@StartChannelResponse' {} a -> s {arn = a} :: StartChannelResponse)

-- | Specification of CDI inputs for this channel
startChannelResponse_cdiInputSpecification :: Lens.Lens' StartChannelResponse (Prelude.Maybe CdiInputSpecification)
startChannelResponse_cdiInputSpecification = Lens.lens (\StartChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@StartChannelResponse' {} a -> s {cdiInputSpecification = a} :: StartChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
startChannelResponse_channelClass :: Lens.Lens' StartChannelResponse (Prelude.Maybe ChannelClass)
startChannelResponse_channelClass = Lens.lens (\StartChannelResponse' {channelClass} -> channelClass) (\s@StartChannelResponse' {} a -> s {channelClass = a} :: StartChannelResponse)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
startChannelResponse_destinations :: Lens.Lens' StartChannelResponse (Prelude.Maybe [OutputDestination])
startChannelResponse_destinations = Lens.lens (\StartChannelResponse' {destinations} -> destinations) (\s@StartChannelResponse' {} a -> s {destinations = a} :: StartChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The endpoints where outgoing connections initiate from
startChannelResponse_egressEndpoints :: Lens.Lens' StartChannelResponse (Prelude.Maybe [ChannelEgressEndpoint])
startChannelResponse_egressEndpoints = Lens.lens (\StartChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@StartChannelResponse' {} a -> s {egressEndpoints = a} :: StartChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
startChannelResponse_encoderSettings :: Lens.Lens' StartChannelResponse (Prelude.Maybe EncoderSettings)
startChannelResponse_encoderSettings = Lens.lens (\StartChannelResponse' {encoderSettings} -> encoderSettings) (\s@StartChannelResponse' {} a -> s {encoderSettings = a} :: StartChannelResponse)

-- | The unique id of the channel.
startChannelResponse_id :: Lens.Lens' StartChannelResponse (Prelude.Maybe Prelude.Text)
startChannelResponse_id = Lens.lens (\StartChannelResponse' {id} -> id) (\s@StartChannelResponse' {} a -> s {id = a} :: StartChannelResponse)

-- | List of input attachments for channel.
startChannelResponse_inputAttachments :: Lens.Lens' StartChannelResponse (Prelude.Maybe [InputAttachment])
startChannelResponse_inputAttachments = Lens.lens (\StartChannelResponse' {inputAttachments} -> inputAttachments) (\s@StartChannelResponse' {} a -> s {inputAttachments = a} :: StartChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specification of network and file inputs for this channel
startChannelResponse_inputSpecification :: Lens.Lens' StartChannelResponse (Prelude.Maybe InputSpecification)
startChannelResponse_inputSpecification = Lens.lens (\StartChannelResponse' {inputSpecification} -> inputSpecification) (\s@StartChannelResponse' {} a -> s {inputSpecification = a} :: StartChannelResponse)

-- | The log level being written to CloudWatch Logs.
startChannelResponse_logLevel :: Lens.Lens' StartChannelResponse (Prelude.Maybe LogLevel)
startChannelResponse_logLevel = Lens.lens (\StartChannelResponse' {logLevel} -> logLevel) (\s@StartChannelResponse' {} a -> s {logLevel = a} :: StartChannelResponse)

-- | Maintenance settings for this channel.
startChannelResponse_maintenance :: Lens.Lens' StartChannelResponse (Prelude.Maybe MaintenanceStatus)
startChannelResponse_maintenance = Lens.lens (\StartChannelResponse' {maintenance} -> maintenance) (\s@StartChannelResponse' {} a -> s {maintenance = a} :: StartChannelResponse)

-- | The name of the channel. (user-mutable)
startChannelResponse_name :: Lens.Lens' StartChannelResponse (Prelude.Maybe Prelude.Text)
startChannelResponse_name = Lens.lens (\StartChannelResponse' {name} -> name) (\s@StartChannelResponse' {} a -> s {name = a} :: StartChannelResponse)

-- | Runtime details for the pipelines of a running channel.
startChannelResponse_pipelineDetails :: Lens.Lens' StartChannelResponse (Prelude.Maybe [PipelineDetail])
startChannelResponse_pipelineDetails = Lens.lens (\StartChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@StartChannelResponse' {} a -> s {pipelineDetails = a} :: StartChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of currently healthy pipelines.
startChannelResponse_pipelinesRunningCount :: Lens.Lens' StartChannelResponse (Prelude.Maybe Prelude.Int)
startChannelResponse_pipelinesRunningCount = Lens.lens (\StartChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@StartChannelResponse' {} a -> s {pipelinesRunningCount = a} :: StartChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
startChannelResponse_roleArn :: Lens.Lens' StartChannelResponse (Prelude.Maybe Prelude.Text)
startChannelResponse_roleArn = Lens.lens (\StartChannelResponse' {roleArn} -> roleArn) (\s@StartChannelResponse' {} a -> s {roleArn = a} :: StartChannelResponse)

-- | Undocumented member.
startChannelResponse_state :: Lens.Lens' StartChannelResponse (Prelude.Maybe ChannelState)
startChannelResponse_state = Lens.lens (\StartChannelResponse' {state} -> state) (\s@StartChannelResponse' {} a -> s {state = a} :: StartChannelResponse)

-- | A collection of key-value pairs.
startChannelResponse_tags :: Lens.Lens' StartChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startChannelResponse_tags = Lens.lens (\StartChannelResponse' {tags} -> tags) (\s@StartChannelResponse' {} a -> s {tags = a} :: StartChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Settings for VPC output
startChannelResponse_vpc :: Lens.Lens' StartChannelResponse (Prelude.Maybe VpcOutputSettingsDescription)
startChannelResponse_vpc = Lens.lens (\StartChannelResponse' {vpc} -> vpc) (\s@StartChannelResponse' {} a -> s {vpc = a} :: StartChannelResponse)

-- | The response's http status code.
startChannelResponse_httpStatus :: Lens.Lens' StartChannelResponse Prelude.Int
startChannelResponse_httpStatus = Lens.lens (\StartChannelResponse' {httpStatus} -> httpStatus) (\s@StartChannelResponse' {} a -> s {httpStatus = a} :: StartChannelResponse)

instance Prelude.NFData StartChannelResponse where
  rnf StartChannelResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf cdiInputSpecification
      `Prelude.seq` Prelude.rnf channelClass
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf egressEndpoints
      `Prelude.seq` Prelude.rnf encoderSettings
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf inputAttachments
      `Prelude.seq` Prelude.rnf inputSpecification
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf maintenance
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf pipelineDetails
      `Prelude.seq` Prelude.rnf pipelinesRunningCount
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf httpStatus
