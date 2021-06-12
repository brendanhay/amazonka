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
-- Module      : Network.AWS.MediaLive.StopChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running channel
module Network.AWS.MediaLive.StopChannel
  ( -- * Creating a Request
    StopChannel (..),
    newStopChannel,

    -- * Request Lenses
    stopChannel_channelId,

    -- * Destructuring the Response
    StopChannelResponse (..),
    newStopChannelResponse,

    -- * Response Lenses
    stopChannelResponse_encoderSettings,
    stopChannelResponse_roleArn,
    stopChannelResponse_inputSpecification,
    stopChannelResponse_arn,
    stopChannelResponse_id,
    stopChannelResponse_pipelinesRunningCount,
    stopChannelResponse_channelClass,
    stopChannelResponse_logLevel,
    stopChannelResponse_destinations,
    stopChannelResponse_state,
    stopChannelResponse_name,
    stopChannelResponse_inputAttachments,
    stopChannelResponse_tags,
    stopChannelResponse_pipelineDetails,
    stopChannelResponse_egressEndpoints,
    stopChannelResponse_vpc,
    stopChannelResponse_cdiInputSpecification,
    stopChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for StopChannelRequest
--
-- /See:/ 'newStopChannel' smart constructor.
data StopChannel = StopChannel'
  { -- | A request to stop a running channel
    channelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'stopChannel_channelId' - A request to stop a running channel
newStopChannel ::
  -- | 'channelId'
  Core.Text ->
  StopChannel
newStopChannel pChannelId_ =
  StopChannel' {channelId = pChannelId_}

-- | A request to stop a running channel
stopChannel_channelId :: Lens.Lens' StopChannel Core.Text
stopChannel_channelId = Lens.lens (\StopChannel' {channelId} -> channelId) (\s@StopChannel' {} a -> s {channelId = a} :: StopChannel)

instance Core.AWSRequest StopChannel where
  type AWSResponse StopChannel = StopChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopChannelResponse'
            Core.<$> (x Core..?> "encoderSettings")
            Core.<*> (x Core..?> "roleArn")
            Core.<*> (x Core..?> "inputSpecification")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "pipelinesRunningCount")
            Core.<*> (x Core..?> "channelClass")
            Core.<*> (x Core..?> "logLevel")
            Core.<*> (x Core..?> "destinations" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "state")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "inputAttachments" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "pipelineDetails" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "egressEndpoints" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "vpc")
            Core.<*> (x Core..?> "cdiInputSpecification")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopChannel

instance Core.NFData StopChannel

instance Core.ToHeaders StopChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopChannel where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath StopChannel where
  toPath StopChannel' {..} =
    Core.mconcat
      ["/prod/channels/", Core.toBS channelId, "/stop"]

instance Core.ToQuery StopChannel where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for StopChannelResponse
--
-- /See:/ 'newStopChannelResponse' smart constructor.
data StopChannelResponse = StopChannelResponse'
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
    cdiInputSpecification :: Core.Maybe CdiInputSpecification,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoderSettings', 'stopChannelResponse_encoderSettings' - Undocumented member.
--
-- 'roleArn', 'stopChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'inputSpecification', 'stopChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'arn', 'stopChannelResponse_arn' - The unique arn of the channel.
--
-- 'id', 'stopChannelResponse_id' - The unique id of the channel.
--
-- 'pipelinesRunningCount', 'stopChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'channelClass', 'stopChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'logLevel', 'stopChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'destinations', 'stopChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'state', 'stopChannelResponse_state' - Undocumented member.
--
-- 'name', 'stopChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'inputAttachments', 'stopChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'tags', 'stopChannelResponse_tags' - A collection of key-value pairs.
--
-- 'pipelineDetails', 'stopChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'egressEndpoints', 'stopChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'vpc', 'stopChannelResponse_vpc' - Settings for VPC output
--
-- 'cdiInputSpecification', 'stopChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'httpStatus', 'stopChannelResponse_httpStatus' - The response's http status code.
newStopChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopChannelResponse
newStopChannelResponse pHttpStatus_ =
  StopChannelResponse'
    { encoderSettings =
        Core.Nothing,
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
      cdiInputSpecification = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
stopChannelResponse_encoderSettings :: Lens.Lens' StopChannelResponse (Core.Maybe EncoderSettings)
stopChannelResponse_encoderSettings = Lens.lens (\StopChannelResponse' {encoderSettings} -> encoderSettings) (\s@StopChannelResponse' {} a -> s {encoderSettings = a} :: StopChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
stopChannelResponse_roleArn :: Lens.Lens' StopChannelResponse (Core.Maybe Core.Text)
stopChannelResponse_roleArn = Lens.lens (\StopChannelResponse' {roleArn} -> roleArn) (\s@StopChannelResponse' {} a -> s {roleArn = a} :: StopChannelResponse)

-- | Specification of network and file inputs for this channel
stopChannelResponse_inputSpecification :: Lens.Lens' StopChannelResponse (Core.Maybe InputSpecification)
stopChannelResponse_inputSpecification = Lens.lens (\StopChannelResponse' {inputSpecification} -> inputSpecification) (\s@StopChannelResponse' {} a -> s {inputSpecification = a} :: StopChannelResponse)

-- | The unique arn of the channel.
stopChannelResponse_arn :: Lens.Lens' StopChannelResponse (Core.Maybe Core.Text)
stopChannelResponse_arn = Lens.lens (\StopChannelResponse' {arn} -> arn) (\s@StopChannelResponse' {} a -> s {arn = a} :: StopChannelResponse)

-- | The unique id of the channel.
stopChannelResponse_id :: Lens.Lens' StopChannelResponse (Core.Maybe Core.Text)
stopChannelResponse_id = Lens.lens (\StopChannelResponse' {id} -> id) (\s@StopChannelResponse' {} a -> s {id = a} :: StopChannelResponse)

-- | The number of currently healthy pipelines.
stopChannelResponse_pipelinesRunningCount :: Lens.Lens' StopChannelResponse (Core.Maybe Core.Int)
stopChannelResponse_pipelinesRunningCount = Lens.lens (\StopChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@StopChannelResponse' {} a -> s {pipelinesRunningCount = a} :: StopChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
stopChannelResponse_channelClass :: Lens.Lens' StopChannelResponse (Core.Maybe ChannelClass)
stopChannelResponse_channelClass = Lens.lens (\StopChannelResponse' {channelClass} -> channelClass) (\s@StopChannelResponse' {} a -> s {channelClass = a} :: StopChannelResponse)

-- | The log level being written to CloudWatch Logs.
stopChannelResponse_logLevel :: Lens.Lens' StopChannelResponse (Core.Maybe LogLevel)
stopChannelResponse_logLevel = Lens.lens (\StopChannelResponse' {logLevel} -> logLevel) (\s@StopChannelResponse' {} a -> s {logLevel = a} :: StopChannelResponse)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
stopChannelResponse_destinations :: Lens.Lens' StopChannelResponse (Core.Maybe [OutputDestination])
stopChannelResponse_destinations = Lens.lens (\StopChannelResponse' {destinations} -> destinations) (\s@StopChannelResponse' {} a -> s {destinations = a} :: StopChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
stopChannelResponse_state :: Lens.Lens' StopChannelResponse (Core.Maybe ChannelState)
stopChannelResponse_state = Lens.lens (\StopChannelResponse' {state} -> state) (\s@StopChannelResponse' {} a -> s {state = a} :: StopChannelResponse)

-- | The name of the channel. (user-mutable)
stopChannelResponse_name :: Lens.Lens' StopChannelResponse (Core.Maybe Core.Text)
stopChannelResponse_name = Lens.lens (\StopChannelResponse' {name} -> name) (\s@StopChannelResponse' {} a -> s {name = a} :: StopChannelResponse)

-- | List of input attachments for channel.
stopChannelResponse_inputAttachments :: Lens.Lens' StopChannelResponse (Core.Maybe [InputAttachment])
stopChannelResponse_inputAttachments = Lens.lens (\StopChannelResponse' {inputAttachments} -> inputAttachments) (\s@StopChannelResponse' {} a -> s {inputAttachments = a} :: StopChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | A collection of key-value pairs.
stopChannelResponse_tags :: Lens.Lens' StopChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
stopChannelResponse_tags = Lens.lens (\StopChannelResponse' {tags} -> tags) (\s@StopChannelResponse' {} a -> s {tags = a} :: StopChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | Runtime details for the pipelines of a running channel.
stopChannelResponse_pipelineDetails :: Lens.Lens' StopChannelResponse (Core.Maybe [PipelineDetail])
stopChannelResponse_pipelineDetails = Lens.lens (\StopChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@StopChannelResponse' {} a -> s {pipelineDetails = a} :: StopChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | The endpoints where outgoing connections initiate from
stopChannelResponse_egressEndpoints :: Lens.Lens' StopChannelResponse (Core.Maybe [ChannelEgressEndpoint])
stopChannelResponse_egressEndpoints = Lens.lens (\StopChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@StopChannelResponse' {} a -> s {egressEndpoints = a} :: StopChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | Settings for VPC output
stopChannelResponse_vpc :: Lens.Lens' StopChannelResponse (Core.Maybe VpcOutputSettings)
stopChannelResponse_vpc = Lens.lens (\StopChannelResponse' {vpc} -> vpc) (\s@StopChannelResponse' {} a -> s {vpc = a} :: StopChannelResponse)

-- | Specification of CDI inputs for this channel
stopChannelResponse_cdiInputSpecification :: Lens.Lens' StopChannelResponse (Core.Maybe CdiInputSpecification)
stopChannelResponse_cdiInputSpecification = Lens.lens (\StopChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@StopChannelResponse' {} a -> s {cdiInputSpecification = a} :: StopChannelResponse)

-- | The response's http status code.
stopChannelResponse_httpStatus :: Lens.Lens' StopChannelResponse Core.Int
stopChannelResponse_httpStatus = Lens.lens (\StopChannelResponse' {httpStatus} -> httpStatus) (\s@StopChannelResponse' {} a -> s {httpStatus = a} :: StopChannelResponse)

instance Core.NFData StopChannelResponse
