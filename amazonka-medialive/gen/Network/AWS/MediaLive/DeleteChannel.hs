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
-- Module      : Network.AWS.MediaLive.DeleteChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts deletion of channel. The associated outputs are also deleted.
module Network.AWS.MediaLive.DeleteChannel
  ( -- * Creating a Request
    DeleteChannel (..),
    newDeleteChannel,

    -- * Request Lenses
    deleteChannel_channelId,

    -- * Destructuring the Response
    DeleteChannelResponse (..),
    newDeleteChannelResponse,

    -- * Response Lenses
    deleteChannelResponse_encoderSettings,
    deleteChannelResponse_roleArn,
    deleteChannelResponse_inputSpecification,
    deleteChannelResponse_arn,
    deleteChannelResponse_id,
    deleteChannelResponse_pipelinesRunningCount,
    deleteChannelResponse_channelClass,
    deleteChannelResponse_logLevel,
    deleteChannelResponse_destinations,
    deleteChannelResponse_state,
    deleteChannelResponse_name,
    deleteChannelResponse_inputAttachments,
    deleteChannelResponse_tags,
    deleteChannelResponse_pipelineDetails,
    deleteChannelResponse_egressEndpoints,
    deleteChannelResponse_vpc,
    deleteChannelResponse_cdiInputSpecification,
    deleteChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteChannelRequest
--
-- /See:/ 'newDeleteChannel' smart constructor.
data DeleteChannel = DeleteChannel'
  { -- | Unique ID of the channel.
    channelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'deleteChannel_channelId' - Unique ID of the channel.
newDeleteChannel ::
  -- | 'channelId'
  Core.Text ->
  DeleteChannel
newDeleteChannel pChannelId_ =
  DeleteChannel' {channelId = pChannelId_}

-- | Unique ID of the channel.
deleteChannel_channelId :: Lens.Lens' DeleteChannel Core.Text
deleteChannel_channelId = Lens.lens (\DeleteChannel' {channelId} -> channelId) (\s@DeleteChannel' {} a -> s {channelId = a} :: DeleteChannel)

instance Core.AWSRequest DeleteChannel where
  type
    AWSResponse DeleteChannel =
      DeleteChannelResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteChannelResponse'
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

instance Core.Hashable DeleteChannel

instance Core.NFData DeleteChannel

instance Core.ToHeaders DeleteChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Core.mconcat
      ["/prod/channels/", Core.toBS channelId]

instance Core.ToQuery DeleteChannel where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DeleteChannelResponse
--
-- /See:/ 'newDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
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
-- Create a value of 'DeleteChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoderSettings', 'deleteChannelResponse_encoderSettings' - Undocumented member.
--
-- 'roleArn', 'deleteChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'inputSpecification', 'deleteChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'arn', 'deleteChannelResponse_arn' - The unique arn of the channel.
--
-- 'id', 'deleteChannelResponse_id' - The unique id of the channel.
--
-- 'pipelinesRunningCount', 'deleteChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'channelClass', 'deleteChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'logLevel', 'deleteChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'destinations', 'deleteChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'state', 'deleteChannelResponse_state' - Undocumented member.
--
-- 'name', 'deleteChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'inputAttachments', 'deleteChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'tags', 'deleteChannelResponse_tags' - A collection of key-value pairs.
--
-- 'pipelineDetails', 'deleteChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'egressEndpoints', 'deleteChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'vpc', 'deleteChannelResponse_vpc' - Settings for VPC output
--
-- 'cdiInputSpecification', 'deleteChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'httpStatus', 'deleteChannelResponse_httpStatus' - The response's http status code.
newDeleteChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteChannelResponse
newDeleteChannelResponse pHttpStatus_ =
  DeleteChannelResponse'
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
deleteChannelResponse_encoderSettings :: Lens.Lens' DeleteChannelResponse (Core.Maybe EncoderSettings)
deleteChannelResponse_encoderSettings = Lens.lens (\DeleteChannelResponse' {encoderSettings} -> encoderSettings) (\s@DeleteChannelResponse' {} a -> s {encoderSettings = a} :: DeleteChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
deleteChannelResponse_roleArn :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
deleteChannelResponse_roleArn = Lens.lens (\DeleteChannelResponse' {roleArn} -> roleArn) (\s@DeleteChannelResponse' {} a -> s {roleArn = a} :: DeleteChannelResponse)

-- | Specification of network and file inputs for this channel
deleteChannelResponse_inputSpecification :: Lens.Lens' DeleteChannelResponse (Core.Maybe InputSpecification)
deleteChannelResponse_inputSpecification = Lens.lens (\DeleteChannelResponse' {inputSpecification} -> inputSpecification) (\s@DeleteChannelResponse' {} a -> s {inputSpecification = a} :: DeleteChannelResponse)

-- | The unique arn of the channel.
deleteChannelResponse_arn :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
deleteChannelResponse_arn = Lens.lens (\DeleteChannelResponse' {arn} -> arn) (\s@DeleteChannelResponse' {} a -> s {arn = a} :: DeleteChannelResponse)

-- | The unique id of the channel.
deleteChannelResponse_id :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
deleteChannelResponse_id = Lens.lens (\DeleteChannelResponse' {id} -> id) (\s@DeleteChannelResponse' {} a -> s {id = a} :: DeleteChannelResponse)

-- | The number of currently healthy pipelines.
deleteChannelResponse_pipelinesRunningCount :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Int)
deleteChannelResponse_pipelinesRunningCount = Lens.lens (\DeleteChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DeleteChannelResponse' {} a -> s {pipelinesRunningCount = a} :: DeleteChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
deleteChannelResponse_channelClass :: Lens.Lens' DeleteChannelResponse (Core.Maybe ChannelClass)
deleteChannelResponse_channelClass = Lens.lens (\DeleteChannelResponse' {channelClass} -> channelClass) (\s@DeleteChannelResponse' {} a -> s {channelClass = a} :: DeleteChannelResponse)

-- | The log level being written to CloudWatch Logs.
deleteChannelResponse_logLevel :: Lens.Lens' DeleteChannelResponse (Core.Maybe LogLevel)
deleteChannelResponse_logLevel = Lens.lens (\DeleteChannelResponse' {logLevel} -> logLevel) (\s@DeleteChannelResponse' {} a -> s {logLevel = a} :: DeleteChannelResponse)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
deleteChannelResponse_destinations :: Lens.Lens' DeleteChannelResponse (Core.Maybe [OutputDestination])
deleteChannelResponse_destinations = Lens.lens (\DeleteChannelResponse' {destinations} -> destinations) (\s@DeleteChannelResponse' {} a -> s {destinations = a} :: DeleteChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
deleteChannelResponse_state :: Lens.Lens' DeleteChannelResponse (Core.Maybe ChannelState)
deleteChannelResponse_state = Lens.lens (\DeleteChannelResponse' {state} -> state) (\s@DeleteChannelResponse' {} a -> s {state = a} :: DeleteChannelResponse)

-- | The name of the channel. (user-mutable)
deleteChannelResponse_name :: Lens.Lens' DeleteChannelResponse (Core.Maybe Core.Text)
deleteChannelResponse_name = Lens.lens (\DeleteChannelResponse' {name} -> name) (\s@DeleteChannelResponse' {} a -> s {name = a} :: DeleteChannelResponse)

-- | List of input attachments for channel.
deleteChannelResponse_inputAttachments :: Lens.Lens' DeleteChannelResponse (Core.Maybe [InputAttachment])
deleteChannelResponse_inputAttachments = Lens.lens (\DeleteChannelResponse' {inputAttachments} -> inputAttachments) (\s@DeleteChannelResponse' {} a -> s {inputAttachments = a} :: DeleteChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | A collection of key-value pairs.
deleteChannelResponse_tags :: Lens.Lens' DeleteChannelResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
deleteChannelResponse_tags = Lens.lens (\DeleteChannelResponse' {tags} -> tags) (\s@DeleteChannelResponse' {} a -> s {tags = a} :: DeleteChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | Runtime details for the pipelines of a running channel.
deleteChannelResponse_pipelineDetails :: Lens.Lens' DeleteChannelResponse (Core.Maybe [PipelineDetail])
deleteChannelResponse_pipelineDetails = Lens.lens (\DeleteChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@DeleteChannelResponse' {} a -> s {pipelineDetails = a} :: DeleteChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | The endpoints where outgoing connections initiate from
deleteChannelResponse_egressEndpoints :: Lens.Lens' DeleteChannelResponse (Core.Maybe [ChannelEgressEndpoint])
deleteChannelResponse_egressEndpoints = Lens.lens (\DeleteChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@DeleteChannelResponse' {} a -> s {egressEndpoints = a} :: DeleteChannelResponse) Core.. Lens.mapping Lens._Coerce

-- | Settings for VPC output
deleteChannelResponse_vpc :: Lens.Lens' DeleteChannelResponse (Core.Maybe VpcOutputSettings)
deleteChannelResponse_vpc = Lens.lens (\DeleteChannelResponse' {vpc} -> vpc) (\s@DeleteChannelResponse' {} a -> s {vpc = a} :: DeleteChannelResponse)

-- | Specification of CDI inputs for this channel
deleteChannelResponse_cdiInputSpecification :: Lens.Lens' DeleteChannelResponse (Core.Maybe CdiInputSpecification)
deleteChannelResponse_cdiInputSpecification = Lens.lens (\DeleteChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@DeleteChannelResponse' {} a -> s {cdiInputSpecification = a} :: DeleteChannelResponse)

-- | The response's http status code.
deleteChannelResponse_httpStatus :: Lens.Lens' DeleteChannelResponse Core.Int
deleteChannelResponse_httpStatus = Lens.lens (\DeleteChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteChannelResponse' {} a -> s {httpStatus = a} :: DeleteChannelResponse)

instance Core.NFData DeleteChannelResponse
