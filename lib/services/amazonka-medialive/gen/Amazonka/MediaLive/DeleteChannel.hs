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
-- Module      : Amazonka.MediaLive.DeleteChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts deletion of channel. The associated outputs are also deleted.
module Amazonka.MediaLive.DeleteChannel
  ( -- * Creating a Request
    DeleteChannel (..),
    newDeleteChannel,

    -- * Request Lenses
    deleteChannel_channelId,

    -- * Destructuring the Response
    DeleteChannelResponse (..),
    newDeleteChannelResponse,

    -- * Response Lenses
    deleteChannelResponse_arn,
    deleteChannelResponse_cdiInputSpecification,
    deleteChannelResponse_channelClass,
    deleteChannelResponse_destinations,
    deleteChannelResponse_egressEndpoints,
    deleteChannelResponse_encoderSettings,
    deleteChannelResponse_id,
    deleteChannelResponse_inputAttachments,
    deleteChannelResponse_inputSpecification,
    deleteChannelResponse_logLevel,
    deleteChannelResponse_maintenance,
    deleteChannelResponse_name,
    deleteChannelResponse_pipelineDetails,
    deleteChannelResponse_pipelinesRunningCount,
    deleteChannelResponse_roleArn,
    deleteChannelResponse_state,
    deleteChannelResponse_tags,
    deleteChannelResponse_vpc,
    deleteChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DeleteChannelRequest
--
-- /See:/ 'newDeleteChannel' smart constructor.
data DeleteChannel = DeleteChannel'
  { -- | Unique ID of the channel.
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteChannel
newDeleteChannel pChannelId_ =
  DeleteChannel' {channelId = pChannelId_}

-- | Unique ID of the channel.
deleteChannel_channelId :: Lens.Lens' DeleteChannel Prelude.Text
deleteChannel_channelId = Lens.lens (\DeleteChannel' {channelId} -> channelId) (\s@DeleteChannel' {} a -> s {channelId = a} :: DeleteChannel)

instance Core.AWSRequest DeleteChannel where
  type
    AWSResponse DeleteChannel =
      DeleteChannelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteChannelResponse'
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

instance Prelude.Hashable DeleteChannel where
  hashWithSalt _salt DeleteChannel' {..} =
    _salt `Prelude.hashWithSalt` channelId

instance Prelude.NFData DeleteChannel where
  rnf DeleteChannel' {..} = Prelude.rnf channelId

instance Data.ToHeaders DeleteChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteChannel where
  toPath DeleteChannel' {..} =
    Prelude.mconcat
      ["/prod/channels/", Data.toBS channelId]

instance Data.ToQuery DeleteChannel where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DeleteChannelResponse
--
-- /See:/ 'newDeleteChannelResponse' smart constructor.
data DeleteChannelResponse = DeleteChannelResponse'
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
-- Create a value of 'DeleteChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteChannelResponse_arn' - The unique arn of the channel.
--
-- 'cdiInputSpecification', 'deleteChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'channelClass', 'deleteChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'destinations', 'deleteChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'egressEndpoints', 'deleteChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'encoderSettings', 'deleteChannelResponse_encoderSettings' - Undocumented member.
--
-- 'id', 'deleteChannelResponse_id' - The unique id of the channel.
--
-- 'inputAttachments', 'deleteChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'inputSpecification', 'deleteChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'logLevel', 'deleteChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'maintenance', 'deleteChannelResponse_maintenance' - Maintenance settings for this channel.
--
-- 'name', 'deleteChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'pipelineDetails', 'deleteChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'pipelinesRunningCount', 'deleteChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'roleArn', 'deleteChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'state', 'deleteChannelResponse_state' - Undocumented member.
--
-- 'tags', 'deleteChannelResponse_tags' - A collection of key-value pairs.
--
-- 'vpc', 'deleteChannelResponse_vpc' - Settings for VPC output
--
-- 'httpStatus', 'deleteChannelResponse_httpStatus' - The response's http status code.
newDeleteChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteChannelResponse
newDeleteChannelResponse pHttpStatus_ =
  DeleteChannelResponse'
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
deleteChannelResponse_arn :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe Prelude.Text)
deleteChannelResponse_arn = Lens.lens (\DeleteChannelResponse' {arn} -> arn) (\s@DeleteChannelResponse' {} a -> s {arn = a} :: DeleteChannelResponse)

-- | Specification of CDI inputs for this channel
deleteChannelResponse_cdiInputSpecification :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe CdiInputSpecification)
deleteChannelResponse_cdiInputSpecification = Lens.lens (\DeleteChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@DeleteChannelResponse' {} a -> s {cdiInputSpecification = a} :: DeleteChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
deleteChannelResponse_channelClass :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe ChannelClass)
deleteChannelResponse_channelClass = Lens.lens (\DeleteChannelResponse' {channelClass} -> channelClass) (\s@DeleteChannelResponse' {} a -> s {channelClass = a} :: DeleteChannelResponse)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
deleteChannelResponse_destinations :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe [OutputDestination])
deleteChannelResponse_destinations = Lens.lens (\DeleteChannelResponse' {destinations} -> destinations) (\s@DeleteChannelResponse' {} a -> s {destinations = a} :: DeleteChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The endpoints where outgoing connections initiate from
deleteChannelResponse_egressEndpoints :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe [ChannelEgressEndpoint])
deleteChannelResponse_egressEndpoints = Lens.lens (\DeleteChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@DeleteChannelResponse' {} a -> s {egressEndpoints = a} :: DeleteChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
deleteChannelResponse_encoderSettings :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe EncoderSettings)
deleteChannelResponse_encoderSettings = Lens.lens (\DeleteChannelResponse' {encoderSettings} -> encoderSettings) (\s@DeleteChannelResponse' {} a -> s {encoderSettings = a} :: DeleteChannelResponse)

-- | The unique id of the channel.
deleteChannelResponse_id :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe Prelude.Text)
deleteChannelResponse_id = Lens.lens (\DeleteChannelResponse' {id} -> id) (\s@DeleteChannelResponse' {} a -> s {id = a} :: DeleteChannelResponse)

-- | List of input attachments for channel.
deleteChannelResponse_inputAttachments :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe [InputAttachment])
deleteChannelResponse_inputAttachments = Lens.lens (\DeleteChannelResponse' {inputAttachments} -> inputAttachments) (\s@DeleteChannelResponse' {} a -> s {inputAttachments = a} :: DeleteChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specification of network and file inputs for this channel
deleteChannelResponse_inputSpecification :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe InputSpecification)
deleteChannelResponse_inputSpecification = Lens.lens (\DeleteChannelResponse' {inputSpecification} -> inputSpecification) (\s@DeleteChannelResponse' {} a -> s {inputSpecification = a} :: DeleteChannelResponse)

-- | The log level being written to CloudWatch Logs.
deleteChannelResponse_logLevel :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe LogLevel)
deleteChannelResponse_logLevel = Lens.lens (\DeleteChannelResponse' {logLevel} -> logLevel) (\s@DeleteChannelResponse' {} a -> s {logLevel = a} :: DeleteChannelResponse)

-- | Maintenance settings for this channel.
deleteChannelResponse_maintenance :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe MaintenanceStatus)
deleteChannelResponse_maintenance = Lens.lens (\DeleteChannelResponse' {maintenance} -> maintenance) (\s@DeleteChannelResponse' {} a -> s {maintenance = a} :: DeleteChannelResponse)

-- | The name of the channel. (user-mutable)
deleteChannelResponse_name :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe Prelude.Text)
deleteChannelResponse_name = Lens.lens (\DeleteChannelResponse' {name} -> name) (\s@DeleteChannelResponse' {} a -> s {name = a} :: DeleteChannelResponse)

-- | Runtime details for the pipelines of a running channel.
deleteChannelResponse_pipelineDetails :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe [PipelineDetail])
deleteChannelResponse_pipelineDetails = Lens.lens (\DeleteChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@DeleteChannelResponse' {} a -> s {pipelineDetails = a} :: DeleteChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of currently healthy pipelines.
deleteChannelResponse_pipelinesRunningCount :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe Prelude.Int)
deleteChannelResponse_pipelinesRunningCount = Lens.lens (\DeleteChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DeleteChannelResponse' {} a -> s {pipelinesRunningCount = a} :: DeleteChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
deleteChannelResponse_roleArn :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe Prelude.Text)
deleteChannelResponse_roleArn = Lens.lens (\DeleteChannelResponse' {roleArn} -> roleArn) (\s@DeleteChannelResponse' {} a -> s {roleArn = a} :: DeleteChannelResponse)

-- | Undocumented member.
deleteChannelResponse_state :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe ChannelState)
deleteChannelResponse_state = Lens.lens (\DeleteChannelResponse' {state} -> state) (\s@DeleteChannelResponse' {} a -> s {state = a} :: DeleteChannelResponse)

-- | A collection of key-value pairs.
deleteChannelResponse_tags :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
deleteChannelResponse_tags = Lens.lens (\DeleteChannelResponse' {tags} -> tags) (\s@DeleteChannelResponse' {} a -> s {tags = a} :: DeleteChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Settings for VPC output
deleteChannelResponse_vpc :: Lens.Lens' DeleteChannelResponse (Prelude.Maybe VpcOutputSettingsDescription)
deleteChannelResponse_vpc = Lens.lens (\DeleteChannelResponse' {vpc} -> vpc) (\s@DeleteChannelResponse' {} a -> s {vpc = a} :: DeleteChannelResponse)

-- | The response's http status code.
deleteChannelResponse_httpStatus :: Lens.Lens' DeleteChannelResponse Prelude.Int
deleteChannelResponse_httpStatus = Lens.lens (\DeleteChannelResponse' {httpStatus} -> httpStatus) (\s@DeleteChannelResponse' {} a -> s {httpStatus = a} :: DeleteChannelResponse)

instance Prelude.NFData DeleteChannelResponse where
  rnf DeleteChannelResponse' {..} =
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
