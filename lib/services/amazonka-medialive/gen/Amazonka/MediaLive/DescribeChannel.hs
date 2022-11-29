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
-- Module      : Amazonka.MediaLive.DescribeChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a channel
module Amazonka.MediaLive.DescribeChannel
  ( -- * Creating a Request
    DescribeChannel (..),
    newDescribeChannel,

    -- * Request Lenses
    describeChannel_channelId,

    -- * Destructuring the Response
    DescribeChannelResponse (..),
    newDescribeChannelResponse,

    -- * Response Lenses
    describeChannelResponse_tags,
    describeChannelResponse_name,
    describeChannelResponse_maintenance,
    describeChannelResponse_roleArn,
    describeChannelResponse_vpc,
    describeChannelResponse_logLevel,
    describeChannelResponse_arn,
    describeChannelResponse_state,
    describeChannelResponse_inputSpecification,
    describeChannelResponse_channelClass,
    describeChannelResponse_id,
    describeChannelResponse_pipelineDetails,
    describeChannelResponse_cdiInputSpecification,
    describeChannelResponse_inputAttachments,
    describeChannelResponse_pipelinesRunningCount,
    describeChannelResponse_destinations,
    describeChannelResponse_encoderSettings,
    describeChannelResponse_egressEndpoints,
    describeChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DescribeChannelRequest
--
-- /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | channel ID
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'describeChannel_channelId' - channel ID
newDescribeChannel ::
  -- | 'channelId'
  Prelude.Text ->
  DescribeChannel
newDescribeChannel pChannelId_ =
  DescribeChannel' {channelId = pChannelId_}

-- | channel ID
describeChannel_channelId :: Lens.Lens' DescribeChannel Prelude.Text
describeChannel_channelId = Lens.lens (\DescribeChannel' {channelId} -> channelId) (\s@DescribeChannel' {} a -> s {channelId = a} :: DescribeChannel)

instance Core.AWSRequest DescribeChannel where
  type
    AWSResponse DescribeChannel =
      DescribeChannelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "maintenance")
            Prelude.<*> (x Core..?> "roleArn")
            Prelude.<*> (x Core..?> "vpc")
            Prelude.<*> (x Core..?> "logLevel")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "state")
            Prelude.<*> (x Core..?> "inputSpecification")
            Prelude.<*> (x Core..?> "channelClass")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> ( x Core..?> "pipelineDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "cdiInputSpecification")
            Prelude.<*> ( x Core..?> "inputAttachments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "pipelinesRunningCount")
            Prelude.<*> (x Core..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "encoderSettings")
            Prelude.<*> ( x Core..?> "egressEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannel where
  hashWithSalt _salt DescribeChannel' {..} =
    _salt `Prelude.hashWithSalt` channelId

instance Prelude.NFData DescribeChannel where
  rnf DescribeChannel' {..} = Prelude.rnf channelId

instance Core.ToHeaders DescribeChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat
      ["/prod/channels/", Core.toBS channelId]

instance Core.ToQuery DescribeChannel where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeChannelResponse
--
-- /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the channel. (user-mutable)
    name :: Prelude.Maybe Prelude.Text,
    -- | Maintenance settings for this channel.
    maintenance :: Prelude.Maybe MaintenanceStatus,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Settings for VPC output
    vpc :: Prelude.Maybe VpcOutputSettingsDescription,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | The unique arn of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    state :: Prelude.Maybe ChannelState,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Prelude.Maybe ChannelClass,
    -- | The unique id of the channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Prelude.Maybe [PipelineDetail],
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | A list of destinations of the channel. For UDP outputs, there is one
    -- destination per output. For other types (HLS, for example), there is one
    -- destination per packager.
    destinations :: Prelude.Maybe [OutputDestination],
    encoderSettings :: Prelude.Maybe EncoderSettings,
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Prelude.Maybe [ChannelEgressEndpoint],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeChannelResponse_tags' - A collection of key-value pairs.
--
-- 'name', 'describeChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'maintenance', 'describeChannelResponse_maintenance' - Maintenance settings for this channel.
--
-- 'roleArn', 'describeChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'vpc', 'describeChannelResponse_vpc' - Settings for VPC output
--
-- 'logLevel', 'describeChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'arn', 'describeChannelResponse_arn' - The unique arn of the channel.
--
-- 'state', 'describeChannelResponse_state' - Undocumented member.
--
-- 'inputSpecification', 'describeChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'channelClass', 'describeChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'id', 'describeChannelResponse_id' - The unique id of the channel.
--
-- 'pipelineDetails', 'describeChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'cdiInputSpecification', 'describeChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'inputAttachments', 'describeChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'pipelinesRunningCount', 'describeChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'destinations', 'describeChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'encoderSettings', 'describeChannelResponse_encoderSettings' - Undocumented member.
--
-- 'egressEndpoints', 'describeChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      maintenance = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      vpc = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      id = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      destinations = Prelude.Nothing,
      encoderSettings = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of key-value pairs.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel. (user-mutable)
describeChannelResponse_name :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_name = Lens.lens (\DescribeChannelResponse' {name} -> name) (\s@DescribeChannelResponse' {} a -> s {name = a} :: DescribeChannelResponse)

-- | Maintenance settings for this channel.
describeChannelResponse_maintenance :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe MaintenanceStatus)
describeChannelResponse_maintenance = Lens.lens (\DescribeChannelResponse' {maintenance} -> maintenance) (\s@DescribeChannelResponse' {} a -> s {maintenance = a} :: DescribeChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
describeChannelResponse_roleArn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_roleArn = Lens.lens (\DescribeChannelResponse' {roleArn} -> roleArn) (\s@DescribeChannelResponse' {} a -> s {roleArn = a} :: DescribeChannelResponse)

-- | Settings for VPC output
describeChannelResponse_vpc :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe VpcOutputSettingsDescription)
describeChannelResponse_vpc = Lens.lens (\DescribeChannelResponse' {vpc} -> vpc) (\s@DescribeChannelResponse' {} a -> s {vpc = a} :: DescribeChannelResponse)

-- | The log level being written to CloudWatch Logs.
describeChannelResponse_logLevel :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe LogLevel)
describeChannelResponse_logLevel = Lens.lens (\DescribeChannelResponse' {logLevel} -> logLevel) (\s@DescribeChannelResponse' {} a -> s {logLevel = a} :: DescribeChannelResponse)

-- | The unique arn of the channel.
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_state :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelState)
describeChannelResponse_state = Lens.lens (\DescribeChannelResponse' {state} -> state) (\s@DescribeChannelResponse' {} a -> s {state = a} :: DescribeChannelResponse)

-- | Specification of network and file inputs for this channel
describeChannelResponse_inputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe InputSpecification)
describeChannelResponse_inputSpecification = Lens.lens (\DescribeChannelResponse' {inputSpecification} -> inputSpecification) (\s@DescribeChannelResponse' {} a -> s {inputSpecification = a} :: DescribeChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
describeChannelResponse_channelClass :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelClass)
describeChannelResponse_channelClass = Lens.lens (\DescribeChannelResponse' {channelClass} -> channelClass) (\s@DescribeChannelResponse' {} a -> s {channelClass = a} :: DescribeChannelResponse)

-- | The unique id of the channel.
describeChannelResponse_id :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_id = Lens.lens (\DescribeChannelResponse' {id} -> id) (\s@DescribeChannelResponse' {} a -> s {id = a} :: DescribeChannelResponse)

-- | Runtime details for the pipelines of a running channel.
describeChannelResponse_pipelineDetails :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [PipelineDetail])
describeChannelResponse_pipelineDetails = Lens.lens (\DescribeChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@DescribeChannelResponse' {} a -> s {pipelineDetails = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specification of CDI inputs for this channel
describeChannelResponse_cdiInputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe CdiInputSpecification)
describeChannelResponse_cdiInputSpecification = Lens.lens (\DescribeChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@DescribeChannelResponse' {} a -> s {cdiInputSpecification = a} :: DescribeChannelResponse)

-- | List of input attachments for channel.
describeChannelResponse_inputAttachments :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [InputAttachment])
describeChannelResponse_inputAttachments = Lens.lens (\DescribeChannelResponse' {inputAttachments} -> inputAttachments) (\s@DescribeChannelResponse' {} a -> s {inputAttachments = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of currently healthy pipelines.
describeChannelResponse_pipelinesRunningCount :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Int)
describeChannelResponse_pipelinesRunningCount = Lens.lens (\DescribeChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DescribeChannelResponse' {} a -> s {pipelinesRunningCount = a} :: DescribeChannelResponse)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
describeChannelResponse_destinations :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [OutputDestination])
describeChannelResponse_destinations = Lens.lens (\DescribeChannelResponse' {destinations} -> destinations) (\s@DescribeChannelResponse' {} a -> s {destinations = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeChannelResponse_encoderSettings :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe EncoderSettings)
describeChannelResponse_encoderSettings = Lens.lens (\DescribeChannelResponse' {encoderSettings} -> encoderSettings) (\s@DescribeChannelResponse' {} a -> s {encoderSettings = a} :: DescribeChannelResponse)

-- | The endpoints where outgoing connections initiate from
describeChannelResponse_egressEndpoints :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [ChannelEgressEndpoint])
describeChannelResponse_egressEndpoints = Lens.lens (\DescribeChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@DescribeChannelResponse' {} a -> s {egressEndpoints = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse where
  rnf DescribeChannelResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf maintenance
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf inputSpecification
      `Prelude.seq` Prelude.rnf channelClass
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf pipelineDetails
      `Prelude.seq` Prelude.rnf cdiInputSpecification
      `Prelude.seq` Prelude.rnf inputAttachments
      `Prelude.seq` Prelude.rnf pipelinesRunningCount
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf encoderSettings
      `Prelude.seq` Prelude.rnf egressEndpoints
      `Prelude.seq` Prelude.rnf httpStatus
