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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    describeChannelResponse_arn,
    describeChannelResponse_cdiInputSpecification,
    describeChannelResponse_channelClass,
    describeChannelResponse_destinations,
    describeChannelResponse_egressEndpoints,
    describeChannelResponse_encoderSettings,
    describeChannelResponse_id,
    describeChannelResponse_inputAttachments,
    describeChannelResponse_inputSpecification,
    describeChannelResponse_logLevel,
    describeChannelResponse_maintenance,
    describeChannelResponse_name,
    describeChannelResponse_pipelineDetails,
    describeChannelResponse_pipelinesRunningCount,
    describeChannelResponse_roleArn,
    describeChannelResponse_state,
    describeChannelResponse_tags,
    describeChannelResponse_vpc,
    describeChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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

instance Prelude.Hashable DescribeChannel where
  hashWithSalt _salt DescribeChannel' {..} =
    _salt `Prelude.hashWithSalt` channelId

instance Prelude.NFData DescribeChannel where
  rnf DescribeChannel' {..} = Prelude.rnf channelId

instance Data.ToHeaders DescribeChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat
      ["/prod/channels/", Data.toBS channelId]

instance Data.ToQuery DescribeChannel where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeChannelResponse
--
-- /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
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
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeChannelResponse_arn' - The unique arn of the channel.
--
-- 'cdiInputSpecification', 'describeChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'channelClass', 'describeChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'destinations', 'describeChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'egressEndpoints', 'describeChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'encoderSettings', 'describeChannelResponse_encoderSettings' - Undocumented member.
--
-- 'id', 'describeChannelResponse_id' - The unique id of the channel.
--
-- 'inputAttachments', 'describeChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'inputSpecification', 'describeChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'logLevel', 'describeChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'maintenance', 'describeChannelResponse_maintenance' - Maintenance settings for this channel.
--
-- 'name', 'describeChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'pipelineDetails', 'describeChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'pipelinesRunningCount', 'describeChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'roleArn', 'describeChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'state', 'describeChannelResponse_state' - Undocumented member.
--
-- 'tags', 'describeChannelResponse_tags' - A collection of key-value pairs.
--
-- 'vpc', 'describeChannelResponse_vpc' - Settings for VPC output
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
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
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | Specification of CDI inputs for this channel
describeChannelResponse_cdiInputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe CdiInputSpecification)
describeChannelResponse_cdiInputSpecification = Lens.lens (\DescribeChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@DescribeChannelResponse' {} a -> s {cdiInputSpecification = a} :: DescribeChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
describeChannelResponse_channelClass :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelClass)
describeChannelResponse_channelClass = Lens.lens (\DescribeChannelResponse' {channelClass} -> channelClass) (\s@DescribeChannelResponse' {} a -> s {channelClass = a} :: DescribeChannelResponse)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
describeChannelResponse_destinations :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [OutputDestination])
describeChannelResponse_destinations = Lens.lens (\DescribeChannelResponse' {destinations} -> destinations) (\s@DescribeChannelResponse' {} a -> s {destinations = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The endpoints where outgoing connections initiate from
describeChannelResponse_egressEndpoints :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [ChannelEgressEndpoint])
describeChannelResponse_egressEndpoints = Lens.lens (\DescribeChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@DescribeChannelResponse' {} a -> s {egressEndpoints = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeChannelResponse_encoderSettings :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe EncoderSettings)
describeChannelResponse_encoderSettings = Lens.lens (\DescribeChannelResponse' {encoderSettings} -> encoderSettings) (\s@DescribeChannelResponse' {} a -> s {encoderSettings = a} :: DescribeChannelResponse)

-- | The unique id of the channel.
describeChannelResponse_id :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_id = Lens.lens (\DescribeChannelResponse' {id} -> id) (\s@DescribeChannelResponse' {} a -> s {id = a} :: DescribeChannelResponse)

-- | List of input attachments for channel.
describeChannelResponse_inputAttachments :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [InputAttachment])
describeChannelResponse_inputAttachments = Lens.lens (\DescribeChannelResponse' {inputAttachments} -> inputAttachments) (\s@DescribeChannelResponse' {} a -> s {inputAttachments = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specification of network and file inputs for this channel
describeChannelResponse_inputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe InputSpecification)
describeChannelResponse_inputSpecification = Lens.lens (\DescribeChannelResponse' {inputSpecification} -> inputSpecification) (\s@DescribeChannelResponse' {} a -> s {inputSpecification = a} :: DescribeChannelResponse)

-- | The log level being written to CloudWatch Logs.
describeChannelResponse_logLevel :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe LogLevel)
describeChannelResponse_logLevel = Lens.lens (\DescribeChannelResponse' {logLevel} -> logLevel) (\s@DescribeChannelResponse' {} a -> s {logLevel = a} :: DescribeChannelResponse)

-- | Maintenance settings for this channel.
describeChannelResponse_maintenance :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe MaintenanceStatus)
describeChannelResponse_maintenance = Lens.lens (\DescribeChannelResponse' {maintenance} -> maintenance) (\s@DescribeChannelResponse' {} a -> s {maintenance = a} :: DescribeChannelResponse)

-- | The name of the channel. (user-mutable)
describeChannelResponse_name :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_name = Lens.lens (\DescribeChannelResponse' {name} -> name) (\s@DescribeChannelResponse' {} a -> s {name = a} :: DescribeChannelResponse)

-- | Runtime details for the pipelines of a running channel.
describeChannelResponse_pipelineDetails :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [PipelineDetail])
describeChannelResponse_pipelineDetails = Lens.lens (\DescribeChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@DescribeChannelResponse' {} a -> s {pipelineDetails = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The number of currently healthy pipelines.
describeChannelResponse_pipelinesRunningCount :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Int)
describeChannelResponse_pipelinesRunningCount = Lens.lens (\DescribeChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DescribeChannelResponse' {} a -> s {pipelinesRunningCount = a} :: DescribeChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
describeChannelResponse_roleArn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_roleArn = Lens.lens (\DescribeChannelResponse' {roleArn} -> roleArn) (\s@DescribeChannelResponse' {} a -> s {roleArn = a} :: DescribeChannelResponse)

-- | Undocumented member.
describeChannelResponse_state :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelState)
describeChannelResponse_state = Lens.lens (\DescribeChannelResponse' {state} -> state) (\s@DescribeChannelResponse' {} a -> s {state = a} :: DescribeChannelResponse)

-- | A collection of key-value pairs.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Settings for VPC output
describeChannelResponse_vpc :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe VpcOutputSettingsDescription)
describeChannelResponse_vpc = Lens.lens (\DescribeChannelResponse' {vpc} -> vpc) (\s@DescribeChannelResponse' {} a -> s {vpc = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse where
  rnf DescribeChannelResponse' {..} =
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
