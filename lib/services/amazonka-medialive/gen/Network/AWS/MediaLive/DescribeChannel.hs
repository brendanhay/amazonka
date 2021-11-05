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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeChannelResponse_state,
    describeChannelResponse_logLevel,
    describeChannelResponse_arn,
    describeChannelResponse_pipelinesRunningCount,
    describeChannelResponse_pipelineDetails,
    describeChannelResponse_inputSpecification,
    describeChannelResponse_inputAttachments,
    describeChannelResponse_destinations,
    describeChannelResponse_name,
    describeChannelResponse_cdiInputSpecification,
    describeChannelResponse_id,
    describeChannelResponse_channelClass,
    describeChannelResponse_vpc,
    describeChannelResponse_egressEndpoints,
    describeChannelResponse_tags,
    describeChannelResponse_encoderSettings,
    describeChannelResponse_roleArn,
    describeChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Prelude.<$> (x Core..?> "state")
            Prelude.<*> (x Core..?> "logLevel")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "pipelinesRunningCount")
            Prelude.<*> ( x Core..?> "pipelineDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "inputSpecification")
            Prelude.<*> ( x Core..?> "inputAttachments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "cdiInputSpecification")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "channelClass")
            Prelude.<*> (x Core..?> "vpc")
            Prelude.<*> ( x Core..?> "egressEndpoints"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "encoderSettings")
            Prelude.<*> (x Core..?> "roleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannel

instance Prelude.NFData DescribeChannel

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
  { state :: Prelude.Maybe ChannelState,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | The unique arn of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Prelude.Maybe [PipelineDetail],
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
    -- | Settings for VPC output
    vpc :: Prelude.Maybe VpcOutputSettingsDescription,
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Prelude.Maybe [ChannelEgressEndpoint],
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    encoderSettings :: Prelude.Maybe EncoderSettings,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
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
-- 'state', 'describeChannelResponse_state' - Undocumented member.
--
-- 'logLevel', 'describeChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'arn', 'describeChannelResponse_arn' - The unique arn of the channel.
--
-- 'pipelinesRunningCount', 'describeChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'pipelineDetails', 'describeChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'inputSpecification', 'describeChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'inputAttachments', 'describeChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'destinations', 'describeChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'name', 'describeChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'cdiInputSpecification', 'describeChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'id', 'describeChannelResponse_id' - The unique id of the channel.
--
-- 'channelClass', 'describeChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'vpc', 'describeChannelResponse_vpc' - Settings for VPC output
--
-- 'egressEndpoints', 'describeChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'tags', 'describeChannelResponse_tags' - A collection of key-value pairs.
--
-- 'encoderSettings', 'describeChannelResponse_encoderSettings' - Undocumented member.
--
-- 'roleArn', 'describeChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { state = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      arn = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing,
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
      encoderSettings = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeChannelResponse_state :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelState)
describeChannelResponse_state = Lens.lens (\DescribeChannelResponse' {state} -> state) (\s@DescribeChannelResponse' {} a -> s {state = a} :: DescribeChannelResponse)

-- | The log level being written to CloudWatch Logs.
describeChannelResponse_logLevel :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe LogLevel)
describeChannelResponse_logLevel = Lens.lens (\DescribeChannelResponse' {logLevel} -> logLevel) (\s@DescribeChannelResponse' {} a -> s {logLevel = a} :: DescribeChannelResponse)

-- | The unique arn of the channel.
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | The number of currently healthy pipelines.
describeChannelResponse_pipelinesRunningCount :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Int)
describeChannelResponse_pipelinesRunningCount = Lens.lens (\DescribeChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DescribeChannelResponse' {} a -> s {pipelinesRunningCount = a} :: DescribeChannelResponse)

-- | Runtime details for the pipelines of a running channel.
describeChannelResponse_pipelineDetails :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [PipelineDetail])
describeChannelResponse_pipelineDetails = Lens.lens (\DescribeChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@DescribeChannelResponse' {} a -> s {pipelineDetails = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Specification of network and file inputs for this channel
describeChannelResponse_inputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe InputSpecification)
describeChannelResponse_inputSpecification = Lens.lens (\DescribeChannelResponse' {inputSpecification} -> inputSpecification) (\s@DescribeChannelResponse' {} a -> s {inputSpecification = a} :: DescribeChannelResponse)

-- | List of input attachments for channel.
describeChannelResponse_inputAttachments :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [InputAttachment])
describeChannelResponse_inputAttachments = Lens.lens (\DescribeChannelResponse' {inputAttachments} -> inputAttachments) (\s@DescribeChannelResponse' {} a -> s {inputAttachments = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
describeChannelResponse_destinations :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [OutputDestination])
describeChannelResponse_destinations = Lens.lens (\DescribeChannelResponse' {destinations} -> destinations) (\s@DescribeChannelResponse' {} a -> s {destinations = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the channel. (user-mutable)
describeChannelResponse_name :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_name = Lens.lens (\DescribeChannelResponse' {name} -> name) (\s@DescribeChannelResponse' {} a -> s {name = a} :: DescribeChannelResponse)

-- | Specification of CDI inputs for this channel
describeChannelResponse_cdiInputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe CdiInputSpecification)
describeChannelResponse_cdiInputSpecification = Lens.lens (\DescribeChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@DescribeChannelResponse' {} a -> s {cdiInputSpecification = a} :: DescribeChannelResponse)

-- | The unique id of the channel.
describeChannelResponse_id :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_id = Lens.lens (\DescribeChannelResponse' {id} -> id) (\s@DescribeChannelResponse' {} a -> s {id = a} :: DescribeChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
describeChannelResponse_channelClass :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelClass)
describeChannelResponse_channelClass = Lens.lens (\DescribeChannelResponse' {channelClass} -> channelClass) (\s@DescribeChannelResponse' {} a -> s {channelClass = a} :: DescribeChannelResponse)

-- | Settings for VPC output
describeChannelResponse_vpc :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe VpcOutputSettingsDescription)
describeChannelResponse_vpc = Lens.lens (\DescribeChannelResponse' {vpc} -> vpc) (\s@DescribeChannelResponse' {} a -> s {vpc = a} :: DescribeChannelResponse)

-- | The endpoints where outgoing connections initiate from
describeChannelResponse_egressEndpoints :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [ChannelEgressEndpoint])
describeChannelResponse_egressEndpoints = Lens.lens (\DescribeChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@DescribeChannelResponse' {} a -> s {egressEndpoints = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | A collection of key-value pairs.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeChannelResponse_encoderSettings :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe EncoderSettings)
describeChannelResponse_encoderSettings = Lens.lens (\DescribeChannelResponse' {encoderSettings} -> encoderSettings) (\s@DescribeChannelResponse' {} a -> s {encoderSettings = a} :: DescribeChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
describeChannelResponse_roleArn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_roleArn = Lens.lens (\DescribeChannelResponse' {roleArn} -> roleArn) (\s@DescribeChannelResponse' {} a -> s {roleArn = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse
