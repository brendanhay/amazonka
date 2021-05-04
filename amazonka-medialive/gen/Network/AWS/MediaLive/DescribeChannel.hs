{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.DescribeChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a channel
module Network.AWS.MediaLive.DescribeChannel
  ( -- * Creating a Request
    DescribeChannel (..),
    newDescribeChannel,

    -- * Request Lenses
    describeChannel_channelId,

    -- * Destructuring the Response
    DescribeChannelResponse (..),
    newDescribeChannelResponse,

    -- * Response Lenses
    describeChannelResponse_encoderSettings,
    describeChannelResponse_roleArn,
    describeChannelResponse_inputSpecification,
    describeChannelResponse_arn,
    describeChannelResponse_id,
    describeChannelResponse_pipelinesRunningCount,
    describeChannelResponse_channelClass,
    describeChannelResponse_logLevel,
    describeChannelResponse_destinations,
    describeChannelResponse_state,
    describeChannelResponse_name,
    describeChannelResponse_inputAttachments,
    describeChannelResponse_tags,
    describeChannelResponse_pipelineDetails,
    describeChannelResponse_egressEndpoints,
    describeChannelResponse_vpc,
    describeChannelResponse_cdiInputSpecification,
    describeChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeChannelRequest
--
-- /See:/ 'newDescribeChannel' smart constructor.
data DescribeChannel = DescribeChannel'
  { -- | channel ID
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeChannel where
  type Rs DescribeChannel = DescribeChannelResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeChannelResponse'
            Prelude.<$> (x Prelude..?> "encoderSettings")
            Prelude.<*> (x Prelude..?> "roleArn")
            Prelude.<*> (x Prelude..?> "inputSpecification")
            Prelude.<*> (x Prelude..?> "arn")
            Prelude.<*> (x Prelude..?> "id")
            Prelude.<*> (x Prelude..?> "pipelinesRunningCount")
            Prelude.<*> (x Prelude..?> "channelClass")
            Prelude.<*> (x Prelude..?> "logLevel")
            Prelude.<*> ( x Prelude..?> "destinations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "state")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> ( x Prelude..?> "inputAttachments"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "tags" Prelude..!@ Prelude.mempty)
            Prelude.<*> ( x Prelude..?> "pipelineDetails"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..?> "egressEndpoints"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "vpc")
            Prelude.<*> (x Prelude..?> "cdiInputSpecification")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeChannel

instance Prelude.NFData DescribeChannel

instance Prelude.ToHeaders DescribeChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DescribeChannel where
  toPath DescribeChannel' {..} =
    Prelude.mconcat
      ["/prod/channels/", Prelude.toBS channelId]

instance Prelude.ToQuery DescribeChannel where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeChannelResponse
--
-- /See:/ 'newDescribeChannelResponse' smart constructor.
data DescribeChannelResponse = DescribeChannelResponse'
  { encoderSettings :: Prelude.Maybe EncoderSettings,
    -- | The Amazon Resource Name (ARN) of the role assumed when running the
    -- Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The unique arn of the channel.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique id of the channel.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Prelude.Maybe Prelude.Int,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Prelude.Maybe ChannelClass,
    -- | The log level being written to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | A list of destinations of the channel. For UDP outputs, there is one
    -- destination per output. For other types (HLS, for example), there is one
    -- destination per packager.
    destinations :: Prelude.Maybe [OutputDestination],
    state :: Prelude.Maybe ChannelState,
    -- | The name of the channel. (user-mutable)
    name :: Prelude.Maybe Prelude.Text,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Runtime details for the pipelines of a running channel.
    pipelineDetails :: Prelude.Maybe [PipelineDetail],
    -- | The endpoints where outgoing connections initiate from
    egressEndpoints :: Prelude.Maybe [ChannelEgressEndpoint],
    -- | Settings for VPC output
    vpc :: Prelude.Maybe VpcOutputSettings,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoderSettings', 'describeChannelResponse_encoderSettings' - Undocumented member.
--
-- 'roleArn', 'describeChannelResponse_roleArn' - The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
--
-- 'inputSpecification', 'describeChannelResponse_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'arn', 'describeChannelResponse_arn' - The unique arn of the channel.
--
-- 'id', 'describeChannelResponse_id' - The unique id of the channel.
--
-- 'pipelinesRunningCount', 'describeChannelResponse_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'channelClass', 'describeChannelResponse_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'logLevel', 'describeChannelResponse_logLevel' - The log level being written to CloudWatch Logs.
--
-- 'destinations', 'describeChannelResponse_destinations' - A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
--
-- 'state', 'describeChannelResponse_state' - Undocumented member.
--
-- 'name', 'describeChannelResponse_name' - The name of the channel. (user-mutable)
--
-- 'inputAttachments', 'describeChannelResponse_inputAttachments' - List of input attachments for channel.
--
-- 'tags', 'describeChannelResponse_tags' - A collection of key-value pairs.
--
-- 'pipelineDetails', 'describeChannelResponse_pipelineDetails' - Runtime details for the pipelines of a running channel.
--
-- 'egressEndpoints', 'describeChannelResponse_egressEndpoints' - The endpoints where outgoing connections initiate from
--
-- 'vpc', 'describeChannelResponse_vpc' - Settings for VPC output
--
-- 'cdiInputSpecification', 'describeChannelResponse_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'httpStatus', 'describeChannelResponse_httpStatus' - The response's http status code.
newDescribeChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeChannelResponse
newDescribeChannelResponse pHttpStatus_ =
  DescribeChannelResponse'
    { encoderSettings =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      pipelinesRunningCount = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      destinations = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      tags = Prelude.Nothing,
      pipelineDetails = Prelude.Nothing,
      egressEndpoints = Prelude.Nothing,
      vpc = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeChannelResponse_encoderSettings :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe EncoderSettings)
describeChannelResponse_encoderSettings = Lens.lens (\DescribeChannelResponse' {encoderSettings} -> encoderSettings) (\s@DescribeChannelResponse' {} a -> s {encoderSettings = a} :: DescribeChannelResponse)

-- | The Amazon Resource Name (ARN) of the role assumed when running the
-- Channel.
describeChannelResponse_roleArn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_roleArn = Lens.lens (\DescribeChannelResponse' {roleArn} -> roleArn) (\s@DescribeChannelResponse' {} a -> s {roleArn = a} :: DescribeChannelResponse)

-- | Specification of network and file inputs for this channel
describeChannelResponse_inputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe InputSpecification)
describeChannelResponse_inputSpecification = Lens.lens (\DescribeChannelResponse' {inputSpecification} -> inputSpecification) (\s@DescribeChannelResponse' {} a -> s {inputSpecification = a} :: DescribeChannelResponse)

-- | The unique arn of the channel.
describeChannelResponse_arn :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_arn = Lens.lens (\DescribeChannelResponse' {arn} -> arn) (\s@DescribeChannelResponse' {} a -> s {arn = a} :: DescribeChannelResponse)

-- | The unique id of the channel.
describeChannelResponse_id :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_id = Lens.lens (\DescribeChannelResponse' {id} -> id) (\s@DescribeChannelResponse' {} a -> s {id = a} :: DescribeChannelResponse)

-- | The number of currently healthy pipelines.
describeChannelResponse_pipelinesRunningCount :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Int)
describeChannelResponse_pipelinesRunningCount = Lens.lens (\DescribeChannelResponse' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@DescribeChannelResponse' {} a -> s {pipelinesRunningCount = a} :: DescribeChannelResponse)

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
describeChannelResponse_channelClass :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelClass)
describeChannelResponse_channelClass = Lens.lens (\DescribeChannelResponse' {channelClass} -> channelClass) (\s@DescribeChannelResponse' {} a -> s {channelClass = a} :: DescribeChannelResponse)

-- | The log level being written to CloudWatch Logs.
describeChannelResponse_logLevel :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe LogLevel)
describeChannelResponse_logLevel = Lens.lens (\DescribeChannelResponse' {logLevel} -> logLevel) (\s@DescribeChannelResponse' {} a -> s {logLevel = a} :: DescribeChannelResponse)

-- | A list of destinations of the channel. For UDP outputs, there is one
-- destination per output. For other types (HLS, for example), there is one
-- destination per packager.
describeChannelResponse_destinations :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [OutputDestination])
describeChannelResponse_destinations = Lens.lens (\DescribeChannelResponse' {destinations} -> destinations) (\s@DescribeChannelResponse' {} a -> s {destinations = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
describeChannelResponse_state :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe ChannelState)
describeChannelResponse_state = Lens.lens (\DescribeChannelResponse' {state} -> state) (\s@DescribeChannelResponse' {} a -> s {state = a} :: DescribeChannelResponse)

-- | The name of the channel. (user-mutable)
describeChannelResponse_name :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe Prelude.Text)
describeChannelResponse_name = Lens.lens (\DescribeChannelResponse' {name} -> name) (\s@DescribeChannelResponse' {} a -> s {name = a} :: DescribeChannelResponse)

-- | List of input attachments for channel.
describeChannelResponse_inputAttachments :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [InputAttachment])
describeChannelResponse_inputAttachments = Lens.lens (\DescribeChannelResponse' {inputAttachments} -> inputAttachments) (\s@DescribeChannelResponse' {} a -> s {inputAttachments = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | A collection of key-value pairs.
describeChannelResponse_tags :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeChannelResponse_tags = Lens.lens (\DescribeChannelResponse' {tags} -> tags) (\s@DescribeChannelResponse' {} a -> s {tags = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Runtime details for the pipelines of a running channel.
describeChannelResponse_pipelineDetails :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [PipelineDetail])
describeChannelResponse_pipelineDetails = Lens.lens (\DescribeChannelResponse' {pipelineDetails} -> pipelineDetails) (\s@DescribeChannelResponse' {} a -> s {pipelineDetails = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The endpoints where outgoing connections initiate from
describeChannelResponse_egressEndpoints :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe [ChannelEgressEndpoint])
describeChannelResponse_egressEndpoints = Lens.lens (\DescribeChannelResponse' {egressEndpoints} -> egressEndpoints) (\s@DescribeChannelResponse' {} a -> s {egressEndpoints = a} :: DescribeChannelResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Settings for VPC output
describeChannelResponse_vpc :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe VpcOutputSettings)
describeChannelResponse_vpc = Lens.lens (\DescribeChannelResponse' {vpc} -> vpc) (\s@DescribeChannelResponse' {} a -> s {vpc = a} :: DescribeChannelResponse)

-- | Specification of CDI inputs for this channel
describeChannelResponse_cdiInputSpecification :: Lens.Lens' DescribeChannelResponse (Prelude.Maybe CdiInputSpecification)
describeChannelResponse_cdiInputSpecification = Lens.lens (\DescribeChannelResponse' {cdiInputSpecification} -> cdiInputSpecification) (\s@DescribeChannelResponse' {} a -> s {cdiInputSpecification = a} :: DescribeChannelResponse)

-- | The response's http status code.
describeChannelResponse_httpStatus :: Lens.Lens' DescribeChannelResponse Prelude.Int
describeChannelResponse_httpStatus = Lens.lens (\DescribeChannelResponse' {httpStatus} -> httpStatus) (\s@DescribeChannelResponse' {} a -> s {httpStatus = a} :: DescribeChannelResponse)

instance Prelude.NFData DescribeChannelResponse
