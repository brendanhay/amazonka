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
-- Module      : Network.AWS.MediaLive.CreateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new channel
module Network.AWS.MediaLive.CreateChannel
  ( -- * Creating a Request
    CreateChannel' (..),
    newCreateChannel',

    -- * Request Lenses
    createChannel'_encoderSettings,
    createChannel'_roleArn,
    createChannel'_inputSpecification,
    createChannel'_channelClass,
    createChannel'_logLevel,
    createChannel'_destinations,
    createChannel'_name,
    createChannel'_reserved,
    createChannel'_requestId,
    createChannel'_inputAttachments,
    createChannel'_tags,
    createChannel'_vpc,
    createChannel'_cdiInputSpecification,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_channel,
    createChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to create a channel
--
-- /See:/ 'newCreateChannel'' smart constructor.
data CreateChannel' = CreateChannel''
  { encoderSettings :: Core.Maybe EncoderSettings,
    -- | An optional Amazon Resource Name (ARN) of the role to assume when
    -- running the Channel.
    roleArn :: Core.Maybe Core.Text,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Core.Maybe InputSpecification,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Core.Maybe ChannelClass,
    -- | The log level to write to CloudWatch Logs.
    logLevel :: Core.Maybe LogLevel,
    destinations :: Core.Maybe [OutputDestination],
    -- | Name of channel.
    name :: Core.Maybe Core.Text,
    -- | Deprecated field that\'s only usable by whitelisted customers.
    reserved :: Core.Maybe Core.Text,
    -- | Unique request ID to be specified. This is needed to prevent retries
    -- from creating multiple resources.
    requestId :: Core.Maybe Core.Text,
    -- | List of input attachments for channel.
    inputAttachments :: Core.Maybe [InputAttachment],
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Settings for VPC output
    vpc :: Core.Maybe VpcOutputSettings,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Core.Maybe CdiInputSpecification
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateChannel'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoderSettings', 'createChannel'_encoderSettings' - Undocumented member.
--
-- 'roleArn', 'createChannel'_roleArn' - An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel.
--
-- 'inputSpecification', 'createChannel'_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'channelClass', 'createChannel'_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'logLevel', 'createChannel'_logLevel' - The log level to write to CloudWatch Logs.
--
-- 'destinations', 'createChannel'_destinations' - Undocumented member.
--
-- 'name', 'createChannel'_name' - Name of channel.
--
-- 'reserved', 'createChannel'_reserved' - Deprecated field that\'s only usable by whitelisted customers.
--
-- 'requestId', 'createChannel'_requestId' - Unique request ID to be specified. This is needed to prevent retries
-- from creating multiple resources.
--
-- 'inputAttachments', 'createChannel'_inputAttachments' - List of input attachments for channel.
--
-- 'tags', 'createChannel'_tags' - A collection of key-value pairs.
--
-- 'vpc', 'createChannel'_vpc' - Settings for VPC output
--
-- 'cdiInputSpecification', 'createChannel'_cdiInputSpecification' - Specification of CDI inputs for this channel
newCreateChannel' ::
  CreateChannel'
newCreateChannel' =
  CreateChannel''
    { encoderSettings = Core.Nothing,
      roleArn = Core.Nothing,
      inputSpecification = Core.Nothing,
      channelClass = Core.Nothing,
      logLevel = Core.Nothing,
      destinations = Core.Nothing,
      name = Core.Nothing,
      reserved = Core.Nothing,
      requestId = Core.Nothing,
      inputAttachments = Core.Nothing,
      tags = Core.Nothing,
      vpc = Core.Nothing,
      cdiInputSpecification = Core.Nothing
    }

-- | Undocumented member.
createChannel'_encoderSettings :: Lens.Lens' CreateChannel' (Core.Maybe EncoderSettings)
createChannel'_encoderSettings = Lens.lens (\CreateChannel'' {encoderSettings} -> encoderSettings) (\s@CreateChannel'' {} a -> s {encoderSettings = a} :: CreateChannel')

-- | An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel.
createChannel'_roleArn :: Lens.Lens' CreateChannel' (Core.Maybe Core.Text)
createChannel'_roleArn = Lens.lens (\CreateChannel'' {roleArn} -> roleArn) (\s@CreateChannel'' {} a -> s {roleArn = a} :: CreateChannel')

-- | Specification of network and file inputs for this channel
createChannel'_inputSpecification :: Lens.Lens' CreateChannel' (Core.Maybe InputSpecification)
createChannel'_inputSpecification = Lens.lens (\CreateChannel'' {inputSpecification} -> inputSpecification) (\s@CreateChannel'' {} a -> s {inputSpecification = a} :: CreateChannel')

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
createChannel'_channelClass :: Lens.Lens' CreateChannel' (Core.Maybe ChannelClass)
createChannel'_channelClass = Lens.lens (\CreateChannel'' {channelClass} -> channelClass) (\s@CreateChannel'' {} a -> s {channelClass = a} :: CreateChannel')

-- | The log level to write to CloudWatch Logs.
createChannel'_logLevel :: Lens.Lens' CreateChannel' (Core.Maybe LogLevel)
createChannel'_logLevel = Lens.lens (\CreateChannel'' {logLevel} -> logLevel) (\s@CreateChannel'' {} a -> s {logLevel = a} :: CreateChannel')

-- | Undocumented member.
createChannel'_destinations :: Lens.Lens' CreateChannel' (Core.Maybe [OutputDestination])
createChannel'_destinations = Lens.lens (\CreateChannel'' {destinations} -> destinations) (\s@CreateChannel'' {} a -> s {destinations = a} :: CreateChannel') Core.. Lens.mapping Lens._Coerce

-- | Name of channel.
createChannel'_name :: Lens.Lens' CreateChannel' (Core.Maybe Core.Text)
createChannel'_name = Lens.lens (\CreateChannel'' {name} -> name) (\s@CreateChannel'' {} a -> s {name = a} :: CreateChannel')

-- | Deprecated field that\'s only usable by whitelisted customers.
createChannel'_reserved :: Lens.Lens' CreateChannel' (Core.Maybe Core.Text)
createChannel'_reserved = Lens.lens (\CreateChannel'' {reserved} -> reserved) (\s@CreateChannel'' {} a -> s {reserved = a} :: CreateChannel')

-- | Unique request ID to be specified. This is needed to prevent retries
-- from creating multiple resources.
createChannel'_requestId :: Lens.Lens' CreateChannel' (Core.Maybe Core.Text)
createChannel'_requestId = Lens.lens (\CreateChannel'' {requestId} -> requestId) (\s@CreateChannel'' {} a -> s {requestId = a} :: CreateChannel')

-- | List of input attachments for channel.
createChannel'_inputAttachments :: Lens.Lens' CreateChannel' (Core.Maybe [InputAttachment])
createChannel'_inputAttachments = Lens.lens (\CreateChannel'' {inputAttachments} -> inputAttachments) (\s@CreateChannel'' {} a -> s {inputAttachments = a} :: CreateChannel') Core.. Lens.mapping Lens._Coerce

-- | A collection of key-value pairs.
createChannel'_tags :: Lens.Lens' CreateChannel' (Core.Maybe (Core.HashMap Core.Text Core.Text))
createChannel'_tags = Lens.lens (\CreateChannel'' {tags} -> tags) (\s@CreateChannel'' {} a -> s {tags = a} :: CreateChannel') Core.. Lens.mapping Lens._Coerce

-- | Settings for VPC output
createChannel'_vpc :: Lens.Lens' CreateChannel' (Core.Maybe VpcOutputSettings)
createChannel'_vpc = Lens.lens (\CreateChannel'' {vpc} -> vpc) (\s@CreateChannel'' {} a -> s {vpc = a} :: CreateChannel')

-- | Specification of CDI inputs for this channel
createChannel'_cdiInputSpecification :: Lens.Lens' CreateChannel' (Core.Maybe CdiInputSpecification)
createChannel'_cdiInputSpecification = Lens.lens (\CreateChannel'' {cdiInputSpecification} -> cdiInputSpecification) (\s@CreateChannel'' {} a -> s {cdiInputSpecification = a} :: CreateChannel')

instance Core.AWSRequest CreateChannel' where
  type
    AWSResponse CreateChannel' =
      CreateChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Core.<$> (x Core..?> "channel")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateChannel'

instance Core.NFData CreateChannel'

instance Core.ToHeaders CreateChannel' where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateChannel' where
  toJSON CreateChannel'' {..} =
    Core.object
      ( Core.catMaybes
          [ ("encoderSettings" Core..=)
              Core.<$> encoderSettings,
            ("roleArn" Core..=) Core.<$> roleArn,
            ("inputSpecification" Core..=)
              Core.<$> inputSpecification,
            ("channelClass" Core..=) Core.<$> channelClass,
            ("logLevel" Core..=) Core.<$> logLevel,
            ("destinations" Core..=) Core.<$> destinations,
            ("name" Core..=) Core.<$> name,
            ("reserved" Core..=) Core.<$> reserved,
            ("requestId" Core..=) Core.<$> requestId,
            ("inputAttachments" Core..=)
              Core.<$> inputAttachments,
            ("tags" Core..=) Core.<$> tags,
            ("vpc" Core..=) Core.<$> vpc,
            ("cdiInputSpecification" Core..=)
              Core.<$> cdiInputSpecification
          ]
      )

instance Core.ToPath CreateChannel' where
  toPath = Core.const "/prod/channels"

instance Core.ToQuery CreateChannel' where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for CreateChannelResponse
--
-- /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { channel :: Core.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'createChannelResponse_channel' - Undocumented member.
--
-- 'httpStatus', 'createChannelResponse_httpStatus' - The response's http status code.
newCreateChannelResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { channel = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createChannelResponse_channel :: Lens.Lens' CreateChannelResponse (Core.Maybe Channel)
createChannelResponse_channel = Lens.lens (\CreateChannelResponse' {channel} -> channel) (\s@CreateChannelResponse' {} a -> s {channel = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Core.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Core.NFData CreateChannelResponse
