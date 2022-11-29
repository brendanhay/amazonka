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
-- Module      : Amazonka.MediaLive.CreateChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new channel
module Amazonka.MediaLive.CreateChannel
  ( -- * Creating a Request
    CreateChannel' (..),
    newCreateChannel',

    -- * Request Lenses
    createChannel'_tags,
    createChannel'_name,
    createChannel'_maintenance,
    createChannel'_roleArn,
    createChannel'_vpc,
    createChannel'_logLevel,
    createChannel'_requestId,
    createChannel'_inputSpecification,
    createChannel'_channelClass,
    createChannel'_cdiInputSpecification,
    createChannel'_inputAttachments,
    createChannel'_destinations,
    createChannel'_reserved,
    createChannel'_encoderSettings,

    -- * Destructuring the Response
    CreateChannelResponse (..),
    newCreateChannelResponse,

    -- * Response Lenses
    createChannelResponse_channel,
    createChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to create a channel
--
-- /See:/ 'newCreateChannel'' smart constructor.
data CreateChannel' = CreateChannel''
  { -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Name of channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | Maintenance settings for this channel.
    maintenance :: Prelude.Maybe MaintenanceCreateSettings,
    -- | An optional Amazon Resource Name (ARN) of the role to assume when
    -- running the Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Settings for the VPC outputs
    vpc :: Prelude.Maybe VpcOutputSettings,
    -- | The log level to write to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | Unique request ID to be specified. This is needed to prevent retries
    -- from creating multiple resources.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Prelude.Maybe ChannelClass,
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    destinations :: Prelude.Maybe [OutputDestination],
    -- | Deprecated field that\'s only usable by whitelisted customers.
    reserved :: Prelude.Maybe Prelude.Text,
    encoderSettings :: Prelude.Maybe EncoderSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChannel'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createChannel'_tags' - A collection of key-value pairs.
--
-- 'name', 'createChannel'_name' - Name of channel.
--
-- 'maintenance', 'createChannel'_maintenance' - Maintenance settings for this channel.
--
-- 'roleArn', 'createChannel'_roleArn' - An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel.
--
-- 'vpc', 'createChannel'_vpc' - Settings for the VPC outputs
--
-- 'logLevel', 'createChannel'_logLevel' - The log level to write to CloudWatch Logs.
--
-- 'requestId', 'createChannel'_requestId' - Unique request ID to be specified. This is needed to prevent retries
-- from creating multiple resources.
--
-- 'inputSpecification', 'createChannel'_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'channelClass', 'createChannel'_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'cdiInputSpecification', 'createChannel'_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'inputAttachments', 'createChannel'_inputAttachments' - List of input attachments for channel.
--
-- 'destinations', 'createChannel'_destinations' - Undocumented member.
--
-- 'reserved', 'createChannel'_reserved' - Deprecated field that\'s only usable by whitelisted customers.
--
-- 'encoderSettings', 'createChannel'_encoderSettings' - Undocumented member.
newCreateChannel' ::
  CreateChannel'
newCreateChannel' =
  CreateChannel''
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      maintenance = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      vpc = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      requestId = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      channelClass = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      destinations = Prelude.Nothing,
      reserved = Prelude.Nothing,
      encoderSettings = Prelude.Nothing
    }

-- | A collection of key-value pairs.
createChannel'_tags :: Lens.Lens' CreateChannel' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannel'_tags = Lens.lens (\CreateChannel'' {tags} -> tags) (\s@CreateChannel'' {} a -> s {tags = a} :: CreateChannel') Prelude.. Lens.mapping Lens.coerced

-- | Name of channel.
createChannel'_name :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_name = Lens.lens (\CreateChannel'' {name} -> name) (\s@CreateChannel'' {} a -> s {name = a} :: CreateChannel')

-- | Maintenance settings for this channel.
createChannel'_maintenance :: Lens.Lens' CreateChannel' (Prelude.Maybe MaintenanceCreateSettings)
createChannel'_maintenance = Lens.lens (\CreateChannel'' {maintenance} -> maintenance) (\s@CreateChannel'' {} a -> s {maintenance = a} :: CreateChannel')

-- | An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel.
createChannel'_roleArn :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_roleArn = Lens.lens (\CreateChannel'' {roleArn} -> roleArn) (\s@CreateChannel'' {} a -> s {roleArn = a} :: CreateChannel')

-- | Settings for the VPC outputs
createChannel'_vpc :: Lens.Lens' CreateChannel' (Prelude.Maybe VpcOutputSettings)
createChannel'_vpc = Lens.lens (\CreateChannel'' {vpc} -> vpc) (\s@CreateChannel'' {} a -> s {vpc = a} :: CreateChannel')

-- | The log level to write to CloudWatch Logs.
createChannel'_logLevel :: Lens.Lens' CreateChannel' (Prelude.Maybe LogLevel)
createChannel'_logLevel = Lens.lens (\CreateChannel'' {logLevel} -> logLevel) (\s@CreateChannel'' {} a -> s {logLevel = a} :: CreateChannel')

-- | Unique request ID to be specified. This is needed to prevent retries
-- from creating multiple resources.
createChannel'_requestId :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_requestId = Lens.lens (\CreateChannel'' {requestId} -> requestId) (\s@CreateChannel'' {} a -> s {requestId = a} :: CreateChannel')

-- | Specification of network and file inputs for this channel
createChannel'_inputSpecification :: Lens.Lens' CreateChannel' (Prelude.Maybe InputSpecification)
createChannel'_inputSpecification = Lens.lens (\CreateChannel'' {inputSpecification} -> inputSpecification) (\s@CreateChannel'' {} a -> s {inputSpecification = a} :: CreateChannel')

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
createChannel'_channelClass :: Lens.Lens' CreateChannel' (Prelude.Maybe ChannelClass)
createChannel'_channelClass = Lens.lens (\CreateChannel'' {channelClass} -> channelClass) (\s@CreateChannel'' {} a -> s {channelClass = a} :: CreateChannel')

-- | Specification of CDI inputs for this channel
createChannel'_cdiInputSpecification :: Lens.Lens' CreateChannel' (Prelude.Maybe CdiInputSpecification)
createChannel'_cdiInputSpecification = Lens.lens (\CreateChannel'' {cdiInputSpecification} -> cdiInputSpecification) (\s@CreateChannel'' {} a -> s {cdiInputSpecification = a} :: CreateChannel')

-- | List of input attachments for channel.
createChannel'_inputAttachments :: Lens.Lens' CreateChannel' (Prelude.Maybe [InputAttachment])
createChannel'_inputAttachments = Lens.lens (\CreateChannel'' {inputAttachments} -> inputAttachments) (\s@CreateChannel'' {} a -> s {inputAttachments = a} :: CreateChannel') Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createChannel'_destinations :: Lens.Lens' CreateChannel' (Prelude.Maybe [OutputDestination])
createChannel'_destinations = Lens.lens (\CreateChannel'' {destinations} -> destinations) (\s@CreateChannel'' {} a -> s {destinations = a} :: CreateChannel') Prelude.. Lens.mapping Lens.coerced

-- | Deprecated field that\'s only usable by whitelisted customers.
createChannel'_reserved :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_reserved = Lens.lens (\CreateChannel'' {reserved} -> reserved) (\s@CreateChannel'' {} a -> s {reserved = a} :: CreateChannel')

-- | Undocumented member.
createChannel'_encoderSettings :: Lens.Lens' CreateChannel' (Prelude.Maybe EncoderSettings)
createChannel'_encoderSettings = Lens.lens (\CreateChannel'' {encoderSettings} -> encoderSettings) (\s@CreateChannel'' {} a -> s {encoderSettings = a} :: CreateChannel')

instance Core.AWSRequest CreateChannel' where
  type
    AWSResponse CreateChannel' =
      CreateChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChannelResponse'
            Prelude.<$> (x Core..?> "channel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel' where
  hashWithSalt _salt CreateChannel'' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` maintenance
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` vpc
      `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` inputSpecification
      `Prelude.hashWithSalt` channelClass
      `Prelude.hashWithSalt` cdiInputSpecification
      `Prelude.hashWithSalt` inputAttachments
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` reserved
      `Prelude.hashWithSalt` encoderSettings

instance Prelude.NFData CreateChannel' where
  rnf CreateChannel'' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf maintenance
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf vpc
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf inputSpecification
      `Prelude.seq` Prelude.rnf channelClass
      `Prelude.seq` Prelude.rnf cdiInputSpecification
      `Prelude.seq` Prelude.rnf inputAttachments
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf reserved
      `Prelude.seq` Prelude.rnf encoderSettings

instance Core.ToHeaders CreateChannel' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateChannel' where
  toJSON CreateChannel'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("name" Core..=) Prelude.<$> name,
            ("maintenance" Core..=) Prelude.<$> maintenance,
            ("roleArn" Core..=) Prelude.<$> roleArn,
            ("vpc" Core..=) Prelude.<$> vpc,
            ("logLevel" Core..=) Prelude.<$> logLevel,
            ("requestId" Core..=) Prelude.<$> requestId,
            ("inputSpecification" Core..=)
              Prelude.<$> inputSpecification,
            ("channelClass" Core..=) Prelude.<$> channelClass,
            ("cdiInputSpecification" Core..=)
              Prelude.<$> cdiInputSpecification,
            ("inputAttachments" Core..=)
              Prelude.<$> inputAttachments,
            ("destinations" Core..=) Prelude.<$> destinations,
            ("reserved" Core..=) Prelude.<$> reserved,
            ("encoderSettings" Core..=)
              Prelude.<$> encoderSettings
          ]
      )

instance Core.ToPath CreateChannel' where
  toPath = Prelude.const "/prod/channels"

instance Core.ToQuery CreateChannel' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for CreateChannelResponse
--
-- /See:/ 'newCreateChannelResponse' smart constructor.
data CreateChannelResponse = CreateChannelResponse'
  { channel :: Prelude.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateChannelResponse
newCreateChannelResponse pHttpStatus_ =
  CreateChannelResponse'
    { channel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createChannelResponse_channel :: Lens.Lens' CreateChannelResponse (Prelude.Maybe Channel)
createChannelResponse_channel = Lens.lens (\CreateChannelResponse' {channel} -> channel) (\s@CreateChannelResponse' {} a -> s {channel = a} :: CreateChannelResponse)

-- | The response's http status code.
createChannelResponse_httpStatus :: Lens.Lens' CreateChannelResponse Prelude.Int
createChannelResponse_httpStatus = Lens.lens (\CreateChannelResponse' {httpStatus} -> httpStatus) (\s@CreateChannelResponse' {} a -> s {httpStatus = a} :: CreateChannelResponse)

instance Prelude.NFData CreateChannelResponse where
  rnf CreateChannelResponse' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf httpStatus
