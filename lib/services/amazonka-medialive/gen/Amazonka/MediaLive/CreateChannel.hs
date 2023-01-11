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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createChannel'_cdiInputSpecification,
    createChannel'_channelClass,
    createChannel'_destinations,
    createChannel'_encoderSettings,
    createChannel'_inputAttachments,
    createChannel'_inputSpecification,
    createChannel'_logLevel,
    createChannel'_maintenance,
    createChannel'_name,
    createChannel'_requestId,
    createChannel'_reserved,
    createChannel'_roleArn,
    createChannel'_tags,
    createChannel'_vpc,

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
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to create a channel
--
-- /See:/ 'newCreateChannel'' smart constructor.
data CreateChannel' = CreateChannel''
  { -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | The class for this channel. STANDARD for a channel with two pipelines or
    -- SINGLE_PIPELINE for a channel with one pipeline.
    channelClass :: Prelude.Maybe ChannelClass,
    destinations :: Prelude.Maybe [OutputDestination],
    encoderSettings :: Prelude.Maybe EncoderSettings,
    -- | List of input attachments for channel.
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The log level to write to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | Maintenance settings for this channel.
    maintenance :: Prelude.Maybe MaintenanceCreateSettings,
    -- | Name of channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | Unique request ID to be specified. This is needed to prevent retries
    -- from creating multiple resources.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Deprecated field that\'s only usable by whitelisted customers.
    reserved :: Prelude.Maybe Prelude.Text,
    -- | An optional Amazon Resource Name (ARN) of the role to assume when
    -- running the Channel.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | A collection of key-value pairs.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Settings for the VPC outputs
    vpc :: Prelude.Maybe VpcOutputSettings
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
-- 'cdiInputSpecification', 'createChannel'_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'channelClass', 'createChannel'_channelClass' - The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
--
-- 'destinations', 'createChannel'_destinations' - Undocumented member.
--
-- 'encoderSettings', 'createChannel'_encoderSettings' - Undocumented member.
--
-- 'inputAttachments', 'createChannel'_inputAttachments' - List of input attachments for channel.
--
-- 'inputSpecification', 'createChannel'_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'logLevel', 'createChannel'_logLevel' - The log level to write to CloudWatch Logs.
--
-- 'maintenance', 'createChannel'_maintenance' - Maintenance settings for this channel.
--
-- 'name', 'createChannel'_name' - Name of channel.
--
-- 'requestId', 'createChannel'_requestId' - Unique request ID to be specified. This is needed to prevent retries
-- from creating multiple resources.
--
-- 'reserved', 'createChannel'_reserved' - Deprecated field that\'s only usable by whitelisted customers.
--
-- 'roleArn', 'createChannel'_roleArn' - An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel.
--
-- 'tags', 'createChannel'_tags' - A collection of key-value pairs.
--
-- 'vpc', 'createChannel'_vpc' - Settings for the VPC outputs
newCreateChannel' ::
  CreateChannel'
newCreateChannel' =
  CreateChannel''
    { cdiInputSpecification =
        Prelude.Nothing,
      channelClass = Prelude.Nothing,
      destinations = Prelude.Nothing,
      encoderSettings = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      maintenance = Prelude.Nothing,
      name = Prelude.Nothing,
      requestId = Prelude.Nothing,
      reserved = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpc = Prelude.Nothing
    }

-- | Specification of CDI inputs for this channel
createChannel'_cdiInputSpecification :: Lens.Lens' CreateChannel' (Prelude.Maybe CdiInputSpecification)
createChannel'_cdiInputSpecification = Lens.lens (\CreateChannel'' {cdiInputSpecification} -> cdiInputSpecification) (\s@CreateChannel'' {} a -> s {cdiInputSpecification = a} :: CreateChannel')

-- | The class for this channel. STANDARD for a channel with two pipelines or
-- SINGLE_PIPELINE for a channel with one pipeline.
createChannel'_channelClass :: Lens.Lens' CreateChannel' (Prelude.Maybe ChannelClass)
createChannel'_channelClass = Lens.lens (\CreateChannel'' {channelClass} -> channelClass) (\s@CreateChannel'' {} a -> s {channelClass = a} :: CreateChannel')

-- | Undocumented member.
createChannel'_destinations :: Lens.Lens' CreateChannel' (Prelude.Maybe [OutputDestination])
createChannel'_destinations = Lens.lens (\CreateChannel'' {destinations} -> destinations) (\s@CreateChannel'' {} a -> s {destinations = a} :: CreateChannel') Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createChannel'_encoderSettings :: Lens.Lens' CreateChannel' (Prelude.Maybe EncoderSettings)
createChannel'_encoderSettings = Lens.lens (\CreateChannel'' {encoderSettings} -> encoderSettings) (\s@CreateChannel'' {} a -> s {encoderSettings = a} :: CreateChannel')

-- | List of input attachments for channel.
createChannel'_inputAttachments :: Lens.Lens' CreateChannel' (Prelude.Maybe [InputAttachment])
createChannel'_inputAttachments = Lens.lens (\CreateChannel'' {inputAttachments} -> inputAttachments) (\s@CreateChannel'' {} a -> s {inputAttachments = a} :: CreateChannel') Prelude.. Lens.mapping Lens.coerced

-- | Specification of network and file inputs for this channel
createChannel'_inputSpecification :: Lens.Lens' CreateChannel' (Prelude.Maybe InputSpecification)
createChannel'_inputSpecification = Lens.lens (\CreateChannel'' {inputSpecification} -> inputSpecification) (\s@CreateChannel'' {} a -> s {inputSpecification = a} :: CreateChannel')

-- | The log level to write to CloudWatch Logs.
createChannel'_logLevel :: Lens.Lens' CreateChannel' (Prelude.Maybe LogLevel)
createChannel'_logLevel = Lens.lens (\CreateChannel'' {logLevel} -> logLevel) (\s@CreateChannel'' {} a -> s {logLevel = a} :: CreateChannel')

-- | Maintenance settings for this channel.
createChannel'_maintenance :: Lens.Lens' CreateChannel' (Prelude.Maybe MaintenanceCreateSettings)
createChannel'_maintenance = Lens.lens (\CreateChannel'' {maintenance} -> maintenance) (\s@CreateChannel'' {} a -> s {maintenance = a} :: CreateChannel')

-- | Name of channel.
createChannel'_name :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_name = Lens.lens (\CreateChannel'' {name} -> name) (\s@CreateChannel'' {} a -> s {name = a} :: CreateChannel')

-- | Unique request ID to be specified. This is needed to prevent retries
-- from creating multiple resources.
createChannel'_requestId :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_requestId = Lens.lens (\CreateChannel'' {requestId} -> requestId) (\s@CreateChannel'' {} a -> s {requestId = a} :: CreateChannel')

-- | Deprecated field that\'s only usable by whitelisted customers.
createChannel'_reserved :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_reserved = Lens.lens (\CreateChannel'' {reserved} -> reserved) (\s@CreateChannel'' {} a -> s {reserved = a} :: CreateChannel')

-- | An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel.
createChannel'_roleArn :: Lens.Lens' CreateChannel' (Prelude.Maybe Prelude.Text)
createChannel'_roleArn = Lens.lens (\CreateChannel'' {roleArn} -> roleArn) (\s@CreateChannel'' {} a -> s {roleArn = a} :: CreateChannel')

-- | A collection of key-value pairs.
createChannel'_tags :: Lens.Lens' CreateChannel' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChannel'_tags = Lens.lens (\CreateChannel'' {tags} -> tags) (\s@CreateChannel'' {} a -> s {tags = a} :: CreateChannel') Prelude.. Lens.mapping Lens.coerced

-- | Settings for the VPC outputs
createChannel'_vpc :: Lens.Lens' CreateChannel' (Prelude.Maybe VpcOutputSettings)
createChannel'_vpc = Lens.lens (\CreateChannel'' {vpc} -> vpc) (\s@CreateChannel'' {} a -> s {vpc = a} :: CreateChannel')

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
            Prelude.<$> (x Data..?> "channel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChannel' where
  hashWithSalt _salt CreateChannel'' {..} =
    _salt `Prelude.hashWithSalt` cdiInputSpecification
      `Prelude.hashWithSalt` channelClass
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` encoderSettings
      `Prelude.hashWithSalt` inputAttachments
      `Prelude.hashWithSalt` inputSpecification
      `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` maintenance
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` reserved
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData CreateChannel' where
  rnf CreateChannel'' {..} =
    Prelude.rnf cdiInputSpecification
      `Prelude.seq` Prelude.rnf channelClass
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf encoderSettings
      `Prelude.seq` Prelude.rnf inputAttachments
      `Prelude.seq` Prelude.rnf inputSpecification
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf maintenance
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf reserved
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpc

instance Data.ToHeaders CreateChannel' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateChannel' where
  toJSON CreateChannel'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cdiInputSpecification" Data..=)
              Prelude.<$> cdiInputSpecification,
            ("channelClass" Data..=) Prelude.<$> channelClass,
            ("destinations" Data..=) Prelude.<$> destinations,
            ("encoderSettings" Data..=)
              Prelude.<$> encoderSettings,
            ("inputAttachments" Data..=)
              Prelude.<$> inputAttachments,
            ("inputSpecification" Data..=)
              Prelude.<$> inputSpecification,
            ("logLevel" Data..=) Prelude.<$> logLevel,
            ("maintenance" Data..=) Prelude.<$> maintenance,
            ("name" Data..=) Prelude.<$> name,
            ("requestId" Data..=) Prelude.<$> requestId,
            ("reserved" Data..=) Prelude.<$> reserved,
            ("roleArn" Data..=) Prelude.<$> roleArn,
            ("tags" Data..=) Prelude.<$> tags,
            ("vpc" Data..=) Prelude.<$> vpc
          ]
      )

instance Data.ToPath CreateChannel' where
  toPath = Prelude.const "/prod/channels"

instance Data.ToQuery CreateChannel' where
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
