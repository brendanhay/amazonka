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
-- Module      : Network.AWS.MediaLive.UpdateChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a channel.
module Network.AWS.MediaLive.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel' (..),
    newUpdateChannel',

    -- * Request Lenses
    updateChannel'_encoderSettings,
    updateChannel'_roleArn,
    updateChannel'_inputSpecification,
    updateChannel'_logLevel,
    updateChannel'_destinations,
    updateChannel'_name,
    updateChannel'_inputAttachments,
    updateChannel'_cdiInputSpecification,
    updateChannel'_channelId,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,

    -- * Response Lenses
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to update a channel.
--
-- /See:/ 'newUpdateChannel'' smart constructor.
data UpdateChannel' = UpdateChannel''
  { -- | The encoder settings for this channel.
    encoderSettings :: Prelude.Maybe EncoderSettings,
    -- | An optional Amazon Resource Name (ARN) of the role to assume when
    -- running the Channel. If you do not specify this on an update call but
    -- the role was previously set that role will be removed.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The log level to write to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | A list of output destinations for this channel.
    destinations :: Prelude.Maybe [OutputDestination],
    -- | The name of the channel.
    name :: Prelude.Maybe Prelude.Text,
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | channel ID
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannel'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encoderSettings', 'updateChannel'_encoderSettings' - The encoder settings for this channel.
--
-- 'roleArn', 'updateChannel'_roleArn' - An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel. If you do not specify this on an update call but
-- the role was previously set that role will be removed.
--
-- 'inputSpecification', 'updateChannel'_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'logLevel', 'updateChannel'_logLevel' - The log level to write to CloudWatch Logs.
--
-- 'destinations', 'updateChannel'_destinations' - A list of output destinations for this channel.
--
-- 'name', 'updateChannel'_name' - The name of the channel.
--
-- 'inputAttachments', 'updateChannel'_inputAttachments' - Undocumented member.
--
-- 'cdiInputSpecification', 'updateChannel'_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'channelId', 'updateChannel'_channelId' - channel ID
newUpdateChannel' ::
  -- | 'channelId'
  Prelude.Text ->
  UpdateChannel'
newUpdateChannel' pChannelId_ =
  UpdateChannel''
    { encoderSettings = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      destinations = Prelude.Nothing,
      name = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      cdiInputSpecification = Prelude.Nothing,
      channelId = pChannelId_
    }

-- | The encoder settings for this channel.
updateChannel'_encoderSettings :: Lens.Lens' UpdateChannel' (Prelude.Maybe EncoderSettings)
updateChannel'_encoderSettings = Lens.lens (\UpdateChannel'' {encoderSettings} -> encoderSettings) (\s@UpdateChannel'' {} a -> s {encoderSettings = a} :: UpdateChannel')

-- | An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel. If you do not specify this on an update call but
-- the role was previously set that role will be removed.
updateChannel'_roleArn :: Lens.Lens' UpdateChannel' (Prelude.Maybe Prelude.Text)
updateChannel'_roleArn = Lens.lens (\UpdateChannel'' {roleArn} -> roleArn) (\s@UpdateChannel'' {} a -> s {roleArn = a} :: UpdateChannel')

-- | Specification of network and file inputs for this channel
updateChannel'_inputSpecification :: Lens.Lens' UpdateChannel' (Prelude.Maybe InputSpecification)
updateChannel'_inputSpecification = Lens.lens (\UpdateChannel'' {inputSpecification} -> inputSpecification) (\s@UpdateChannel'' {} a -> s {inputSpecification = a} :: UpdateChannel')

-- | The log level to write to CloudWatch Logs.
updateChannel'_logLevel :: Lens.Lens' UpdateChannel' (Prelude.Maybe LogLevel)
updateChannel'_logLevel = Lens.lens (\UpdateChannel'' {logLevel} -> logLevel) (\s@UpdateChannel'' {} a -> s {logLevel = a} :: UpdateChannel')

-- | A list of output destinations for this channel.
updateChannel'_destinations :: Lens.Lens' UpdateChannel' (Prelude.Maybe [OutputDestination])
updateChannel'_destinations = Lens.lens (\UpdateChannel'' {destinations} -> destinations) (\s@UpdateChannel'' {} a -> s {destinations = a} :: UpdateChannel') Prelude.. Lens.mapping Lens._Coerce

-- | The name of the channel.
updateChannel'_name :: Lens.Lens' UpdateChannel' (Prelude.Maybe Prelude.Text)
updateChannel'_name = Lens.lens (\UpdateChannel'' {name} -> name) (\s@UpdateChannel'' {} a -> s {name = a} :: UpdateChannel')

-- | Undocumented member.
updateChannel'_inputAttachments :: Lens.Lens' UpdateChannel' (Prelude.Maybe [InputAttachment])
updateChannel'_inputAttachments = Lens.lens (\UpdateChannel'' {inputAttachments} -> inputAttachments) (\s@UpdateChannel'' {} a -> s {inputAttachments = a} :: UpdateChannel') Prelude.. Lens.mapping Lens._Coerce

-- | Specification of CDI inputs for this channel
updateChannel'_cdiInputSpecification :: Lens.Lens' UpdateChannel' (Prelude.Maybe CdiInputSpecification)
updateChannel'_cdiInputSpecification = Lens.lens (\UpdateChannel'' {cdiInputSpecification} -> cdiInputSpecification) (\s@UpdateChannel'' {} a -> s {cdiInputSpecification = a} :: UpdateChannel')

-- | channel ID
updateChannel'_channelId :: Lens.Lens' UpdateChannel' Prelude.Text
updateChannel'_channelId = Lens.lens (\UpdateChannel'' {channelId} -> channelId) (\s@UpdateChannel'' {} a -> s {channelId = a} :: UpdateChannel')

instance Core.AWSRequest UpdateChannel' where
  type
    AWSResponse UpdateChannel' =
      UpdateChannelResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Prelude.<$> (x Core..?> "channel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannel'

instance Prelude.NFData UpdateChannel'

instance Core.ToHeaders UpdateChannel' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateChannel' where
  toJSON UpdateChannel'' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encoderSettings" Core..=)
              Prelude.<$> encoderSettings,
            ("roleArn" Core..=) Prelude.<$> roleArn,
            ("inputSpecification" Core..=)
              Prelude.<$> inputSpecification,
            ("logLevel" Core..=) Prelude.<$> logLevel,
            ("destinations" Core..=) Prelude.<$> destinations,
            ("name" Core..=) Prelude.<$> name,
            ("inputAttachments" Core..=)
              Prelude.<$> inputAttachments,
            ("cdiInputSpecification" Core..=)
              Prelude.<$> cdiInputSpecification
          ]
      )

instance Core.ToPath UpdateChannel' where
  toPath UpdateChannel'' {..} =
    Prelude.mconcat
      ["/prod/channels/", Core.toBS channelId]

instance Core.ToQuery UpdateChannel' where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for UpdateChannelResponse
--
-- /See:/ 'newUpdateChannelResponse' smart constructor.
data UpdateChannelResponse = UpdateChannelResponse'
  { channel :: Prelude.Maybe Channel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channel', 'updateChannelResponse_channel' - Undocumented member.
--
-- 'httpStatus', 'updateChannelResponse_httpStatus' - The response's http status code.
newUpdateChannelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelResponse
newUpdateChannelResponse pHttpStatus_ =
  UpdateChannelResponse'
    { channel = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateChannelResponse_channel :: Lens.Lens' UpdateChannelResponse (Prelude.Maybe Channel)
updateChannelResponse_channel = Lens.lens (\UpdateChannelResponse' {channel} -> channel) (\s@UpdateChannelResponse' {} a -> s {channel = a} :: UpdateChannelResponse)

-- | The response's http status code.
updateChannelResponse_httpStatus :: Lens.Lens' UpdateChannelResponse Prelude.Int
updateChannelResponse_httpStatus = Lens.lens (\UpdateChannelResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelResponse' {} a -> s {httpStatus = a} :: UpdateChannelResponse)

instance Prelude.NFData UpdateChannelResponse
