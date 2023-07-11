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
-- Module      : Amazonka.MediaLive.UpdateChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a channel.
module Amazonka.MediaLive.UpdateChannel
  ( -- * Creating a Request
    UpdateChannel' (..),
    newUpdateChannel',

    -- * Request Lenses
    updateChannel'_cdiInputSpecification,
    updateChannel'_destinations,
    updateChannel'_encoderSettings,
    updateChannel'_inputAttachments,
    updateChannel'_inputSpecification,
    updateChannel'_logLevel,
    updateChannel'_maintenance,
    updateChannel'_name,
    updateChannel'_roleArn,
    updateChannel'_channelId,

    -- * Destructuring the Response
    UpdateChannelResponse (..),
    newUpdateChannelResponse,

    -- * Response Lenses
    updateChannelResponse_channel,
    updateChannelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update a channel.
--
-- /See:/ 'newUpdateChannel'' smart constructor.
data UpdateChannel' = UpdateChannel''
  { -- | Specification of CDI inputs for this channel
    cdiInputSpecification :: Prelude.Maybe CdiInputSpecification,
    -- | A list of output destinations for this channel.
    destinations :: Prelude.Maybe [OutputDestination],
    -- | The encoder settings for this channel.
    encoderSettings :: Prelude.Maybe EncoderSettings,
    inputAttachments :: Prelude.Maybe [InputAttachment],
    -- | Specification of network and file inputs for this channel
    inputSpecification :: Prelude.Maybe InputSpecification,
    -- | The log level to write to CloudWatch Logs.
    logLevel :: Prelude.Maybe LogLevel,
    -- | Maintenance settings for this channel.
    maintenance :: Prelude.Maybe MaintenanceUpdateSettings,
    -- | The name of the channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | An optional Amazon Resource Name (ARN) of the role to assume when
    -- running the Channel. If you do not specify this on an update call but
    -- the role was previously set that role will be removed.
    roleArn :: Prelude.Maybe Prelude.Text,
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
-- 'cdiInputSpecification', 'updateChannel'_cdiInputSpecification' - Specification of CDI inputs for this channel
--
-- 'destinations', 'updateChannel'_destinations' - A list of output destinations for this channel.
--
-- 'encoderSettings', 'updateChannel'_encoderSettings' - The encoder settings for this channel.
--
-- 'inputAttachments', 'updateChannel'_inputAttachments' - Undocumented member.
--
-- 'inputSpecification', 'updateChannel'_inputSpecification' - Specification of network and file inputs for this channel
--
-- 'logLevel', 'updateChannel'_logLevel' - The log level to write to CloudWatch Logs.
--
-- 'maintenance', 'updateChannel'_maintenance' - Maintenance settings for this channel.
--
-- 'name', 'updateChannel'_name' - The name of the channel.
--
-- 'roleArn', 'updateChannel'_roleArn' - An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel. If you do not specify this on an update call but
-- the role was previously set that role will be removed.
--
-- 'channelId', 'updateChannel'_channelId' - channel ID
newUpdateChannel' ::
  -- | 'channelId'
  Prelude.Text ->
  UpdateChannel'
newUpdateChannel' pChannelId_ =
  UpdateChannel''
    { cdiInputSpecification =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      encoderSettings = Prelude.Nothing,
      inputAttachments = Prelude.Nothing,
      inputSpecification = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      maintenance = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      channelId = pChannelId_
    }

-- | Specification of CDI inputs for this channel
updateChannel'_cdiInputSpecification :: Lens.Lens' UpdateChannel' (Prelude.Maybe CdiInputSpecification)
updateChannel'_cdiInputSpecification = Lens.lens (\UpdateChannel'' {cdiInputSpecification} -> cdiInputSpecification) (\s@UpdateChannel'' {} a -> s {cdiInputSpecification = a} :: UpdateChannel')

-- | A list of output destinations for this channel.
updateChannel'_destinations :: Lens.Lens' UpdateChannel' (Prelude.Maybe [OutputDestination])
updateChannel'_destinations = Lens.lens (\UpdateChannel'' {destinations} -> destinations) (\s@UpdateChannel'' {} a -> s {destinations = a} :: UpdateChannel') Prelude.. Lens.mapping Lens.coerced

-- | The encoder settings for this channel.
updateChannel'_encoderSettings :: Lens.Lens' UpdateChannel' (Prelude.Maybe EncoderSettings)
updateChannel'_encoderSettings = Lens.lens (\UpdateChannel'' {encoderSettings} -> encoderSettings) (\s@UpdateChannel'' {} a -> s {encoderSettings = a} :: UpdateChannel')

-- | Undocumented member.
updateChannel'_inputAttachments :: Lens.Lens' UpdateChannel' (Prelude.Maybe [InputAttachment])
updateChannel'_inputAttachments = Lens.lens (\UpdateChannel'' {inputAttachments} -> inputAttachments) (\s@UpdateChannel'' {} a -> s {inputAttachments = a} :: UpdateChannel') Prelude.. Lens.mapping Lens.coerced

-- | Specification of network and file inputs for this channel
updateChannel'_inputSpecification :: Lens.Lens' UpdateChannel' (Prelude.Maybe InputSpecification)
updateChannel'_inputSpecification = Lens.lens (\UpdateChannel'' {inputSpecification} -> inputSpecification) (\s@UpdateChannel'' {} a -> s {inputSpecification = a} :: UpdateChannel')

-- | The log level to write to CloudWatch Logs.
updateChannel'_logLevel :: Lens.Lens' UpdateChannel' (Prelude.Maybe LogLevel)
updateChannel'_logLevel = Lens.lens (\UpdateChannel'' {logLevel} -> logLevel) (\s@UpdateChannel'' {} a -> s {logLevel = a} :: UpdateChannel')

-- | Maintenance settings for this channel.
updateChannel'_maintenance :: Lens.Lens' UpdateChannel' (Prelude.Maybe MaintenanceUpdateSettings)
updateChannel'_maintenance = Lens.lens (\UpdateChannel'' {maintenance} -> maintenance) (\s@UpdateChannel'' {} a -> s {maintenance = a} :: UpdateChannel')

-- | The name of the channel.
updateChannel'_name :: Lens.Lens' UpdateChannel' (Prelude.Maybe Prelude.Text)
updateChannel'_name = Lens.lens (\UpdateChannel'' {name} -> name) (\s@UpdateChannel'' {} a -> s {name = a} :: UpdateChannel')

-- | An optional Amazon Resource Name (ARN) of the role to assume when
-- running the Channel. If you do not specify this on an update call but
-- the role was previously set that role will be removed.
updateChannel'_roleArn :: Lens.Lens' UpdateChannel' (Prelude.Maybe Prelude.Text)
updateChannel'_roleArn = Lens.lens (\UpdateChannel'' {roleArn} -> roleArn) (\s@UpdateChannel'' {} a -> s {roleArn = a} :: UpdateChannel')

-- | channel ID
updateChannel'_channelId :: Lens.Lens' UpdateChannel' Prelude.Text
updateChannel'_channelId = Lens.lens (\UpdateChannel'' {channelId} -> channelId) (\s@UpdateChannel'' {} a -> s {channelId = a} :: UpdateChannel')

instance Core.AWSRequest UpdateChannel' where
  type
    AWSResponse UpdateChannel' =
      UpdateChannelResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelResponse'
            Prelude.<$> (x Data..?> "channel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannel' where
  hashWithSalt _salt UpdateChannel'' {..} =
    _salt
      `Prelude.hashWithSalt` cdiInputSpecification
      `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` encoderSettings
      `Prelude.hashWithSalt` inputAttachments
      `Prelude.hashWithSalt` inputSpecification
      `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` maintenance
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` channelId

instance Prelude.NFData UpdateChannel' where
  rnf UpdateChannel'' {..} =
    Prelude.rnf cdiInputSpecification
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf encoderSettings
      `Prelude.seq` Prelude.rnf inputAttachments
      `Prelude.seq` Prelude.rnf inputSpecification
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf maintenance
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf channelId

instance Data.ToHeaders UpdateChannel' where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateChannel' where
  toJSON UpdateChannel'' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cdiInputSpecification" Data..=)
              Prelude.<$> cdiInputSpecification,
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
            ("roleArn" Data..=) Prelude.<$> roleArn
          ]
      )

instance Data.ToPath UpdateChannel' where
  toPath UpdateChannel'' {..} =
    Prelude.mconcat
      ["/prod/channels/", Data.toBS channelId]

instance Data.ToQuery UpdateChannel' where
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

instance Prelude.NFData UpdateChannelResponse where
  rnf UpdateChannelResponse' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf httpStatus
