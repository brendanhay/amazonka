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
-- Module      : Amazonka.ChimeSDKMessaging.PutChannelExpirationSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the number of days before the channel is automatically deleted.
--
-- -   A background process deletes expired channels within 6 hours of
--     expiration. Actual deletion times may vary.
--
-- -   Expired channels that have not yet been deleted appear as active,
--     and you can update their expiration settings. The system honors the
--     new settings.
--
-- -   The @x-amz-chime-bearer@ request header is mandatory. Use the ARN of
--     the @AppInstanceUser@ or @AppInstanceBot@ that makes the API call as
--     the value in the header.
module Amazonka.ChimeSDKMessaging.PutChannelExpirationSettings
  ( -- * Creating a Request
    PutChannelExpirationSettings (..),
    newPutChannelExpirationSettings,

    -- * Request Lenses
    putChannelExpirationSettings_chimeBearer,
    putChannelExpirationSettings_expirationSettings,
    putChannelExpirationSettings_channelArn,

    -- * Destructuring the Response
    PutChannelExpirationSettingsResponse (..),
    newPutChannelExpirationSettingsResponse,

    -- * Response Lenses
    putChannelExpirationSettingsResponse_channelArn,
    putChannelExpirationSettingsResponse_expirationSettings,
    putChannelExpirationSettingsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutChannelExpirationSettings' smart constructor.
data PutChannelExpirationSettings = PutChannelExpirationSettings'
  { -- | The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
    -- call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | Settings that control the interval after which a channel is deleted.
    expirationSettings :: Prelude.Maybe ExpirationSettings,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChannelExpirationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'putChannelExpirationSettings_chimeBearer' - The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
-- call.
--
-- 'expirationSettings', 'putChannelExpirationSettings_expirationSettings' - Settings that control the interval after which a channel is deleted.
--
-- 'channelArn', 'putChannelExpirationSettings_channelArn' - The ARN of the channel.
newPutChannelExpirationSettings ::
  -- | 'channelArn'
  Prelude.Text ->
  PutChannelExpirationSettings
newPutChannelExpirationSettings pChannelArn_ =
  PutChannelExpirationSettings'
    { chimeBearer =
        Prelude.Nothing,
      expirationSettings = Prelude.Nothing,
      channelArn = pChannelArn_
    }

-- | The ARN of the @AppInstanceUser@ or @AppInstanceBot@ that makes the API
-- call.
putChannelExpirationSettings_chimeBearer :: Lens.Lens' PutChannelExpirationSettings (Prelude.Maybe Prelude.Text)
putChannelExpirationSettings_chimeBearer = Lens.lens (\PutChannelExpirationSettings' {chimeBearer} -> chimeBearer) (\s@PutChannelExpirationSettings' {} a -> s {chimeBearer = a} :: PutChannelExpirationSettings)

-- | Settings that control the interval after which a channel is deleted.
putChannelExpirationSettings_expirationSettings :: Lens.Lens' PutChannelExpirationSettings (Prelude.Maybe ExpirationSettings)
putChannelExpirationSettings_expirationSettings = Lens.lens (\PutChannelExpirationSettings' {expirationSettings} -> expirationSettings) (\s@PutChannelExpirationSettings' {} a -> s {expirationSettings = a} :: PutChannelExpirationSettings)

-- | The ARN of the channel.
putChannelExpirationSettings_channelArn :: Lens.Lens' PutChannelExpirationSettings Prelude.Text
putChannelExpirationSettings_channelArn = Lens.lens (\PutChannelExpirationSettings' {channelArn} -> channelArn) (\s@PutChannelExpirationSettings' {} a -> s {channelArn = a} :: PutChannelExpirationSettings)

instance Core.AWSRequest PutChannelExpirationSettings where
  type
    AWSResponse PutChannelExpirationSettings =
      PutChannelExpirationSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutChannelExpirationSettingsResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "ExpirationSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutChannelExpirationSettings
  where
  hashWithSalt _salt PutChannelExpirationSettings' {..} =
    _salt
      `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` expirationSettings
      `Prelude.hashWithSalt` channelArn

instance Prelude.NFData PutChannelExpirationSettings where
  rnf PutChannelExpirationSettings' {..} =
    Prelude.rnf chimeBearer
      `Prelude.seq` Prelude.rnf expirationSettings
      `Prelude.seq` Prelude.rnf channelArn

instance Data.ToHeaders PutChannelExpirationSettings where
  toHeaders PutChannelExpirationSettings' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON PutChannelExpirationSettings where
  toJSON PutChannelExpirationSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpirationSettings" Data..=)
              Prelude.<$> expirationSettings
          ]
      )

instance Data.ToPath PutChannelExpirationSettings where
  toPath PutChannelExpirationSettings' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/expiration-settings"
      ]

instance Data.ToQuery PutChannelExpirationSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutChannelExpirationSettingsResponse' smart constructor.
data PutChannelExpirationSettingsResponse = PutChannelExpirationSettingsResponse'
  { -- | The channel ARN.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | Settings that control the interval after which a channel is deleted.
    expirationSettings :: Prelude.Maybe ExpirationSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChannelExpirationSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'putChannelExpirationSettingsResponse_channelArn' - The channel ARN.
--
-- 'expirationSettings', 'putChannelExpirationSettingsResponse_expirationSettings' - Settings that control the interval after which a channel is deleted.
--
-- 'httpStatus', 'putChannelExpirationSettingsResponse_httpStatus' - The response's http status code.
newPutChannelExpirationSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutChannelExpirationSettingsResponse
newPutChannelExpirationSettingsResponse pHttpStatus_ =
  PutChannelExpirationSettingsResponse'
    { channelArn =
        Prelude.Nothing,
      expirationSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The channel ARN.
putChannelExpirationSettingsResponse_channelArn :: Lens.Lens' PutChannelExpirationSettingsResponse (Prelude.Maybe Prelude.Text)
putChannelExpirationSettingsResponse_channelArn = Lens.lens (\PutChannelExpirationSettingsResponse' {channelArn} -> channelArn) (\s@PutChannelExpirationSettingsResponse' {} a -> s {channelArn = a} :: PutChannelExpirationSettingsResponse)

-- | Settings that control the interval after which a channel is deleted.
putChannelExpirationSettingsResponse_expirationSettings :: Lens.Lens' PutChannelExpirationSettingsResponse (Prelude.Maybe ExpirationSettings)
putChannelExpirationSettingsResponse_expirationSettings = Lens.lens (\PutChannelExpirationSettingsResponse' {expirationSettings} -> expirationSettings) (\s@PutChannelExpirationSettingsResponse' {} a -> s {expirationSettings = a} :: PutChannelExpirationSettingsResponse)

-- | The response's http status code.
putChannelExpirationSettingsResponse_httpStatus :: Lens.Lens' PutChannelExpirationSettingsResponse Prelude.Int
putChannelExpirationSettingsResponse_httpStatus = Lens.lens (\PutChannelExpirationSettingsResponse' {httpStatus} -> httpStatus) (\s@PutChannelExpirationSettingsResponse' {} a -> s {httpStatus = a} :: PutChannelExpirationSettingsResponse)

instance
  Prelude.NFData
    PutChannelExpirationSettingsResponse
  where
  rnf PutChannelExpirationSettingsResponse' {..} =
    Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf expirationSettings
      `Prelude.seq` Prelude.rnf httpStatus
