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
-- Module      : Amazonka.Chime.UpdateChannelReadMarker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The details of the time when a user last read messages in a channel.
--
-- The @x-amz-chime-bearer@ request header is mandatory. Use the
-- @AppInstanceUserArn@ of the user that makes the API call as the value in
-- the header.
module Amazonka.Chime.UpdateChannelReadMarker
  ( -- * Creating a Request
    UpdateChannelReadMarker (..),
    newUpdateChannelReadMarker,

    -- * Request Lenses
    updateChannelReadMarker_chimeBearer,
    updateChannelReadMarker_channelArn,

    -- * Destructuring the Response
    UpdateChannelReadMarkerResponse (..),
    newUpdateChannelReadMarkerResponse,

    -- * Response Lenses
    updateChannelReadMarkerResponse_channelArn,
    updateChannelReadMarkerResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateChannelReadMarker' smart constructor.
data UpdateChannelReadMarker = UpdateChannelReadMarker'
  { -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the channel.
    channelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelReadMarker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chimeBearer', 'updateChannelReadMarker_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
--
-- 'channelArn', 'updateChannelReadMarker_channelArn' - The ARN of the channel.
newUpdateChannelReadMarker ::
  -- | 'channelArn'
  Prelude.Text ->
  UpdateChannelReadMarker
newUpdateChannelReadMarker pChannelArn_ =
  UpdateChannelReadMarker'
    { chimeBearer =
        Prelude.Nothing,
      channelArn = pChannelArn_
    }

-- | The @AppInstanceUserArn@ of the user that makes the API call.
updateChannelReadMarker_chimeBearer :: Lens.Lens' UpdateChannelReadMarker (Prelude.Maybe Prelude.Text)
updateChannelReadMarker_chimeBearer = Lens.lens (\UpdateChannelReadMarker' {chimeBearer} -> chimeBearer) (\s@UpdateChannelReadMarker' {} a -> s {chimeBearer = a} :: UpdateChannelReadMarker)

-- | The ARN of the channel.
updateChannelReadMarker_channelArn :: Lens.Lens' UpdateChannelReadMarker Prelude.Text
updateChannelReadMarker_channelArn = Lens.lens (\UpdateChannelReadMarker' {channelArn} -> channelArn) (\s@UpdateChannelReadMarker' {} a -> s {channelArn = a} :: UpdateChannelReadMarker)

instance Core.AWSRequest UpdateChannelReadMarker where
  type
    AWSResponse UpdateChannelReadMarker =
      UpdateChannelReadMarkerResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateChannelReadMarkerResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateChannelReadMarker where
  hashWithSalt _salt UpdateChannelReadMarker' {..} =
    _salt
      `Prelude.hashWithSalt` chimeBearer
      `Prelude.hashWithSalt` channelArn

instance Prelude.NFData UpdateChannelReadMarker where
  rnf UpdateChannelReadMarker' {..} =
    Prelude.rnf chimeBearer `Prelude.seq`
      Prelude.rnf channelArn

instance Data.ToHeaders UpdateChannelReadMarker where
  toHeaders UpdateChannelReadMarker' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON UpdateChannelReadMarker where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath UpdateChannelReadMarker where
  toPath UpdateChannelReadMarker' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/readMarker"]

instance Data.ToQuery UpdateChannelReadMarker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateChannelReadMarkerResponse' smart constructor.
data UpdateChannelReadMarkerResponse = UpdateChannelReadMarkerResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateChannelReadMarkerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'updateChannelReadMarkerResponse_channelArn' - The ARN of the channel.
--
-- 'httpStatus', 'updateChannelReadMarkerResponse_httpStatus' - The response's http status code.
newUpdateChannelReadMarkerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateChannelReadMarkerResponse
newUpdateChannelReadMarkerResponse pHttpStatus_ =
  UpdateChannelReadMarkerResponse'
    { channelArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the channel.
updateChannelReadMarkerResponse_channelArn :: Lens.Lens' UpdateChannelReadMarkerResponse (Prelude.Maybe Prelude.Text)
updateChannelReadMarkerResponse_channelArn = Lens.lens (\UpdateChannelReadMarkerResponse' {channelArn} -> channelArn) (\s@UpdateChannelReadMarkerResponse' {} a -> s {channelArn = a} :: UpdateChannelReadMarkerResponse)

-- | The response's http status code.
updateChannelReadMarkerResponse_httpStatus :: Lens.Lens' UpdateChannelReadMarkerResponse Prelude.Int
updateChannelReadMarkerResponse_httpStatus = Lens.lens (\UpdateChannelReadMarkerResponse' {httpStatus} -> httpStatus) (\s@UpdateChannelReadMarkerResponse' {} a -> s {httpStatus = a} :: UpdateChannelReadMarkerResponse)

instance
  Prelude.NFData
    UpdateChannelReadMarkerResponse
  where
  rnf UpdateChannelReadMarkerResponse' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf httpStatus
