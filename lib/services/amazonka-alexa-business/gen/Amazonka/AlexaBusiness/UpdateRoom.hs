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
-- Module      : Amazonka.AlexaBusiness.UpdateRoom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room details by room ARN.
module Amazonka.AlexaBusiness.UpdateRoom
  ( -- * Creating a Request
    UpdateRoom (..),
    newUpdateRoom,

    -- * Request Lenses
    updateRoom_description,
    updateRoom_profileArn,
    updateRoom_providerCalendarId,
    updateRoom_roomArn,
    updateRoom_roomName,

    -- * Destructuring the Response
    UpdateRoomResponse (..),
    newUpdateRoomResponse,

    -- * Response Lenses
    updateRoomResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoom' smart constructor.
data UpdateRoom = UpdateRoom'
  { -- | The updated description for the room.
    description :: Prelude.Maybe Prelude.Text,
    -- | The updated profile ARN for the room.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The updated provider calendar ARN for the room.
    providerCalendarId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the room to update.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The updated name for the room.
    roomName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateRoom_description' - The updated description for the room.
--
-- 'profileArn', 'updateRoom_profileArn' - The updated profile ARN for the room.
--
-- 'providerCalendarId', 'updateRoom_providerCalendarId' - The updated provider calendar ARN for the room.
--
-- 'roomArn', 'updateRoom_roomArn' - The ARN of the room to update.
--
-- 'roomName', 'updateRoom_roomName' - The updated name for the room.
newUpdateRoom ::
  UpdateRoom
newUpdateRoom =
  UpdateRoom'
    { description = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      providerCalendarId = Prelude.Nothing,
      roomArn = Prelude.Nothing,
      roomName = Prelude.Nothing
    }

-- | The updated description for the room.
updateRoom_description :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Text)
updateRoom_description = Lens.lens (\UpdateRoom' {description} -> description) (\s@UpdateRoom' {} a -> s {description = a} :: UpdateRoom)

-- | The updated profile ARN for the room.
updateRoom_profileArn :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Text)
updateRoom_profileArn = Lens.lens (\UpdateRoom' {profileArn} -> profileArn) (\s@UpdateRoom' {} a -> s {profileArn = a} :: UpdateRoom)

-- | The updated provider calendar ARN for the room.
updateRoom_providerCalendarId :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Text)
updateRoom_providerCalendarId = Lens.lens (\UpdateRoom' {providerCalendarId} -> providerCalendarId) (\s@UpdateRoom' {} a -> s {providerCalendarId = a} :: UpdateRoom)

-- | The ARN of the room to update.
updateRoom_roomArn :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Text)
updateRoom_roomArn = Lens.lens (\UpdateRoom' {roomArn} -> roomArn) (\s@UpdateRoom' {} a -> s {roomArn = a} :: UpdateRoom)

-- | The updated name for the room.
updateRoom_roomName :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Text)
updateRoom_roomName = Lens.lens (\UpdateRoom' {roomName} -> roomName) (\s@UpdateRoom' {} a -> s {roomName = a} :: UpdateRoom)

instance Core.AWSRequest UpdateRoom where
  type AWSResponse UpdateRoom = UpdateRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRoomResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoom where
  hashWithSalt _salt UpdateRoom' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` providerCalendarId
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` roomName

instance Prelude.NFData UpdateRoom where
  rnf UpdateRoom' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf providerCalendarId
      `Prelude.seq` Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf roomName

instance Data.ToHeaders UpdateRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.UpdateRoom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoom where
  toJSON UpdateRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ProfileArn" Data..=) Prelude.<$> profileArn,
            ("ProviderCalendarId" Data..=)
              Prelude.<$> providerCalendarId,
            ("RoomArn" Data..=) Prelude.<$> roomArn,
            ("RoomName" Data..=) Prelude.<$> roomName
          ]
      )

instance Data.ToPath UpdateRoom where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoomResponse' smart constructor.
data UpdateRoomResponse = UpdateRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRoomResponse_httpStatus' - The response's http status code.
newUpdateRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoomResponse
newUpdateRoomResponse pHttpStatus_ =
  UpdateRoomResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateRoomResponse_httpStatus :: Lens.Lens' UpdateRoomResponse Prelude.Int
updateRoomResponse_httpStatus = Lens.lens (\UpdateRoomResponse' {httpStatus} -> httpStatus) (\s@UpdateRoomResponse' {} a -> s {httpStatus = a} :: UpdateRoomResponse)

instance Prelude.NFData UpdateRoomResponse where
  rnf UpdateRoomResponse' {..} = Prelude.rnf httpStatus
