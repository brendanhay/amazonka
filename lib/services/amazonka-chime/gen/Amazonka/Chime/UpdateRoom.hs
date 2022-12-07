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
-- Module      : Amazonka.Chime.UpdateRoom
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room details, such as the room name, for a room in an Amazon
-- Chime Enterprise account.
module Amazonka.Chime.UpdateRoom
  ( -- * Creating a Request
    UpdateRoom (..),
    newUpdateRoom,

    -- * Request Lenses
    updateRoom_name,
    updateRoom_accountId,
    updateRoom_roomId,

    -- * Destructuring the Response
    UpdateRoomResponse (..),
    newUpdateRoomResponse,

    -- * Response Lenses
    updateRoomResponse_room,
    updateRoomResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoom' smart constructor.
data UpdateRoom = UpdateRoom'
  { -- | The room name.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The room ID.
    roomId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateRoom_name' - The room name.
--
-- 'accountId', 'updateRoom_accountId' - The Amazon Chime account ID.
--
-- 'roomId', 'updateRoom_roomId' - The room ID.
newUpdateRoom ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'roomId'
  Prelude.Text ->
  UpdateRoom
newUpdateRoom pAccountId_ pRoomId_ =
  UpdateRoom'
    { name = Prelude.Nothing,
      accountId = pAccountId_,
      roomId = pRoomId_
    }

-- | The room name.
updateRoom_name :: Lens.Lens' UpdateRoom (Prelude.Maybe Prelude.Text)
updateRoom_name = Lens.lens (\UpdateRoom' {name} -> name) (\s@UpdateRoom' {} a -> s {name = a} :: UpdateRoom) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Chime account ID.
updateRoom_accountId :: Lens.Lens' UpdateRoom Prelude.Text
updateRoom_accountId = Lens.lens (\UpdateRoom' {accountId} -> accountId) (\s@UpdateRoom' {} a -> s {accountId = a} :: UpdateRoom)

-- | The room ID.
updateRoom_roomId :: Lens.Lens' UpdateRoom Prelude.Text
updateRoom_roomId = Lens.lens (\UpdateRoom' {roomId} -> roomId) (\s@UpdateRoom' {} a -> s {roomId = a} :: UpdateRoom)

instance Core.AWSRequest UpdateRoom where
  type AWSResponse UpdateRoom = UpdateRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRoomResponse'
            Prelude.<$> (x Data..?> "Room")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoom where
  hashWithSalt _salt UpdateRoom' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roomId

instance Prelude.NFData UpdateRoom where
  rnf UpdateRoom' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf roomId

instance Data.ToHeaders UpdateRoom where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateRoom where
  toJSON UpdateRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )

instance Data.ToPath UpdateRoom where
  toPath UpdateRoom' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/rooms/",
        Data.toBS roomId
      ]

instance Data.ToQuery UpdateRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoomResponse' smart constructor.
data UpdateRoomResponse = UpdateRoomResponse'
  { -- | The room details.
    room :: Prelude.Maybe Room,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'room', 'updateRoomResponse_room' - The room details.
--
-- 'httpStatus', 'updateRoomResponse_httpStatus' - The response's http status code.
newUpdateRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoomResponse
newUpdateRoomResponse pHttpStatus_ =
  UpdateRoomResponse'
    { room = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The room details.
updateRoomResponse_room :: Lens.Lens' UpdateRoomResponse (Prelude.Maybe Room)
updateRoomResponse_room = Lens.lens (\UpdateRoomResponse' {room} -> room) (\s@UpdateRoomResponse' {} a -> s {room = a} :: UpdateRoomResponse)

-- | The response's http status code.
updateRoomResponse_httpStatus :: Lens.Lens' UpdateRoomResponse Prelude.Int
updateRoomResponse_httpStatus = Lens.lens (\UpdateRoomResponse' {httpStatus} -> httpStatus) (\s@UpdateRoomResponse' {} a -> s {httpStatus = a} :: UpdateRoomResponse)

instance Prelude.NFData UpdateRoomResponse where
  rnf UpdateRoomResponse' {..} =
    Prelude.rnf room
      `Prelude.seq` Prelude.rnf httpStatus
