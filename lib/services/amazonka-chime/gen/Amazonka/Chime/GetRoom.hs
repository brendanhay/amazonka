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
-- Module      : Amazonka.Chime.GetRoom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves room details, such as the room name, for a room in an Amazon
-- Chime Enterprise account.
module Amazonka.Chime.GetRoom
  ( -- * Creating a Request
    GetRoom (..),
    newGetRoom,

    -- * Request Lenses
    getRoom_accountId,
    getRoom_roomId,

    -- * Destructuring the Response
    GetRoomResponse (..),
    newGetRoomResponse,

    -- * Response Lenses
    getRoomResponse_room,
    getRoomResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRoom' smart constructor.
data GetRoom = GetRoom'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The room ID.
    roomId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getRoom_accountId' - The Amazon Chime account ID.
--
-- 'roomId', 'getRoom_roomId' - The room ID.
newGetRoom ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'roomId'
  Prelude.Text ->
  GetRoom
newGetRoom pAccountId_ pRoomId_ =
  GetRoom'
    { accountId = pAccountId_,
      roomId = pRoomId_
    }

-- | The Amazon Chime account ID.
getRoom_accountId :: Lens.Lens' GetRoom Prelude.Text
getRoom_accountId = Lens.lens (\GetRoom' {accountId} -> accountId) (\s@GetRoom' {} a -> s {accountId = a} :: GetRoom)

-- | The room ID.
getRoom_roomId :: Lens.Lens' GetRoom Prelude.Text
getRoom_roomId = Lens.lens (\GetRoom' {roomId} -> roomId) (\s@GetRoom' {} a -> s {roomId = a} :: GetRoom)

instance Core.AWSRequest GetRoom where
  type AWSResponse GetRoom = GetRoomResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoomResponse'
            Prelude.<$> (x Data..?> "Room")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRoom where
  hashWithSalt _salt GetRoom' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roomId

instance Prelude.NFData GetRoom where
  rnf GetRoom' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf roomId

instance Data.ToHeaders GetRoom where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetRoom where
  toPath GetRoom' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/rooms/",
        Data.toBS roomId
      ]

instance Data.ToQuery GetRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRoomResponse' smart constructor.
data GetRoomResponse = GetRoomResponse'
  { -- | The room details.
    room :: Prelude.Maybe Room,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'room', 'getRoomResponse_room' - The room details.
--
-- 'httpStatus', 'getRoomResponse_httpStatus' - The response's http status code.
newGetRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRoomResponse
newGetRoomResponse pHttpStatus_ =
  GetRoomResponse'
    { room = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The room details.
getRoomResponse_room :: Lens.Lens' GetRoomResponse (Prelude.Maybe Room)
getRoomResponse_room = Lens.lens (\GetRoomResponse' {room} -> room) (\s@GetRoomResponse' {} a -> s {room = a} :: GetRoomResponse)

-- | The response's http status code.
getRoomResponse_httpStatus :: Lens.Lens' GetRoomResponse Prelude.Int
getRoomResponse_httpStatus = Lens.lens (\GetRoomResponse' {httpStatus} -> httpStatus) (\s@GetRoomResponse' {} a -> s {httpStatus = a} :: GetRoomResponse)

instance Prelude.NFData GetRoomResponse where
  rnf GetRoomResponse' {..} =
    Prelude.rnf room
      `Prelude.seq` Prelude.rnf httpStatus
