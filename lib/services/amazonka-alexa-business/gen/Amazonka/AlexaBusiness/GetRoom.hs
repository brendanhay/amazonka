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
-- Module      : Amazonka.AlexaBusiness.GetRoom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets room details by room ARN.
module Amazonka.AlexaBusiness.GetRoom
  ( -- * Creating a Request
    GetRoom (..),
    newGetRoom,

    -- * Request Lenses
    getRoom_roomArn,

    -- * Destructuring the Response
    GetRoomResponse (..),
    newGetRoomResponse,

    -- * Response Lenses
    getRoomResponse_room,
    getRoomResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRoom' smart constructor.
data GetRoom = GetRoom'
  { -- | The ARN of the room for which to request details. Required.
    roomArn :: Prelude.Maybe Prelude.Text
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
-- 'roomArn', 'getRoom_roomArn' - The ARN of the room for which to request details. Required.
newGetRoom ::
  GetRoom
newGetRoom = GetRoom' {roomArn = Prelude.Nothing}

-- | The ARN of the room for which to request details. Required.
getRoom_roomArn :: Lens.Lens' GetRoom (Prelude.Maybe Prelude.Text)
getRoom_roomArn = Lens.lens (\GetRoom' {roomArn} -> roomArn) (\s@GetRoom' {} a -> s {roomArn = a} :: GetRoom)

instance Core.AWSRequest GetRoom where
  type AWSResponse GetRoom = GetRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRoomResponse'
            Prelude.<$> (x Data..?> "Room")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRoom where
  hashWithSalt _salt GetRoom' {..} =
    _salt `Prelude.hashWithSalt` roomArn

instance Prelude.NFData GetRoom where
  rnf GetRoom' {..} = Prelude.rnf roomArn

instance Data.ToHeaders GetRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AlexaForBusiness.GetRoom" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRoom where
  toJSON GetRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [("RoomArn" Data..=) Prelude.<$> roomArn]
      )

instance Data.ToPath GetRoom where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRoomResponse' smart constructor.
data GetRoomResponse = GetRoomResponse'
  { -- | The details of the room requested.
    room :: Prelude.Maybe Room,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'room', 'getRoomResponse_room' - The details of the room requested.
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

-- | The details of the room requested.
getRoomResponse_room :: Lens.Lens' GetRoomResponse (Prelude.Maybe Room)
getRoomResponse_room = Lens.lens (\GetRoomResponse' {room} -> room) (\s@GetRoomResponse' {} a -> s {room = a} :: GetRoomResponse)

-- | The response's http status code.
getRoomResponse_httpStatus :: Lens.Lens' GetRoomResponse Prelude.Int
getRoomResponse_httpStatus = Lens.lens (\GetRoomResponse' {httpStatus} -> httpStatus) (\s@GetRoomResponse' {} a -> s {httpStatus = a} :: GetRoomResponse)

instance Prelude.NFData GetRoomResponse where
  rnf GetRoomResponse' {..} =
    Prelude.rnf room
      `Prelude.seq` Prelude.rnf httpStatus
