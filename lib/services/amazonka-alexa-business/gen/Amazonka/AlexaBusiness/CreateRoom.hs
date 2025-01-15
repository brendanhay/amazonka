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
-- Module      : Amazonka.AlexaBusiness.CreateRoom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a room with the specified details.
module Amazonka.AlexaBusiness.CreateRoom
  ( -- * Creating a Request
    CreateRoom (..),
    newCreateRoom,

    -- * Request Lenses
    createRoom_clientRequestToken,
    createRoom_description,
    createRoom_profileArn,
    createRoom_providerCalendarId,
    createRoom_tags,
    createRoom_roomName,

    -- * Destructuring the Response
    CreateRoomResponse (..),
    newCreateRoomResponse,

    -- * Response Lenses
    createRoomResponse_roomArn,
    createRoomResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRoom' smart constructor.
data CreateRoom = CreateRoom'
  { -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The description for the room.
    description :: Prelude.Maybe Prelude.Text,
    -- | The profile ARN for the room. This is required.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The calendar ARN for the room.
    providerCalendarId :: Prelude.Maybe Prelude.Text,
    -- | The tags for the room.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the room.
    roomName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createRoom_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'description', 'createRoom_description' - The description for the room.
--
-- 'profileArn', 'createRoom_profileArn' - The profile ARN for the room. This is required.
--
-- 'providerCalendarId', 'createRoom_providerCalendarId' - The calendar ARN for the room.
--
-- 'tags', 'createRoom_tags' - The tags for the room.
--
-- 'roomName', 'createRoom_roomName' - The name for the room.
newCreateRoom ::
  -- | 'roomName'
  Prelude.Text ->
  CreateRoom
newCreateRoom pRoomName_ =
  CreateRoom'
    { clientRequestToken = Prelude.Nothing,
      description = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      providerCalendarId = Prelude.Nothing,
      tags = Prelude.Nothing,
      roomName = pRoomName_
    }

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createRoom_clientRequestToken :: Lens.Lens' CreateRoom (Prelude.Maybe Prelude.Text)
createRoom_clientRequestToken = Lens.lens (\CreateRoom' {clientRequestToken} -> clientRequestToken) (\s@CreateRoom' {} a -> s {clientRequestToken = a} :: CreateRoom)

-- | The description for the room.
createRoom_description :: Lens.Lens' CreateRoom (Prelude.Maybe Prelude.Text)
createRoom_description = Lens.lens (\CreateRoom' {description} -> description) (\s@CreateRoom' {} a -> s {description = a} :: CreateRoom)

-- | The profile ARN for the room. This is required.
createRoom_profileArn :: Lens.Lens' CreateRoom (Prelude.Maybe Prelude.Text)
createRoom_profileArn = Lens.lens (\CreateRoom' {profileArn} -> profileArn) (\s@CreateRoom' {} a -> s {profileArn = a} :: CreateRoom)

-- | The calendar ARN for the room.
createRoom_providerCalendarId :: Lens.Lens' CreateRoom (Prelude.Maybe Prelude.Text)
createRoom_providerCalendarId = Lens.lens (\CreateRoom' {providerCalendarId} -> providerCalendarId) (\s@CreateRoom' {} a -> s {providerCalendarId = a} :: CreateRoom)

-- | The tags for the room.
createRoom_tags :: Lens.Lens' CreateRoom (Prelude.Maybe [Tag])
createRoom_tags = Lens.lens (\CreateRoom' {tags} -> tags) (\s@CreateRoom' {} a -> s {tags = a} :: CreateRoom) Prelude.. Lens.mapping Lens.coerced

-- | The name for the room.
createRoom_roomName :: Lens.Lens' CreateRoom Prelude.Text
createRoom_roomName = Lens.lens (\CreateRoom' {roomName} -> roomName) (\s@CreateRoom' {} a -> s {roomName = a} :: CreateRoom)

instance Core.AWSRequest CreateRoom where
  type AWSResponse CreateRoom = CreateRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoomResponse'
            Prelude.<$> (x Data..?> "RoomArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoom where
  hashWithSalt _salt CreateRoom' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` providerCalendarId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` roomName

instance Prelude.NFData CreateRoom where
  rnf CreateRoom' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf profileArn `Prelude.seq`
          Prelude.rnf providerCalendarId `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf roomName

instance Data.ToHeaders CreateRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateRoom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRoom where
  toJSON CreateRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("ProfileArn" Data..=) Prelude.<$> profileArn,
            ("ProviderCalendarId" Data..=)
              Prelude.<$> providerCalendarId,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("RoomName" Data..= roomName)
          ]
      )

instance Data.ToPath CreateRoom where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRoomResponse' smart constructor.
data CreateRoomResponse = CreateRoomResponse'
  { -- | The ARN of the newly created room in the response.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'createRoomResponse_roomArn' - The ARN of the newly created room in the response.
--
-- 'httpStatus', 'createRoomResponse_httpStatus' - The response's http status code.
newCreateRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRoomResponse
newCreateRoomResponse pHttpStatus_ =
  CreateRoomResponse'
    { roomArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created room in the response.
createRoomResponse_roomArn :: Lens.Lens' CreateRoomResponse (Prelude.Maybe Prelude.Text)
createRoomResponse_roomArn = Lens.lens (\CreateRoomResponse' {roomArn} -> roomArn) (\s@CreateRoomResponse' {} a -> s {roomArn = a} :: CreateRoomResponse)

-- | The response's http status code.
createRoomResponse_httpStatus :: Lens.Lens' CreateRoomResponse Prelude.Int
createRoomResponse_httpStatus = Lens.lens (\CreateRoomResponse' {httpStatus} -> httpStatus) (\s@CreateRoomResponse' {} a -> s {httpStatus = a} :: CreateRoomResponse)

instance Prelude.NFData CreateRoomResponse where
  rnf CreateRoomResponse' {..} =
    Prelude.rnf roomArn `Prelude.seq`
      Prelude.rnf httpStatus
