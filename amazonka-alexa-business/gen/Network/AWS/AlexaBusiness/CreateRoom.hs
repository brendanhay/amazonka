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
-- Module      : Network.AWS.AlexaBusiness.CreateRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a room with the specified details.
module Network.AWS.AlexaBusiness.CreateRoom
  ( -- * Creating a Request
    CreateRoom (..),
    newCreateRoom,

    -- * Request Lenses
    createRoom_tags,
    createRoom_providerCalendarId,
    createRoom_profileArn,
    createRoom_description,
    createRoom_clientRequestToken,
    createRoom_roomName,

    -- * Destructuring the Response
    CreateRoomResponse (..),
    newCreateRoomResponse,

    -- * Response Lenses
    createRoomResponse_roomArn,
    createRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateRoom' smart constructor.
data CreateRoom = CreateRoom'
  { -- | The tags for the room.
    tags :: Core.Maybe [Tag],
    -- | The calendar ARN for the room.
    providerCalendarId :: Core.Maybe Core.Text,
    -- | The profile ARN for the room. This is required.
    profileArn :: Core.Maybe Core.Text,
    -- | The description for the room.
    description :: Core.Maybe Core.Text,
    -- | A unique, user-specified identifier for this request that ensures
    -- idempotency.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name for the room.
    roomName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRoom_tags' - The tags for the room.
--
-- 'providerCalendarId', 'createRoom_providerCalendarId' - The calendar ARN for the room.
--
-- 'profileArn', 'createRoom_profileArn' - The profile ARN for the room. This is required.
--
-- 'description', 'createRoom_description' - The description for the room.
--
-- 'clientRequestToken', 'createRoom_clientRequestToken' - A unique, user-specified identifier for this request that ensures
-- idempotency.
--
-- 'roomName', 'createRoom_roomName' - The name for the room.
newCreateRoom ::
  -- | 'roomName'
  Core.Text ->
  CreateRoom
newCreateRoom pRoomName_ =
  CreateRoom'
    { tags = Core.Nothing,
      providerCalendarId = Core.Nothing,
      profileArn = Core.Nothing,
      description = Core.Nothing,
      clientRequestToken = Core.Nothing,
      roomName = pRoomName_
    }

-- | The tags for the room.
createRoom_tags :: Lens.Lens' CreateRoom (Core.Maybe [Tag])
createRoom_tags = Lens.lens (\CreateRoom' {tags} -> tags) (\s@CreateRoom' {} a -> s {tags = a} :: CreateRoom) Core.. Lens.mapping Lens._Coerce

-- | The calendar ARN for the room.
createRoom_providerCalendarId :: Lens.Lens' CreateRoom (Core.Maybe Core.Text)
createRoom_providerCalendarId = Lens.lens (\CreateRoom' {providerCalendarId} -> providerCalendarId) (\s@CreateRoom' {} a -> s {providerCalendarId = a} :: CreateRoom)

-- | The profile ARN for the room. This is required.
createRoom_profileArn :: Lens.Lens' CreateRoom (Core.Maybe Core.Text)
createRoom_profileArn = Lens.lens (\CreateRoom' {profileArn} -> profileArn) (\s@CreateRoom' {} a -> s {profileArn = a} :: CreateRoom)

-- | The description for the room.
createRoom_description :: Lens.Lens' CreateRoom (Core.Maybe Core.Text)
createRoom_description = Lens.lens (\CreateRoom' {description} -> description) (\s@CreateRoom' {} a -> s {description = a} :: CreateRoom)

-- | A unique, user-specified identifier for this request that ensures
-- idempotency.
createRoom_clientRequestToken :: Lens.Lens' CreateRoom (Core.Maybe Core.Text)
createRoom_clientRequestToken = Lens.lens (\CreateRoom' {clientRequestToken} -> clientRequestToken) (\s@CreateRoom' {} a -> s {clientRequestToken = a} :: CreateRoom)

-- | The name for the room.
createRoom_roomName :: Lens.Lens' CreateRoom Core.Text
createRoom_roomName = Lens.lens (\CreateRoom' {roomName} -> roomName) (\s@CreateRoom' {} a -> s {roomName = a} :: CreateRoom)

instance Core.AWSRequest CreateRoom where
  type AWSResponse CreateRoom = CreateRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoomResponse'
            Core.<$> (x Core..?> "RoomArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateRoom

instance Core.NFData CreateRoom

instance Core.ToHeaders CreateRoom where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.CreateRoom" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateRoom where
  toJSON CreateRoom' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            ("ProviderCalendarId" Core..=)
              Core.<$> providerCalendarId,
            ("ProfileArn" Core..=) Core.<$> profileArn,
            ("Description" Core..=) Core.<$> description,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("RoomName" Core..= roomName)
          ]
      )

instance Core.ToPath CreateRoom where
  toPath = Core.const "/"

instance Core.ToQuery CreateRoom where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateRoomResponse' smart constructor.
data CreateRoomResponse = CreateRoomResponse'
  { -- | The ARN of the newly created room in the response.
    roomArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateRoomResponse
newCreateRoomResponse pHttpStatus_ =
  CreateRoomResponse'
    { roomArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created room in the response.
createRoomResponse_roomArn :: Lens.Lens' CreateRoomResponse (Core.Maybe Core.Text)
createRoomResponse_roomArn = Lens.lens (\CreateRoomResponse' {roomArn} -> roomArn) (\s@CreateRoomResponse' {} a -> s {roomArn = a} :: CreateRoomResponse)

-- | The response's http status code.
createRoomResponse_httpStatus :: Lens.Lens' CreateRoomResponse Core.Int
createRoomResponse_httpStatus = Lens.lens (\CreateRoomResponse' {httpStatus} -> httpStatus) (\s@CreateRoomResponse' {} a -> s {httpStatus = a} :: CreateRoomResponse)

instance Core.NFData CreateRoomResponse
