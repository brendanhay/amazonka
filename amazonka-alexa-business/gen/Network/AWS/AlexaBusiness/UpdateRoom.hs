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
-- Module      : Network.AWS.AlexaBusiness.UpdateRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room details by room ARN.
module Network.AWS.AlexaBusiness.UpdateRoom
  ( -- * Creating a Request
    UpdateRoom (..),
    newUpdateRoom,

    -- * Request Lenses
    updateRoom_roomArn,
    updateRoom_providerCalendarId,
    updateRoom_profileArn,
    updateRoom_description,
    updateRoom_roomName,

    -- * Destructuring the Response
    UpdateRoomResponse (..),
    newUpdateRoomResponse,

    -- * Response Lenses
    updateRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRoom' smart constructor.
data UpdateRoom = UpdateRoom'
  { -- | The ARN of the room to update.
    roomArn :: Core.Maybe Core.Text,
    -- | The updated provider calendar ARN for the room.
    providerCalendarId :: Core.Maybe Core.Text,
    -- | The updated profile ARN for the room.
    profileArn :: Core.Maybe Core.Text,
    -- | The updated description for the room.
    description :: Core.Maybe Core.Text,
    -- | The updated name for the room.
    roomName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'updateRoom_roomArn' - The ARN of the room to update.
--
-- 'providerCalendarId', 'updateRoom_providerCalendarId' - The updated provider calendar ARN for the room.
--
-- 'profileArn', 'updateRoom_profileArn' - The updated profile ARN for the room.
--
-- 'description', 'updateRoom_description' - The updated description for the room.
--
-- 'roomName', 'updateRoom_roomName' - The updated name for the room.
newUpdateRoom ::
  UpdateRoom
newUpdateRoom =
  UpdateRoom'
    { roomArn = Core.Nothing,
      providerCalendarId = Core.Nothing,
      profileArn = Core.Nothing,
      description = Core.Nothing,
      roomName = Core.Nothing
    }

-- | The ARN of the room to update.
updateRoom_roomArn :: Lens.Lens' UpdateRoom (Core.Maybe Core.Text)
updateRoom_roomArn = Lens.lens (\UpdateRoom' {roomArn} -> roomArn) (\s@UpdateRoom' {} a -> s {roomArn = a} :: UpdateRoom)

-- | The updated provider calendar ARN for the room.
updateRoom_providerCalendarId :: Lens.Lens' UpdateRoom (Core.Maybe Core.Text)
updateRoom_providerCalendarId = Lens.lens (\UpdateRoom' {providerCalendarId} -> providerCalendarId) (\s@UpdateRoom' {} a -> s {providerCalendarId = a} :: UpdateRoom)

-- | The updated profile ARN for the room.
updateRoom_profileArn :: Lens.Lens' UpdateRoom (Core.Maybe Core.Text)
updateRoom_profileArn = Lens.lens (\UpdateRoom' {profileArn} -> profileArn) (\s@UpdateRoom' {} a -> s {profileArn = a} :: UpdateRoom)

-- | The updated description for the room.
updateRoom_description :: Lens.Lens' UpdateRoom (Core.Maybe Core.Text)
updateRoom_description = Lens.lens (\UpdateRoom' {description} -> description) (\s@UpdateRoom' {} a -> s {description = a} :: UpdateRoom)

-- | The updated name for the room.
updateRoom_roomName :: Lens.Lens' UpdateRoom (Core.Maybe Core.Text)
updateRoom_roomName = Lens.lens (\UpdateRoom' {roomName} -> roomName) (\s@UpdateRoom' {} a -> s {roomName = a} :: UpdateRoom)

instance Core.AWSRequest UpdateRoom where
  type AWSResponse UpdateRoom = UpdateRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRoomResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateRoom

instance Core.NFData UpdateRoom

instance Core.ToHeaders UpdateRoom where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AlexaForBusiness.UpdateRoom" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateRoom where
  toJSON UpdateRoom' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomArn" Core..=) Core.<$> roomArn,
            ("ProviderCalendarId" Core..=)
              Core.<$> providerCalendarId,
            ("ProfileArn" Core..=) Core.<$> profileArn,
            ("Description" Core..=) Core.<$> description,
            ("RoomName" Core..=) Core.<$> roomName
          ]
      )

instance Core.ToPath UpdateRoom where
  toPath = Core.const "/"

instance Core.ToQuery UpdateRoom where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRoomResponse' smart constructor.
data UpdateRoomResponse = UpdateRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateRoomResponse
newUpdateRoomResponse pHttpStatus_ =
  UpdateRoomResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateRoomResponse_httpStatus :: Lens.Lens' UpdateRoomResponse Core.Int
updateRoomResponse_httpStatus = Lens.lens (\UpdateRoomResponse' {httpStatus} -> httpStatus) (\s@UpdateRoomResponse' {} a -> s {httpStatus = a} :: UpdateRoomResponse)

instance Core.NFData UpdateRoomResponse
