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
-- Module      : Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with a given room. This applies all the settings
-- from the room profile to the device, and all the skills in any skill
-- groups added to that room. This operation requires the device to be
-- online, or else a manual sync is required.
module Network.AWS.AlexaBusiness.AssociateDeviceWithRoom
  ( -- * Creating a Request
    AssociateDeviceWithRoom (..),
    newAssociateDeviceWithRoom,

    -- * Request Lenses
    associateDeviceWithRoom_deviceArn,
    associateDeviceWithRoom_roomArn,

    -- * Destructuring the Response
    AssociateDeviceWithRoomResponse (..),
    newAssociateDeviceWithRoomResponse,

    -- * Response Lenses
    associateDeviceWithRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateDeviceWithRoom' smart constructor.
data AssociateDeviceWithRoom = AssociateDeviceWithRoom'
  { -- | The ARN of the device to associate to a room. Required.
    deviceArn :: Core.Maybe Core.Text,
    -- | The ARN of the room with which to associate the device. Required.
    roomArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateDeviceWithRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'associateDeviceWithRoom_deviceArn' - The ARN of the device to associate to a room. Required.
--
-- 'roomArn', 'associateDeviceWithRoom_roomArn' - The ARN of the room with which to associate the device. Required.
newAssociateDeviceWithRoom ::
  AssociateDeviceWithRoom
newAssociateDeviceWithRoom =
  AssociateDeviceWithRoom'
    { deviceArn = Core.Nothing,
      roomArn = Core.Nothing
    }

-- | The ARN of the device to associate to a room. Required.
associateDeviceWithRoom_deviceArn :: Lens.Lens' AssociateDeviceWithRoom (Core.Maybe Core.Text)
associateDeviceWithRoom_deviceArn = Lens.lens (\AssociateDeviceWithRoom' {deviceArn} -> deviceArn) (\s@AssociateDeviceWithRoom' {} a -> s {deviceArn = a} :: AssociateDeviceWithRoom)

-- | The ARN of the room with which to associate the device. Required.
associateDeviceWithRoom_roomArn :: Lens.Lens' AssociateDeviceWithRoom (Core.Maybe Core.Text)
associateDeviceWithRoom_roomArn = Lens.lens (\AssociateDeviceWithRoom' {roomArn} -> roomArn) (\s@AssociateDeviceWithRoom' {} a -> s {roomArn = a} :: AssociateDeviceWithRoom)

instance Core.AWSRequest AssociateDeviceWithRoom where
  type
    AWSResponse AssociateDeviceWithRoom =
      AssociateDeviceWithRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDeviceWithRoomResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateDeviceWithRoom

instance Core.NFData AssociateDeviceWithRoom

instance Core.ToHeaders AssociateDeviceWithRoom where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.AssociateDeviceWithRoom" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateDeviceWithRoom where
  toJSON AssociateDeviceWithRoom' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeviceArn" Core..=) Core.<$> deviceArn,
            ("RoomArn" Core..=) Core.<$> roomArn
          ]
      )

instance Core.ToPath AssociateDeviceWithRoom where
  toPath = Core.const "/"

instance Core.ToQuery AssociateDeviceWithRoom where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateDeviceWithRoomResponse' smart constructor.
data AssociateDeviceWithRoomResponse = AssociateDeviceWithRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateDeviceWithRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDeviceWithRoomResponse_httpStatus' - The response's http status code.
newAssociateDeviceWithRoomResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateDeviceWithRoomResponse
newAssociateDeviceWithRoomResponse pHttpStatus_ =
  AssociateDeviceWithRoomResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDeviceWithRoomResponse_httpStatus :: Lens.Lens' AssociateDeviceWithRoomResponse Core.Int
associateDeviceWithRoomResponse_httpStatus = Lens.lens (\AssociateDeviceWithRoomResponse' {httpStatus} -> httpStatus) (\s@AssociateDeviceWithRoomResponse' {} a -> s {httpStatus = a} :: AssociateDeviceWithRoomResponse)

instance Core.NFData AssociateDeviceWithRoomResponse
