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
-- Module      : Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a device from its current room. The device continues to be
-- connected to the Wi-Fi network and is still registered to the account.
-- The device settings and skills are removed from the room.
module Network.AWS.AlexaBusiness.DisassociateDeviceFromRoom
  ( -- * Creating a Request
    DisassociateDeviceFromRoom (..),
    newDisassociateDeviceFromRoom,

    -- * Request Lenses
    disassociateDeviceFromRoom_deviceArn,

    -- * Destructuring the Response
    DisassociateDeviceFromRoomResponse (..),
    newDisassociateDeviceFromRoomResponse,

    -- * Response Lenses
    disassociateDeviceFromRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateDeviceFromRoom' smart constructor.
data DisassociateDeviceFromRoom = DisassociateDeviceFromRoom'
  { -- | The ARN of the device to disassociate from a room. Required.
    deviceArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateDeviceFromRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceArn', 'disassociateDeviceFromRoom_deviceArn' - The ARN of the device to disassociate from a room. Required.
newDisassociateDeviceFromRoom ::
  DisassociateDeviceFromRoom
newDisassociateDeviceFromRoom =
  DisassociateDeviceFromRoom'
    { deviceArn =
        Core.Nothing
    }

-- | The ARN of the device to disassociate from a room. Required.
disassociateDeviceFromRoom_deviceArn :: Lens.Lens' DisassociateDeviceFromRoom (Core.Maybe Core.Text)
disassociateDeviceFromRoom_deviceArn = Lens.lens (\DisassociateDeviceFromRoom' {deviceArn} -> deviceArn) (\s@DisassociateDeviceFromRoom' {} a -> s {deviceArn = a} :: DisassociateDeviceFromRoom)

instance Core.AWSRequest DisassociateDeviceFromRoom where
  type
    AWSResponse DisassociateDeviceFromRoom =
      DisassociateDeviceFromRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDeviceFromRoomResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateDeviceFromRoom

instance Core.NFData DisassociateDeviceFromRoom

instance Core.ToHeaders DisassociateDeviceFromRoom where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DisassociateDeviceFromRoom" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateDeviceFromRoom where
  toJSON DisassociateDeviceFromRoom' {..} =
    Core.object
      ( Core.catMaybes
          [("DeviceArn" Core..=) Core.<$> deviceArn]
      )

instance Core.ToPath DisassociateDeviceFromRoom where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateDeviceFromRoom where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateDeviceFromRoomResponse' smart constructor.
data DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateDeviceFromRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateDeviceFromRoomResponse_httpStatus' - The response's http status code.
newDisassociateDeviceFromRoomResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateDeviceFromRoomResponse
newDisassociateDeviceFromRoomResponse pHttpStatus_ =
  DisassociateDeviceFromRoomResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateDeviceFromRoomResponse_httpStatus :: Lens.Lens' DisassociateDeviceFromRoomResponse Core.Int
disassociateDeviceFromRoomResponse_httpStatus = Lens.lens (\DisassociateDeviceFromRoomResponse' {httpStatus} -> httpStatus) (\s@DisassociateDeviceFromRoomResponse' {} a -> s {httpStatus = a} :: DisassociateDeviceFromRoomResponse)

instance
  Core.NFData
    DisassociateDeviceFromRoomResponse
