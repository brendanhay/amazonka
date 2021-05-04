{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateDeviceFromRoom' smart constructor.
data DisassociateDeviceFromRoom = DisassociateDeviceFromRoom'
  { -- | The ARN of the device to disassociate from a room. Required.
    deviceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The ARN of the device to disassociate from a room. Required.
disassociateDeviceFromRoom_deviceArn :: Lens.Lens' DisassociateDeviceFromRoom (Prelude.Maybe Prelude.Text)
disassociateDeviceFromRoom_deviceArn = Lens.lens (\DisassociateDeviceFromRoom' {deviceArn} -> deviceArn) (\s@DisassociateDeviceFromRoom' {} a -> s {deviceArn = a} :: DisassociateDeviceFromRoom)

instance
  Prelude.AWSRequest
    DisassociateDeviceFromRoom
  where
  type
    Rs DisassociateDeviceFromRoom =
      DisassociateDeviceFromRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDeviceFromRoomResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateDeviceFromRoom

instance Prelude.NFData DisassociateDeviceFromRoom

instance Prelude.ToHeaders DisassociateDeviceFromRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DisassociateDeviceFromRoom" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateDeviceFromRoom where
  toJSON DisassociateDeviceFromRoom' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("DeviceArn" Prelude..=) Prelude.<$> deviceArn]
      )

instance Prelude.ToPath DisassociateDeviceFromRoom where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateDeviceFromRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDeviceFromRoomResponse' smart constructor.
data DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisassociateDeviceFromRoomResponse
newDisassociateDeviceFromRoomResponse pHttpStatus_ =
  DisassociateDeviceFromRoomResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateDeviceFromRoomResponse_httpStatus :: Lens.Lens' DisassociateDeviceFromRoomResponse Prelude.Int
disassociateDeviceFromRoomResponse_httpStatus = Lens.lens (\DisassociateDeviceFromRoomResponse' {httpStatus} -> httpStatus) (\s@DisassociateDeviceFromRoomResponse' {} a -> s {httpStatus = a} :: DisassociateDeviceFromRoomResponse)

instance
  Prelude.NFData
    DisassociateDeviceFromRoomResponse
