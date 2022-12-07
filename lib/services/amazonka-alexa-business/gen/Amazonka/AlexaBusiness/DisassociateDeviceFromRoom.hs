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
-- Module      : Amazonka.AlexaBusiness.DisassociateDeviceFromRoom
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a device from its current room. The device continues to be
-- connected to the Wi-Fi network and is still registered to the account.
-- The device settings and skills are removed from the room.
module Amazonka.AlexaBusiness.DisassociateDeviceFromRoom
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateDeviceFromRoom' smart constructor.
data DisassociateDeviceFromRoom = DisassociateDeviceFromRoom'
  { -- | The ARN of the device to disassociate from a room. Required.
    deviceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DisassociateDeviceFromRoom where
  type
    AWSResponse DisassociateDeviceFromRoom =
      DisassociateDeviceFromRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDeviceFromRoomResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateDeviceFromRoom where
  hashWithSalt _salt DisassociateDeviceFromRoom' {..} =
    _salt `Prelude.hashWithSalt` deviceArn

instance Prelude.NFData DisassociateDeviceFromRoom where
  rnf DisassociateDeviceFromRoom' {..} =
    Prelude.rnf deviceArn

instance Data.ToHeaders DisassociateDeviceFromRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.DisassociateDeviceFromRoom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateDeviceFromRoom where
  toJSON DisassociateDeviceFromRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [("DeviceArn" Data..=) Prelude.<$> deviceArn]
      )

instance Data.ToPath DisassociateDeviceFromRoom where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateDeviceFromRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDeviceFromRoomResponse' smart constructor.
data DisassociateDeviceFromRoomResponse = DisassociateDeviceFromRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisassociateDeviceFromRoomResponse' {..} =
    Prelude.rnf httpStatus
