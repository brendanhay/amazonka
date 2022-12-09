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
-- Module      : Amazonka.AlexaBusiness.AssociateDeviceWithRoom
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a device with a given room. This applies all the settings
-- from the room profile to the device, and all the skills in any skill
-- groups added to that room. This operation requires the device to be
-- online, or else a manual sync is required.
module Amazonka.AlexaBusiness.AssociateDeviceWithRoom
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateDeviceWithRoom' smart constructor.
data AssociateDeviceWithRoom = AssociateDeviceWithRoom'
  { -- | The ARN of the device to associate to a room. Required.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the room with which to associate the device. Required.
    roomArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { deviceArn =
        Prelude.Nothing,
      roomArn = Prelude.Nothing
    }

-- | The ARN of the device to associate to a room. Required.
associateDeviceWithRoom_deviceArn :: Lens.Lens' AssociateDeviceWithRoom (Prelude.Maybe Prelude.Text)
associateDeviceWithRoom_deviceArn = Lens.lens (\AssociateDeviceWithRoom' {deviceArn} -> deviceArn) (\s@AssociateDeviceWithRoom' {} a -> s {deviceArn = a} :: AssociateDeviceWithRoom)

-- | The ARN of the room with which to associate the device. Required.
associateDeviceWithRoom_roomArn :: Lens.Lens' AssociateDeviceWithRoom (Prelude.Maybe Prelude.Text)
associateDeviceWithRoom_roomArn = Lens.lens (\AssociateDeviceWithRoom' {roomArn} -> roomArn) (\s@AssociateDeviceWithRoom' {} a -> s {roomArn = a} :: AssociateDeviceWithRoom)

instance Core.AWSRequest AssociateDeviceWithRoom where
  type
    AWSResponse AssociateDeviceWithRoom =
      AssociateDeviceWithRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDeviceWithRoomResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateDeviceWithRoom where
  hashWithSalt _salt AssociateDeviceWithRoom' {..} =
    _salt `Prelude.hashWithSalt` deviceArn
      `Prelude.hashWithSalt` roomArn

instance Prelude.NFData AssociateDeviceWithRoom where
  rnf AssociateDeviceWithRoom' {..} =
    Prelude.rnf deviceArn
      `Prelude.seq` Prelude.rnf roomArn

instance Data.ToHeaders AssociateDeviceWithRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.AssociateDeviceWithRoom" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateDeviceWithRoom where
  toJSON AssociateDeviceWithRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceArn" Data..=) Prelude.<$> deviceArn,
            ("RoomArn" Data..=) Prelude.<$> roomArn
          ]
      )

instance Data.ToPath AssociateDeviceWithRoom where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateDeviceWithRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDeviceWithRoomResponse' smart constructor.
data AssociateDeviceWithRoomResponse = AssociateDeviceWithRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AssociateDeviceWithRoomResponse
newAssociateDeviceWithRoomResponse pHttpStatus_ =
  AssociateDeviceWithRoomResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDeviceWithRoomResponse_httpStatus :: Lens.Lens' AssociateDeviceWithRoomResponse Prelude.Int
associateDeviceWithRoomResponse_httpStatus = Lens.lens (\AssociateDeviceWithRoomResponse' {httpStatus} -> httpStatus) (\s@AssociateDeviceWithRoomResponse' {} a -> s {httpStatus = a} :: AssociateDeviceWithRoomResponse)

instance
  Prelude.NFData
    AssociateDeviceWithRoomResponse
  where
  rnf AssociateDeviceWithRoomResponse' {..} =
    Prelude.rnf httpStatus
