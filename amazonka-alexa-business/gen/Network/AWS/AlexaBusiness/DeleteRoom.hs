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
-- Module      : Network.AWS.AlexaBusiness.DeleteRoom
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a room by the room ARN.
module Network.AWS.AlexaBusiness.DeleteRoom
  ( -- * Creating a Request
    DeleteRoom (..),
    newDeleteRoom,

    -- * Request Lenses
    deleteRoom_roomArn,

    -- * Destructuring the Response
    DeleteRoomResponse (..),
    newDeleteRoomResponse,

    -- * Response Lenses
    deleteRoomResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRoom' smart constructor.
data DeleteRoom = DeleteRoom'
  { -- | The ARN of the room to delete. Required.
    roomArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'deleteRoom_roomArn' - The ARN of the room to delete. Required.
newDeleteRoom ::
  DeleteRoom
newDeleteRoom =
  DeleteRoom' {roomArn = Prelude.Nothing}

-- | The ARN of the room to delete. Required.
deleteRoom_roomArn :: Lens.Lens' DeleteRoom (Prelude.Maybe Prelude.Text)
deleteRoom_roomArn = Lens.lens (\DeleteRoom' {roomArn} -> roomArn) (\s@DeleteRoom' {} a -> s {roomArn = a} :: DeleteRoom)

instance Prelude.AWSRequest DeleteRoom where
  type Rs DeleteRoom = DeleteRoomResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRoomResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRoom

instance Prelude.NFData DeleteRoom

instance Prelude.ToHeaders DeleteRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.DeleteRoom" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRoom where
  toJSON DeleteRoom' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("RoomArn" Prelude..=) Prelude.<$> roomArn]
      )

instance Prelude.ToPath DeleteRoom where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRoomResponse' smart constructor.
data DeleteRoomResponse = DeleteRoomResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRoomResponse_httpStatus' - The response's http status code.
newDeleteRoomResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRoomResponse
newDeleteRoomResponse pHttpStatus_ =
  DeleteRoomResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteRoomResponse_httpStatus :: Lens.Lens' DeleteRoomResponse Prelude.Int
deleteRoomResponse_httpStatus = Lens.lens (\DeleteRoomResponse' {httpStatus} -> httpStatus) (\s@DeleteRoomResponse' {} a -> s {httpStatus = a} :: DeleteRoomResponse)

instance Prelude.NFData DeleteRoomResponse
