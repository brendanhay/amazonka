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
-- Module      : Amazonka.Chime.DeleteRoom
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a chat room in an Amazon Chime Enterprise account.
module Amazonka.Chime.DeleteRoom
  ( -- * Creating a Request
    DeleteRoom (..),
    newDeleteRoom,

    -- * Request Lenses
    deleteRoom_accountId,
    deleteRoom_roomId,

    -- * Destructuring the Response
    DeleteRoomResponse (..),
    newDeleteRoomResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoom' smart constructor.
data DeleteRoom = DeleteRoom'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The chat room ID.
    roomId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoom' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteRoom_accountId' - The Amazon Chime account ID.
--
-- 'roomId', 'deleteRoom_roomId' - The chat room ID.
newDeleteRoom ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'roomId'
  Prelude.Text ->
  DeleteRoom
newDeleteRoom pAccountId_ pRoomId_ =
  DeleteRoom'
    { accountId = pAccountId_,
      roomId = pRoomId_
    }

-- | The Amazon Chime account ID.
deleteRoom_accountId :: Lens.Lens' DeleteRoom Prelude.Text
deleteRoom_accountId = Lens.lens (\DeleteRoom' {accountId} -> accountId) (\s@DeleteRoom' {} a -> s {accountId = a} :: DeleteRoom)

-- | The chat room ID.
deleteRoom_roomId :: Lens.Lens' DeleteRoom Prelude.Text
deleteRoom_roomId = Lens.lens (\DeleteRoom' {roomId} -> roomId) (\s@DeleteRoom' {} a -> s {roomId = a} :: DeleteRoom)

instance Core.AWSRequest DeleteRoom where
  type AWSResponse DeleteRoom = DeleteRoomResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteRoomResponse'

instance Prelude.Hashable DeleteRoom where
  hashWithSalt _salt DeleteRoom' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roomId

instance Prelude.NFData DeleteRoom where
  rnf DeleteRoom' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf roomId

instance Core.ToHeaders DeleteRoom where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteRoom where
  toPath DeleteRoom' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS accountId,
        "/rooms/",
        Core.toBS roomId
      ]

instance Core.ToQuery DeleteRoom where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRoomResponse' smart constructor.
data DeleteRoomResponse = DeleteRoomResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoomResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRoomResponse ::
  DeleteRoomResponse
newDeleteRoomResponse = DeleteRoomResponse'

instance Prelude.NFData DeleteRoomResponse where
  rnf _ = ()
