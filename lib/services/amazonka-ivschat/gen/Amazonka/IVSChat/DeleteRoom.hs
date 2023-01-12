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
-- Module      : Amazonka.IVSChat.DeleteRoom
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified room.
module Amazonka.IVSChat.DeleteRoom
  ( -- * Creating a Request
    DeleteRoom (..),
    newDeleteRoom,

    -- * Request Lenses
    deleteRoom_identifier,

    -- * Destructuring the Response
    DeleteRoomResponse (..),
    newDeleteRoomResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSChat.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoom' smart constructor.
data DeleteRoom = DeleteRoom'
  { -- | Identifier of the room to be deleted. Currently this must be an ARN.
    identifier :: Prelude.Text
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
-- 'identifier', 'deleteRoom_identifier' - Identifier of the room to be deleted. Currently this must be an ARN.
newDeleteRoom ::
  -- | 'identifier'
  Prelude.Text ->
  DeleteRoom
newDeleteRoom pIdentifier_ =
  DeleteRoom' {identifier = pIdentifier_}

-- | Identifier of the room to be deleted. Currently this must be an ARN.
deleteRoom_identifier :: Lens.Lens' DeleteRoom Prelude.Text
deleteRoom_identifier = Lens.lens (\DeleteRoom' {identifier} -> identifier) (\s@DeleteRoom' {} a -> s {identifier = a} :: DeleteRoom)

instance Core.AWSRequest DeleteRoom where
  type AWSResponse DeleteRoom = DeleteRoomResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteRoomResponse'

instance Prelude.Hashable DeleteRoom where
  hashWithSalt _salt DeleteRoom' {..} =
    _salt `Prelude.hashWithSalt` identifier

instance Prelude.NFData DeleteRoom where
  rnf DeleteRoom' {..} = Prelude.rnf identifier

instance Data.ToHeaders DeleteRoom where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRoom where
  toJSON DeleteRoom' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("identifier" Data..= identifier)]
      )

instance Data.ToPath DeleteRoom where
  toPath = Prelude.const "/DeleteRoom"

instance Data.ToQuery DeleteRoom where
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
