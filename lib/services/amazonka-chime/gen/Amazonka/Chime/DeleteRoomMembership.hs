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
-- Module      : Amazonka.Chime.DeleteRoomMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a member from a chat room in an Amazon Chime Enterprise account.
module Amazonka.Chime.DeleteRoomMembership
  ( -- * Creating a Request
    DeleteRoomMembership (..),
    newDeleteRoomMembership,

    -- * Request Lenses
    deleteRoomMembership_accountId,
    deleteRoomMembership_roomId,
    deleteRoomMembership_memberId,

    -- * Destructuring the Response
    DeleteRoomMembershipResponse (..),
    newDeleteRoomMembershipResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRoomMembership' smart constructor.
data DeleteRoomMembership = DeleteRoomMembership'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The room ID.
    roomId :: Prelude.Text,
    -- | The member ID (user ID or bot ID).
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoomMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteRoomMembership_accountId' - The Amazon Chime account ID.
--
-- 'roomId', 'deleteRoomMembership_roomId' - The room ID.
--
-- 'memberId', 'deleteRoomMembership_memberId' - The member ID (user ID or bot ID).
newDeleteRoomMembership ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'roomId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  DeleteRoomMembership
newDeleteRoomMembership
  pAccountId_
  pRoomId_
  pMemberId_ =
    DeleteRoomMembership'
      { accountId = pAccountId_,
        roomId = pRoomId_,
        memberId = pMemberId_
      }

-- | The Amazon Chime account ID.
deleteRoomMembership_accountId :: Lens.Lens' DeleteRoomMembership Prelude.Text
deleteRoomMembership_accountId = Lens.lens (\DeleteRoomMembership' {accountId} -> accountId) (\s@DeleteRoomMembership' {} a -> s {accountId = a} :: DeleteRoomMembership)

-- | The room ID.
deleteRoomMembership_roomId :: Lens.Lens' DeleteRoomMembership Prelude.Text
deleteRoomMembership_roomId = Lens.lens (\DeleteRoomMembership' {roomId} -> roomId) (\s@DeleteRoomMembership' {} a -> s {roomId = a} :: DeleteRoomMembership)

-- | The member ID (user ID or bot ID).
deleteRoomMembership_memberId :: Lens.Lens' DeleteRoomMembership Prelude.Text
deleteRoomMembership_memberId = Lens.lens (\DeleteRoomMembership' {memberId} -> memberId) (\s@DeleteRoomMembership' {} a -> s {memberId = a} :: DeleteRoomMembership)

instance Core.AWSRequest DeleteRoomMembership where
  type
    AWSResponse DeleteRoomMembership =
      DeleteRoomMembershipResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteRoomMembershipResponse'

instance Prelude.Hashable DeleteRoomMembership where
  hashWithSalt _salt DeleteRoomMembership' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roomId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData DeleteRoomMembership where
  rnf DeleteRoomMembership' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf roomId
      `Prelude.seq` Prelude.rnf memberId

instance Core.ToHeaders DeleteRoomMembership where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteRoomMembership where
  toPath DeleteRoomMembership' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS accountId,
        "/rooms/",
        Core.toBS roomId,
        "/memberships/",
        Core.toBS memberId
      ]

instance Core.ToQuery DeleteRoomMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRoomMembershipResponse' smart constructor.
data DeleteRoomMembershipResponse = DeleteRoomMembershipResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRoomMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRoomMembershipResponse ::
  DeleteRoomMembershipResponse
newDeleteRoomMembershipResponse =
  DeleteRoomMembershipResponse'

instance Prelude.NFData DeleteRoomMembershipResponse where
  rnf _ = ()
