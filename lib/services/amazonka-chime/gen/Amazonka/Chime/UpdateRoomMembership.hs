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
-- Module      : Amazonka.Chime.UpdateRoomMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates room membership details, such as the member role, for a room in
-- an Amazon Chime Enterprise account. The member role designates whether
-- the member is a chat room administrator or a general chat room member.
-- The member role can be updated only for user IDs.
module Amazonka.Chime.UpdateRoomMembership
  ( -- * Creating a Request
    UpdateRoomMembership (..),
    newUpdateRoomMembership,

    -- * Request Lenses
    updateRoomMembership_role,
    updateRoomMembership_accountId,
    updateRoomMembership_roomId,
    updateRoomMembership_memberId,

    -- * Destructuring the Response
    UpdateRoomMembershipResponse (..),
    newUpdateRoomMembershipResponse,

    -- * Response Lenses
    updateRoomMembershipResponse_roomMembership,
    updateRoomMembershipResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRoomMembership' smart constructor.
data UpdateRoomMembership = UpdateRoomMembership'
  { -- | The role of the member.
    role' :: Prelude.Maybe RoomMembershipRole,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The room ID.
    roomId :: Prelude.Text,
    -- | The member ID.
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoomMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'updateRoomMembership_role' - The role of the member.
--
-- 'accountId', 'updateRoomMembership_accountId' - The Amazon Chime account ID.
--
-- 'roomId', 'updateRoomMembership_roomId' - The room ID.
--
-- 'memberId', 'updateRoomMembership_memberId' - The member ID.
newUpdateRoomMembership ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'roomId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  UpdateRoomMembership
newUpdateRoomMembership
  pAccountId_
  pRoomId_
  pMemberId_ =
    UpdateRoomMembership'
      { role' = Prelude.Nothing,
        accountId = pAccountId_,
        roomId = pRoomId_,
        memberId = pMemberId_
      }

-- | The role of the member.
updateRoomMembership_role :: Lens.Lens' UpdateRoomMembership (Prelude.Maybe RoomMembershipRole)
updateRoomMembership_role = Lens.lens (\UpdateRoomMembership' {role'} -> role') (\s@UpdateRoomMembership' {} a -> s {role' = a} :: UpdateRoomMembership)

-- | The Amazon Chime account ID.
updateRoomMembership_accountId :: Lens.Lens' UpdateRoomMembership Prelude.Text
updateRoomMembership_accountId = Lens.lens (\UpdateRoomMembership' {accountId} -> accountId) (\s@UpdateRoomMembership' {} a -> s {accountId = a} :: UpdateRoomMembership)

-- | The room ID.
updateRoomMembership_roomId :: Lens.Lens' UpdateRoomMembership Prelude.Text
updateRoomMembership_roomId = Lens.lens (\UpdateRoomMembership' {roomId} -> roomId) (\s@UpdateRoomMembership' {} a -> s {roomId = a} :: UpdateRoomMembership)

-- | The member ID.
updateRoomMembership_memberId :: Lens.Lens' UpdateRoomMembership Prelude.Text
updateRoomMembership_memberId = Lens.lens (\UpdateRoomMembership' {memberId} -> memberId) (\s@UpdateRoomMembership' {} a -> s {memberId = a} :: UpdateRoomMembership)

instance Core.AWSRequest UpdateRoomMembership where
  type
    AWSResponse UpdateRoomMembership =
      UpdateRoomMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRoomMembershipResponse'
            Prelude.<$> (x Data..?> "RoomMembership")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoomMembership where
  hashWithSalt _salt UpdateRoomMembership' {..} =
    _salt
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roomId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData UpdateRoomMembership where
  rnf UpdateRoomMembership' {..} =
    Prelude.rnf role' `Prelude.seq`
      Prelude.rnf accountId `Prelude.seq`
        Prelude.rnf roomId `Prelude.seq`
          Prelude.rnf memberId

instance Data.ToHeaders UpdateRoomMembership where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateRoomMembership where
  toJSON UpdateRoomMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Role" Data..=) Prelude.<$> role']
      )

instance Data.ToPath UpdateRoomMembership where
  toPath UpdateRoomMembership' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/rooms/",
        Data.toBS roomId,
        "/memberships/",
        Data.toBS memberId
      ]

instance Data.ToQuery UpdateRoomMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoomMembershipResponse' smart constructor.
data UpdateRoomMembershipResponse = UpdateRoomMembershipResponse'
  { -- | The room membership details.
    roomMembership :: Prelude.Maybe RoomMembership,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoomMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomMembership', 'updateRoomMembershipResponse_roomMembership' - The room membership details.
--
-- 'httpStatus', 'updateRoomMembershipResponse_httpStatus' - The response's http status code.
newUpdateRoomMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoomMembershipResponse
newUpdateRoomMembershipResponse pHttpStatus_ =
  UpdateRoomMembershipResponse'
    { roomMembership =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The room membership details.
updateRoomMembershipResponse_roomMembership :: Lens.Lens' UpdateRoomMembershipResponse (Prelude.Maybe RoomMembership)
updateRoomMembershipResponse_roomMembership = Lens.lens (\UpdateRoomMembershipResponse' {roomMembership} -> roomMembership) (\s@UpdateRoomMembershipResponse' {} a -> s {roomMembership = a} :: UpdateRoomMembershipResponse)

-- | The response's http status code.
updateRoomMembershipResponse_httpStatus :: Lens.Lens' UpdateRoomMembershipResponse Prelude.Int
updateRoomMembershipResponse_httpStatus = Lens.lens (\UpdateRoomMembershipResponse' {httpStatus} -> httpStatus) (\s@UpdateRoomMembershipResponse' {} a -> s {httpStatus = a} :: UpdateRoomMembershipResponse)

instance Prelude.NFData UpdateRoomMembershipResponse where
  rnf UpdateRoomMembershipResponse' {..} =
    Prelude.rnf roomMembership `Prelude.seq`
      Prelude.rnf httpStatus
