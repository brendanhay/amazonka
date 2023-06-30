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
-- Module      : Amazonka.Chime.CreateRoomMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a member to a chat room in an Amazon Chime Enterprise account. A
-- member can be either a user or a bot. The member role designates whether
-- the member is a chat room administrator or a general chat room member.
module Amazonka.Chime.CreateRoomMembership
  ( -- * Creating a Request
    CreateRoomMembership (..),
    newCreateRoomMembership,

    -- * Request Lenses
    createRoomMembership_role,
    createRoomMembership_accountId,
    createRoomMembership_roomId,
    createRoomMembership_memberId,

    -- * Destructuring the Response
    CreateRoomMembershipResponse (..),
    newCreateRoomMembershipResponse,

    -- * Response Lenses
    createRoomMembershipResponse_roomMembership,
    createRoomMembershipResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRoomMembership' smart constructor.
data CreateRoomMembership = CreateRoomMembership'
  { -- | The role of the member.
    role' :: Prelude.Maybe RoomMembershipRole,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The room ID.
    roomId :: Prelude.Text,
    -- | The Amazon Chime member ID (user ID or bot ID).
    memberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoomMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'role'', 'createRoomMembership_role' - The role of the member.
--
-- 'accountId', 'createRoomMembership_accountId' - The Amazon Chime account ID.
--
-- 'roomId', 'createRoomMembership_roomId' - The room ID.
--
-- 'memberId', 'createRoomMembership_memberId' - The Amazon Chime member ID (user ID or bot ID).
newCreateRoomMembership ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'roomId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  CreateRoomMembership
newCreateRoomMembership
  pAccountId_
  pRoomId_
  pMemberId_ =
    CreateRoomMembership'
      { role' = Prelude.Nothing,
        accountId = pAccountId_,
        roomId = pRoomId_,
        memberId = pMemberId_
      }

-- | The role of the member.
createRoomMembership_role :: Lens.Lens' CreateRoomMembership (Prelude.Maybe RoomMembershipRole)
createRoomMembership_role = Lens.lens (\CreateRoomMembership' {role'} -> role') (\s@CreateRoomMembership' {} a -> s {role' = a} :: CreateRoomMembership)

-- | The Amazon Chime account ID.
createRoomMembership_accountId :: Lens.Lens' CreateRoomMembership Prelude.Text
createRoomMembership_accountId = Lens.lens (\CreateRoomMembership' {accountId} -> accountId) (\s@CreateRoomMembership' {} a -> s {accountId = a} :: CreateRoomMembership)

-- | The room ID.
createRoomMembership_roomId :: Lens.Lens' CreateRoomMembership Prelude.Text
createRoomMembership_roomId = Lens.lens (\CreateRoomMembership' {roomId} -> roomId) (\s@CreateRoomMembership' {} a -> s {roomId = a} :: CreateRoomMembership)

-- | The Amazon Chime member ID (user ID or bot ID).
createRoomMembership_memberId :: Lens.Lens' CreateRoomMembership Prelude.Text
createRoomMembership_memberId = Lens.lens (\CreateRoomMembership' {memberId} -> memberId) (\s@CreateRoomMembership' {} a -> s {memberId = a} :: CreateRoomMembership)

instance Core.AWSRequest CreateRoomMembership where
  type
    AWSResponse CreateRoomMembership =
      CreateRoomMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRoomMembershipResponse'
            Prelude.<$> (x Data..?> "RoomMembership")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRoomMembership where
  hashWithSalt _salt CreateRoomMembership' {..} =
    _salt
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` roomId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData CreateRoomMembership where
  rnf CreateRoomMembership' {..} =
    Prelude.rnf role'
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf roomId
      `Prelude.seq` Prelude.rnf memberId

instance Data.ToHeaders CreateRoomMembership where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateRoomMembership where
  toJSON CreateRoomMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Role" Data..=) Prelude.<$> role',
            Prelude.Just ("MemberId" Data..= memberId)
          ]
      )

instance Data.ToPath CreateRoomMembership where
  toPath CreateRoomMembership' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/rooms/",
        Data.toBS roomId,
        "/memberships"
      ]

instance Data.ToQuery CreateRoomMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRoomMembershipResponse' smart constructor.
data CreateRoomMembershipResponse = CreateRoomMembershipResponse'
  { -- | The room membership details.
    roomMembership :: Prelude.Maybe RoomMembership,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRoomMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomMembership', 'createRoomMembershipResponse_roomMembership' - The room membership details.
--
-- 'httpStatus', 'createRoomMembershipResponse_httpStatus' - The response's http status code.
newCreateRoomMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRoomMembershipResponse
newCreateRoomMembershipResponse pHttpStatus_ =
  CreateRoomMembershipResponse'
    { roomMembership =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The room membership details.
createRoomMembershipResponse_roomMembership :: Lens.Lens' CreateRoomMembershipResponse (Prelude.Maybe RoomMembership)
createRoomMembershipResponse_roomMembership = Lens.lens (\CreateRoomMembershipResponse' {roomMembership} -> roomMembership) (\s@CreateRoomMembershipResponse' {} a -> s {roomMembership = a} :: CreateRoomMembershipResponse)

-- | The response's http status code.
createRoomMembershipResponse_httpStatus :: Lens.Lens' CreateRoomMembershipResponse Prelude.Int
createRoomMembershipResponse_httpStatus = Lens.lens (\CreateRoomMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateRoomMembershipResponse' {} a -> s {httpStatus = a} :: CreateRoomMembershipResponse)

instance Prelude.NFData CreateRoomMembershipResponse where
  rnf CreateRoomMembershipResponse' {..} =
    Prelude.rnf roomMembership
      `Prelude.seq` Prelude.rnf httpStatus
