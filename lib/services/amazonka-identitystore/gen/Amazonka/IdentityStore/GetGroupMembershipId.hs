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
-- Module      : Amazonka.IdentityStore.GetGroupMembershipId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @MembershipId@ in an identity store.
module Amazonka.IdentityStore.GetGroupMembershipId
  ( -- * Creating a Request
    GetGroupMembershipId (..),
    newGetGroupMembershipId,

    -- * Request Lenses
    getGroupMembershipId_identityStoreId,
    getGroupMembershipId_groupId,
    getGroupMembershipId_memberId,

    -- * Destructuring the Response
    GetGroupMembershipIdResponse (..),
    newGetGroupMembershipIdResponse,

    -- * Response Lenses
    getGroupMembershipIdResponse_httpStatus,
    getGroupMembershipIdResponse_membershipId,
    getGroupMembershipIdResponse_identityStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGroupMembershipId' smart constructor.
data GetGroupMembershipId = GetGroupMembershipId'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text,
    -- | An object that contains the identifier of a group member. Setting the
    -- @UserID@ field to the specific identifier for a user indicates that the
    -- user is a member of the group.
    memberId :: MemberId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupMembershipId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'getGroupMembershipId_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'groupId', 'getGroupMembershipId_groupId' - The identifier for a group in the identity store.
--
-- 'memberId', 'getGroupMembershipId_memberId' - An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
newGetGroupMembershipId ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'memberId'
  MemberId ->
  GetGroupMembershipId
newGetGroupMembershipId
  pIdentityStoreId_
  pGroupId_
  pMemberId_ =
    GetGroupMembershipId'
      { identityStoreId =
          pIdentityStoreId_,
        groupId = pGroupId_,
        memberId = pMemberId_
      }

-- | The globally unique identifier for the identity store.
getGroupMembershipId_identityStoreId :: Lens.Lens' GetGroupMembershipId Prelude.Text
getGroupMembershipId_identityStoreId = Lens.lens (\GetGroupMembershipId' {identityStoreId} -> identityStoreId) (\s@GetGroupMembershipId' {} a -> s {identityStoreId = a} :: GetGroupMembershipId)

-- | The identifier for a group in the identity store.
getGroupMembershipId_groupId :: Lens.Lens' GetGroupMembershipId Prelude.Text
getGroupMembershipId_groupId = Lens.lens (\GetGroupMembershipId' {groupId} -> groupId) (\s@GetGroupMembershipId' {} a -> s {groupId = a} :: GetGroupMembershipId)

-- | An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
getGroupMembershipId_memberId :: Lens.Lens' GetGroupMembershipId MemberId
getGroupMembershipId_memberId = Lens.lens (\GetGroupMembershipId' {memberId} -> memberId) (\s@GetGroupMembershipId' {} a -> s {memberId = a} :: GetGroupMembershipId)

instance Core.AWSRequest GetGroupMembershipId where
  type
    AWSResponse GetGroupMembershipId =
      GetGroupMembershipIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupMembershipIdResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MembershipId")
            Prelude.<*> (x Data..:> "IdentityStoreId")
      )

instance Prelude.Hashable GetGroupMembershipId where
  hashWithSalt _salt GetGroupMembershipId' {..} =
    _salt `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData GetGroupMembershipId where
  rnf GetGroupMembershipId' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf memberId

instance Data.ToHeaders GetGroupMembershipId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.GetGroupMembershipId" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetGroupMembershipId where
  toJSON GetGroupMembershipId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("MemberId" Data..= memberId)
          ]
      )

instance Data.ToPath GetGroupMembershipId where
  toPath = Prelude.const "/"

instance Data.ToQuery GetGroupMembershipId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupMembershipIdResponse' smart constructor.
data GetGroupMembershipIdResponse = GetGroupMembershipIdResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier for a @GroupMembership@ in an identity store.
    membershipId :: Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGroupMembershipIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getGroupMembershipIdResponse_httpStatus' - The response's http status code.
--
-- 'membershipId', 'getGroupMembershipIdResponse_membershipId' - The identifier for a @GroupMembership@ in an identity store.
--
-- 'identityStoreId', 'getGroupMembershipIdResponse_identityStoreId' - The globally unique identifier for the identity store.
newGetGroupMembershipIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'membershipId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  GetGroupMembershipIdResponse
newGetGroupMembershipIdResponse
  pHttpStatus_
  pMembershipId_
  pIdentityStoreId_ =
    GetGroupMembershipIdResponse'
      { httpStatus =
          pHttpStatus_,
        membershipId = pMembershipId_,
        identityStoreId = pIdentityStoreId_
      }

-- | The response's http status code.
getGroupMembershipIdResponse_httpStatus :: Lens.Lens' GetGroupMembershipIdResponse Prelude.Int
getGroupMembershipIdResponse_httpStatus = Lens.lens (\GetGroupMembershipIdResponse' {httpStatus} -> httpStatus) (\s@GetGroupMembershipIdResponse' {} a -> s {httpStatus = a} :: GetGroupMembershipIdResponse)

-- | The identifier for a @GroupMembership@ in an identity store.
getGroupMembershipIdResponse_membershipId :: Lens.Lens' GetGroupMembershipIdResponse Prelude.Text
getGroupMembershipIdResponse_membershipId = Lens.lens (\GetGroupMembershipIdResponse' {membershipId} -> membershipId) (\s@GetGroupMembershipIdResponse' {} a -> s {membershipId = a} :: GetGroupMembershipIdResponse)

-- | The globally unique identifier for the identity store.
getGroupMembershipIdResponse_identityStoreId :: Lens.Lens' GetGroupMembershipIdResponse Prelude.Text
getGroupMembershipIdResponse_identityStoreId = Lens.lens (\GetGroupMembershipIdResponse' {identityStoreId} -> identityStoreId) (\s@GetGroupMembershipIdResponse' {} a -> s {identityStoreId = a} :: GetGroupMembershipIdResponse)

instance Prelude.NFData GetGroupMembershipIdResponse where
  rnf GetGroupMembershipIdResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf identityStoreId
