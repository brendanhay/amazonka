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
-- Module      : Amazonka.IdentityStore.CreateGroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a relationship between a member and a group. The following
-- identifiers must be specified: @GroupId@, @IdentityStoreId@, and
-- @MemberId@.
module Amazonka.IdentityStore.CreateGroupMembership
  ( -- * Creating a Request
    CreateGroupMembership (..),
    newCreateGroupMembership,

    -- * Request Lenses
    createGroupMembership_identityStoreId,
    createGroupMembership_groupId,
    createGroupMembership_memberId,

    -- * Destructuring the Response
    CreateGroupMembershipResponse (..),
    newCreateGroupMembershipResponse,

    -- * Response Lenses
    createGroupMembershipResponse_httpStatus,
    createGroupMembershipResponse_membershipId,
    createGroupMembershipResponse_identityStoreId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGroupMembership' smart constructor.
data CreateGroupMembership = CreateGroupMembership'
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
-- Create a value of 'CreateGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'createGroupMembership_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'groupId', 'createGroupMembership_groupId' - The identifier for a group in the identity store.
--
-- 'memberId', 'createGroupMembership_memberId' - An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
newCreateGroupMembership ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'memberId'
  MemberId ->
  CreateGroupMembership
newCreateGroupMembership
  pIdentityStoreId_
  pGroupId_
  pMemberId_ =
    CreateGroupMembership'
      { identityStoreId =
          pIdentityStoreId_,
        groupId = pGroupId_,
        memberId = pMemberId_
      }

-- | The globally unique identifier for the identity store.
createGroupMembership_identityStoreId :: Lens.Lens' CreateGroupMembership Prelude.Text
createGroupMembership_identityStoreId = Lens.lens (\CreateGroupMembership' {identityStoreId} -> identityStoreId) (\s@CreateGroupMembership' {} a -> s {identityStoreId = a} :: CreateGroupMembership)

-- | The identifier for a group in the identity store.
createGroupMembership_groupId :: Lens.Lens' CreateGroupMembership Prelude.Text
createGroupMembership_groupId = Lens.lens (\CreateGroupMembership' {groupId} -> groupId) (\s@CreateGroupMembership' {} a -> s {groupId = a} :: CreateGroupMembership)

-- | An object that contains the identifier of a group member. Setting the
-- @UserID@ field to the specific identifier for a user indicates that the
-- user is a member of the group.
createGroupMembership_memberId :: Lens.Lens' CreateGroupMembership MemberId
createGroupMembership_memberId = Lens.lens (\CreateGroupMembership' {memberId} -> memberId) (\s@CreateGroupMembership' {} a -> s {memberId = a} :: CreateGroupMembership)

instance Core.AWSRequest CreateGroupMembership where
  type
    AWSResponse CreateGroupMembership =
      CreateGroupMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MembershipId")
            Prelude.<*> (x Data..:> "IdentityStoreId")
      )

instance Prelude.Hashable CreateGroupMembership where
  hashWithSalt _salt CreateGroupMembership' {..} =
    _salt
      `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` memberId

instance Prelude.NFData CreateGroupMembership where
  rnf CreateGroupMembership' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf memberId

instance Data.ToHeaders CreateGroupMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIdentityStore.CreateGroupMembership" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGroupMembership where
  toJSON CreateGroupMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Data..= identityStoreId),
            Prelude.Just ("GroupId" Data..= groupId),
            Prelude.Just ("MemberId" Data..= memberId)
          ]
      )

instance Data.ToPath CreateGroupMembership where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateGroupMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupMembershipResponse' smart constructor.
data CreateGroupMembershipResponse = CreateGroupMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier for a newly created @GroupMembership@ in an identity
    -- store.
    membershipId :: Prelude.Text,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createGroupMembershipResponse_httpStatus' - The response's http status code.
--
-- 'membershipId', 'createGroupMembershipResponse_membershipId' - The identifier for a newly created @GroupMembership@ in an identity
-- store.
--
-- 'identityStoreId', 'createGroupMembershipResponse_identityStoreId' - The globally unique identifier for the identity store.
newCreateGroupMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'membershipId'
  Prelude.Text ->
  -- | 'identityStoreId'
  Prelude.Text ->
  CreateGroupMembershipResponse
newCreateGroupMembershipResponse
  pHttpStatus_
  pMembershipId_
  pIdentityStoreId_ =
    CreateGroupMembershipResponse'
      { httpStatus =
          pHttpStatus_,
        membershipId = pMembershipId_,
        identityStoreId = pIdentityStoreId_
      }

-- | The response's http status code.
createGroupMembershipResponse_httpStatus :: Lens.Lens' CreateGroupMembershipResponse Prelude.Int
createGroupMembershipResponse_httpStatus = Lens.lens (\CreateGroupMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateGroupMembershipResponse' {} a -> s {httpStatus = a} :: CreateGroupMembershipResponse)

-- | The identifier for a newly created @GroupMembership@ in an identity
-- store.
createGroupMembershipResponse_membershipId :: Lens.Lens' CreateGroupMembershipResponse Prelude.Text
createGroupMembershipResponse_membershipId = Lens.lens (\CreateGroupMembershipResponse' {membershipId} -> membershipId) (\s@CreateGroupMembershipResponse' {} a -> s {membershipId = a} :: CreateGroupMembershipResponse)

-- | The globally unique identifier for the identity store.
createGroupMembershipResponse_identityStoreId :: Lens.Lens' CreateGroupMembershipResponse Prelude.Text
createGroupMembershipResponse_identityStoreId = Lens.lens (\CreateGroupMembershipResponse' {identityStoreId} -> identityStoreId) (\s@CreateGroupMembershipResponse' {} a -> s {identityStoreId = a} :: CreateGroupMembershipResponse)

instance Prelude.NFData CreateGroupMembershipResponse where
  rnf CreateGroupMembershipResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf identityStoreId
