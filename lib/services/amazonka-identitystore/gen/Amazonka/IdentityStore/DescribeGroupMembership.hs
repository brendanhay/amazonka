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
-- Module      : Amazonka.IdentityStore.DescribeGroupMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves membership metadata and attributes from @MembershipId@ in an
-- identity store.
module Amazonka.IdentityStore.DescribeGroupMembership
  ( -- * Creating a Request
    DescribeGroupMembership (..),
    newDescribeGroupMembership,

    -- * Request Lenses
    describeGroupMembership_identityStoreId,
    describeGroupMembership_membershipId,

    -- * Destructuring the Response
    DescribeGroupMembershipResponse (..),
    newDescribeGroupMembershipResponse,

    -- * Response Lenses
    describeGroupMembershipResponse_httpStatus,
    describeGroupMembershipResponse_identityStoreId,
    describeGroupMembershipResponse_membershipId,
    describeGroupMembershipResponse_groupId,
    describeGroupMembershipResponse_memberId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IdentityStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGroupMembership' smart constructor.
data DescribeGroupMembership = DescribeGroupMembership'
  { -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a @GroupMembership@ in an identity store.
    membershipId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityStoreId', 'describeGroupMembership_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'membershipId', 'describeGroupMembership_membershipId' - The identifier for a @GroupMembership@ in an identity store.
newDescribeGroupMembership ::
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'membershipId'
  Prelude.Text ->
  DescribeGroupMembership
newDescribeGroupMembership
  pIdentityStoreId_
  pMembershipId_ =
    DescribeGroupMembership'
      { identityStoreId =
          pIdentityStoreId_,
        membershipId = pMembershipId_
      }

-- | The globally unique identifier for the identity store.
describeGroupMembership_identityStoreId :: Lens.Lens' DescribeGroupMembership Prelude.Text
describeGroupMembership_identityStoreId = Lens.lens (\DescribeGroupMembership' {identityStoreId} -> identityStoreId) (\s@DescribeGroupMembership' {} a -> s {identityStoreId = a} :: DescribeGroupMembership)

-- | The identifier for a @GroupMembership@ in an identity store.
describeGroupMembership_membershipId :: Lens.Lens' DescribeGroupMembership Prelude.Text
describeGroupMembership_membershipId = Lens.lens (\DescribeGroupMembership' {membershipId} -> membershipId) (\s@DescribeGroupMembership' {} a -> s {membershipId = a} :: DescribeGroupMembership)

instance Core.AWSRequest DescribeGroupMembership where
  type
    AWSResponse DescribeGroupMembership =
      DescribeGroupMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupMembershipResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "IdentityStoreId")
            Prelude.<*> (x Core..:> "MembershipId")
            Prelude.<*> (x Core..:> "GroupId")
            Prelude.<*> (x Core..:> "MemberId")
      )

instance Prelude.Hashable DescribeGroupMembership where
  hashWithSalt _salt DescribeGroupMembership' {..} =
    _salt `Prelude.hashWithSalt` identityStoreId
      `Prelude.hashWithSalt` membershipId

instance Prelude.NFData DescribeGroupMembership where
  rnf DescribeGroupMembership' {..} =
    Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf membershipId

instance Core.ToHeaders DescribeGroupMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSIdentityStore.DescribeGroupMembership" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeGroupMembership where
  toJSON DescribeGroupMembership' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdentityStoreId" Core..= identityStoreId),
            Prelude.Just ("MembershipId" Core..= membershipId)
          ]
      )

instance Core.ToPath DescribeGroupMembership where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGroupMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGroupMembershipResponse' smart constructor.
data DescribeGroupMembershipResponse = DescribeGroupMembershipResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The globally unique identifier for the identity store.
    identityStoreId :: Prelude.Text,
    -- | The identifier for a @GroupMembership@ in an identity store.
    membershipId :: Prelude.Text,
    -- | The identifier for a group in the identity store.
    groupId :: Prelude.Text,
    memberId :: MemberId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGroupMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeGroupMembershipResponse_httpStatus' - The response's http status code.
--
-- 'identityStoreId', 'describeGroupMembershipResponse_identityStoreId' - The globally unique identifier for the identity store.
--
-- 'membershipId', 'describeGroupMembershipResponse_membershipId' - The identifier for a @GroupMembership@ in an identity store.
--
-- 'groupId', 'describeGroupMembershipResponse_groupId' - The identifier for a group in the identity store.
--
-- 'memberId', 'describeGroupMembershipResponse_memberId' - Undocumented member.
newDescribeGroupMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'identityStoreId'
  Prelude.Text ->
  -- | 'membershipId'
  Prelude.Text ->
  -- | 'groupId'
  Prelude.Text ->
  -- | 'memberId'
  MemberId ->
  DescribeGroupMembershipResponse
newDescribeGroupMembershipResponse
  pHttpStatus_
  pIdentityStoreId_
  pMembershipId_
  pGroupId_
  pMemberId_ =
    DescribeGroupMembershipResponse'
      { httpStatus =
          pHttpStatus_,
        identityStoreId = pIdentityStoreId_,
        membershipId = pMembershipId_,
        groupId = pGroupId_,
        memberId = pMemberId_
      }

-- | The response's http status code.
describeGroupMembershipResponse_httpStatus :: Lens.Lens' DescribeGroupMembershipResponse Prelude.Int
describeGroupMembershipResponse_httpStatus = Lens.lens (\DescribeGroupMembershipResponse' {httpStatus} -> httpStatus) (\s@DescribeGroupMembershipResponse' {} a -> s {httpStatus = a} :: DescribeGroupMembershipResponse)

-- | The globally unique identifier for the identity store.
describeGroupMembershipResponse_identityStoreId :: Lens.Lens' DescribeGroupMembershipResponse Prelude.Text
describeGroupMembershipResponse_identityStoreId = Lens.lens (\DescribeGroupMembershipResponse' {identityStoreId} -> identityStoreId) (\s@DescribeGroupMembershipResponse' {} a -> s {identityStoreId = a} :: DescribeGroupMembershipResponse)

-- | The identifier for a @GroupMembership@ in an identity store.
describeGroupMembershipResponse_membershipId :: Lens.Lens' DescribeGroupMembershipResponse Prelude.Text
describeGroupMembershipResponse_membershipId = Lens.lens (\DescribeGroupMembershipResponse' {membershipId} -> membershipId) (\s@DescribeGroupMembershipResponse' {} a -> s {membershipId = a} :: DescribeGroupMembershipResponse)

-- | The identifier for a group in the identity store.
describeGroupMembershipResponse_groupId :: Lens.Lens' DescribeGroupMembershipResponse Prelude.Text
describeGroupMembershipResponse_groupId = Lens.lens (\DescribeGroupMembershipResponse' {groupId} -> groupId) (\s@DescribeGroupMembershipResponse' {} a -> s {groupId = a} :: DescribeGroupMembershipResponse)

-- | Undocumented member.
describeGroupMembershipResponse_memberId :: Lens.Lens' DescribeGroupMembershipResponse MemberId
describeGroupMembershipResponse_memberId = Lens.lens (\DescribeGroupMembershipResponse' {memberId} -> memberId) (\s@DescribeGroupMembershipResponse' {} a -> s {memberId = a} :: DescribeGroupMembershipResponse)

instance
  Prelude.NFData
    DescribeGroupMembershipResponse
  where
  rnf DescribeGroupMembershipResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf identityStoreId
      `Prelude.seq` Prelude.rnf membershipId
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf memberId
