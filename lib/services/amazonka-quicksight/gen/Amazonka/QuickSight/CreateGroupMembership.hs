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
-- Module      : Amazonka.QuickSight.CreateGroupMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an Amazon QuickSight user to an Amazon QuickSight group.
module Amazonka.QuickSight.CreateGroupMembership
  ( -- * Creating a Request
    CreateGroupMembership (..),
    newCreateGroupMembership,

    -- * Request Lenses
    createGroupMembership_memberName,
    createGroupMembership_groupName,
    createGroupMembership_awsAccountId,
    createGroupMembership_namespace,

    -- * Destructuring the Response
    CreateGroupMembershipResponse (..),
    newCreateGroupMembershipResponse,

    -- * Response Lenses
    createGroupMembershipResponse_groupMember,
    createGroupMembershipResponse_requestId,
    createGroupMembershipResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateGroupMembership' smart constructor.
data CreateGroupMembership = CreateGroupMembership'
  { -- | The name of the user that you want to add to the group membership.
    memberName :: Prelude.Text,
    -- | The name of the group that you want to add the user to.
    groupName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace that you want the user to be a part of.
    namespace :: Prelude.Text
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
-- 'memberName', 'createGroupMembership_memberName' - The name of the user that you want to add to the group membership.
--
-- 'groupName', 'createGroupMembership_groupName' - The name of the group that you want to add the user to.
--
-- 'awsAccountId', 'createGroupMembership_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'createGroupMembership_namespace' - The namespace that you want the user to be a part of.
newCreateGroupMembership ::
  -- | 'memberName'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  CreateGroupMembership
newCreateGroupMembership
  pMemberName_
  pGroupName_
  pAwsAccountId_
  pNamespace_ =
    CreateGroupMembership'
      { memberName = pMemberName_,
        groupName = pGroupName_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The name of the user that you want to add to the group membership.
createGroupMembership_memberName :: Lens.Lens' CreateGroupMembership Prelude.Text
createGroupMembership_memberName = Lens.lens (\CreateGroupMembership' {memberName} -> memberName) (\s@CreateGroupMembership' {} a -> s {memberName = a} :: CreateGroupMembership)

-- | The name of the group that you want to add the user to.
createGroupMembership_groupName :: Lens.Lens' CreateGroupMembership Prelude.Text
createGroupMembership_groupName = Lens.lens (\CreateGroupMembership' {groupName} -> groupName) (\s@CreateGroupMembership' {} a -> s {groupName = a} :: CreateGroupMembership)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
createGroupMembership_awsAccountId :: Lens.Lens' CreateGroupMembership Prelude.Text
createGroupMembership_awsAccountId = Lens.lens (\CreateGroupMembership' {awsAccountId} -> awsAccountId) (\s@CreateGroupMembership' {} a -> s {awsAccountId = a} :: CreateGroupMembership)

-- | The namespace that you want the user to be a part of.
createGroupMembership_namespace :: Lens.Lens' CreateGroupMembership Prelude.Text
createGroupMembership_namespace = Lens.lens (\CreateGroupMembership' {namespace} -> namespace) (\s@CreateGroupMembership' {} a -> s {namespace = a} :: CreateGroupMembership)

instance Core.AWSRequest CreateGroupMembership where
  type
    AWSResponse CreateGroupMembership =
      CreateGroupMembershipResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupMembershipResponse'
            Prelude.<$> (x Data..?> "GroupMember")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateGroupMembership where
  hashWithSalt _salt CreateGroupMembership' {..} =
    _salt `Prelude.hashWithSalt` memberName
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData CreateGroupMembership where
  rnf CreateGroupMembership' {..} =
    Prelude.rnf memberName
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders CreateGroupMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateGroupMembership where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CreateGroupMembership where
  toPath CreateGroupMembership' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/groups/",
        Data.toBS groupName,
        "/members/",
        Data.toBS memberName
      ]

instance Data.ToQuery CreateGroupMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupMembershipResponse' smart constructor.
data CreateGroupMembershipResponse = CreateGroupMembershipResponse'
  { -- | The group member.
    groupMember :: Prelude.Maybe GroupMember,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
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
-- 'groupMember', 'createGroupMembershipResponse_groupMember' - The group member.
--
-- 'requestId', 'createGroupMembershipResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'createGroupMembershipResponse_status' - The HTTP status of the request.
newCreateGroupMembershipResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateGroupMembershipResponse
newCreateGroupMembershipResponse pStatus_ =
  CreateGroupMembershipResponse'
    { groupMember =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The group member.
createGroupMembershipResponse_groupMember :: Lens.Lens' CreateGroupMembershipResponse (Prelude.Maybe GroupMember)
createGroupMembershipResponse_groupMember = Lens.lens (\CreateGroupMembershipResponse' {groupMember} -> groupMember) (\s@CreateGroupMembershipResponse' {} a -> s {groupMember = a} :: CreateGroupMembershipResponse)

-- | The Amazon Web Services request ID for this operation.
createGroupMembershipResponse_requestId :: Lens.Lens' CreateGroupMembershipResponse (Prelude.Maybe Prelude.Text)
createGroupMembershipResponse_requestId = Lens.lens (\CreateGroupMembershipResponse' {requestId} -> requestId) (\s@CreateGroupMembershipResponse' {} a -> s {requestId = a} :: CreateGroupMembershipResponse)

-- | The HTTP status of the request.
createGroupMembershipResponse_status :: Lens.Lens' CreateGroupMembershipResponse Prelude.Int
createGroupMembershipResponse_status = Lens.lens (\CreateGroupMembershipResponse' {status} -> status) (\s@CreateGroupMembershipResponse' {} a -> s {status = a} :: CreateGroupMembershipResponse)

instance Prelude.NFData CreateGroupMembershipResponse where
  rnf CreateGroupMembershipResponse' {..} =
    Prelude.rnf groupMember
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
