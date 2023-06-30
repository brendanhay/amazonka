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
-- Module      : Amazonka.QuickSight.DeleteGroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a user from a group so that the user is no longer a member of
-- the group.
module Amazonka.QuickSight.DeleteGroupMembership
  ( -- * Creating a Request
    DeleteGroupMembership (..),
    newDeleteGroupMembership,

    -- * Request Lenses
    deleteGroupMembership_memberName,
    deleteGroupMembership_groupName,
    deleteGroupMembership_awsAccountId,
    deleteGroupMembership_namespace,

    -- * Destructuring the Response
    DeleteGroupMembershipResponse (..),
    newDeleteGroupMembershipResponse,

    -- * Response Lenses
    deleteGroupMembershipResponse_requestId,
    deleteGroupMembershipResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGroupMembership' smart constructor.
data DeleteGroupMembership = DeleteGroupMembership'
  { -- | The name of the user that you want to delete from the group membership.
    memberName :: Prelude.Text,
    -- | The name of the group that you want to delete the user from.
    groupName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace of the group that you want to remove a user from.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberName', 'deleteGroupMembership_memberName' - The name of the user that you want to delete from the group membership.
--
-- 'groupName', 'deleteGroupMembership_groupName' - The name of the group that you want to delete the user from.
--
-- 'awsAccountId', 'deleteGroupMembership_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'deleteGroupMembership_namespace' - The namespace of the group that you want to remove a user from.
newDeleteGroupMembership ::
  -- | 'memberName'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DeleteGroupMembership
newDeleteGroupMembership
  pMemberName_
  pGroupName_
  pAwsAccountId_
  pNamespace_ =
    DeleteGroupMembership'
      { memberName = pMemberName_,
        groupName = pGroupName_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The name of the user that you want to delete from the group membership.
deleteGroupMembership_memberName :: Lens.Lens' DeleteGroupMembership Prelude.Text
deleteGroupMembership_memberName = Lens.lens (\DeleteGroupMembership' {memberName} -> memberName) (\s@DeleteGroupMembership' {} a -> s {memberName = a} :: DeleteGroupMembership)

-- | The name of the group that you want to delete the user from.
deleteGroupMembership_groupName :: Lens.Lens' DeleteGroupMembership Prelude.Text
deleteGroupMembership_groupName = Lens.lens (\DeleteGroupMembership' {groupName} -> groupName) (\s@DeleteGroupMembership' {} a -> s {groupName = a} :: DeleteGroupMembership)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
deleteGroupMembership_awsAccountId :: Lens.Lens' DeleteGroupMembership Prelude.Text
deleteGroupMembership_awsAccountId = Lens.lens (\DeleteGroupMembership' {awsAccountId} -> awsAccountId) (\s@DeleteGroupMembership' {} a -> s {awsAccountId = a} :: DeleteGroupMembership)

-- | The namespace of the group that you want to remove a user from.
deleteGroupMembership_namespace :: Lens.Lens' DeleteGroupMembership Prelude.Text
deleteGroupMembership_namespace = Lens.lens (\DeleteGroupMembership' {namespace} -> namespace) (\s@DeleteGroupMembership' {} a -> s {namespace = a} :: DeleteGroupMembership)

instance Core.AWSRequest DeleteGroupMembership where
  type
    AWSResponse DeleteGroupMembership =
      DeleteGroupMembershipResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGroupMembershipResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGroupMembership where
  hashWithSalt _salt DeleteGroupMembership' {..} =
    _salt
      `Prelude.hashWithSalt` memberName
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DeleteGroupMembership where
  rnf DeleteGroupMembership' {..} =
    Prelude.rnf memberName
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders DeleteGroupMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteGroupMembership where
  toPath DeleteGroupMembership' {..} =
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

instance Data.ToQuery DeleteGroupMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGroupMembershipResponse' smart constructor.
data DeleteGroupMembershipResponse = DeleteGroupMembershipResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGroupMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteGroupMembershipResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteGroupMembershipResponse_status' - The HTTP status of the request.
newDeleteGroupMembershipResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteGroupMembershipResponse
newDeleteGroupMembershipResponse pStatus_ =
  DeleteGroupMembershipResponse'
    { requestId =
        Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteGroupMembershipResponse_requestId :: Lens.Lens' DeleteGroupMembershipResponse (Prelude.Maybe Prelude.Text)
deleteGroupMembershipResponse_requestId = Lens.lens (\DeleteGroupMembershipResponse' {requestId} -> requestId) (\s@DeleteGroupMembershipResponse' {} a -> s {requestId = a} :: DeleteGroupMembershipResponse)

-- | The HTTP status of the request.
deleteGroupMembershipResponse_status :: Lens.Lens' DeleteGroupMembershipResponse Prelude.Int
deleteGroupMembershipResponse_status = Lens.lens (\DeleteGroupMembershipResponse' {status} -> status) (\s@DeleteGroupMembershipResponse' {} a -> s {status = a} :: DeleteGroupMembershipResponse)

instance Prelude.NFData DeleteGroupMembershipResponse where
  rnf DeleteGroupMembershipResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
