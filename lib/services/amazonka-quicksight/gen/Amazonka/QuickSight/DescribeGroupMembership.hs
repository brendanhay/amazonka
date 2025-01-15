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
-- Module      : Amazonka.QuickSight.DescribeGroupMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use the @DescribeGroupMembership@ operation to determine if a user is a
-- member of the specified group. If the user exists and is a member of the
-- specified group, an associated @GroupMember@ object is returned.
module Amazonka.QuickSight.DescribeGroupMembership
  ( -- * Creating a Request
    DescribeGroupMembership (..),
    newDescribeGroupMembership,

    -- * Request Lenses
    describeGroupMembership_memberName,
    describeGroupMembership_groupName,
    describeGroupMembership_awsAccountId,
    describeGroupMembership_namespace,

    -- * Destructuring the Response
    DescribeGroupMembershipResponse (..),
    newDescribeGroupMembershipResponse,

    -- * Response Lenses
    describeGroupMembershipResponse_groupMember,
    describeGroupMembershipResponse_requestId,
    describeGroupMembershipResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGroupMembership' smart constructor.
data DescribeGroupMembership = DescribeGroupMembership'
  { -- | The user name of the user that you want to search for.
    memberName :: Prelude.Text,
    -- | The name of the group that you want to search.
    groupName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace that includes the group you are searching within.
    namespace :: Prelude.Text
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
-- 'memberName', 'describeGroupMembership_memberName' - The user name of the user that you want to search for.
--
-- 'groupName', 'describeGroupMembership_groupName' - The name of the group that you want to search.
--
-- 'awsAccountId', 'describeGroupMembership_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'describeGroupMembership_namespace' - The namespace that includes the group you are searching within.
newDescribeGroupMembership ::
  -- | 'memberName'
  Prelude.Text ->
  -- | 'groupName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  DescribeGroupMembership
newDescribeGroupMembership
  pMemberName_
  pGroupName_
  pAwsAccountId_
  pNamespace_ =
    DescribeGroupMembership'
      { memberName = pMemberName_,
        groupName = pGroupName_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The user name of the user that you want to search for.
describeGroupMembership_memberName :: Lens.Lens' DescribeGroupMembership Prelude.Text
describeGroupMembership_memberName = Lens.lens (\DescribeGroupMembership' {memberName} -> memberName) (\s@DescribeGroupMembership' {} a -> s {memberName = a} :: DescribeGroupMembership)

-- | The name of the group that you want to search.
describeGroupMembership_groupName :: Lens.Lens' DescribeGroupMembership Prelude.Text
describeGroupMembership_groupName = Lens.lens (\DescribeGroupMembership' {groupName} -> groupName) (\s@DescribeGroupMembership' {} a -> s {groupName = a} :: DescribeGroupMembership)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
describeGroupMembership_awsAccountId :: Lens.Lens' DescribeGroupMembership Prelude.Text
describeGroupMembership_awsAccountId = Lens.lens (\DescribeGroupMembership' {awsAccountId} -> awsAccountId) (\s@DescribeGroupMembership' {} a -> s {awsAccountId = a} :: DescribeGroupMembership)

-- | The namespace that includes the group you are searching within.
describeGroupMembership_namespace :: Lens.Lens' DescribeGroupMembership Prelude.Text
describeGroupMembership_namespace = Lens.lens (\DescribeGroupMembership' {namespace} -> namespace) (\s@DescribeGroupMembership' {} a -> s {namespace = a} :: DescribeGroupMembership)

instance Core.AWSRequest DescribeGroupMembership where
  type
    AWSResponse DescribeGroupMembership =
      DescribeGroupMembershipResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGroupMembershipResponse'
            Prelude.<$> (x Data..?> "GroupMember")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGroupMembership where
  hashWithSalt _salt DescribeGroupMembership' {..} =
    _salt
      `Prelude.hashWithSalt` memberName
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData DescribeGroupMembership where
  rnf DescribeGroupMembership' {..} =
    Prelude.rnf memberName `Prelude.seq`
      Prelude.rnf groupName `Prelude.seq`
        Prelude.rnf awsAccountId `Prelude.seq`
          Prelude.rnf namespace

instance Data.ToHeaders DescribeGroupMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeGroupMembership where
  toPath DescribeGroupMembership' {..} =
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

instance Data.ToQuery DescribeGroupMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGroupMembershipResponse' smart constructor.
data DescribeGroupMembershipResponse = DescribeGroupMembershipResponse'
  { groupMember :: Prelude.Maybe GroupMember,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
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
-- 'groupMember', 'describeGroupMembershipResponse_groupMember' - Undocumented member.
--
-- 'requestId', 'describeGroupMembershipResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'describeGroupMembershipResponse_status' - The HTTP status of the request.
newDescribeGroupMembershipResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeGroupMembershipResponse
newDescribeGroupMembershipResponse pStatus_ =
  DescribeGroupMembershipResponse'
    { groupMember =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Undocumented member.
describeGroupMembershipResponse_groupMember :: Lens.Lens' DescribeGroupMembershipResponse (Prelude.Maybe GroupMember)
describeGroupMembershipResponse_groupMember = Lens.lens (\DescribeGroupMembershipResponse' {groupMember} -> groupMember) (\s@DescribeGroupMembershipResponse' {} a -> s {groupMember = a} :: DescribeGroupMembershipResponse)

-- | The Amazon Web Services request ID for this operation.
describeGroupMembershipResponse_requestId :: Lens.Lens' DescribeGroupMembershipResponse (Prelude.Maybe Prelude.Text)
describeGroupMembershipResponse_requestId = Lens.lens (\DescribeGroupMembershipResponse' {requestId} -> requestId) (\s@DescribeGroupMembershipResponse' {} a -> s {requestId = a} :: DescribeGroupMembershipResponse)

-- | The HTTP status of the request.
describeGroupMembershipResponse_status :: Lens.Lens' DescribeGroupMembershipResponse Prelude.Int
describeGroupMembershipResponse_status = Lens.lens (\DescribeGroupMembershipResponse' {status} -> status) (\s@DescribeGroupMembershipResponse' {} a -> s {status = a} :: DescribeGroupMembershipResponse)

instance
  Prelude.NFData
    DescribeGroupMembershipResponse
  where
  rnf DescribeGroupMembershipResponse' {..} =
    Prelude.rnf groupMember `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf status
