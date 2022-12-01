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
-- Module      : Amazonka.QuickSight.ListGroupMemberships
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists member users in a group.
module Amazonka.QuickSight.ListGroupMemberships
  ( -- * Creating a Request
    ListGroupMemberships (..),
    newListGroupMemberships,

    -- * Request Lenses
    listGroupMemberships_nextToken,
    listGroupMemberships_maxResults,
    listGroupMemberships_groupName,
    listGroupMemberships_awsAccountId,
    listGroupMemberships_namespace,

    -- * Destructuring the Response
    ListGroupMembershipsResponse (..),
    newListGroupMembershipsResponse,

    -- * Response Lenses
    listGroupMembershipsResponse_nextToken,
    listGroupMembershipsResponse_requestId,
    listGroupMembershipsResponse_groupMemberList,
    listGroupMembershipsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGroupMemberships' smart constructor.
data ListGroupMemberships = ListGroupMemberships'
  { -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return from this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the group that you want to see a membership list of.
    groupName :: Prelude.Text,
    -- | The ID for the Amazon Web Services account that the group is in.
    -- Currently, you use the ID for the Amazon Web Services account that
    -- contains your Amazon QuickSight account.
    awsAccountId :: Prelude.Text,
    -- | The namespace of the group that you want a list of users from.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupMemberships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupMemberships_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'maxResults', 'listGroupMemberships_maxResults' - The maximum number of results to return from this request.
--
-- 'groupName', 'listGroupMemberships_groupName' - The name of the group that you want to see a membership list of.
--
-- 'awsAccountId', 'listGroupMemberships_awsAccountId' - The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
--
-- 'namespace', 'listGroupMemberships_namespace' - The namespace of the group that you want a list of users from.
newListGroupMemberships ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  ListGroupMemberships
newListGroupMemberships
  pGroupName_
  pAwsAccountId_
  pNamespace_ =
    ListGroupMemberships'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        groupName = pGroupName_,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | A pagination token that can be used in a subsequent request.
listGroupMemberships_nextToken :: Lens.Lens' ListGroupMemberships (Prelude.Maybe Prelude.Text)
listGroupMemberships_nextToken = Lens.lens (\ListGroupMemberships' {nextToken} -> nextToken) (\s@ListGroupMemberships' {} a -> s {nextToken = a} :: ListGroupMemberships)

-- | The maximum number of results to return from this request.
listGroupMemberships_maxResults :: Lens.Lens' ListGroupMemberships (Prelude.Maybe Prelude.Natural)
listGroupMemberships_maxResults = Lens.lens (\ListGroupMemberships' {maxResults} -> maxResults) (\s@ListGroupMemberships' {} a -> s {maxResults = a} :: ListGroupMemberships)

-- | The name of the group that you want to see a membership list of.
listGroupMemberships_groupName :: Lens.Lens' ListGroupMemberships Prelude.Text
listGroupMemberships_groupName = Lens.lens (\ListGroupMemberships' {groupName} -> groupName) (\s@ListGroupMemberships' {} a -> s {groupName = a} :: ListGroupMemberships)

-- | The ID for the Amazon Web Services account that the group is in.
-- Currently, you use the ID for the Amazon Web Services account that
-- contains your Amazon QuickSight account.
listGroupMemberships_awsAccountId :: Lens.Lens' ListGroupMemberships Prelude.Text
listGroupMemberships_awsAccountId = Lens.lens (\ListGroupMemberships' {awsAccountId} -> awsAccountId) (\s@ListGroupMemberships' {} a -> s {awsAccountId = a} :: ListGroupMemberships)

-- | The namespace of the group that you want a list of users from.
listGroupMemberships_namespace :: Lens.Lens' ListGroupMemberships Prelude.Text
listGroupMemberships_namespace = Lens.lens (\ListGroupMemberships' {namespace} -> namespace) (\s@ListGroupMemberships' {} a -> s {namespace = a} :: ListGroupMemberships)

instance Core.AWSRequest ListGroupMemberships where
  type
    AWSResponse ListGroupMemberships =
      ListGroupMembershipsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupMembershipsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> ( x Core..?> "GroupMemberList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGroupMemberships where
  hashWithSalt _salt ListGroupMemberships' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData ListGroupMemberships where
  rnf ListGroupMemberships' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToHeaders ListGroupMemberships where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListGroupMemberships where
  toPath ListGroupMemberships' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/namespaces/",
        Core.toBS namespace,
        "/groups/",
        Core.toBS groupName,
        "/members"
      ]

instance Core.ToQuery ListGroupMemberships where
  toQuery ListGroupMemberships' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListGroupMembershipsResponse' smart constructor.
data ListGroupMembershipsResponse = ListGroupMembershipsResponse'
  { -- | A pagination token that can be used in a subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The list of the members of the group.
    groupMemberList :: Prelude.Maybe [GroupMember],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGroupMembershipsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listGroupMembershipsResponse_nextToken' - A pagination token that can be used in a subsequent request.
--
-- 'requestId', 'listGroupMembershipsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'groupMemberList', 'listGroupMembershipsResponse_groupMemberList' - The list of the members of the group.
--
-- 'status', 'listGroupMembershipsResponse_status' - The HTTP status of the request.
newListGroupMembershipsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListGroupMembershipsResponse
newListGroupMembershipsResponse pStatus_ =
  ListGroupMembershipsResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      groupMemberList = Prelude.Nothing,
      status = pStatus_
    }

-- | A pagination token that can be used in a subsequent request.
listGroupMembershipsResponse_nextToken :: Lens.Lens' ListGroupMembershipsResponse (Prelude.Maybe Prelude.Text)
listGroupMembershipsResponse_nextToken = Lens.lens (\ListGroupMembershipsResponse' {nextToken} -> nextToken) (\s@ListGroupMembershipsResponse' {} a -> s {nextToken = a} :: ListGroupMembershipsResponse)

-- | The Amazon Web Services request ID for this operation.
listGroupMembershipsResponse_requestId :: Lens.Lens' ListGroupMembershipsResponse (Prelude.Maybe Prelude.Text)
listGroupMembershipsResponse_requestId = Lens.lens (\ListGroupMembershipsResponse' {requestId} -> requestId) (\s@ListGroupMembershipsResponse' {} a -> s {requestId = a} :: ListGroupMembershipsResponse)

-- | The list of the members of the group.
listGroupMembershipsResponse_groupMemberList :: Lens.Lens' ListGroupMembershipsResponse (Prelude.Maybe [GroupMember])
listGroupMembershipsResponse_groupMemberList = Lens.lens (\ListGroupMembershipsResponse' {groupMemberList} -> groupMemberList) (\s@ListGroupMembershipsResponse' {} a -> s {groupMemberList = a} :: ListGroupMembershipsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listGroupMembershipsResponse_status :: Lens.Lens' ListGroupMembershipsResponse Prelude.Int
listGroupMembershipsResponse_status = Lens.lens (\ListGroupMembershipsResponse' {status} -> status) (\s@ListGroupMembershipsResponse' {} a -> s {status = a} :: ListGroupMembershipsResponse)

instance Prelude.NFData ListGroupMembershipsResponse where
  rnf ListGroupMembershipsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf groupMemberList
      `Prelude.seq` Prelude.rnf status
