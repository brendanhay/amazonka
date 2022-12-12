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
-- Module      : Amazonka.QuickSight.ListIAMPolicyAssignmentsForUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the IAM policy assignments, including the Amazon Resource
-- Names (ARNs) for the IAM policies assigned to the specified user and
-- group or groups that the user belongs to.
module Amazonka.QuickSight.ListIAMPolicyAssignmentsForUser
  ( -- * Creating a Request
    ListIAMPolicyAssignmentsForUser (..),
    newListIAMPolicyAssignmentsForUser,

    -- * Request Lenses
    listIAMPolicyAssignmentsForUser_maxResults,
    listIAMPolicyAssignmentsForUser_nextToken,
    listIAMPolicyAssignmentsForUser_awsAccountId,
    listIAMPolicyAssignmentsForUser_userName,
    listIAMPolicyAssignmentsForUser_namespace,

    -- * Destructuring the Response
    ListIAMPolicyAssignmentsForUserResponse (..),
    newListIAMPolicyAssignmentsForUserResponse,

    -- * Response Lenses
    listIAMPolicyAssignmentsForUserResponse_activeAssignments,
    listIAMPolicyAssignmentsForUserResponse_nextToken,
    listIAMPolicyAssignmentsForUserResponse_requestId,
    listIAMPolicyAssignmentsForUserResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIAMPolicyAssignmentsForUser' smart constructor.
data ListIAMPolicyAssignmentsForUser = ListIAMPolicyAssignmentsForUser'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the assignments.
    awsAccountId :: Prelude.Text,
    -- | The name of the user.
    userName :: Prelude.Text,
    -- | The namespace of the assignment.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIAMPolicyAssignmentsForUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIAMPolicyAssignmentsForUser_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listIAMPolicyAssignmentsForUser_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listIAMPolicyAssignmentsForUser_awsAccountId' - The ID of the Amazon Web Services account that contains the assignments.
--
-- 'userName', 'listIAMPolicyAssignmentsForUser_userName' - The name of the user.
--
-- 'namespace', 'listIAMPolicyAssignmentsForUser_namespace' - The namespace of the assignment.
newListIAMPolicyAssignmentsForUser ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  ListIAMPolicyAssignmentsForUser
newListIAMPolicyAssignmentsForUser
  pAwsAccountId_
  pUserName_
  pNamespace_ =
    ListIAMPolicyAssignmentsForUser'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        userName = pUserName_,
        namespace = pNamespace_
      }

-- | The maximum number of results to be returned per request.
listIAMPolicyAssignmentsForUser_maxResults :: Lens.Lens' ListIAMPolicyAssignmentsForUser (Prelude.Maybe Prelude.Natural)
listIAMPolicyAssignmentsForUser_maxResults = Lens.lens (\ListIAMPolicyAssignmentsForUser' {maxResults} -> maxResults) (\s@ListIAMPolicyAssignmentsForUser' {} a -> s {maxResults = a} :: ListIAMPolicyAssignmentsForUser)

-- | The token for the next set of results, or null if there are no more
-- results.
listIAMPolicyAssignmentsForUser_nextToken :: Lens.Lens' ListIAMPolicyAssignmentsForUser (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignmentsForUser_nextToken = Lens.lens (\ListIAMPolicyAssignmentsForUser' {nextToken} -> nextToken) (\s@ListIAMPolicyAssignmentsForUser' {} a -> s {nextToken = a} :: ListIAMPolicyAssignmentsForUser)

-- | The ID of the Amazon Web Services account that contains the assignments.
listIAMPolicyAssignmentsForUser_awsAccountId :: Lens.Lens' ListIAMPolicyAssignmentsForUser Prelude.Text
listIAMPolicyAssignmentsForUser_awsAccountId = Lens.lens (\ListIAMPolicyAssignmentsForUser' {awsAccountId} -> awsAccountId) (\s@ListIAMPolicyAssignmentsForUser' {} a -> s {awsAccountId = a} :: ListIAMPolicyAssignmentsForUser)

-- | The name of the user.
listIAMPolicyAssignmentsForUser_userName :: Lens.Lens' ListIAMPolicyAssignmentsForUser Prelude.Text
listIAMPolicyAssignmentsForUser_userName = Lens.lens (\ListIAMPolicyAssignmentsForUser' {userName} -> userName) (\s@ListIAMPolicyAssignmentsForUser' {} a -> s {userName = a} :: ListIAMPolicyAssignmentsForUser)

-- | The namespace of the assignment.
listIAMPolicyAssignmentsForUser_namespace :: Lens.Lens' ListIAMPolicyAssignmentsForUser Prelude.Text
listIAMPolicyAssignmentsForUser_namespace = Lens.lens (\ListIAMPolicyAssignmentsForUser' {namespace} -> namespace) (\s@ListIAMPolicyAssignmentsForUser' {} a -> s {namespace = a} :: ListIAMPolicyAssignmentsForUser)

instance
  Core.AWSRequest
    ListIAMPolicyAssignmentsForUser
  where
  type
    AWSResponse ListIAMPolicyAssignmentsForUser =
      ListIAMPolicyAssignmentsForUserResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIAMPolicyAssignmentsForUserResponse'
            Prelude.<$> ( x Data..?> "ActiveAssignments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListIAMPolicyAssignmentsForUser
  where
  hashWithSalt
    _salt
    ListIAMPolicyAssignmentsForUser' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` awsAccountId
        `Prelude.hashWithSalt` userName
        `Prelude.hashWithSalt` namespace

instance
  Prelude.NFData
    ListIAMPolicyAssignmentsForUser
  where
  rnf ListIAMPolicyAssignmentsForUser' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf namespace

instance
  Data.ToHeaders
    ListIAMPolicyAssignmentsForUser
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIAMPolicyAssignmentsForUser where
  toPath ListIAMPolicyAssignmentsForUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/users/",
        Data.toBS userName,
        "/iam-policy-assignments"
      ]

instance Data.ToQuery ListIAMPolicyAssignmentsForUser where
  toQuery ListIAMPolicyAssignmentsForUser' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListIAMPolicyAssignmentsForUserResponse' smart constructor.
data ListIAMPolicyAssignmentsForUserResponse = ListIAMPolicyAssignmentsForUserResponse'
  { -- | The active assignments for this user.
    activeAssignments :: Prelude.Maybe [ActiveIAMPolicyAssignment],
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIAMPolicyAssignmentsForUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeAssignments', 'listIAMPolicyAssignmentsForUserResponse_activeAssignments' - The active assignments for this user.
--
-- 'nextToken', 'listIAMPolicyAssignmentsForUserResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listIAMPolicyAssignmentsForUserResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listIAMPolicyAssignmentsForUserResponse_status' - The HTTP status of the request.
newListIAMPolicyAssignmentsForUserResponse ::
  -- | 'status'
  Prelude.Int ->
  ListIAMPolicyAssignmentsForUserResponse
newListIAMPolicyAssignmentsForUserResponse pStatus_ =
  ListIAMPolicyAssignmentsForUserResponse'
    { activeAssignments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | The active assignments for this user.
listIAMPolicyAssignmentsForUserResponse_activeAssignments :: Lens.Lens' ListIAMPolicyAssignmentsForUserResponse (Prelude.Maybe [ActiveIAMPolicyAssignment])
listIAMPolicyAssignmentsForUserResponse_activeAssignments = Lens.lens (\ListIAMPolicyAssignmentsForUserResponse' {activeAssignments} -> activeAssignments) (\s@ListIAMPolicyAssignmentsForUserResponse' {} a -> s {activeAssignments = a} :: ListIAMPolicyAssignmentsForUserResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listIAMPolicyAssignmentsForUserResponse_nextToken :: Lens.Lens' ListIAMPolicyAssignmentsForUserResponse (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignmentsForUserResponse_nextToken = Lens.lens (\ListIAMPolicyAssignmentsForUserResponse' {nextToken} -> nextToken) (\s@ListIAMPolicyAssignmentsForUserResponse' {} a -> s {nextToken = a} :: ListIAMPolicyAssignmentsForUserResponse)

-- | The Amazon Web Services request ID for this operation.
listIAMPolicyAssignmentsForUserResponse_requestId :: Lens.Lens' ListIAMPolicyAssignmentsForUserResponse (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignmentsForUserResponse_requestId = Lens.lens (\ListIAMPolicyAssignmentsForUserResponse' {requestId} -> requestId) (\s@ListIAMPolicyAssignmentsForUserResponse' {} a -> s {requestId = a} :: ListIAMPolicyAssignmentsForUserResponse)

-- | The HTTP status of the request.
listIAMPolicyAssignmentsForUserResponse_status :: Lens.Lens' ListIAMPolicyAssignmentsForUserResponse Prelude.Int
listIAMPolicyAssignmentsForUserResponse_status = Lens.lens (\ListIAMPolicyAssignmentsForUserResponse' {status} -> status) (\s@ListIAMPolicyAssignmentsForUserResponse' {} a -> s {status = a} :: ListIAMPolicyAssignmentsForUserResponse)

instance
  Prelude.NFData
    ListIAMPolicyAssignmentsForUserResponse
  where
  rnf ListIAMPolicyAssignmentsForUserResponse' {..} =
    Prelude.rnf activeAssignments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
