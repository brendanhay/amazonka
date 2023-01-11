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
-- Module      : Amazonka.QuickSight.ListIAMPolicyAssignments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists IAM policy assignments in the current Amazon QuickSight account.
module Amazonka.QuickSight.ListIAMPolicyAssignments
  ( -- * Creating a Request
    ListIAMPolicyAssignments (..),
    newListIAMPolicyAssignments,

    -- * Request Lenses
    listIAMPolicyAssignments_assignmentStatus,
    listIAMPolicyAssignments_maxResults,
    listIAMPolicyAssignments_nextToken,
    listIAMPolicyAssignments_awsAccountId,
    listIAMPolicyAssignments_namespace,

    -- * Destructuring the Response
    ListIAMPolicyAssignmentsResponse (..),
    newListIAMPolicyAssignmentsResponse,

    -- * Response Lenses
    listIAMPolicyAssignmentsResponse_iAMPolicyAssignments,
    listIAMPolicyAssignmentsResponse_nextToken,
    listIAMPolicyAssignmentsResponse_requestId,
    listIAMPolicyAssignmentsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIAMPolicyAssignments' smart constructor.
data ListIAMPolicyAssignments = ListIAMPolicyAssignments'
  { -- | The status of the assignments.
    assignmentStatus :: Prelude.Maybe AssignmentStatus,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains these IAM policy
    -- assignments.
    awsAccountId :: Prelude.Text,
    -- | The namespace for the assignments.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIAMPolicyAssignments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentStatus', 'listIAMPolicyAssignments_assignmentStatus' - The status of the assignments.
--
-- 'maxResults', 'listIAMPolicyAssignments_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listIAMPolicyAssignments_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listIAMPolicyAssignments_awsAccountId' - The ID of the Amazon Web Services account that contains these IAM policy
-- assignments.
--
-- 'namespace', 'listIAMPolicyAssignments_namespace' - The namespace for the assignments.
newListIAMPolicyAssignments ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  ListIAMPolicyAssignments
newListIAMPolicyAssignments
  pAwsAccountId_
  pNamespace_ =
    ListIAMPolicyAssignments'
      { assignmentStatus =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The status of the assignments.
listIAMPolicyAssignments_assignmentStatus :: Lens.Lens' ListIAMPolicyAssignments (Prelude.Maybe AssignmentStatus)
listIAMPolicyAssignments_assignmentStatus = Lens.lens (\ListIAMPolicyAssignments' {assignmentStatus} -> assignmentStatus) (\s@ListIAMPolicyAssignments' {} a -> s {assignmentStatus = a} :: ListIAMPolicyAssignments)

-- | The maximum number of results to be returned per request.
listIAMPolicyAssignments_maxResults :: Lens.Lens' ListIAMPolicyAssignments (Prelude.Maybe Prelude.Natural)
listIAMPolicyAssignments_maxResults = Lens.lens (\ListIAMPolicyAssignments' {maxResults} -> maxResults) (\s@ListIAMPolicyAssignments' {} a -> s {maxResults = a} :: ListIAMPolicyAssignments)

-- | The token for the next set of results, or null if there are no more
-- results.
listIAMPolicyAssignments_nextToken :: Lens.Lens' ListIAMPolicyAssignments (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignments_nextToken = Lens.lens (\ListIAMPolicyAssignments' {nextToken} -> nextToken) (\s@ListIAMPolicyAssignments' {} a -> s {nextToken = a} :: ListIAMPolicyAssignments)

-- | The ID of the Amazon Web Services account that contains these IAM policy
-- assignments.
listIAMPolicyAssignments_awsAccountId :: Lens.Lens' ListIAMPolicyAssignments Prelude.Text
listIAMPolicyAssignments_awsAccountId = Lens.lens (\ListIAMPolicyAssignments' {awsAccountId} -> awsAccountId) (\s@ListIAMPolicyAssignments' {} a -> s {awsAccountId = a} :: ListIAMPolicyAssignments)

-- | The namespace for the assignments.
listIAMPolicyAssignments_namespace :: Lens.Lens' ListIAMPolicyAssignments Prelude.Text
listIAMPolicyAssignments_namespace = Lens.lens (\ListIAMPolicyAssignments' {namespace} -> namespace) (\s@ListIAMPolicyAssignments' {} a -> s {namespace = a} :: ListIAMPolicyAssignments)

instance Core.AWSRequest ListIAMPolicyAssignments where
  type
    AWSResponse ListIAMPolicyAssignments =
      ListIAMPolicyAssignmentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIAMPolicyAssignmentsResponse'
            Prelude.<$> ( x Data..?> "IAMPolicyAssignments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIAMPolicyAssignments where
  hashWithSalt _salt ListIAMPolicyAssignments' {..} =
    _salt `Prelude.hashWithSalt` assignmentStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData ListIAMPolicyAssignments where
  rnf ListIAMPolicyAssignments' {..} =
    Prelude.rnf assignmentStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf namespace

instance Data.ToHeaders ListIAMPolicyAssignments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIAMPolicyAssignments where
  toPath ListIAMPolicyAssignments' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/namespaces/",
        Data.toBS namespace,
        "/iam-policy-assignments"
      ]

instance Data.ToQuery ListIAMPolicyAssignments where
  toQuery ListIAMPolicyAssignments' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListIAMPolicyAssignmentsResponse' smart constructor.
data ListIAMPolicyAssignmentsResponse = ListIAMPolicyAssignmentsResponse'
  { -- | Information describing the IAM policy assignments.
    iAMPolicyAssignments :: Prelude.Maybe [IAMPolicyAssignmentSummary],
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
-- Create a value of 'ListIAMPolicyAssignmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iAMPolicyAssignments', 'listIAMPolicyAssignmentsResponse_iAMPolicyAssignments' - Information describing the IAM policy assignments.
--
-- 'nextToken', 'listIAMPolicyAssignmentsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listIAMPolicyAssignmentsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'listIAMPolicyAssignmentsResponse_status' - The HTTP status of the request.
newListIAMPolicyAssignmentsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListIAMPolicyAssignmentsResponse
newListIAMPolicyAssignmentsResponse pStatus_ =
  ListIAMPolicyAssignmentsResponse'
    { iAMPolicyAssignments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      status = pStatus_
    }

-- | Information describing the IAM policy assignments.
listIAMPolicyAssignmentsResponse_iAMPolicyAssignments :: Lens.Lens' ListIAMPolicyAssignmentsResponse (Prelude.Maybe [IAMPolicyAssignmentSummary])
listIAMPolicyAssignmentsResponse_iAMPolicyAssignments = Lens.lens (\ListIAMPolicyAssignmentsResponse' {iAMPolicyAssignments} -> iAMPolicyAssignments) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {iAMPolicyAssignments = a} :: ListIAMPolicyAssignmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no more
-- results.
listIAMPolicyAssignmentsResponse_nextToken :: Lens.Lens' ListIAMPolicyAssignmentsResponse (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignmentsResponse_nextToken = Lens.lens (\ListIAMPolicyAssignmentsResponse' {nextToken} -> nextToken) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {nextToken = a} :: ListIAMPolicyAssignmentsResponse)

-- | The Amazon Web Services request ID for this operation.
listIAMPolicyAssignmentsResponse_requestId :: Lens.Lens' ListIAMPolicyAssignmentsResponse (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignmentsResponse_requestId = Lens.lens (\ListIAMPolicyAssignmentsResponse' {requestId} -> requestId) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {requestId = a} :: ListIAMPolicyAssignmentsResponse)

-- | The HTTP status of the request.
listIAMPolicyAssignmentsResponse_status :: Lens.Lens' ListIAMPolicyAssignmentsResponse Prelude.Int
listIAMPolicyAssignmentsResponse_status = Lens.lens (\ListIAMPolicyAssignmentsResponse' {status} -> status) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {status = a} :: ListIAMPolicyAssignmentsResponse)

instance
  Prelude.NFData
    ListIAMPolicyAssignmentsResponse
  where
  rnf ListIAMPolicyAssignmentsResponse' {..} =
    Prelude.rnf iAMPolicyAssignments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
