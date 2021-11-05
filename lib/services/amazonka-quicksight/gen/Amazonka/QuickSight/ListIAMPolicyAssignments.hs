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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists IAMpolicy assignments in the current Amazon QuickSight account.
module Amazonka.QuickSight.ListIAMPolicyAssignments
  ( -- * Creating a Request
    ListIAMPolicyAssignments (..),
    newListIAMPolicyAssignments,

    -- * Request Lenses
    listIAMPolicyAssignments_nextToken,
    listIAMPolicyAssignments_assignmentStatus,
    listIAMPolicyAssignments_maxResults,
    listIAMPolicyAssignments_awsAccountId,
    listIAMPolicyAssignments_namespace,

    -- * Destructuring the Response
    ListIAMPolicyAssignmentsResponse (..),
    newListIAMPolicyAssignmentsResponse,

    -- * Response Lenses
    listIAMPolicyAssignmentsResponse_requestId,
    listIAMPolicyAssignmentsResponse_nextToken,
    listIAMPolicyAssignmentsResponse_iAMPolicyAssignments,
    listIAMPolicyAssignmentsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIAMPolicyAssignments' smart constructor.
data ListIAMPolicyAssignments = ListIAMPolicyAssignments'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the assignments.
    assignmentStatus :: Prelude.Maybe AssignmentStatus,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains these IAMpolicy
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
-- 'nextToken', 'listIAMPolicyAssignments_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'assignmentStatus', 'listIAMPolicyAssignments_assignmentStatus' - The status of the assignments.
--
-- 'maxResults', 'listIAMPolicyAssignments_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'listIAMPolicyAssignments_awsAccountId' - The ID of the Amazon Web Services account that contains these IAMpolicy
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
      { nextToken =
          Prelude.Nothing,
        assignmentStatus = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        namespace = pNamespace_
      }

-- | The token for the next set of results, or null if there are no more
-- results.
listIAMPolicyAssignments_nextToken :: Lens.Lens' ListIAMPolicyAssignments (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignments_nextToken = Lens.lens (\ListIAMPolicyAssignments' {nextToken} -> nextToken) (\s@ListIAMPolicyAssignments' {} a -> s {nextToken = a} :: ListIAMPolicyAssignments)

-- | The status of the assignments.
listIAMPolicyAssignments_assignmentStatus :: Lens.Lens' ListIAMPolicyAssignments (Prelude.Maybe AssignmentStatus)
listIAMPolicyAssignments_assignmentStatus = Lens.lens (\ListIAMPolicyAssignments' {assignmentStatus} -> assignmentStatus) (\s@ListIAMPolicyAssignments' {} a -> s {assignmentStatus = a} :: ListIAMPolicyAssignments)

-- | The maximum number of results to be returned per request.
listIAMPolicyAssignments_maxResults :: Lens.Lens' ListIAMPolicyAssignments (Prelude.Maybe Prelude.Natural)
listIAMPolicyAssignments_maxResults = Lens.lens (\ListIAMPolicyAssignments' {maxResults} -> maxResults) (\s@ListIAMPolicyAssignments' {} a -> s {maxResults = a} :: ListIAMPolicyAssignments)

-- | The ID of the Amazon Web Services account that contains these IAMpolicy
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIAMPolicyAssignmentsResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "IAMPolicyAssignments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIAMPolicyAssignments

instance Prelude.NFData ListIAMPolicyAssignments

instance Core.ToHeaders ListIAMPolicyAssignments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListIAMPolicyAssignments where
  toPath ListIAMPolicyAssignments' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/namespaces/",
        Core.toBS namespace,
        "/iam-policy-assignments"
      ]

instance Core.ToQuery ListIAMPolicyAssignments where
  toQuery ListIAMPolicyAssignments' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListIAMPolicyAssignmentsResponse' smart constructor.
data ListIAMPolicyAssignmentsResponse = ListIAMPolicyAssignmentsResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information describing the IAMpolicy assignments.
    iAMPolicyAssignments :: Prelude.Maybe [IAMPolicyAssignmentSummary],
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
-- 'requestId', 'listIAMPolicyAssignmentsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'nextToken', 'listIAMPolicyAssignmentsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'iAMPolicyAssignments', 'listIAMPolicyAssignmentsResponse_iAMPolicyAssignments' - Information describing the IAMpolicy assignments.
--
-- 'status', 'listIAMPolicyAssignmentsResponse_status' - The HTTP status of the request.
newListIAMPolicyAssignmentsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListIAMPolicyAssignmentsResponse
newListIAMPolicyAssignmentsResponse pStatus_ =
  ListIAMPolicyAssignmentsResponse'
    { requestId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      iAMPolicyAssignments = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Web Services request ID for this operation.
listIAMPolicyAssignmentsResponse_requestId :: Lens.Lens' ListIAMPolicyAssignmentsResponse (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignmentsResponse_requestId = Lens.lens (\ListIAMPolicyAssignmentsResponse' {requestId} -> requestId) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {requestId = a} :: ListIAMPolicyAssignmentsResponse)

-- | The token for the next set of results, or null if there are no more
-- results.
listIAMPolicyAssignmentsResponse_nextToken :: Lens.Lens' ListIAMPolicyAssignmentsResponse (Prelude.Maybe Prelude.Text)
listIAMPolicyAssignmentsResponse_nextToken = Lens.lens (\ListIAMPolicyAssignmentsResponse' {nextToken} -> nextToken) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {nextToken = a} :: ListIAMPolicyAssignmentsResponse)

-- | Information describing the IAMpolicy assignments.
listIAMPolicyAssignmentsResponse_iAMPolicyAssignments :: Lens.Lens' ListIAMPolicyAssignmentsResponse (Prelude.Maybe [IAMPolicyAssignmentSummary])
listIAMPolicyAssignmentsResponse_iAMPolicyAssignments = Lens.lens (\ListIAMPolicyAssignmentsResponse' {iAMPolicyAssignments} -> iAMPolicyAssignments) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {iAMPolicyAssignments = a} :: ListIAMPolicyAssignmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listIAMPolicyAssignmentsResponse_status :: Lens.Lens' ListIAMPolicyAssignmentsResponse Prelude.Int
listIAMPolicyAssignmentsResponse_status = Lens.lens (\ListIAMPolicyAssignmentsResponse' {status} -> status) (\s@ListIAMPolicyAssignmentsResponse' {} a -> s {status = a} :: ListIAMPolicyAssignmentsResponse)

instance
  Prelude.NFData
    ListIAMPolicyAssignmentsResponse
