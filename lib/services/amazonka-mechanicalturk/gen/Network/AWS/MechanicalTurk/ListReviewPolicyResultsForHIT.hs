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
-- Module      : Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewPolicyResultsForHIT@ operation retrieves the computed
-- results and the actions taken in the course of executing your Review
-- Policies for a given HIT. For information about how to specify Review
-- Policies when you call CreateHIT, see Review Policies. The
-- ListReviewPolicyResultsForHIT operation can return results for both
-- Assignment-level and HIT-level review results.
module Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
  ( -- * Creating a Request
    ListReviewPolicyResultsForHIT (..),
    newListReviewPolicyResultsForHIT,

    -- * Request Lenses
    listReviewPolicyResultsForHIT_retrieveResults,
    listReviewPolicyResultsForHIT_policyLevels,
    listReviewPolicyResultsForHIT_retrieveActions,
    listReviewPolicyResultsForHIT_nextToken,
    listReviewPolicyResultsForHIT_maxResults,
    listReviewPolicyResultsForHIT_hITId,

    -- * Destructuring the Response
    ListReviewPolicyResultsForHITResponse (..),
    newListReviewPolicyResultsForHITResponse,

    -- * Response Lenses
    listReviewPolicyResultsForHITResponse_hITReviewPolicy,
    listReviewPolicyResultsForHITResponse_hITReviewReport,
    listReviewPolicyResultsForHITResponse_nextToken,
    listReviewPolicyResultsForHITResponse_assignmentReviewReport,
    listReviewPolicyResultsForHITResponse_hITId,
    listReviewPolicyResultsForHITResponse_assignmentReviewPolicy,
    listReviewPolicyResultsForHITResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReviewPolicyResultsForHIT' smart constructor.
data ListReviewPolicyResultsForHIT = ListReviewPolicyResultsForHIT'
  { -- | Specify if the operation should retrieve a list of the results computed
    -- by the Review Policies.
    retrieveResults :: Prelude.Maybe Prelude.Bool,
    -- | The Policy Level(s) to retrieve review results for - HIT or Assignment.
    -- If omitted, the default behavior is to retrieve all data for both policy
    -- levels. For a list of all the described policies, see Review Policies.
    policyLevels :: Prelude.Maybe [ReviewPolicyLevel],
    -- | Specify if the operation should retrieve a list of the actions taken
    -- executing the Review Policies and their outcomes.
    retrieveActions :: Prelude.Maybe Prelude.Bool,
    -- | Pagination token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Limit the number of results returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the HIT to retrieve review results for.
    hITId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReviewPolicyResultsForHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retrieveResults', 'listReviewPolicyResultsForHIT_retrieveResults' - Specify if the operation should retrieve a list of the results computed
-- by the Review Policies.
--
-- 'policyLevels', 'listReviewPolicyResultsForHIT_policyLevels' - The Policy Level(s) to retrieve review results for - HIT or Assignment.
-- If omitted, the default behavior is to retrieve all data for both policy
-- levels. For a list of all the described policies, see Review Policies.
--
-- 'retrieveActions', 'listReviewPolicyResultsForHIT_retrieveActions' - Specify if the operation should retrieve a list of the actions taken
-- executing the Review Policies and their outcomes.
--
-- 'nextToken', 'listReviewPolicyResultsForHIT_nextToken' - Pagination token
--
-- 'maxResults', 'listReviewPolicyResultsForHIT_maxResults' - Limit the number of results returned.
--
-- 'hITId', 'listReviewPolicyResultsForHIT_hITId' - The unique identifier of the HIT to retrieve review results for.
newListReviewPolicyResultsForHIT ::
  -- | 'hITId'
  Prelude.Text ->
  ListReviewPolicyResultsForHIT
newListReviewPolicyResultsForHIT pHITId_ =
  ListReviewPolicyResultsForHIT'
    { retrieveResults =
        Prelude.Nothing,
      policyLevels = Prelude.Nothing,
      retrieveActions = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      hITId = pHITId_
    }

-- | Specify if the operation should retrieve a list of the results computed
-- by the Review Policies.
listReviewPolicyResultsForHIT_retrieveResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Prelude.Maybe Prelude.Bool)
listReviewPolicyResultsForHIT_retrieveResults = Lens.lens (\ListReviewPolicyResultsForHIT' {retrieveResults} -> retrieveResults) (\s@ListReviewPolicyResultsForHIT' {} a -> s {retrieveResults = a} :: ListReviewPolicyResultsForHIT)

-- | The Policy Level(s) to retrieve review results for - HIT or Assignment.
-- If omitted, the default behavior is to retrieve all data for both policy
-- levels. For a list of all the described policies, see Review Policies.
listReviewPolicyResultsForHIT_policyLevels :: Lens.Lens' ListReviewPolicyResultsForHIT (Prelude.Maybe [ReviewPolicyLevel])
listReviewPolicyResultsForHIT_policyLevels = Lens.lens (\ListReviewPolicyResultsForHIT' {policyLevels} -> policyLevels) (\s@ListReviewPolicyResultsForHIT' {} a -> s {policyLevels = a} :: ListReviewPolicyResultsForHIT) Prelude.. Lens.mapping Lens.coerced

-- | Specify if the operation should retrieve a list of the actions taken
-- executing the Review Policies and their outcomes.
listReviewPolicyResultsForHIT_retrieveActions :: Lens.Lens' ListReviewPolicyResultsForHIT (Prelude.Maybe Prelude.Bool)
listReviewPolicyResultsForHIT_retrieveActions = Lens.lens (\ListReviewPolicyResultsForHIT' {retrieveActions} -> retrieveActions) (\s@ListReviewPolicyResultsForHIT' {} a -> s {retrieveActions = a} :: ListReviewPolicyResultsForHIT)

-- | Pagination token
listReviewPolicyResultsForHIT_nextToken :: Lens.Lens' ListReviewPolicyResultsForHIT (Prelude.Maybe Prelude.Text)
listReviewPolicyResultsForHIT_nextToken = Lens.lens (\ListReviewPolicyResultsForHIT' {nextToken} -> nextToken) (\s@ListReviewPolicyResultsForHIT' {} a -> s {nextToken = a} :: ListReviewPolicyResultsForHIT)

-- | Limit the number of results returned.
listReviewPolicyResultsForHIT_maxResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Prelude.Maybe Prelude.Natural)
listReviewPolicyResultsForHIT_maxResults = Lens.lens (\ListReviewPolicyResultsForHIT' {maxResults} -> maxResults) (\s@ListReviewPolicyResultsForHIT' {} a -> s {maxResults = a} :: ListReviewPolicyResultsForHIT)

-- | The unique identifier of the HIT to retrieve review results for.
listReviewPolicyResultsForHIT_hITId :: Lens.Lens' ListReviewPolicyResultsForHIT Prelude.Text
listReviewPolicyResultsForHIT_hITId = Lens.lens (\ListReviewPolicyResultsForHIT' {hITId} -> hITId) (\s@ListReviewPolicyResultsForHIT' {} a -> s {hITId = a} :: ListReviewPolicyResultsForHIT)

instance
  Core.AWSRequest
    ListReviewPolicyResultsForHIT
  where
  type
    AWSResponse ListReviewPolicyResultsForHIT =
      ListReviewPolicyResultsForHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReviewPolicyResultsForHITResponse'
            Prelude.<$> (x Core..?> "HITReviewPolicy")
            Prelude.<*> (x Core..?> "HITReviewReport")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "AssignmentReviewReport")
            Prelude.<*> (x Core..?> "HITId")
            Prelude.<*> (x Core..?> "AssignmentReviewPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListReviewPolicyResultsForHIT

instance Prelude.NFData ListReviewPolicyResultsForHIT

instance Core.ToHeaders ListReviewPolicyResultsForHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListReviewPolicyResultsForHIT" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListReviewPolicyResultsForHIT where
  toJSON ListReviewPolicyResultsForHIT' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RetrieveResults" Core..=)
              Prelude.<$> retrieveResults,
            ("PolicyLevels" Core..=) Prelude.<$> policyLevels,
            ("RetrieveActions" Core..=)
              Prelude.<$> retrieveActions,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("HITId" Core..= hITId)
          ]
      )

instance Core.ToPath ListReviewPolicyResultsForHIT where
  toPath = Prelude.const "/"

instance Core.ToQuery ListReviewPolicyResultsForHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReviewPolicyResultsForHITResponse' smart constructor.
data ListReviewPolicyResultsForHITResponse = ListReviewPolicyResultsForHITResponse'
  { -- | The name of the HIT-level Review Policy. This contains only the
    -- PolicyName element.
    hITReviewPolicy :: Prelude.Maybe ReviewPolicy,
    -- | Contains both ReviewResult and ReviewAction elements for a particular
    -- HIT.
    hITReviewReport :: Prelude.Maybe ReviewReport,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains both ReviewResult and ReviewAction elements for an Assignment.
    assignmentReviewReport :: Prelude.Maybe ReviewReport,
    -- | The HITId of the HIT for which results have been returned.
    hITId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Assignment-level Review Policy. This contains only the
    -- PolicyName element.
    assignmentReviewPolicy :: Prelude.Maybe ReviewPolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReviewPolicyResultsForHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITReviewPolicy', 'listReviewPolicyResultsForHITResponse_hITReviewPolicy' - The name of the HIT-level Review Policy. This contains only the
-- PolicyName element.
--
-- 'hITReviewReport', 'listReviewPolicyResultsForHITResponse_hITReviewReport' - Contains both ReviewResult and ReviewAction elements for a particular
-- HIT.
--
-- 'nextToken', 'listReviewPolicyResultsForHITResponse_nextToken' - Undocumented member.
--
-- 'assignmentReviewReport', 'listReviewPolicyResultsForHITResponse_assignmentReviewReport' - Contains both ReviewResult and ReviewAction elements for an Assignment.
--
-- 'hITId', 'listReviewPolicyResultsForHITResponse_hITId' - The HITId of the HIT for which results have been returned.
--
-- 'assignmentReviewPolicy', 'listReviewPolicyResultsForHITResponse_assignmentReviewPolicy' - The name of the Assignment-level Review Policy. This contains only the
-- PolicyName element.
--
-- 'httpStatus', 'listReviewPolicyResultsForHITResponse_httpStatus' - The response's http status code.
newListReviewPolicyResultsForHITResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReviewPolicyResultsForHITResponse
newListReviewPolicyResultsForHITResponse pHttpStatus_ =
  ListReviewPolicyResultsForHITResponse'
    { hITReviewPolicy =
        Prelude.Nothing,
      hITReviewReport = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assignmentReviewReport =
        Prelude.Nothing,
      hITId = Prelude.Nothing,
      assignmentReviewPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the HIT-level Review Policy. This contains only the
-- PolicyName element.
listReviewPolicyResultsForHITResponse_hITReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Prelude.Maybe ReviewPolicy)
listReviewPolicyResultsForHITResponse_hITReviewPolicy = Lens.lens (\ListReviewPolicyResultsForHITResponse' {hITReviewPolicy} -> hITReviewPolicy) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {hITReviewPolicy = a} :: ListReviewPolicyResultsForHITResponse)

-- | Contains both ReviewResult and ReviewAction elements for a particular
-- HIT.
listReviewPolicyResultsForHITResponse_hITReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Prelude.Maybe ReviewReport)
listReviewPolicyResultsForHITResponse_hITReviewReport = Lens.lens (\ListReviewPolicyResultsForHITResponse' {hITReviewReport} -> hITReviewReport) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {hITReviewReport = a} :: ListReviewPolicyResultsForHITResponse)

-- | Undocumented member.
listReviewPolicyResultsForHITResponse_nextToken :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Prelude.Maybe Prelude.Text)
listReviewPolicyResultsForHITResponse_nextToken = Lens.lens (\ListReviewPolicyResultsForHITResponse' {nextToken} -> nextToken) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {nextToken = a} :: ListReviewPolicyResultsForHITResponse)

-- | Contains both ReviewResult and ReviewAction elements for an Assignment.
listReviewPolicyResultsForHITResponse_assignmentReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Prelude.Maybe ReviewReport)
listReviewPolicyResultsForHITResponse_assignmentReviewReport = Lens.lens (\ListReviewPolicyResultsForHITResponse' {assignmentReviewReport} -> assignmentReviewReport) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {assignmentReviewReport = a} :: ListReviewPolicyResultsForHITResponse)

-- | The HITId of the HIT for which results have been returned.
listReviewPolicyResultsForHITResponse_hITId :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Prelude.Maybe Prelude.Text)
listReviewPolicyResultsForHITResponse_hITId = Lens.lens (\ListReviewPolicyResultsForHITResponse' {hITId} -> hITId) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {hITId = a} :: ListReviewPolicyResultsForHITResponse)

-- | The name of the Assignment-level Review Policy. This contains only the
-- PolicyName element.
listReviewPolicyResultsForHITResponse_assignmentReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Prelude.Maybe ReviewPolicy)
listReviewPolicyResultsForHITResponse_assignmentReviewPolicy = Lens.lens (\ListReviewPolicyResultsForHITResponse' {assignmentReviewPolicy} -> assignmentReviewPolicy) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {assignmentReviewPolicy = a} :: ListReviewPolicyResultsForHITResponse)

-- | The response's http status code.
listReviewPolicyResultsForHITResponse_httpStatus :: Lens.Lens' ListReviewPolicyResultsForHITResponse Prelude.Int
listReviewPolicyResultsForHITResponse_httpStatus = Lens.lens (\ListReviewPolicyResultsForHITResponse' {httpStatus} -> httpStatus) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {httpStatus = a} :: ListReviewPolicyResultsForHITResponse)

instance
  Prelude.NFData
    ListReviewPolicyResultsForHITResponse
