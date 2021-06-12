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
    listReviewPolicyResultsForHIT_nextToken,
    listReviewPolicyResultsForHIT_maxResults,
    listReviewPolicyResultsForHIT_retrieveResults,
    listReviewPolicyResultsForHIT_retrieveActions,
    listReviewPolicyResultsForHIT_policyLevels,
    listReviewPolicyResultsForHIT_hITId,

    -- * Destructuring the Response
    ListReviewPolicyResultsForHITResponse (..),
    newListReviewPolicyResultsForHITResponse,

    -- * Response Lenses
    listReviewPolicyResultsForHITResponse_nextToken,
    listReviewPolicyResultsForHITResponse_hITId,
    listReviewPolicyResultsForHITResponse_hITReviewPolicy,
    listReviewPolicyResultsForHITResponse_assignmentReviewReport,
    listReviewPolicyResultsForHITResponse_hITReviewReport,
    listReviewPolicyResultsForHITResponse_assignmentReviewPolicy,
    listReviewPolicyResultsForHITResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListReviewPolicyResultsForHIT' smart constructor.
data ListReviewPolicyResultsForHIT = ListReviewPolicyResultsForHIT'
  { -- | Pagination token
    nextToken :: Core.Maybe Core.Text,
    -- | Limit the number of results returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specify if the operation should retrieve a list of the results computed
    -- by the Review Policies.
    retrieveResults :: Core.Maybe Core.Bool,
    -- | Specify if the operation should retrieve a list of the actions taken
    -- executing the Review Policies and their outcomes.
    retrieveActions :: Core.Maybe Core.Bool,
    -- | The Policy Level(s) to retrieve review results for - HIT or Assignment.
    -- If omitted, the default behavior is to retrieve all data for both policy
    -- levels. For a list of all the described policies, see Review Policies.
    policyLevels :: Core.Maybe [ReviewPolicyLevel],
    -- | The unique identifier of the HIT to retrieve review results for.
    hITId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReviewPolicyResultsForHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReviewPolicyResultsForHIT_nextToken' - Pagination token
--
-- 'maxResults', 'listReviewPolicyResultsForHIT_maxResults' - Limit the number of results returned.
--
-- 'retrieveResults', 'listReviewPolicyResultsForHIT_retrieveResults' - Specify if the operation should retrieve a list of the results computed
-- by the Review Policies.
--
-- 'retrieveActions', 'listReviewPolicyResultsForHIT_retrieveActions' - Specify if the operation should retrieve a list of the actions taken
-- executing the Review Policies and their outcomes.
--
-- 'policyLevels', 'listReviewPolicyResultsForHIT_policyLevels' - The Policy Level(s) to retrieve review results for - HIT or Assignment.
-- If omitted, the default behavior is to retrieve all data for both policy
-- levels. For a list of all the described policies, see Review Policies.
--
-- 'hITId', 'listReviewPolicyResultsForHIT_hITId' - The unique identifier of the HIT to retrieve review results for.
newListReviewPolicyResultsForHIT ::
  -- | 'hITId'
  Core.Text ->
  ListReviewPolicyResultsForHIT
newListReviewPolicyResultsForHIT pHITId_ =
  ListReviewPolicyResultsForHIT'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      retrieveResults = Core.Nothing,
      retrieveActions = Core.Nothing,
      policyLevels = Core.Nothing,
      hITId = pHITId_
    }

-- | Pagination token
listReviewPolicyResultsForHIT_nextToken :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Core.Text)
listReviewPolicyResultsForHIT_nextToken = Lens.lens (\ListReviewPolicyResultsForHIT' {nextToken} -> nextToken) (\s@ListReviewPolicyResultsForHIT' {} a -> s {nextToken = a} :: ListReviewPolicyResultsForHIT)

-- | Limit the number of results returned.
listReviewPolicyResultsForHIT_maxResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Core.Natural)
listReviewPolicyResultsForHIT_maxResults = Lens.lens (\ListReviewPolicyResultsForHIT' {maxResults} -> maxResults) (\s@ListReviewPolicyResultsForHIT' {} a -> s {maxResults = a} :: ListReviewPolicyResultsForHIT)

-- | Specify if the operation should retrieve a list of the results computed
-- by the Review Policies.
listReviewPolicyResultsForHIT_retrieveResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Core.Bool)
listReviewPolicyResultsForHIT_retrieveResults = Lens.lens (\ListReviewPolicyResultsForHIT' {retrieveResults} -> retrieveResults) (\s@ListReviewPolicyResultsForHIT' {} a -> s {retrieveResults = a} :: ListReviewPolicyResultsForHIT)

-- | Specify if the operation should retrieve a list of the actions taken
-- executing the Review Policies and their outcomes.
listReviewPolicyResultsForHIT_retrieveActions :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Core.Bool)
listReviewPolicyResultsForHIT_retrieveActions = Lens.lens (\ListReviewPolicyResultsForHIT' {retrieveActions} -> retrieveActions) (\s@ListReviewPolicyResultsForHIT' {} a -> s {retrieveActions = a} :: ListReviewPolicyResultsForHIT)

-- | The Policy Level(s) to retrieve review results for - HIT or Assignment.
-- If omitted, the default behavior is to retrieve all data for both policy
-- levels. For a list of all the described policies, see Review Policies.
listReviewPolicyResultsForHIT_policyLevels :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe [ReviewPolicyLevel])
listReviewPolicyResultsForHIT_policyLevels = Lens.lens (\ListReviewPolicyResultsForHIT' {policyLevels} -> policyLevels) (\s@ListReviewPolicyResultsForHIT' {} a -> s {policyLevels = a} :: ListReviewPolicyResultsForHIT) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier of the HIT to retrieve review results for.
listReviewPolicyResultsForHIT_hITId :: Lens.Lens' ListReviewPolicyResultsForHIT Core.Text
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "HITId")
            Core.<*> (x Core..?> "HITReviewPolicy")
            Core.<*> (x Core..?> "AssignmentReviewReport")
            Core.<*> (x Core..?> "HITReviewReport")
            Core.<*> (x Core..?> "AssignmentReviewPolicy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListReviewPolicyResultsForHIT

instance Core.NFData ListReviewPolicyResultsForHIT

instance Core.ToHeaders ListReviewPolicyResultsForHIT where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.ListReviewPolicyResultsForHIT" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListReviewPolicyResultsForHIT where
  toJSON ListReviewPolicyResultsForHIT' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("RetrieveResults" Core..=) Core.<$> retrieveResults,
            ("RetrieveActions" Core..=) Core.<$> retrieveActions,
            ("PolicyLevels" Core..=) Core.<$> policyLevels,
            Core.Just ("HITId" Core..= hITId)
          ]
      )

instance Core.ToPath ListReviewPolicyResultsForHIT where
  toPath = Core.const "/"

instance Core.ToQuery ListReviewPolicyResultsForHIT where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListReviewPolicyResultsForHITResponse' smart constructor.
data ListReviewPolicyResultsForHITResponse = ListReviewPolicyResultsForHITResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The HITId of the HIT for which results have been returned.
    hITId :: Core.Maybe Core.Text,
    -- | The name of the HIT-level Review Policy. This contains only the
    -- PolicyName element.
    hITReviewPolicy :: Core.Maybe ReviewPolicy,
    -- | Contains both ReviewResult and ReviewAction elements for an Assignment.
    assignmentReviewReport :: Core.Maybe ReviewReport,
    -- | Contains both ReviewResult and ReviewAction elements for a particular
    -- HIT.
    hITReviewReport :: Core.Maybe ReviewReport,
    -- | The name of the Assignment-level Review Policy. This contains only the
    -- PolicyName element.
    assignmentReviewPolicy :: Core.Maybe ReviewPolicy,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListReviewPolicyResultsForHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReviewPolicyResultsForHITResponse_nextToken' - Undocumented member.
--
-- 'hITId', 'listReviewPolicyResultsForHITResponse_hITId' - The HITId of the HIT for which results have been returned.
--
-- 'hITReviewPolicy', 'listReviewPolicyResultsForHITResponse_hITReviewPolicy' - The name of the HIT-level Review Policy. This contains only the
-- PolicyName element.
--
-- 'assignmentReviewReport', 'listReviewPolicyResultsForHITResponse_assignmentReviewReport' - Contains both ReviewResult and ReviewAction elements for an Assignment.
--
-- 'hITReviewReport', 'listReviewPolicyResultsForHITResponse_hITReviewReport' - Contains both ReviewResult and ReviewAction elements for a particular
-- HIT.
--
-- 'assignmentReviewPolicy', 'listReviewPolicyResultsForHITResponse_assignmentReviewPolicy' - The name of the Assignment-level Review Policy. This contains only the
-- PolicyName element.
--
-- 'httpStatus', 'listReviewPolicyResultsForHITResponse_httpStatus' - The response's http status code.
newListReviewPolicyResultsForHITResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListReviewPolicyResultsForHITResponse
newListReviewPolicyResultsForHITResponse pHttpStatus_ =
  ListReviewPolicyResultsForHITResponse'
    { nextToken =
        Core.Nothing,
      hITId = Core.Nothing,
      hITReviewPolicy = Core.Nothing,
      assignmentReviewReport =
        Core.Nothing,
      hITReviewReport = Core.Nothing,
      assignmentReviewPolicy =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listReviewPolicyResultsForHITResponse_nextToken :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Core.Text)
listReviewPolicyResultsForHITResponse_nextToken = Lens.lens (\ListReviewPolicyResultsForHITResponse' {nextToken} -> nextToken) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {nextToken = a} :: ListReviewPolicyResultsForHITResponse)

-- | The HITId of the HIT for which results have been returned.
listReviewPolicyResultsForHITResponse_hITId :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Core.Text)
listReviewPolicyResultsForHITResponse_hITId = Lens.lens (\ListReviewPolicyResultsForHITResponse' {hITId} -> hITId) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {hITId = a} :: ListReviewPolicyResultsForHITResponse)

-- | The name of the HIT-level Review Policy. This contains only the
-- PolicyName element.
listReviewPolicyResultsForHITResponse_hITReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe ReviewPolicy)
listReviewPolicyResultsForHITResponse_hITReviewPolicy = Lens.lens (\ListReviewPolicyResultsForHITResponse' {hITReviewPolicy} -> hITReviewPolicy) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {hITReviewPolicy = a} :: ListReviewPolicyResultsForHITResponse)

-- | Contains both ReviewResult and ReviewAction elements for an Assignment.
listReviewPolicyResultsForHITResponse_assignmentReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe ReviewReport)
listReviewPolicyResultsForHITResponse_assignmentReviewReport = Lens.lens (\ListReviewPolicyResultsForHITResponse' {assignmentReviewReport} -> assignmentReviewReport) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {assignmentReviewReport = a} :: ListReviewPolicyResultsForHITResponse)

-- | Contains both ReviewResult and ReviewAction elements for a particular
-- HIT.
listReviewPolicyResultsForHITResponse_hITReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe ReviewReport)
listReviewPolicyResultsForHITResponse_hITReviewReport = Lens.lens (\ListReviewPolicyResultsForHITResponse' {hITReviewReport} -> hITReviewReport) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {hITReviewReport = a} :: ListReviewPolicyResultsForHITResponse)

-- | The name of the Assignment-level Review Policy. This contains only the
-- PolicyName element.
listReviewPolicyResultsForHITResponse_assignmentReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe ReviewPolicy)
listReviewPolicyResultsForHITResponse_assignmentReviewPolicy = Lens.lens (\ListReviewPolicyResultsForHITResponse' {assignmentReviewPolicy} -> assignmentReviewPolicy) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {assignmentReviewPolicy = a} :: ListReviewPolicyResultsForHITResponse)

-- | The response's http status code.
listReviewPolicyResultsForHITResponse_httpStatus :: Lens.Lens' ListReviewPolicyResultsForHITResponse Core.Int
listReviewPolicyResultsForHITResponse_httpStatus = Lens.lens (\ListReviewPolicyResultsForHITResponse' {httpStatus} -> httpStatus) (\s@ListReviewPolicyResultsForHITResponse' {} a -> s {httpStatus = a} :: ListReviewPolicyResultsForHITResponse)

instance
  Core.NFData
    ListReviewPolicyResultsForHITResponse
