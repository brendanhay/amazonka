{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewPolicyResultsForHIT@ operation retrieves the computed results and the actions taken in the course of executing your Review Policies for a given HIT. For information about how to specify Review Policies when you call CreateHIT, see Review Policies. The ListReviewPolicyResultsForHIT operation can return results for both Assignment-level and HIT-level review results.
module Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
  ( -- * Creating a request
    ListReviewPolicyResultsForHIT (..),
    mkListReviewPolicyResultsForHIT,

    -- ** Request lenses
    lrprfhitRetrieveResults,
    lrprfhitPolicyLevels,
    lrprfhitRetrieveActions,
    lrprfhitNextToken,
    lrprfhitHITId,
    lrprfhitMaxResults,

    -- * Destructuring the response
    ListReviewPolicyResultsForHITResponse (..),
    mkListReviewPolicyResultsForHITResponse,

    -- ** Response lenses
    lrprfhitrsHITReviewPolicy,
    lrprfhitrsHITReviewReport,
    lrprfhitrsNextToken,
    lrprfhitrsAssignmentReviewReport,
    lrprfhitrsHITId,
    lrprfhitrsAssignmentReviewPolicy,
    lrprfhitrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListReviewPolicyResultsForHIT' smart constructor.
data ListReviewPolicyResultsForHIT = ListReviewPolicyResultsForHIT'
  { -- | Specify if the operation should retrieve a list of the results computed by the Review Policies.
    retrieveResults :: Lude.Maybe Lude.Bool,
    -- | The Policy Level(s) to retrieve review results for - HIT or Assignment. If omitted, the default behavior is to retrieve all data for both policy levels. For a list of all the described policies, see Review Policies.
    policyLevels :: Lude.Maybe [ReviewPolicyLevel],
    -- | Specify if the operation should retrieve a list of the actions taken executing the Review Policies and their outcomes.
    retrieveActions :: Lude.Maybe Lude.Bool,
    -- | Pagination token
    nextToken :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the HIT to retrieve review results for.
    hITId :: Lude.Text,
    -- | Limit the number of results returned.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReviewPolicyResultsForHIT' with the minimum fields required to make a request.
--
-- * 'retrieveResults' - Specify if the operation should retrieve a list of the results computed by the Review Policies.
-- * 'policyLevels' - The Policy Level(s) to retrieve review results for - HIT or Assignment. If omitted, the default behavior is to retrieve all data for both policy levels. For a list of all the described policies, see Review Policies.
-- * 'retrieveActions' - Specify if the operation should retrieve a list of the actions taken executing the Review Policies and their outcomes.
-- * 'nextToken' - Pagination token
-- * 'hITId' - The unique identifier of the HIT to retrieve review results for.
-- * 'maxResults' - Limit the number of results returned.
mkListReviewPolicyResultsForHIT ::
  -- | 'hITId'
  Lude.Text ->
  ListReviewPolicyResultsForHIT
mkListReviewPolicyResultsForHIT pHITId_ =
  ListReviewPolicyResultsForHIT'
    { retrieveResults = Lude.Nothing,
      policyLevels = Lude.Nothing,
      retrieveActions = Lude.Nothing,
      nextToken = Lude.Nothing,
      hITId = pHITId_,
      maxResults = Lude.Nothing
    }

-- | Specify if the operation should retrieve a list of the results computed by the Review Policies.
--
-- /Note:/ Consider using 'retrieveResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitRetrieveResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Lude.Maybe Lude.Bool)
lrprfhitRetrieveResults = Lens.lens (retrieveResults :: ListReviewPolicyResultsForHIT -> Lude.Maybe Lude.Bool) (\s a -> s {retrieveResults = a} :: ListReviewPolicyResultsForHIT)
{-# DEPRECATED lrprfhitRetrieveResults "Use generic-lens or generic-optics with 'retrieveResults' instead." #-}

-- | The Policy Level(s) to retrieve review results for - HIT or Assignment. If omitted, the default behavior is to retrieve all data for both policy levels. For a list of all the described policies, see Review Policies.
--
-- /Note:/ Consider using 'policyLevels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitPolicyLevels :: Lens.Lens' ListReviewPolicyResultsForHIT (Lude.Maybe [ReviewPolicyLevel])
lrprfhitPolicyLevels = Lens.lens (policyLevels :: ListReviewPolicyResultsForHIT -> Lude.Maybe [ReviewPolicyLevel]) (\s a -> s {policyLevels = a} :: ListReviewPolicyResultsForHIT)
{-# DEPRECATED lrprfhitPolicyLevels "Use generic-lens or generic-optics with 'policyLevels' instead." #-}

-- | Specify if the operation should retrieve a list of the actions taken executing the Review Policies and their outcomes.
--
-- /Note:/ Consider using 'retrieveActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitRetrieveActions :: Lens.Lens' ListReviewPolicyResultsForHIT (Lude.Maybe Lude.Bool)
lrprfhitRetrieveActions = Lens.lens (retrieveActions :: ListReviewPolicyResultsForHIT -> Lude.Maybe Lude.Bool) (\s a -> s {retrieveActions = a} :: ListReviewPolicyResultsForHIT)
{-# DEPRECATED lrprfhitRetrieveActions "Use generic-lens or generic-optics with 'retrieveActions' instead." #-}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitNextToken :: Lens.Lens' ListReviewPolicyResultsForHIT (Lude.Maybe Lude.Text)
lrprfhitNextToken = Lens.lens (nextToken :: ListReviewPolicyResultsForHIT -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReviewPolicyResultsForHIT)
{-# DEPRECATED lrprfhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique identifier of the HIT to retrieve review results for.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitHITId :: Lens.Lens' ListReviewPolicyResultsForHIT Lude.Text
lrprfhitHITId = Lens.lens (hITId :: ListReviewPolicyResultsForHIT -> Lude.Text) (\s a -> s {hITId = a} :: ListReviewPolicyResultsForHIT)
{-# DEPRECATED lrprfhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | Limit the number of results returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitMaxResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Lude.Maybe Lude.Natural)
lrprfhitMaxResults = Lens.lens (maxResults :: ListReviewPolicyResultsForHIT -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListReviewPolicyResultsForHIT)
{-# DEPRECATED lrprfhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListReviewPolicyResultsForHIT where
  type
    Rs ListReviewPolicyResultsForHIT =
      ListReviewPolicyResultsForHITResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListReviewPolicyResultsForHITResponse'
            Lude.<$> (x Lude..?> "HITReviewPolicy")
            Lude.<*> (x Lude..?> "HITReviewReport")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "AssignmentReviewReport")
            Lude.<*> (x Lude..?> "HITId")
            Lude.<*> (x Lude..?> "AssignmentReviewPolicy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReviewPolicyResultsForHIT where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.ListReviewPolicyResultsForHIT" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListReviewPolicyResultsForHIT where
  toJSON ListReviewPolicyResultsForHIT' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RetrieveResults" Lude..=) Lude.<$> retrieveResults,
            ("PolicyLevels" Lude..=) Lude.<$> policyLevels,
            ("RetrieveActions" Lude..=) Lude.<$> retrieveActions,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("HITId" Lude..= hITId),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListReviewPolicyResultsForHIT where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReviewPolicyResultsForHIT where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListReviewPolicyResultsForHITResponse' smart constructor.
data ListReviewPolicyResultsForHITResponse = ListReviewPolicyResultsForHITResponse'
  { -- | The name of the HIT-level Review Policy. This contains only the PolicyName element.
    hITReviewPolicy :: Lude.Maybe ReviewPolicy,
    -- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
    hITReviewReport :: Lude.Maybe ReviewReport,
    nextToken :: Lude.Maybe Lude.Text,
    -- | Contains both ReviewResult and ReviewAction elements for an Assignment.
    assignmentReviewReport :: Lude.Maybe ReviewReport,
    -- | The HITId of the HIT for which results have been returned.
    hITId :: Lude.Maybe Lude.Text,
    -- | The name of the Assignment-level Review Policy. This contains only the PolicyName element.
    assignmentReviewPolicy :: Lude.Maybe ReviewPolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReviewPolicyResultsForHITResponse' with the minimum fields required to make a request.
--
-- * 'hITReviewPolicy' - The name of the HIT-level Review Policy. This contains only the PolicyName element.
-- * 'hITReviewReport' - Contains both ReviewResult and ReviewAction elements for a particular HIT.
-- * 'nextToken' -
-- * 'assignmentReviewReport' - Contains both ReviewResult and ReviewAction elements for an Assignment.
-- * 'hITId' - The HITId of the HIT for which results have been returned.
-- * 'assignmentReviewPolicy' - The name of the Assignment-level Review Policy. This contains only the PolicyName element.
-- * 'responseStatus' - The response status code.
mkListReviewPolicyResultsForHITResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReviewPolicyResultsForHITResponse
mkListReviewPolicyResultsForHITResponse pResponseStatus_ =
  ListReviewPolicyResultsForHITResponse'
    { hITReviewPolicy =
        Lude.Nothing,
      hITReviewReport = Lude.Nothing,
      nextToken = Lude.Nothing,
      assignmentReviewReport = Lude.Nothing,
      hITId = Lude.Nothing,
      assignmentReviewPolicy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the HIT-level Review Policy. This contains only the PolicyName element.
--
-- /Note:/ Consider using 'hITReviewPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrsHITReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Lude.Maybe ReviewPolicy)
lrprfhitrsHITReviewPolicy = Lens.lens (hITReviewPolicy :: ListReviewPolicyResultsForHITResponse -> Lude.Maybe ReviewPolicy) (\s a -> s {hITReviewPolicy = a} :: ListReviewPolicyResultsForHITResponse)
{-# DEPRECATED lrprfhitrsHITReviewPolicy "Use generic-lens or generic-optics with 'hITReviewPolicy' instead." #-}

-- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
--
-- /Note:/ Consider using 'hITReviewReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrsHITReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Lude.Maybe ReviewReport)
lrprfhitrsHITReviewReport = Lens.lens (hITReviewReport :: ListReviewPolicyResultsForHITResponse -> Lude.Maybe ReviewReport) (\s a -> s {hITReviewReport = a} :: ListReviewPolicyResultsForHITResponse)
{-# DEPRECATED lrprfhitrsHITReviewReport "Use generic-lens or generic-optics with 'hITReviewReport' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrsNextToken :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Lude.Maybe Lude.Text)
lrprfhitrsNextToken = Lens.lens (nextToken :: ListReviewPolicyResultsForHITResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReviewPolicyResultsForHITResponse)
{-# DEPRECATED lrprfhitrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Contains both ReviewResult and ReviewAction elements for an Assignment.
--
-- /Note:/ Consider using 'assignmentReviewReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrsAssignmentReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Lude.Maybe ReviewReport)
lrprfhitrsAssignmentReviewReport = Lens.lens (assignmentReviewReport :: ListReviewPolicyResultsForHITResponse -> Lude.Maybe ReviewReport) (\s a -> s {assignmentReviewReport = a} :: ListReviewPolicyResultsForHITResponse)
{-# DEPRECATED lrprfhitrsAssignmentReviewReport "Use generic-lens or generic-optics with 'assignmentReviewReport' instead." #-}

-- | The HITId of the HIT for which results have been returned.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrsHITId :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Lude.Maybe Lude.Text)
lrprfhitrsHITId = Lens.lens (hITId :: ListReviewPolicyResultsForHITResponse -> Lude.Maybe Lude.Text) (\s a -> s {hITId = a} :: ListReviewPolicyResultsForHITResponse)
{-# DEPRECATED lrprfhitrsHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The name of the Assignment-level Review Policy. This contains only the PolicyName element.
--
-- /Note:/ Consider using 'assignmentReviewPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrsAssignmentReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Lude.Maybe ReviewPolicy)
lrprfhitrsAssignmentReviewPolicy = Lens.lens (assignmentReviewPolicy :: ListReviewPolicyResultsForHITResponse -> Lude.Maybe ReviewPolicy) (\s a -> s {assignmentReviewPolicy = a} :: ListReviewPolicyResultsForHITResponse)
{-# DEPRECATED lrprfhitrsAssignmentReviewPolicy "Use generic-lens or generic-optics with 'assignmentReviewPolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrsResponseStatus :: Lens.Lens' ListReviewPolicyResultsForHITResponse Lude.Int
lrprfhitrsResponseStatus = Lens.lens (responseStatus :: ListReviewPolicyResultsForHITResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReviewPolicyResultsForHITResponse)
{-# DEPRECATED lrprfhitrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
