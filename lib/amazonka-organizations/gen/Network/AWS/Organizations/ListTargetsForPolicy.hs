{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListTargetsForPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the roots, organizational units (OUs), and accounts that the specified policy is attached to.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListTargetsForPolicy
  ( -- * Creating a request
    ListTargetsForPolicy (..),
    mkListTargetsForPolicy,

    -- ** Request lenses
    ltfpPolicyId,
    ltfpNextToken,
    ltfpMaxResults,

    -- * Destructuring the response
    ListTargetsForPolicyResponse (..),
    mkListTargetsForPolicyResponse,

    -- ** Response lenses
    ltfprsNextToken,
    ltfprsTargets,
    ltfprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { -- | The unique identifier (ID) of the policy whose attachments you want to know.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
    policyId :: Lude.Text,
    -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTargetsForPolicy' with the minimum fields required to make a request.
--
-- * 'policyId' - The unique identifier (ID) of the policy whose attachments you want to know.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
mkListTargetsForPolicy ::
  -- | 'policyId'
  Lude.Text ->
  ListTargetsForPolicy
mkListTargetsForPolicy pPolicyId_ =
  ListTargetsForPolicy'
    { policyId = pPolicyId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The unique identifier (ID) of the policy whose attachments you want to know.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpPolicyId :: Lens.Lens' ListTargetsForPolicy Lude.Text
ltfpPolicyId = Lens.lens (policyId :: ListTargetsForPolicy -> Lude.Text) (\s a -> s {policyId = a} :: ListTargetsForPolicy)
{-# DEPRECATED ltfpPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpNextToken :: Lens.Lens' ListTargetsForPolicy (Lude.Maybe Lude.Text)
ltfpNextToken = Lens.lens (nextToken :: ListTargetsForPolicy -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTargetsForPolicy)
{-# DEPRECATED ltfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfpMaxResults :: Lens.Lens' ListTargetsForPolicy (Lude.Maybe Lude.Natural)
ltfpMaxResults = Lens.lens (maxResults :: ListTargetsForPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTargetsForPolicy)
{-# DEPRECATED ltfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTargetsForPolicy where
  page rq rs
    | Page.stop (rs Lens.^. ltfprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltfprsTargets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltfpNextToken Lens..~ rs Lens.^. ltfprsNextToken

instance Lude.AWSRequest ListTargetsForPolicy where
  type Rs ListTargetsForPolicy = ListTargetsForPolicyResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTargetsForPolicyResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Targets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTargetsForPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListTargetsForPolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTargetsForPolicy where
  toJSON ListTargetsForPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyId" Lude..= policyId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTargetsForPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTargetsForPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of structures, each of which contains details about one of the entities to which the specified policy is attached.
    targets :: Lude.Maybe [PolicyTargetSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTargetsForPolicyResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'targets' - A list of structures, each of which contains details about one of the entities to which the specified policy is attached.
-- * 'responseStatus' - The response status code.
mkListTargetsForPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTargetsForPolicyResponse
mkListTargetsForPolicyResponse pResponseStatus_ =
  ListTargetsForPolicyResponse'
    { nextToken = Lude.Nothing,
      targets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsNextToken :: Lens.Lens' ListTargetsForPolicyResponse (Lude.Maybe Lude.Text)
ltfprsNextToken = Lens.lens (nextToken :: ListTargetsForPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTargetsForPolicyResponse)
{-# DEPRECATED ltfprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of structures, each of which contains details about one of the entities to which the specified policy is attached.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsTargets :: Lens.Lens' ListTargetsForPolicyResponse (Lude.Maybe [PolicyTargetSummary])
ltfprsTargets = Lens.lens (targets :: ListTargetsForPolicyResponse -> Lude.Maybe [PolicyTargetSummary]) (\s a -> s {targets = a} :: ListTargetsForPolicyResponse)
{-# DEPRECATED ltfprsTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfprsResponseStatus :: Lens.Lens' ListTargetsForPolicyResponse Lude.Int
ltfprsResponseStatus = Lens.lens (responseStatus :: ListTargetsForPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTargetsForPolicyResponse)
{-# DEPRECATED ltfprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
