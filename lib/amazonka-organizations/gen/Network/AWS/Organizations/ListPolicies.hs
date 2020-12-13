{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the list of all policies in an organization of a specified type.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListPolicies
  ( -- * Creating a request
    ListPolicies (..),
    mkListPolicies,

    -- ** Request lenses
    lNextToken,
    lFilter,
    lMaxResults,

    -- * Destructuring the response
    ListPoliciesResponse (..),
    mkListPoliciesResponse,

    -- ** Response lenses
    lprsNextToken,
    lprsPolicies,
    lprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Specifies the type of policy that you want to include in the response. You must specify one of the following values:
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    filter :: PolicyType,
    -- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'filter' - Specifies the type of policy that you want to include in the response. You must specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
-- * 'maxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
mkListPolicies ::
  -- | 'filter'
  PolicyType ->
  ListPolicies
mkListPolicies pFilter_ =
  ListPolicies'
    { nextToken = Lude.Nothing,
      filter = pFilter_,
      maxResults = Lude.Nothing
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListPolicies -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPolicies)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the type of policy that you want to include in the response. You must specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lFilter :: Lens.Lens' ListPolicies PolicyType
lFilter = Lens.lens (filter :: ListPolicies -> PolicyType) (\s a -> s {filter = a} :: ListPolicies)
{-# DEPRECATED lFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPolicies)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lprsPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lprsNextToken

instance Lude.AWSRequest ListPolicies where
  type Rs ListPolicies = ListPoliciesResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPoliciesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Policies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPolicies where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.ListPolicies" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPolicies where
  toJSON ListPolicies' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("Filter" Lude..= filter),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPolicies where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of policies that match the filter criteria in the request. The output list doesn't include the policy contents. To see the content for a policy, see 'DescribePolicy' .
    policies :: Lude.Maybe [PolicySummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'policies' - A list of policies that match the filter criteria in the request. The output list doesn't include the policy contents. To see the content for a policy, see 'DescribePolicy' .
-- * 'responseStatus' - The response status code.
mkListPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPoliciesResponse
mkListPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { nextToken = Lude.Nothing,
      policies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsNextToken :: Lens.Lens' ListPoliciesResponse (Lude.Maybe Lude.Text)
lprsNextToken = Lens.lens (nextToken :: ListPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of policies that match the filter criteria in the request. The output list doesn't include the policy contents. To see the content for a policy, see 'DescribePolicy' .
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPolicies :: Lens.Lens' ListPoliciesResponse (Lude.Maybe [PolicySummary])
lprsPolicies = Lens.lens (policies :: ListPoliciesResponse -> Lude.Maybe [PolicySummary]) (\s a -> s {policies = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPoliciesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
