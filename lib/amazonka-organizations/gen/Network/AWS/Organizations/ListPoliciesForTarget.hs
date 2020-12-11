{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.ListPoliciesForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies that are directly attached to the specified target root, organizational unit (OU), or account. You must specify the policy type that you want included in the returned list.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
--
-- This operation returns paginated results.
module Network.AWS.Organizations.ListPoliciesForTarget
  ( -- * Creating a request
    ListPoliciesForTarget (..),
    mkListPoliciesForTarget,

    -- ** Request lenses
    lpftNextToken,
    lpftMaxResults,
    lpftTargetId,
    lpftFilter,

    -- * Destructuring the response
    ListPoliciesForTargetResponse (..),
    mkListPoliciesForTargetResponse,

    -- ** Response lenses
    lpftrsNextToken,
    lpftrsPolicies,
    lpftrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPoliciesForTarget' smart constructor.
data ListPoliciesForTarget = ListPoliciesForTarget'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    targetId :: Lude.Text,
    filter :: PolicyType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesForTarget' with the minimum fields required to make a request.
--
-- * 'filter' - The type of policy that you want to include in the returned list. You must specify one of the following values:
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
-- * 'nextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
-- * 'targetId' - The unique identifier (ID) of the root, organizational unit, or account whose policies you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
mkListPoliciesForTarget ::
  -- | 'targetId'
  Lude.Text ->
  -- | 'filter'
  PolicyType ->
  ListPoliciesForTarget
mkListPoliciesForTarget pTargetId_ pFilter_ =
  ListPoliciesForTarget'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      targetId = pTargetId_,
      filter = pFilter_
    }

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value of the previous call's @NextToken@ response to indicate where the output should continue from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftNextToken :: Lens.Lens' ListPoliciesForTarget (Lude.Maybe Lude.Text)
lpftNextToken = Lens.lens (nextToken :: ListPoliciesForTarget -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPoliciesForTarget)
{-# DEPRECATED lpftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that Organizations might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftMaxResults :: Lens.Lens' ListPoliciesForTarget (Lude.Maybe Lude.Natural)
lpftMaxResults = Lens.lens (maxResults :: ListPoliciesForTarget -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListPoliciesForTarget)
{-# DEPRECATED lpftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier (ID) of the root, organizational unit, or account whose policies you want to list.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:
--
--     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.
--
--
--     * __Account__ - A string that consists of exactly 12 digits.
--
--
--     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
--
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftTargetId :: Lens.Lens' ListPoliciesForTarget Lude.Text
lpftTargetId = Lens.lens (targetId :: ListPoliciesForTarget -> Lude.Text) (\s a -> s {targetId = a} :: ListPoliciesForTarget)
{-# DEPRECATED lpftTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The type of policy that you want to include in the returned list. You must specify one of the following values:
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
lpftFilter :: Lens.Lens' ListPoliciesForTarget PolicyType
lpftFilter = Lens.lens (filter :: ListPoliciesForTarget -> PolicyType) (\s a -> s {filter = a} :: ListPoliciesForTarget)
{-# DEPRECATED lpftFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Page.AWSPager ListPoliciesForTarget where
  page rq rs
    | Page.stop (rs Lens.^. lpftrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lpftrsPolicies) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lpftNextToken Lens..~ rs Lens.^. lpftrsNextToken

instance Lude.AWSRequest ListPoliciesForTarget where
  type Rs ListPoliciesForTarget = ListPoliciesForTargetResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListPoliciesForTargetResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Policies" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPoliciesForTarget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.ListPoliciesForTarget" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListPoliciesForTarget where
  toJSON ListPoliciesForTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("TargetId" Lude..= targetId),
            Lude.Just ("Filter" Lude..= filter)
          ]
      )

instance Lude.ToPath ListPoliciesForTarget where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPoliciesForTarget where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListPoliciesForTargetResponse' smart constructor.
data ListPoliciesForTargetResponse = ListPoliciesForTargetResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    policies ::
      Lude.Maybe [PolicySummary],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesForTargetResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
-- * 'policies' - The list of policies that match the criteria in the request.
-- * 'responseStatus' - The response status code.
mkListPoliciesForTargetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPoliciesForTargetResponse
mkListPoliciesForTargetResponse pResponseStatus_ =
  ListPoliciesForTargetResponse'
    { nextToken = Lude.Nothing,
      policies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftrsNextToken :: Lens.Lens' ListPoliciesForTargetResponse (Lude.Maybe Lude.Text)
lpftrsNextToken = Lens.lens (nextToken :: ListPoliciesForTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListPoliciesForTargetResponse)
{-# DEPRECATED lpftrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of policies that match the criteria in the request.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftrsPolicies :: Lens.Lens' ListPoliciesForTargetResponse (Lude.Maybe [PolicySummary])
lpftrsPolicies = Lens.lens (policies :: ListPoliciesForTargetResponse -> Lude.Maybe [PolicySummary]) (\s a -> s {policies = a} :: ListPoliciesForTargetResponse)
{-# DEPRECATED lpftrsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpftrsResponseStatus :: Lens.Lens' ListPoliciesForTargetResponse Lude.Int
lpftrsResponseStatus = Lens.lens (responseStatus :: ListPoliciesForTargetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPoliciesForTargetResponse)
{-# DEPRECATED lpftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
