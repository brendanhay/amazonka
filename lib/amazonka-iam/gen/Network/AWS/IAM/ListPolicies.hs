{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the managed policies that are available in your AWS account, including your own customer-defined managed policies and all AWS managed policies.
--
-- You can filter the list of policies that is returned using the optional @OnlyAttached@ , @Scope@ , and @PathPrefix@ parameters. For example, to list only the customer managed policies in your AWS account, set @Scope@ to @Local@ . To list only AWS managed policies, set @Scope@ to @AWS@ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
-- For more information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListPolicies
  ( -- * Creating a request
    ListPolicies (..),
    mkListPolicies,

    -- ** Request lenses
    lpPathPrefix,
    lpOnlyAttached,
    lpMarker,
    lpScope,
    lpMaxItems,
    lpPolicyUsageFilter,

    -- * Destructuring the response
    ListPoliciesResponse (..),
    mkListPoliciesResponse,

    -- ** Response lenses
    lprsMarker,
    lprsIsTruncated,
    lprsPolicies,
    lprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListPolicies' smart constructor.
data ListPolicies = ListPolicies'
  { -- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    pathPrefix :: Lude.Maybe Lude.Text,
    -- | A flag to filter the results to only the attached policies.
    --
    -- When @OnlyAttached@ is @true@ , the returned list contains only the policies that are attached to an IAM user, group, or role. When @OnlyAttached@ is @false@ , or when the parameter is not included, all policies are returned.
    onlyAttached :: Lude.Maybe Lude.Bool,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | The scope to use for filtering the results.
    --
    -- To list only AWS managed policies, set @Scope@ to @AWS@ . To list only the customer managed policies in your AWS account, set @Scope@ to @Local@ .
    -- This parameter is optional. If it is not included, or if it is set to @All@ , all policies are returned.
    scope :: Lude.Maybe PolicyScopeType,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | The policy usage method to use for filtering the results.
    --
    -- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
    -- This parameter is optional. If it is not included, all policies are returned.
    policyUsageFilter :: Lude.Maybe PolicyUsageType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPolicies' with the minimum fields required to make a request.
--
-- * 'pathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'onlyAttached' - A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@ , the returned list contains only the policies that are attached to an IAM user, group, or role. When @OnlyAttached@ is @false@ , or when the parameter is not included, all policies are returned.
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'scope' - The scope to use for filtering the results.
--
-- To list only AWS managed policies, set @Scope@ to @AWS@ . To list only the customer managed policies in your AWS account, set @Scope@ to @Local@ .
-- This parameter is optional. If it is not included, or if it is set to @All@ , all policies are returned.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'policyUsageFilter' - The policy usage method to use for filtering the results.
--
-- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
-- This parameter is optional. If it is not included, all policies are returned.
mkListPolicies ::
  ListPolicies
mkListPolicies =
  ListPolicies'
    { pathPrefix = Lude.Nothing,
      onlyAttached = Lude.Nothing,
      marker = Lude.Nothing,
      scope = Lude.Nothing,
      maxItems = Lude.Nothing,
      policyUsageFilter = Lude.Nothing
    }

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPathPrefix :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Text)
lpPathPrefix = Lens.lens (pathPrefix :: ListPolicies -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListPolicies)
{-# DEPRECATED lpPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | A flag to filter the results to only the attached policies.
--
-- When @OnlyAttached@ is @true@ , the returned list contains only the policies that are attached to an IAM user, group, or role. When @OnlyAttached@ is @false@ , or when the parameter is not included, all policies are returned.
--
-- /Note:/ Consider using 'onlyAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpOnlyAttached :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Bool)
lpOnlyAttached = Lens.lens (onlyAttached :: ListPolicies -> Lude.Maybe Lude.Bool) (\s a -> s {onlyAttached = a} :: ListPolicies)
{-# DEPRECATED lpOnlyAttached "Use generic-lens or generic-optics with 'onlyAttached' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMarker :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Text)
lpMarker = Lens.lens (marker :: ListPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPolicies)
{-# DEPRECATED lpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The scope to use for filtering the results.
--
-- To list only AWS managed policies, set @Scope@ to @AWS@ . To list only the customer managed policies in your AWS account, set @Scope@ to @Local@ .
-- This parameter is optional. If it is not included, or if it is set to @All@ , all policies are returned.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpScope :: Lens.Lens' ListPolicies (Lude.Maybe PolicyScopeType)
lpScope = Lens.lens (scope :: ListPolicies -> Lude.Maybe PolicyScopeType) (\s a -> s {scope = a} :: ListPolicies)
{-# DEPRECATED lpScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpMaxItems :: Lens.Lens' ListPolicies (Lude.Maybe Lude.Natural)
lpMaxItems = Lens.lens (maxItems :: ListPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListPolicies)
{-# DEPRECATED lpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The policy usage method to use for filtering the results.
--
-- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
-- This parameter is optional. If it is not included, all policies are returned.
--
-- /Note:/ Consider using 'policyUsageFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPolicyUsageFilter :: Lens.Lens' ListPolicies (Lude.Maybe PolicyUsageType)
lpPolicyUsageFilter = Lens.lens (policyUsageFilter :: ListPolicies -> Lude.Maybe PolicyUsageType) (\s a -> s {policyUsageFilter = a} :: ListPolicies)
{-# DEPRECATED lpPolicyUsageFilter "Use generic-lens or generic-optics with 'policyUsageFilter' instead." #-}

instance Page.AWSPager ListPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lpMarker Lens..~ rs Lens.^. lprsMarker

instance Lude.AWSRequest ListPolicies where
  type Rs ListPolicies = ListPoliciesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListPoliciesResult"
      ( \s h x ->
          ListPoliciesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> ( x Lude..@? "Policies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListPolicies where
  toQuery ListPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "OnlyAttached" Lude.=: onlyAttached,
        "Marker" Lude.=: marker,
        "Scope" Lude.=: scope,
        "MaxItems" Lude.=: maxItems,
        "PolicyUsageFilter" Lude.=: policyUsageFilter
      ]

-- | Contains the response to a successful 'ListPolicies' request.
--
-- /See:/ 'mkListPoliciesResponse' smart constructor.
data ListPoliciesResponse = ListPoliciesResponse'
  { -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | A list of policies.
    policies :: Lude.Maybe [Policy],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'policies' - A list of policies.
-- * 'responseStatus' - The response status code.
mkListPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListPoliciesResponse
mkListPoliciesResponse pResponseStatus_ =
  ListPoliciesResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      policies = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsMarker :: Lens.Lens' ListPoliciesResponse (Lude.Maybe Lude.Text)
lprsMarker = Lens.lens (marker :: ListPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsIsTruncated :: Lens.Lens' ListPoliciesResponse (Lude.Maybe Lude.Bool)
lprsIsTruncated = Lens.lens (isTruncated :: ListPoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | A list of policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsPolicies :: Lens.Lens' ListPoliciesResponse (Lude.Maybe [Policy])
lprsPolicies = Lens.lens (policies :: ListPoliciesResponse -> Lude.Maybe [Policy]) (\s a -> s {policies = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprsResponseStatus :: Lens.Lens' ListPoliciesResponse Lude.Int
lprsResponseStatus = Lens.lens (responseStatus :: ListPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListPoliciesResponse)
{-# DEPRECATED lprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
