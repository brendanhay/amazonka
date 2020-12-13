{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListEntitiesForPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all IAM users, groups, and roles that the specified managed policy is attached to.
--
-- You can use the optional @EntityFilter@ parameter to limit the results to a particular type of entity (users, groups, or roles). For example, to list only the roles that are attached to the specified policy, set @EntityFilter@ to @Role@ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListEntitiesForPolicy
  ( -- * Creating a request
    ListEntitiesForPolicy (..),
    mkListEntitiesForPolicy,

    -- ** Request lenses
    lefpPathPrefix,
    lefpEntityFilter,
    lefpMarker,
    lefpMaxItems,
    lefpPolicyUsageFilter,
    lefpPolicyARN,

    -- * Destructuring the response
    ListEntitiesForPolicyResponse (..),
    mkListEntitiesForPolicyResponse,

    -- ** Response lenses
    lefprsPolicyGroups,
    lefprsPolicyRoles,
    lefprsMarker,
    lefprsPolicyUsers,
    lefprsIsTruncated,
    lefprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListEntitiesForPolicy' smart constructor.
data ListEntitiesForPolicy = ListEntitiesForPolicy'
  { -- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    pathPrefix :: Lude.Maybe Lude.Text,
    -- | The entity type to use for filtering the results.
    --
    -- For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
    entityFilter :: Lude.Maybe EntityType,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | The policy usage method to use for filtering the results.
    --
    -- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
    -- This parameter is optional. If it is not included, all policies are returned.
    policyUsageFilter :: Lude.Maybe PolicyUsageType,
    -- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
    --
    -- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
    policyARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEntitiesForPolicy' with the minimum fields required to make a request.
--
-- * 'pathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'entityFilter' - The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'policyUsageFilter' - The policy usage method to use for filtering the results.
--
-- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
-- This parameter is optional. If it is not included, all policies are returned.
-- * 'policyARN' - The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
mkListEntitiesForPolicy ::
  -- | 'policyARN'
  Lude.Text ->
  ListEntitiesForPolicy
mkListEntitiesForPolicy pPolicyARN_ =
  ListEntitiesForPolicy'
    { pathPrefix = Lude.Nothing,
      entityFilter = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      policyUsageFilter = Lude.Nothing,
      policyARN = pPolicyARN_
    }

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all entities.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpPathPrefix :: Lens.Lens' ListEntitiesForPolicy (Lude.Maybe Lude.Text)
lefpPathPrefix = Lens.lens (pathPrefix :: ListEntitiesForPolicy -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListEntitiesForPolicy)
{-# DEPRECATED lefpPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | The entity type to use for filtering the results.
--
-- For example, when @EntityFilter@ is @Role@ , only the roles that are attached to the specified policy are returned. This parameter is optional. If it is not included, all attached entities (users, groups, and roles) are returned. The argument for this parameter must be one of the valid values listed below.
--
-- /Note:/ Consider using 'entityFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpEntityFilter :: Lens.Lens' ListEntitiesForPolicy (Lude.Maybe EntityType)
lefpEntityFilter = Lens.lens (entityFilter :: ListEntitiesForPolicy -> Lude.Maybe EntityType) (\s a -> s {entityFilter = a} :: ListEntitiesForPolicy)
{-# DEPRECATED lefpEntityFilter "Use generic-lens or generic-optics with 'entityFilter' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpMarker :: Lens.Lens' ListEntitiesForPolicy (Lude.Maybe Lude.Text)
lefpMarker = Lens.lens (marker :: ListEntitiesForPolicy -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListEntitiesForPolicy)
{-# DEPRECATED lefpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpMaxItems :: Lens.Lens' ListEntitiesForPolicy (Lude.Maybe Lude.Natural)
lefpMaxItems = Lens.lens (maxItems :: ListEntitiesForPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListEntitiesForPolicy)
{-# DEPRECATED lefpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The policy usage method to use for filtering the results.
--
-- To list only permissions policies, set @PolicyUsageFilter@ to @PermissionsPolicy@ . To list only the policies used to set permissions boundaries, set the value to @PermissionsBoundary@ .
-- This parameter is optional. If it is not included, all policies are returned.
--
-- /Note:/ Consider using 'policyUsageFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpPolicyUsageFilter :: Lens.Lens' ListEntitiesForPolicy (Lude.Maybe PolicyUsageType)
lefpPolicyUsageFilter = Lens.lens (policyUsageFilter :: ListEntitiesForPolicy -> Lude.Maybe PolicyUsageType) (\s a -> s {policyUsageFilter = a} :: ListEntitiesForPolicy)
{-# DEPRECATED lefpPolicyUsageFilter "Use generic-lens or generic-optics with 'policyUsageFilter' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM policy for which you want the versions.
--
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefpPolicyARN :: Lens.Lens' ListEntitiesForPolicy Lude.Text
lefpPolicyARN = Lens.lens (policyARN :: ListEntitiesForPolicy -> Lude.Text) (\s a -> s {policyARN = a} :: ListEntitiesForPolicy)
{-# DEPRECATED lefpPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

instance Page.AWSPager ListEntitiesForPolicy where
  page rq rs
    | Page.stop (rs Lens.^. lefprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lefprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lefpMarker Lens..~ rs Lens.^. lefprsMarker

instance Lude.AWSRequest ListEntitiesForPolicy where
  type Rs ListEntitiesForPolicy = ListEntitiesForPolicyResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListEntitiesForPolicyResult"
      ( \s h x ->
          ListEntitiesForPolicyResponse'
            Lude.<$> ( x Lude..@? "PolicyGroups" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> ( x Lude..@? "PolicyRoles" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> ( x Lude..@? "PolicyUsers" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListEntitiesForPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListEntitiesForPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery ListEntitiesForPolicy where
  toQuery ListEntitiesForPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListEntitiesForPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "EntityFilter" Lude.=: entityFilter,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "PolicyUsageFilter" Lude.=: policyUsageFilter,
        "PolicyArn" Lude.=: policyARN
      ]

-- | Contains the response to a successful 'ListEntitiesForPolicy' request.
--
-- /See:/ 'mkListEntitiesForPolicyResponse' smart constructor.
data ListEntitiesForPolicyResponse = ListEntitiesForPolicyResponse'
  { -- | A list of IAM groups that the policy is attached to.
    policyGroups :: Lude.Maybe [PolicyGroup],
    -- | A list of IAM roles that the policy is attached to.
    policyRoles :: Lude.Maybe [PolicyRole],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A list of IAM users that the policy is attached to.
    policyUsers :: Lude.Maybe [PolicyUser],
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListEntitiesForPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyGroups' - A list of IAM groups that the policy is attached to.
-- * 'policyRoles' - A list of IAM roles that the policy is attached to.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'policyUsers' - A list of IAM users that the policy is attached to.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListEntitiesForPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListEntitiesForPolicyResponse
mkListEntitiesForPolicyResponse pResponseStatus_ =
  ListEntitiesForPolicyResponse'
    { policyGroups = Lude.Nothing,
      policyRoles = Lude.Nothing,
      marker = Lude.Nothing,
      policyUsers = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of IAM groups that the policy is attached to.
--
-- /Note:/ Consider using 'policyGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprsPolicyGroups :: Lens.Lens' ListEntitiesForPolicyResponse (Lude.Maybe [PolicyGroup])
lefprsPolicyGroups = Lens.lens (policyGroups :: ListEntitiesForPolicyResponse -> Lude.Maybe [PolicyGroup]) (\s a -> s {policyGroups = a} :: ListEntitiesForPolicyResponse)
{-# DEPRECATED lefprsPolicyGroups "Use generic-lens or generic-optics with 'policyGroups' instead." #-}

-- | A list of IAM roles that the policy is attached to.
--
-- /Note:/ Consider using 'policyRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprsPolicyRoles :: Lens.Lens' ListEntitiesForPolicyResponse (Lude.Maybe [PolicyRole])
lefprsPolicyRoles = Lens.lens (policyRoles :: ListEntitiesForPolicyResponse -> Lude.Maybe [PolicyRole]) (\s a -> s {policyRoles = a} :: ListEntitiesForPolicyResponse)
{-# DEPRECATED lefprsPolicyRoles "Use generic-lens or generic-optics with 'policyRoles' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprsMarker :: Lens.Lens' ListEntitiesForPolicyResponse (Lude.Maybe Lude.Text)
lefprsMarker = Lens.lens (marker :: ListEntitiesForPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListEntitiesForPolicyResponse)
{-# DEPRECATED lefprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A list of IAM users that the policy is attached to.
--
-- /Note:/ Consider using 'policyUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprsPolicyUsers :: Lens.Lens' ListEntitiesForPolicyResponse (Lude.Maybe [PolicyUser])
lefprsPolicyUsers = Lens.lens (policyUsers :: ListEntitiesForPolicyResponse -> Lude.Maybe [PolicyUser]) (\s a -> s {policyUsers = a} :: ListEntitiesForPolicyResponse)
{-# DEPRECATED lefprsPolicyUsers "Use generic-lens or generic-optics with 'policyUsers' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprsIsTruncated :: Lens.Lens' ListEntitiesForPolicyResponse (Lude.Maybe Lude.Bool)
lefprsIsTruncated = Lens.lens (isTruncated :: ListEntitiesForPolicyResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListEntitiesForPolicyResponse)
{-# DEPRECATED lefprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lefprsResponseStatus :: Lens.Lens' ListEntitiesForPolicyResponse Lude.Int
lefprsResponseStatus = Lens.lens (responseStatus :: ListEntitiesForPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListEntitiesForPolicyResponse)
{-# DEPRECATED lefprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
