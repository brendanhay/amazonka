{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAttachedGroupPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM group.
--
-- An IAM group can also have inline policies embedded with it. To list the inline policies for a group, use the 'ListGroupPolicies' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. You can use the @PathPrefix@ parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedGroupPolicies
  ( -- * Creating a request
    ListAttachedGroupPolicies (..),
    mkListAttachedGroupPolicies,

    -- ** Request lenses
    lagpPathPrefix,
    lagpMarker,
    lagpMaxItems,
    lagpGroupName,

    -- * Destructuring the response
    ListAttachedGroupPoliciesResponse (..),
    mkListAttachedGroupPoliciesResponse,

    -- ** Response lenses
    lagprsAttachedPolicies,
    lagprsMarker,
    lagprsIsTruncated,
    lagprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAttachedGroupPolicies' smart constructor.
data ListAttachedGroupPolicies = ListAttachedGroupPolicies'
  { -- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    pathPrefix :: Lude.Maybe Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural,
    -- | The name (friendly name, not ARN) of the group to list attached policies for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedGroupPolicies' with the minimum fields required to make a request.
--
-- * 'pathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'groupName' - The name (friendly name, not ARN) of the group to list attached policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkListAttachedGroupPolicies ::
  -- | 'groupName'
  Lude.Text ->
  ListAttachedGroupPolicies
mkListAttachedGroupPolicies pGroupName_ =
  ListAttachedGroupPolicies'
    { pathPrefix = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      groupName = pGroupName_
    }

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagpPathPrefix :: Lens.Lens' ListAttachedGroupPolicies (Lude.Maybe Lude.Text)
lagpPathPrefix = Lens.lens (pathPrefix :: ListAttachedGroupPolicies -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListAttachedGroupPolicies)
{-# DEPRECATED lagpPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagpMarker :: Lens.Lens' ListAttachedGroupPolicies (Lude.Maybe Lude.Text)
lagpMarker = Lens.lens (marker :: ListAttachedGroupPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAttachedGroupPolicies)
{-# DEPRECATED lagpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagpMaxItems :: Lens.Lens' ListAttachedGroupPolicies (Lude.Maybe Lude.Natural)
lagpMaxItems = Lens.lens (maxItems :: ListAttachedGroupPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListAttachedGroupPolicies)
{-# DEPRECATED lagpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name (friendly name, not ARN) of the group to list attached policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagpGroupName :: Lens.Lens' ListAttachedGroupPolicies Lude.Text
lagpGroupName = Lens.lens (groupName :: ListAttachedGroupPolicies -> Lude.Text) (\s a -> s {groupName = a} :: ListAttachedGroupPolicies)
{-# DEPRECATED lagpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Page.AWSPager ListAttachedGroupPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lagprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lagprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lagpMarker Lens..~ rs Lens.^. lagprsMarker

instance Lude.AWSRequest ListAttachedGroupPolicies where
  type
    Rs ListAttachedGroupPolicies =
      ListAttachedGroupPoliciesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListAttachedGroupPoliciesResult"
      ( \s h x ->
          ListAttachedGroupPoliciesResponse'
            Lude.<$> ( x Lude..@? "AttachedPolicies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAttachedGroupPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAttachedGroupPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAttachedGroupPolicies where
  toQuery ListAttachedGroupPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListAttachedGroupPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "GroupName" Lude.=: groupName
      ]

-- | Contains the response to a successful 'ListAttachedGroupPolicies' request.
--
-- /See:/ 'mkListAttachedGroupPoliciesResponse' smart constructor.
data ListAttachedGroupPoliciesResponse = ListAttachedGroupPoliciesResponse'
  { -- | A list of the attached policies.
    attachedPolicies :: Lude.Maybe [AttachedPolicy],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedGroupPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'attachedPolicies' - A list of the attached policies.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListAttachedGroupPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAttachedGroupPoliciesResponse
mkListAttachedGroupPoliciesResponse pResponseStatus_ =
  ListAttachedGroupPoliciesResponse'
    { attachedPolicies =
        Lude.Nothing,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the attached policies.
--
-- /Note:/ Consider using 'attachedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagprsAttachedPolicies :: Lens.Lens' ListAttachedGroupPoliciesResponse (Lude.Maybe [AttachedPolicy])
lagprsAttachedPolicies = Lens.lens (attachedPolicies :: ListAttachedGroupPoliciesResponse -> Lude.Maybe [AttachedPolicy]) (\s a -> s {attachedPolicies = a} :: ListAttachedGroupPoliciesResponse)
{-# DEPRECATED lagprsAttachedPolicies "Use generic-lens or generic-optics with 'attachedPolicies' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagprsMarker :: Lens.Lens' ListAttachedGroupPoliciesResponse (Lude.Maybe Lude.Text)
lagprsMarker = Lens.lens (marker :: ListAttachedGroupPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAttachedGroupPoliciesResponse)
{-# DEPRECATED lagprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagprsIsTruncated :: Lens.Lens' ListAttachedGroupPoliciesResponse (Lude.Maybe Lude.Bool)
lagprsIsTruncated = Lens.lens (isTruncated :: ListAttachedGroupPoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListAttachedGroupPoliciesResponse)
{-# DEPRECATED lagprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lagprsResponseStatus :: Lens.Lens' ListAttachedGroupPoliciesResponse Lude.Int
lagprsResponseStatus = Lens.lens (responseStatus :: ListAttachedGroupPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAttachedGroupPoliciesResponse)
{-# DEPRECATED lagprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
