{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAttachedRolePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM role.
--
-- An IAM role can also have inline policies embedded with it. To list the inline policies for a role, use the 'ListRolePolicies' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. You can use the @PathPrefix@ parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified role (or none that match the specified path prefix), the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedRolePolicies
  ( -- * Creating a request
    ListAttachedRolePolicies (..),
    mkListAttachedRolePolicies,

    -- ** Request lenses
    larpPathPrefix,
    larpRoleName,
    larpMarker,
    larpMaxItems,

    -- * Destructuring the response
    ListAttachedRolePoliciesResponse (..),
    mkListAttachedRolePoliciesResponse,

    -- ** Response lenses
    larprsAttachedPolicies,
    larprsMarker,
    larprsIsTruncated,
    larprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAttachedRolePolicies' smart constructor.
data ListAttachedRolePolicies = ListAttachedRolePolicies'
  { -- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    pathPrefix :: Lude.Maybe Lude.Text,
    -- | The name (friendly name, not ARN) of the role to list attached policies for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedRolePolicies' with the minimum fields required to make a request.
--
-- * 'pathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'roleName' - The name (friendly name, not ARN) of the role to list attached policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListAttachedRolePolicies ::
  -- | 'roleName'
  Lude.Text ->
  ListAttachedRolePolicies
mkListAttachedRolePolicies pRoleName_ =
  ListAttachedRolePolicies'
    { pathPrefix = Lude.Nothing,
      roleName = pRoleName_,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larpPathPrefix :: Lens.Lens' ListAttachedRolePolicies (Lude.Maybe Lude.Text)
larpPathPrefix = Lens.lens (pathPrefix :: ListAttachedRolePolicies -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListAttachedRolePolicies)
{-# DEPRECATED larpPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | The name (friendly name, not ARN) of the role to list attached policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larpRoleName :: Lens.Lens' ListAttachedRolePolicies Lude.Text
larpRoleName = Lens.lens (roleName :: ListAttachedRolePolicies -> Lude.Text) (\s a -> s {roleName = a} :: ListAttachedRolePolicies)
{-# DEPRECATED larpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larpMarker :: Lens.Lens' ListAttachedRolePolicies (Lude.Maybe Lude.Text)
larpMarker = Lens.lens (marker :: ListAttachedRolePolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAttachedRolePolicies)
{-# DEPRECATED larpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larpMaxItems :: Lens.Lens' ListAttachedRolePolicies (Lude.Maybe Lude.Natural)
larpMaxItems = Lens.lens (maxItems :: ListAttachedRolePolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListAttachedRolePolicies)
{-# DEPRECATED larpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListAttachedRolePolicies where
  page rq rs
    | Page.stop (rs Lens.^. larprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. larprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& larpMarker Lens..~ rs Lens.^. larprsMarker

instance Lude.AWSRequest ListAttachedRolePolicies where
  type Rs ListAttachedRolePolicies = ListAttachedRolePoliciesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListAttachedRolePoliciesResult"
      ( \s h x ->
          ListAttachedRolePoliciesResponse'
            Lude.<$> ( x Lude..@? "AttachedPolicies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAttachedRolePolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAttachedRolePolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAttachedRolePolicies where
  toQuery ListAttachedRolePolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListAttachedRolePolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "RoleName" Lude.=: roleName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListAttachedRolePolicies' request.
--
-- /See:/ 'mkListAttachedRolePoliciesResponse' smart constructor.
data ListAttachedRolePoliciesResponse = ListAttachedRolePoliciesResponse'
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

-- | Creates a value of 'ListAttachedRolePoliciesResponse' with the minimum fields required to make a request.
--
-- * 'attachedPolicies' - A list of the attached policies.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListAttachedRolePoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAttachedRolePoliciesResponse
mkListAttachedRolePoliciesResponse pResponseStatus_ =
  ListAttachedRolePoliciesResponse'
    { attachedPolicies =
        Lude.Nothing,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the attached policies.
--
-- /Note:/ Consider using 'attachedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larprsAttachedPolicies :: Lens.Lens' ListAttachedRolePoliciesResponse (Lude.Maybe [AttachedPolicy])
larprsAttachedPolicies = Lens.lens (attachedPolicies :: ListAttachedRolePoliciesResponse -> Lude.Maybe [AttachedPolicy]) (\s a -> s {attachedPolicies = a} :: ListAttachedRolePoliciesResponse)
{-# DEPRECATED larprsAttachedPolicies "Use generic-lens or generic-optics with 'attachedPolicies' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larprsMarker :: Lens.Lens' ListAttachedRolePoliciesResponse (Lude.Maybe Lude.Text)
larprsMarker = Lens.lens (marker :: ListAttachedRolePoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAttachedRolePoliciesResponse)
{-# DEPRECATED larprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larprsIsTruncated :: Lens.Lens' ListAttachedRolePoliciesResponse (Lude.Maybe Lude.Bool)
larprsIsTruncated = Lens.lens (isTruncated :: ListAttachedRolePoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListAttachedRolePoliciesResponse)
{-# DEPRECATED larprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larprsResponseStatus :: Lens.Lens' ListAttachedRolePoliciesResponse Lude.Int
larprsResponseStatus = Lens.lens (responseStatus :: ListAttachedRolePoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAttachedRolePoliciesResponse)
{-# DEPRECATED larprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
