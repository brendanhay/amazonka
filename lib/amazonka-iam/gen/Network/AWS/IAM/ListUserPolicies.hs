{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListUserPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies embedded in the specified IAM user.
--
-- An IAM user can also have managed policies attached to it. To list the managed policies that are attached to a user, use 'ListAttachedUserPolicies' . For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. If there are no inline policies embedded with the specified user, the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListUserPolicies
  ( -- * Creating a request
    ListUserPolicies (..),
    mkListUserPolicies,

    -- ** Request lenses
    lupUserName,
    lupMarker,
    lupMaxItems,

    -- * Destructuring the response
    ListUserPoliciesResponse (..),
    mkListUserPoliciesResponse,

    -- ** Response lenses
    luprsPolicyNames,
    luprsMarker,
    luprsIsTruncated,
    luprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUserPolicies' smart constructor.
data ListUserPolicies = ListUserPolicies'
  { -- | The name of the user to list policies for.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserPolicies' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user to list policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListUserPolicies ::
  -- | 'userName'
  Lude.Text ->
  ListUserPolicies
mkListUserPolicies pUserName_ =
  ListUserPolicies'
    { userName = pUserName_,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the user to list policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupUserName :: Lens.Lens' ListUserPolicies Lude.Text
lupUserName = Lens.lens (userName :: ListUserPolicies -> Lude.Text) (\s a -> s {userName = a} :: ListUserPolicies)
{-# DEPRECATED lupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupMarker :: Lens.Lens' ListUserPolicies (Lude.Maybe Lude.Text)
lupMarker = Lens.lens (marker :: ListUserPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListUserPolicies)
{-# DEPRECATED lupMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lupMaxItems :: Lens.Lens' ListUserPolicies (Lude.Maybe Lude.Natural)
lupMaxItems = Lens.lens (maxItems :: ListUserPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListUserPolicies)
{-# DEPRECATED lupMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListUserPolicies where
  page rq rs
    | Page.stop (rs Lens.^. luprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. luprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lupMarker Lens..~ rs Lens.^. luprsMarker

instance Lude.AWSRequest ListUserPolicies where
  type Rs ListUserPolicies = ListUserPoliciesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListUserPoliciesResult"
      ( \s h x ->
          ListUserPoliciesResponse'
            Lude.<$> ( x Lude..@? "PolicyNames" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListUserPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListUserPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUserPolicies where
  toQuery ListUserPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListUserPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListUserPolicies' request.
--
-- /See:/ 'mkListUserPoliciesResponse' smart constructor.
data ListUserPoliciesResponse = ListUserPoliciesResponse'
  { -- | A list of policy names.
    policyNames :: [Lude.Text],
    -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'policyNames' - A list of policy names.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'responseStatus' - The response status code.
mkListUserPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserPoliciesResponse
mkListUserPoliciesResponse pResponseStatus_ =
  ListUserPoliciesResponse'
    { policyNames = Lude.mempty,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of policy names.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsPolicyNames :: Lens.Lens' ListUserPoliciesResponse [Lude.Text]
luprsPolicyNames = Lens.lens (policyNames :: ListUserPoliciesResponse -> [Lude.Text]) (\s a -> s {policyNames = a} :: ListUserPoliciesResponse)
{-# DEPRECATED luprsPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsMarker :: Lens.Lens' ListUserPoliciesResponse (Lude.Maybe Lude.Text)
luprsMarker = Lens.lens (marker :: ListUserPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListUserPoliciesResponse)
{-# DEPRECATED luprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsIsTruncated :: Lens.Lens' ListUserPoliciesResponse (Lude.Maybe Lude.Bool)
luprsIsTruncated = Lens.lens (isTruncated :: ListUserPoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListUserPoliciesResponse)
{-# DEPRECATED luprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
luprsResponseStatus :: Lens.Lens' ListUserPoliciesResponse Lude.Int
luprsResponseStatus = Lens.lens (responseStatus :: ListUserPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUserPoliciesResponse)
{-# DEPRECATED luprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
