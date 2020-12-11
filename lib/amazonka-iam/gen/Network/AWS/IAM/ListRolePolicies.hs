{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListRolePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the specified IAM role.
--
-- An IAM role can also have managed policies attached to it. To list the managed policies that are attached to a role, use 'ListAttachedRolePolicies' . For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. If there are no inline policies embedded with the specified role, the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListRolePolicies
  ( -- * Creating a request
    ListRolePolicies (..),
    mkListRolePolicies,

    -- ** Request lenses
    lrpMarker,
    lrpMaxItems,
    lrpRoleName,

    -- * Destructuring the response
    ListRolePoliciesResponse (..),
    mkListRolePoliciesResponse,

    -- ** Response lenses
    lrprsMarker,
    lrprsIsTruncated,
    lrprsResponseStatus,
    lrprsPolicyNames,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRolePolicies' smart constructor.
data ListRolePolicies = ListRolePolicies'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural,
    roleName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRolePolicies' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'roleName' - The name of the role to list policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkListRolePolicies ::
  -- | 'roleName'
  Lude.Text ->
  ListRolePolicies
mkListRolePolicies pRoleName_ =
  ListRolePolicies'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      roleName = pRoleName_
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpMarker :: Lens.Lens' ListRolePolicies (Lude.Maybe Lude.Text)
lrpMarker = Lens.lens (marker :: ListRolePolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListRolePolicies)
{-# DEPRECATED lrpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpMaxItems :: Lens.Lens' ListRolePolicies (Lude.Maybe Lude.Natural)
lrpMaxItems = Lens.lens (maxItems :: ListRolePolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListRolePolicies)
{-# DEPRECATED lrpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the role to list policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrpRoleName :: Lens.Lens' ListRolePolicies Lude.Text
lrpRoleName = Lens.lens (roleName :: ListRolePolicies -> Lude.Text) (\s a -> s {roleName = a} :: ListRolePolicies)
{-# DEPRECATED lrpRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Page.AWSPager ListRolePolicies where
  page rq rs
    | Page.stop (rs Lens.^. lrprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lrprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lrpMarker Lens..~ rs Lens.^. lrprsMarker

instance Lude.AWSRequest ListRolePolicies where
  type Rs ListRolePolicies = ListRolePoliciesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListRolePoliciesResult"
      ( \s h x ->
          ListRolePoliciesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "PolicyNames" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListRolePolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListRolePolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRolePolicies where
  toQuery ListRolePolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListRolePolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "RoleName" Lude.=: roleName
      ]

-- | Contains the response to a successful 'ListRolePolicies' request.
--
-- /See:/ 'mkListRolePoliciesResponse' smart constructor.
data ListRolePoliciesResponse = ListRolePoliciesResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    policyNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRolePoliciesResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'policyNames' - A list of policy names.
-- * 'responseStatus' - The response status code.
mkListRolePoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRolePoliciesResponse
mkListRolePoliciesResponse pResponseStatus_ =
  ListRolePoliciesResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      policyNames = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsMarker :: Lens.Lens' ListRolePoliciesResponse (Lude.Maybe Lude.Text)
lrprsMarker = Lens.lens (marker :: ListRolePoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListRolePoliciesResponse)
{-# DEPRECATED lrprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsIsTruncated :: Lens.Lens' ListRolePoliciesResponse (Lude.Maybe Lude.Bool)
lrprsIsTruncated = Lens.lens (isTruncated :: ListRolePoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListRolePoliciesResponse)
{-# DEPRECATED lrprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsResponseStatus :: Lens.Lens' ListRolePoliciesResponse Lude.Int
lrprsResponseStatus = Lens.lens (responseStatus :: ListRolePoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRolePoliciesResponse)
{-# DEPRECATED lrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of policy names.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprsPolicyNames :: Lens.Lens' ListRolePoliciesResponse [Lude.Text]
lrprsPolicyNames = Lens.lens (policyNames :: ListRolePoliciesResponse -> [Lude.Text]) (\s a -> s {policyNames = a} :: ListRolePoliciesResponse)
{-# DEPRECATED lrprsPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}
