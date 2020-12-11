{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListGroupPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the names of the inline policies that are embedded in the specified IAM group.
--
-- An IAM group can also have managed policies attached to it. To list the managed policies that are attached to a group, use 'ListAttachedGroupPolicies' . For more information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. If there are no inline policies embedded with the specified group, the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListGroupPolicies
  ( -- * Creating a request
    ListGroupPolicies (..),
    mkListGroupPolicies,

    -- ** Request lenses
    lgpMarker,
    lgpMaxItems,
    lgpGroupName,

    -- * Destructuring the response
    ListGroupPoliciesResponse (..),
    mkListGroupPoliciesResponse,

    -- ** Response lenses
    lgprsMarker,
    lgprsIsTruncated,
    lgprsResponseStatus,
    lgprsPolicyNames,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListGroupPolicies' smart constructor.
data ListGroupPolicies = ListGroupPolicies'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural,
    groupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGroupPolicies' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the group to list policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListGroupPolicies ::
  -- | 'groupName'
  Lude.Text ->
  ListGroupPolicies
mkListGroupPolicies pGroupName_ =
  ListGroupPolicies'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      groupName = pGroupName_
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgpMarker :: Lens.Lens' ListGroupPolicies (Lude.Maybe Lude.Text)
lgpMarker = Lens.lens (marker :: ListGroupPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGroupPolicies)
{-# DEPRECATED lgpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgpMaxItems :: Lens.Lens' ListGroupPolicies (Lude.Maybe Lude.Natural)
lgpMaxItems = Lens.lens (maxItems :: ListGroupPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListGroupPolicies)
{-# DEPRECATED lgpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the group to list policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgpGroupName :: Lens.Lens' ListGroupPolicies Lude.Text
lgpGroupName = Lens.lens (groupName :: ListGroupPolicies -> Lude.Text) (\s a -> s {groupName = a} :: ListGroupPolicies)
{-# DEPRECATED lgpGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Page.AWSPager ListGroupPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lgprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lgprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lgpMarker Lens..~ rs Lens.^. lgprsMarker

instance Lude.AWSRequest ListGroupPolicies where
  type Rs ListGroupPolicies = ListGroupPoliciesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListGroupPoliciesResult"
      ( \s h x ->
          ListGroupPoliciesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "PolicyNames" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListGroupPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListGroupPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListGroupPolicies where
  toQuery ListGroupPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListGroupPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "GroupName" Lude.=: groupName
      ]

-- | Contains the response to a successful 'ListGroupPolicies' request.
--
-- /See:/ 'mkListGroupPoliciesResponse' smart constructor.
data ListGroupPoliciesResponse = ListGroupPoliciesResponse'
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

-- | Creates a value of 'ListGroupPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'policyNames' - A list of policy names.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'responseStatus' - The response status code.
mkListGroupPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListGroupPoliciesResponse
mkListGroupPoliciesResponse pResponseStatus_ =
  ListGroupPoliciesResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      policyNames = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgprsMarker :: Lens.Lens' ListGroupPoliciesResponse (Lude.Maybe Lude.Text)
lgprsMarker = Lens.lens (marker :: ListGroupPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListGroupPoliciesResponse)
{-# DEPRECATED lgprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgprsIsTruncated :: Lens.Lens' ListGroupPoliciesResponse (Lude.Maybe Lude.Bool)
lgprsIsTruncated = Lens.lens (isTruncated :: ListGroupPoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListGroupPoliciesResponse)
{-# DEPRECATED lgprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgprsResponseStatus :: Lens.Lens' ListGroupPoliciesResponse Lude.Int
lgprsResponseStatus = Lens.lens (responseStatus :: ListGroupPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGroupPoliciesResponse)
{-# DEPRECATED lgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of policy names.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgprsPolicyNames :: Lens.Lens' ListGroupPoliciesResponse [Lude.Text]
lgprsPolicyNames = Lens.lens (policyNames :: ListGroupPoliciesResponse -> [Lude.Text]) (\s a -> s {policyNames = a} :: ListGroupPoliciesResponse)
{-# DEPRECATED lgprsPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}
