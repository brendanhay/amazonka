{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAttachedUserPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all managed policies that are attached to the specified IAM user.
--
-- An IAM user can also have inline policies embedded with it. To list the inline policies for a user, use the 'ListUserPolicies' API. For information about policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters. You can use the @PathPrefix@ parameter to limit the list of policies to only those matching the specified path prefix. If there are no policies attached to the specified group (or none that match the specified path prefix), the operation returns an empty list.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAttachedUserPolicies
  ( -- * Creating a request
    ListAttachedUserPolicies (..),
    mkListAttachedUserPolicies,

    -- ** Request lenses
    laupPathPrefix,
    laupMarker,
    laupMaxItems,
    laupUserName,

    -- * Destructuring the response
    ListAttachedUserPoliciesResponse (..),
    mkListAttachedUserPoliciesResponse,

    -- ** Response lenses
    lauprsAttachedPolicies,
    lauprsMarker,
    lauprsIsTruncated,
    lauprsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListAttachedUserPolicies' smart constructor.
data ListAttachedUserPolicies = ListAttachedUserPolicies'
  { pathPrefix ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural,
    userName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedUserPolicies' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'pathPrefix' - The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'userName' - The name (friendly name, not ARN) of the user to list attached policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkListAttachedUserPolicies ::
  -- | 'userName'
  Lude.Text ->
  ListAttachedUserPolicies
mkListAttachedUserPolicies pUserName_ =
  ListAttachedUserPolicies'
    { pathPrefix = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      userName = pUserName_
    }

-- | The path prefix for filtering the results. This parameter is optional. If it is not included, it defaults to a slash (/), listing all policies.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupPathPrefix :: Lens.Lens' ListAttachedUserPolicies (Lude.Maybe Lude.Text)
laupPathPrefix = Lens.lens (pathPrefix :: ListAttachedUserPolicies -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListAttachedUserPolicies)
{-# DEPRECATED laupPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupMarker :: Lens.Lens' ListAttachedUserPolicies (Lude.Maybe Lude.Text)
laupMarker = Lens.lens (marker :: ListAttachedUserPolicies -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAttachedUserPolicies)
{-# DEPRECATED laupMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupMaxItems :: Lens.Lens' ListAttachedUserPolicies (Lude.Maybe Lude.Natural)
laupMaxItems = Lens.lens (maxItems :: ListAttachedUserPolicies -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListAttachedUserPolicies)
{-# DEPRECATED laupMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name (friendly name, not ARN) of the user to list attached policies for.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laupUserName :: Lens.Lens' ListAttachedUserPolicies Lude.Text
laupUserName = Lens.lens (userName :: ListAttachedUserPolicies -> Lude.Text) (\s a -> s {userName = a} :: ListAttachedUserPolicies)
{-# DEPRECATED laupUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Page.AWSPager ListAttachedUserPolicies where
  page rq rs
    | Page.stop (rs Lens.^. lauprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lauprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& laupMarker Lens..~ rs Lens.^. lauprsMarker

instance Lude.AWSRequest ListAttachedUserPolicies where
  type Rs ListAttachedUserPolicies = ListAttachedUserPoliciesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListAttachedUserPoliciesResult"
      ( \s h x ->
          ListAttachedUserPoliciesResponse'
            Lude.<$> ( x Lude..@? "AttachedPolicies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListAttachedUserPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListAttachedUserPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListAttachedUserPolicies where
  toQuery ListAttachedUserPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListAttachedUserPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "UserName" Lude.=: userName
      ]

-- | Contains the response to a successful 'ListAttachedUserPolicies' request.
--
-- /See:/ 'mkListAttachedUserPoliciesResponse' smart constructor.
data ListAttachedUserPoliciesResponse = ListAttachedUserPoliciesResponse'
  { attachedPolicies ::
      Lude.Maybe
        [AttachedPolicy],
    marker ::
      Lude.Maybe Lude.Text,
    isTruncated ::
      Lude.Maybe Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListAttachedUserPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'attachedPolicies' - A list of the attached policies.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
mkListAttachedUserPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListAttachedUserPoliciesResponse
mkListAttachedUserPoliciesResponse pResponseStatus_ =
  ListAttachedUserPoliciesResponse'
    { attachedPolicies =
        Lude.Nothing,
      marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the attached policies.
--
-- /Note:/ Consider using 'attachedPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprsAttachedPolicies :: Lens.Lens' ListAttachedUserPoliciesResponse (Lude.Maybe [AttachedPolicy])
lauprsAttachedPolicies = Lens.lens (attachedPolicies :: ListAttachedUserPoliciesResponse -> Lude.Maybe [AttachedPolicy]) (\s a -> s {attachedPolicies = a} :: ListAttachedUserPoliciesResponse)
{-# DEPRECATED lauprsAttachedPolicies "Use generic-lens or generic-optics with 'attachedPolicies' instead." #-}

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprsMarker :: Lens.Lens' ListAttachedUserPoliciesResponse (Lude.Maybe Lude.Text)
lauprsMarker = Lens.lens (marker :: ListAttachedUserPoliciesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListAttachedUserPoliciesResponse)
{-# DEPRECATED lauprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprsIsTruncated :: Lens.Lens' ListAttachedUserPoliciesResponse (Lude.Maybe Lude.Bool)
lauprsIsTruncated = Lens.lens (isTruncated :: ListAttachedUserPoliciesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListAttachedUserPoliciesResponse)
{-# DEPRECATED lauprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lauprsResponseStatus :: Lens.Lens' ListAttachedUserPoliciesResponse Lude.Int
lauprsResponseStatus = Lens.lens (responseStatus :: ListAttachedUserPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListAttachedUserPoliciesResponse)
{-# DEPRECATED lauprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
