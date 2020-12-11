{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListInstanceProfiles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the instance profiles that have the specified path prefix. If there are none, the operation returns an empty list. For more information about instance profiles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
-- You can paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListInstanceProfiles
  ( -- * Creating a request
    ListInstanceProfiles (..),
    mkListInstanceProfiles,

    -- ** Request lenses
    lipPathPrefix,
    lipMarker,
    lipMaxItems,

    -- * Destructuring the response
    ListInstanceProfilesResponse (..),
    mkListInstanceProfilesResponse,

    -- ** Response lenses
    liprsMarker,
    liprsIsTruncated,
    liprsResponseStatus,
    liprsInstanceProfiles,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInstanceProfiles' smart constructor.
data ListInstanceProfiles = ListInstanceProfiles'
  { pathPrefix ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceProfiles' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'pathPrefix' - The path prefix for filtering the results. For example, the prefix @/application_abc/component_xyz/@ gets all instance profiles whose path starts with @/application_abc/component_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all instance profiles. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
mkListInstanceProfiles ::
  ListInstanceProfiles
mkListInstanceProfiles =
  ListInstanceProfiles'
    { pathPrefix = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The path prefix for filtering the results. For example, the prefix @/application_abc/component_xyz/@ gets all instance profiles whose path starts with @/application_abc/component_xyz/@ .
--
-- This parameter is optional. If it is not included, it defaults to a slash (/), listing all instance profiles. This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'pathPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipPathPrefix :: Lens.Lens' ListInstanceProfiles (Lude.Maybe Lude.Text)
lipPathPrefix = Lens.lens (pathPrefix :: ListInstanceProfiles -> Lude.Maybe Lude.Text) (\s a -> s {pathPrefix = a} :: ListInstanceProfiles)
{-# DEPRECATED lipPathPrefix "Use generic-lens or generic-optics with 'pathPrefix' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMarker :: Lens.Lens' ListInstanceProfiles (Lude.Maybe Lude.Text)
lipMarker = Lens.lens (marker :: ListInstanceProfiles -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceProfiles)
{-# DEPRECATED lipMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, the number of items defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipMaxItems :: Lens.Lens' ListInstanceProfiles (Lude.Maybe Lude.Natural)
lipMaxItems = Lens.lens (maxItems :: ListInstanceProfiles -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListInstanceProfiles)
{-# DEPRECATED lipMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListInstanceProfiles where
  page rq rs
    | Page.stop (rs Lens.^. liprsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. liprsMarker) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lipMarker Lens..~ rs Lens.^. liprsMarker

instance Lude.AWSRequest ListInstanceProfiles where
  type Rs ListInstanceProfiles = ListInstanceProfilesResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListInstanceProfilesResult"
      ( \s h x ->
          ListInstanceProfilesResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "InstanceProfiles" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListInstanceProfiles where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListInstanceProfiles where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInstanceProfiles where
  toQuery ListInstanceProfiles' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListInstanceProfiles" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "PathPrefix" Lude.=: pathPrefix,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | Contains the response to a successful 'ListInstanceProfiles' request.
--
-- /See:/ 'mkListInstanceProfilesResponse' smart constructor.
data ListInstanceProfilesResponse = ListInstanceProfilesResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    instanceProfiles ::
      [InstanceProfile]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceProfilesResponse' with the minimum fields required to make a request.
--
-- * 'instanceProfiles' - A list of instance profiles.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
mkListInstanceProfilesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstanceProfilesResponse
mkListInstanceProfilesResponse pResponseStatus_ =
  ListInstanceProfilesResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      instanceProfiles = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsMarker :: Lens.Lens' ListInstanceProfilesResponse (Lude.Maybe Lude.Text)
liprsMarker = Lens.lens (marker :: ListInstanceProfilesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceProfilesResponse)
{-# DEPRECATED liprsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsIsTruncated :: Lens.Lens' ListInstanceProfilesResponse (Lude.Maybe Lude.Bool)
liprsIsTruncated = Lens.lens (isTruncated :: ListInstanceProfilesResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListInstanceProfilesResponse)
{-# DEPRECATED liprsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsResponseStatus :: Lens.Lens' ListInstanceProfilesResponse Lude.Int
liprsResponseStatus = Lens.lens (responseStatus :: ListInstanceProfilesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstanceProfilesResponse)
{-# DEPRECATED liprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of instance profiles.
--
-- /Note:/ Consider using 'instanceProfiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsInstanceProfiles :: Lens.Lens' ListInstanceProfilesResponse [InstanceProfile]
liprsInstanceProfiles = Lens.lens (instanceProfiles :: ListInstanceProfilesResponse -> [InstanceProfile]) (\s a -> s {instanceProfiles = a} :: ListInstanceProfilesResponse)
{-# DEPRECATED liprsInstanceProfiles "Use generic-lens or generic-optics with 'instanceProfiles' instead." #-}
