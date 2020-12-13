{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListRoleTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified role. The returned list of tags is sorted by tag key. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.ListRoleTags
  ( -- * Creating a request
    ListRoleTags (..),
    mkListRoleTags,

    -- ** Request lenses
    lrtRoleName,
    lrtMarker,
    lrtMaxItems,

    -- * Destructuring the response
    ListRoleTagsResponse (..),
    mkListRoleTagsResponse,

    -- ** Response lenses
    lrtrsMarker,
    lrtrsIsTruncated,
    lrtrsTags,
    lrtrsResponseStatus,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRoleTags' smart constructor.
data ListRoleTags = ListRoleTags'
  { -- | The name of the IAM role for which you want to see the list of tags.
    --
    -- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    roleName :: Lude.Text,
    -- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
    marker :: Lude.Maybe Lude.Text,
    -- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
    --
    -- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRoleTags' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
mkListRoleTags ::
  -- | 'roleName'
  Lude.Text ->
  ListRoleTags
mkListRoleTags pRoleName_ =
  ListRoleTags'
    { roleName = pRoleName_,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name of the IAM role for which you want to see the list of tags.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtRoleName :: Lens.Lens' ListRoleTags Lude.Text
lrtRoleName = Lens.lens (roleName :: ListRoleTags -> Lude.Text) (\s a -> s {roleName = a} :: ListRoleTags)
{-# DEPRECATED lrtRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMarker :: Lens.Lens' ListRoleTags (Lude.Maybe Lude.Text)
lrtMarker = Lens.lens (marker :: ListRoleTags -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListRoleTags)
{-# DEPRECATED lrtMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtMaxItems :: Lens.Lens' ListRoleTags (Lude.Maybe Lude.Natural)
lrtMaxItems = Lens.lens (maxItems :: ListRoleTags -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListRoleTags)
{-# DEPRECATED lrtMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListRoleTags where
  type Rs ListRoleTags = ListRoleTagsResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListRoleTagsResult"
      ( \s h x ->
          ListRoleTagsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRoleTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListRoleTags where
  toPath = Lude.const "/"

instance Lude.ToQuery ListRoleTags where
  toQuery ListRoleTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListRoleTags" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | /See:/ 'mkListRoleTagsResponse' smart constructor.
data ListRoleTagsResponse = ListRoleTagsResponse'
  { -- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
    marker :: Lude.Maybe Lude.Text,
    -- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
    isTruncated :: Lude.Maybe Lude.Bool,
    -- | The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
    tags :: [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRoleTagsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
-- * 'tags' - The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
-- * 'responseStatus' - The response status code.
mkListRoleTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRoleTagsResponse
mkListRoleTagsResponse pResponseStatus_ =
  ListRoleTagsResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      tags = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsMarker :: Lens.Lens' ListRoleTagsResponse (Lude.Maybe Lude.Text)
lrtrsMarker = Lens.lens (marker :: ListRoleTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListRoleTagsResponse)
{-# DEPRECATED lrtrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsIsTruncated :: Lens.Lens' ListRoleTagsResponse (Lude.Maybe Lude.Bool)
lrtrsIsTruncated = Lens.lens (isTruncated :: ListRoleTagsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListRoleTagsResponse)
{-# DEPRECATED lrtrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The list of tags currently that is attached to the role. Each tag consists of a key name and an associated value. If no tags are attached to the specified role, the response contains an empty list.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsTags :: Lens.Lens' ListRoleTagsResponse [Tag]
lrtrsTags = Lens.lens (tags :: ListRoleTagsResponse -> [Tag]) (\s a -> s {tags = a} :: ListRoleTagsResponse)
{-# DEPRECATED lrtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrtrsResponseStatus :: Lens.Lens' ListRoleTagsResponse Lude.Int
lrtrsResponseStatus = Lens.lens (responseStatus :: ListRoleTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRoleTagsResponse)
{-# DEPRECATED lrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
