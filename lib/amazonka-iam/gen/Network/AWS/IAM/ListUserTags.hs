{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListUserTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that are attached to the specified user. The returned list of tags is sorted by tag key. For more information about tagging, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM Identities> in the /IAM User Guide/ .
module Network.AWS.IAM.ListUserTags
  ( -- * Creating a request
    ListUserTags (..),
    mkListUserTags,

    -- ** Request lenses
    lutMarker,
    lutMaxItems,
    lutUserName,

    -- * Destructuring the response
    ListUserTagsResponse (..),
    mkListUserTagsResponse,

    -- ** Response lenses
    lutrsMarker,
    lutrsIsTruncated,
    lutrsResponseStatus,
    lutrsTags,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListUserTags' smart constructor.
data ListUserTags = ListUserTags'
  { marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListUserTags' with the minimum fields required to make a request.
--
-- * 'marker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
-- * 'maxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
-- * 'userName' - The name of the IAM user whose tags you want to see.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
mkListUserTags ::
  -- | 'userName'
  Lude.Text ->
  ListUserTags
mkListUserTags pUserName_ =
  ListUserTags'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing,
      userName = pUserName_
    }

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutMarker :: Lens.Lens' ListUserTags (Lude.Maybe Lude.Text)
lutMarker = Lens.lens (marker :: ListUserTags -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListUserTags)
{-# DEPRECATED lutMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | (Optional) Use this only when paginating results to indicate the maximum number of items that you want in the response. If additional items exist beyond the maximum that you specify, the @IsTruncated@ response element is @true@ .
--
-- If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when more results are available. In that case, the @IsTruncated@ response element returns @true@ , and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutMaxItems :: Lens.Lens' ListUserTags (Lude.Maybe Lude.Natural)
lutMaxItems = Lens.lens (maxItems :: ListUserTags -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListUserTags)
{-# DEPRECATED lutMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The name of the IAM user whose tags you want to see.
--
-- This parameter accepts (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that consist of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutUserName :: Lens.Lens' ListUserTags Lude.Text
lutUserName = Lens.lens (userName :: ListUserTags -> Lude.Text) (\s a -> s {userName = a} :: ListUserTags)
{-# DEPRECATED lutUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest ListUserTags where
  type Rs ListUserTags = ListUserTagsResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "ListUserTagsResult"
      ( \s h x ->
          ListUserTagsResponse'
            Lude.<$> (x Lude..@? "Marker")
            Lude.<*> (x Lude..@? "IsTruncated")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListUserTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListUserTags where
  toPath = Lude.const "/"

instance Lude.ToQuery ListUserTags where
  toQuery ListUserTags' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListUserTags" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkListUserTagsResponse' smart constructor.
data ListUserTagsResponse = ListUserTagsResponse'
  { marker ::
      Lude.Maybe Lude.Text,
    isTruncated :: Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int,
    tags :: [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListUserTagsResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
-- * 'marker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The list of tags that are currently attached to the user. Each tag consists of a key name and an associated value. If no tags are attached to the specified user, the response contains an empty list.
mkListUserTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListUserTagsResponse
mkListUserTagsResponse pResponseStatus_ =
  ListUserTagsResponse'
    { marker = Lude.Nothing,
      isTruncated = Lude.Nothing,
      responseStatus = pResponseStatus_,
      tags = Lude.mempty
    }

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrsMarker :: Lens.Lens' ListUserTagsResponse (Lude.Maybe Lude.Text)
lutrsMarker = Lens.lens (marker :: ListUserTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListUserTagsResponse)
{-# DEPRECATED lutrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can use the @Marker@ request parameter to make a subsequent pagination request that retrieves more items. Note that IAM might return fewer than the @MaxItems@ number of results even when more results are available. Check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrsIsTruncated :: Lens.Lens' ListUserTagsResponse (Lude.Maybe Lude.Bool)
lutrsIsTruncated = Lens.lens (isTruncated :: ListUserTagsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isTruncated = a} :: ListUserTagsResponse)
{-# DEPRECATED lutrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrsResponseStatus :: Lens.Lens' ListUserTagsResponse Lude.Int
lutrsResponseStatus = Lens.lens (responseStatus :: ListUserTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListUserTagsResponse)
{-# DEPRECATED lutrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The list of tags that are currently attached to the user. Each tag consists of a key name and an associated value. If no tags are attached to the specified user, the response contains an empty list.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lutrsTags :: Lens.Lens' ListUserTagsResponse [Tag]
lutrsTags = Lens.lens (tags :: ListUserTagsResponse -> [Tag]) (\s a -> s {tags = a} :: ListUserTagsResponse)
{-# DEPRECATED lutrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}
