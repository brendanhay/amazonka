{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified tags for the specified WorkSpaces resource.
module Network.AWS.WorkSpaces.DescribeTags
  ( -- * Creating a request
    DescribeTags (..),
    mkDescribeTags,

    -- ** Request lenses
    dtResourceId,

    -- * Destructuring the response
    DescribeTagsResponse (..),
    mkDescribeTagsResponse,

    -- ** Response lenses
    dtrsTagList,
    dtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDescribeTags' smart constructor.
newtype DescribeTags = DescribeTags'
  { -- | The identifier of the WorkSpaces resource. The supported resource types are WorkSpaces, registered directories, images, custom bundles, IP access control groups, and connection aliases.
    resourceId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the WorkSpaces resource. The supported resource types are WorkSpaces, registered directories, images, custom bundles, IP access control groups, and connection aliases.
mkDescribeTags ::
  -- | 'resourceId'
  Lude.Text ->
  DescribeTags
mkDescribeTags pResourceId_ =
  DescribeTags' {resourceId = pResourceId_}

-- | The identifier of the WorkSpaces resource. The supported resource types are WorkSpaces, registered directories, images, custom bundles, IP access control groups, and connection aliases.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtResourceId :: Lens.Lens' DescribeTags Lude.Text
dtResourceId = Lens.lens (resourceId :: DescribeTags -> Lude.Text) (\s a -> s {resourceId = a} :: DescribeTags)
{-# DEPRECATED dtResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest DescribeTags where
  type Rs DescribeTags = DescribeTagsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTagsResponse'
            Lude.<$> (x Lude..?> "TagList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DescribeTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTags where
  toJSON DescribeTags' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceId" Lude..= resourceId)])

instance Lude.ToPath DescribeTags where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { -- | The tags.
    tagList :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- * 'tagList' - The tags.
-- * 'responseStatus' - The response status code.
mkDescribeTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTagsResponse
mkDescribeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { tagList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTagList :: Lens.Lens' DescribeTagsResponse (Lude.Maybe [Tag])
dtrsTagList = Lens.lens (tagList :: DescribeTagsResponse -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTagsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTagsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
