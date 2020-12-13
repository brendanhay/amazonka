{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the tags assigned to the specified resource.
module Network.AWS.SSM.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrResourceId,
    ltfrResourceType,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsTagList,
    ltfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The resource ID for which you want to see a list of tags.
    resourceId :: Lude.Text,
    -- | Returns a list of tags for a specific resource type.
    resourceType :: ResourceTypeForTagging
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The resource ID for which you want to see a list of tags.
-- * 'resourceType' - Returns a list of tags for a specific resource type.
mkListTagsForResource ::
  -- | 'resourceId'
  Lude.Text ->
  -- | 'resourceType'
  ResourceTypeForTagging ->
  ListTagsForResource
mkListTagsForResource pResourceId_ pResourceType_ =
  ListTagsForResource'
    { resourceId = pResourceId_,
      resourceType = pResourceType_
    }

-- | The resource ID for which you want to see a list of tags.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceId :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceId = Lens.lens (resourceId :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceId = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Returns a list of tags for a specific resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceType :: Lens.Lens' ListTagsForResource ResourceTypeForTagging
ltfrResourceType = Lens.lens (resourceType :: ListTagsForResource -> ResourceTypeForTagging) (\s a -> s {resourceType = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (x Lude..?> "TagList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListTagsForResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("ResourceType" Lude..= resourceType)
          ]
      )

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | A list of tags.
    tagList :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'tagList' - A list of tags.
-- * 'responseStatus' - The response status code.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { tagList = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsTagList :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe [Tag])
ltfrrsTagList = Lens.lens (tagList :: ListTagsForResourceResponse -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
