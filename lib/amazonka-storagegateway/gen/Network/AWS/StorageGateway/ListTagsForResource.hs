{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the tags that have been added to the specified resource. This operation is supported in storage gateways of all types.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrMarker,
    ltfrLimit,
    ltfrResourceARN,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsResourceARN,
    ltfrrsMarker,
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | ListTagsForResourceInput
--
-- /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { marker ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    resourceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'limit' - Specifies that the list of tags returned be limited to the specified number of items.
-- * 'marker' - An opaque string that indicates the position at which to begin returning the list of tags.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource for which you want to list tags.
mkListTagsForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      resourceARN = pResourceARN_
    }

-- | An opaque string that indicates the position at which to begin returning the list of tags.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrMarker :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Text)
ltfrMarker = Lens.lens (marker :: ListTagsForResource -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListTagsForResource)
{-# DEPRECATED ltfrMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies that the list of tags returned be limited to the specified number of items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrLimit :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Natural)
ltfrLimit = Lens.lens (limit :: ListTagsForResource -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListTagsForResource)
{-# DEPRECATED ltfrLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource for which you want to list tags.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceARN = Lens.lens (resourceARN :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Page.AWSPager ListTagsForResource where
  page rq rs
    | Page.stop (rs Lens.^. ltfrrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ltfrrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltfrMarker Lens..~ rs Lens.^. ltfrrsMarker

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (x Lude..?> "ResourceARN")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ListTagsForResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            Lude.Just ("ResourceARN" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | ListTagsForResourceOutput
--
-- /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { resourceARN ::
      Lude.Maybe Lude.Text,
    marker :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An opaque string that indicates the position at which to stop returning the list of tags.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource for which you want to list tags.
-- * 'responseStatus' - The response status code.
-- * 'tags' - An array that contains the tags for the specified resource.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { resourceARN = Lude.Nothing,
      marker = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource for which you want to list tags.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResourceARN :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe Lude.Text)
ltfrrsResourceARN = Lens.lens (resourceARN :: ListTagsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | An opaque string that indicates the position at which to stop returning the list of tags.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsMarker :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe Lude.Text)
ltfrrsMarker = Lens.lens (marker :: ListTagsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array that contains the tags for the specified resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsTags :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe [Tag])
ltfrrsTags = Lens.lens (tags :: ListTagsForResourceResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
