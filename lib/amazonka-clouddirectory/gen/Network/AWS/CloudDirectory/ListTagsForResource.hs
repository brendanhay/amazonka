{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns tags for a resource. Tagging is currently supported only for directories with a limit of 50 tags per directory. All 50 tags are returned for a given directory with this API call.
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrNextToken,
    ltfrResourceARN,
    ltfrMaxResults,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsNextToken,
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | The pagination token. This is for future use. Currently pagination is not supported for tagging.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the resource. Tagging is only supported for directories.
    resourceARN :: Lude.Text,
    -- | The @MaxResults@ parameter sets the maximum number of results returned in a single page. This is for future use and is not supported currently.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination token. This is for future use. Currently pagination is not supported for tagging.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource. Tagging is only supported for directories.
-- * 'maxResults' - The @MaxResults@ parameter sets the maximum number of results returned in a single page. This is for future use and is not supported currently.
mkListTagsForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { nextToken = Lude.Nothing,
      resourceARN = pResourceARN_,
      maxResults = Lude.Nothing
    }

-- | The pagination token. This is for future use. Currently pagination is not supported for tagging.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextToken :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Text)
ltfrNextToken = Lens.lens (nextToken :: ListTagsForResource -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResource)
{-# DEPRECATED ltfrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource. Tagging is only supported for directories.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceARN = Lens.lens (resourceARN :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The @MaxResults@ parameter sets the maximum number of results returned in a single page. This is for future use and is not supported currently.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrMaxResults :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Natural)
ltfrMaxResults = Lens.lens (maxResults :: ListTagsForResource -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTagsForResource)
{-# DEPRECATED ltfrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTagsForResource where
  page rq rs
    | Page.stop (rs Lens.^. ltfrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltfrrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltfrNextToken Lens..~ rs Lens.^. ltfrrsNextToken

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ResourceArn" Lude..= resourceARN),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/tags"

instance Lude.ToQuery ListTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of tag key value pairs that are associated with the response.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is null when there are no more results to return.
-- * 'tags' - A list of tag key value pairs that are associated with the response.
-- * 'responseStatus' - The response status code.
mkListTagsForResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsNextToken :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe Lude.Text)
ltfrrsNextToken = Lens.lens (nextToken :: ListTagsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of tag key value pairs that are associated with the response.
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
