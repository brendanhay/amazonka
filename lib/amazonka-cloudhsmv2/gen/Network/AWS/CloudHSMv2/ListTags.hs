{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of tags for the specified AWS CloudHSM cluster.
--
-- This is a paginated operation, which means that each response might contain only a subset of all the tags. When the response contains only a subset of tags, it includes a @NextToken@ value. Use this value in a subsequent @ListTags@ request to get more tags. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more tags to get.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.ListTags
  ( -- * Creating a request
    ListTags (..),
    mkListTags,

    -- ** Request lenses
    ltResourceId,
    ltNextToken,
    ltMaxResults,

    -- * Destructuring the response
    ListTagsResponse (..),
    mkListTagsResponse,

    -- ** Response lenses
    ltrsTagList,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { -- | The cluster identifier (ID) for the cluster whose tags you are getting. To find the cluster ID, use 'DescribeClusters' .
    resourceId :: Lude.Text,
    -- | The @NextToken@ value that you received in the previous response. Use this value to get more tags.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of tags to return in the response. When there are more tags than the number you specify, the response contains a @NextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - The cluster identifier (ID) for the cluster whose tags you are getting. To find the cluster ID, use 'DescribeClusters' .
-- * 'nextToken' - The @NextToken@ value that you received in the previous response. Use this value to get more tags.
-- * 'maxResults' - The maximum number of tags to return in the response. When there are more tags than the number you specify, the response contains a @NextToken@ value.
mkListTags ::
  -- | 'resourceId'
  Lude.Text ->
  ListTags
mkListTags pResourceId_ =
  ListTags'
    { resourceId = pResourceId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The cluster identifier (ID) for the cluster whose tags you are getting. To find the cluster ID, use 'DescribeClusters' .
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltResourceId :: Lens.Lens' ListTags Lude.Text
ltResourceId = Lens.lens (resourceId :: ListTags -> Lude.Text) (\s a -> s {resourceId = a} :: ListTags)
{-# DEPRECATED ltResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The @NextToken@ value that you received in the previous response. Use this value to get more tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTags (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTags -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTags)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of tags to return in the response. When there are more tags than the number you specify, the response contains a @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTags (Lude.Maybe Lude.Natural)
ltMaxResults = Lens.lens (maxResults :: ListTags -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTags)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTags where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTagList) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTags where
  type Rs ListTags = ListTagsResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Lude.<$> (x Lude..?> "TagList" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.ListTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTags where
  toJSON ListTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTags where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | A list of tags.
    tagList :: [Tag],
    -- | An opaque string that indicates that the response contains only a subset of tags. Use this value in a subsequent @ListTags@ request to get more tags.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- * 'tagList' - A list of tags.
-- * 'nextToken' - An opaque string that indicates that the response contains only a subset of tags. Use this value in a subsequent @ListTags@ request to get more tags.
-- * 'responseStatus' - The response status code.
mkListTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsResponse
mkListTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { tagList = Lude.mempty,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTagList :: Lens.Lens' ListTagsResponse [Tag]
ltrsTagList = Lens.lens (tagList :: ListTagsResponse -> [Tag]) (\s a -> s {tagList = a} :: ListTagsResponse)
{-# DEPRECATED ltrsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | An opaque string that indicates that the response contains only a subset of tags. Use this value in a subsequent @ListTags@ request to get more tags.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTagsResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTagsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
