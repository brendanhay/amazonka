{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tags for the specified Amazon SageMaker resource.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListTags
  ( -- * Creating a request
    ListTags (..),
    mkListTags,

    -- ** Request lenses
    ltsNextToken,
    ltsResourceARN,
    ltsMaxResults,

    -- * Destructuring the response
    ListTagsResponse (..),
    mkListTagsResponse,

    -- ** Response lenses
    ltrsNextToken,
    ltrsTags,
    ltrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { -- | If the response to the previous @ListTags@ request is truncated, Amazon SageMaker returns this token. To retrieve the next set of tags, use it in the subsequent request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the resource whose tags you want to retrieve.
    resourceARN :: Lude.Text,
    -- | Maximum number of tags to return.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the response to the previous @ListTags@ request is truncated, Amazon SageMaker returns this token. To retrieve the next set of tags, use it in the subsequent request.
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource whose tags you want to retrieve.
-- * 'maxResults' - Maximum number of tags to return.
mkListTags ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTags
mkListTags pResourceARN_ =
  ListTags'
    { nextToken = Lude.Nothing,
      resourceARN = pResourceARN_,
      maxResults = Lude.Nothing
    }

-- | If the response to the previous @ListTags@ request is truncated, Amazon SageMaker returns this token. To retrieve the next set of tags, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsNextToken :: Lens.Lens' ListTags (Lude.Maybe Lude.Text)
ltsNextToken = Lens.lens (nextToken :: ListTags -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTags)
{-# DEPRECATED ltsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the resource whose tags you want to retrieve.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsResourceARN :: Lens.Lens' ListTags Lude.Text
ltsResourceARN = Lens.lens (resourceARN :: ListTags -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTags)
{-# DEPRECATED ltsResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Maximum number of tags to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltsMaxResults :: Lens.Lens' ListTags (Lude.Maybe Lude.Natural)
ltsMaxResults = Lens.lens (maxResults :: ListTags -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTags)
{-# DEPRECATED ltsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListTags where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltsNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTags where
  type Rs ListTags = ListTagsResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("SageMaker.ListTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTags where
  toJSON ListTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ResourceArn" Lude..= resourceARN),
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListTags where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | If response is truncated, Amazon SageMaker includes a token in the response. You can use this token in your subsequent request to fetch next set of tokens.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An array of @Tag@ objects, each with a tag key and a value.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If response is truncated, Amazon SageMaker includes a token in the response. You can use this token in your subsequent request to fetch next set of tokens.
-- * 'tags' - An array of @Tag@ objects, each with a tag key and a value.
-- * 'responseStatus' - The response status code.
mkListTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsResponse
mkListTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If response is truncated, Amazon SageMaker includes a token in the response. You can use this token in your subsequent request to fetch next set of tokens.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTagsResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of @Tag@ objects, each with a tag key and a value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTags :: Lens.Lens' ListTagsResponse (Lude.Maybe [Tag])
ltrsTags = Lens.lens (tags :: ListTagsResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ListTagsResponse)
{-# DEPRECATED ltrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTagsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
