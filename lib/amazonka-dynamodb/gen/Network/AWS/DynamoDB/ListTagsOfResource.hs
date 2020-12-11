{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListTagsOfResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all tags on an Amazon DynamoDB resource. You can call ListTagsOfResource up to 10 times per second, per account.
--
-- For an overview on tagging DynamoDB resources, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> in the /Amazon DynamoDB Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListTagsOfResource
  ( -- * Creating a request
    ListTagsOfResource (..),
    mkListTagsOfResource,

    -- ** Request lenses
    ltorNextToken,
    ltorResourceARN,

    -- * Destructuring the response
    ListTagsOfResourceResponse (..),
    mkListTagsOfResourceResponse,

    -- ** Response lenses
    ltorrsNextToken,
    ltorrsTags,
    ltorrsResponseStatus,
  )
where

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsOfResource' smart constructor.
data ListTagsOfResource = ListTagsOfResource'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTagsOfResource' with the minimum fields required to make a request.
--
-- * 'nextToken' - An optional string that, if supplied, must be copied from the output of a previous call to ListTagOfResource. When provided in this manner, this API fetches the next page of results.
-- * 'resourceARN' - The Amazon DynamoDB resource with tags to be listed. This value is an Amazon Resource Name (ARN).
mkListTagsOfResource ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTagsOfResource
mkListTagsOfResource pResourceARN_ =
  ListTagsOfResource'
    { nextToken = Lude.Nothing,
      resourceARN = pResourceARN_
    }

-- | An optional string that, if supplied, must be copied from the output of a previous call to ListTagOfResource. When provided in this manner, this API fetches the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorNextToken :: Lens.Lens' ListTagsOfResource (Lude.Maybe Lude.Text)
ltorNextToken = Lens.lens (nextToken :: ListTagsOfResource -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsOfResource)
{-# DEPRECATED ltorNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon DynamoDB resource with tags to be listed. This value is an Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorResourceARN :: Lens.Lens' ListTagsOfResource Lude.Text
ltorResourceARN = Lens.lens (resourceARN :: ListTagsOfResource -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTagsOfResource)
{-# DEPRECATED ltorResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Page.AWSPager ListTagsOfResource where
  page rq rs
    | Page.stop (rs Lens.^. ltorrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltorrsTags) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltorNextToken Lens..~ rs Lens.^. ltorrsNextToken

instance Lude.AWSRequest ListTagsOfResource where
  type Rs ListTagsOfResource = ListTagsOfResourceResponse
  request = Req.postJSON dynamoDBService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsOfResourceResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsOfResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DynamoDB_20120810.ListTagsOfResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsOfResource where
  toJSON ListTagsOfResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ResourceArn" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath ListTagsOfResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsOfResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsOfResourceResponse' smart constructor.
data ListTagsOfResourceResponse = ListTagsOfResourceResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListTagsOfResourceResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If this value is returned, there are additional results to be displayed. To retrieve them, call ListTagsOfResource again, with NextToken set to this value.
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags currently associated with the Amazon DynamoDB resource.
mkListTagsOfResourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsOfResourceResponse
mkListTagsOfResourceResponse pResponseStatus_ =
  ListTagsOfResourceResponse'
    { nextToken = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If this value is returned, there are additional results to be displayed. To retrieve them, call ListTagsOfResource again, with NextToken set to this value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorrsNextToken :: Lens.Lens' ListTagsOfResourceResponse (Lude.Maybe Lude.Text)
ltorrsNextToken = Lens.lens (nextToken :: ListTagsOfResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsOfResourceResponse)
{-# DEPRECATED ltorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The tags currently associated with the Amazon DynamoDB resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorrsTags :: Lens.Lens' ListTagsOfResourceResponse (Lude.Maybe [Tag])
ltorrsTags = Lens.lens (tags :: ListTagsOfResourceResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ListTagsOfResourceResponse)
{-# DEPRECATED ltorrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltorrsResponseStatus :: Lens.Lens' ListTagsOfResourceResponse Lude.Int
ltorrsResponseStatus = Lens.lens (responseStatus :: ListTagsOfResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsOfResourceResponse)
{-# DEPRECATED ltorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
