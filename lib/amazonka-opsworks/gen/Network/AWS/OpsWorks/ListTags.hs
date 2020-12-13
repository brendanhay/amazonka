{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are applied to the specified stack or layer.
module Network.AWS.OpsWorks.ListTags
  ( -- * Creating a request
    ListTags (..),
    mkListTags,

    -- ** Request lenses
    ltNextToken,
    ltResourceARN,
    ltMaxResults,

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
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTags' smart constructor.
data ListTags = ListTags'
  { -- | Do not use. A validation exception occurs if you add a @NextToken@ parameter to a @ListTagsRequest@ call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The stack or layer's Amazon Resource Number (ARN).
    resourceARN :: Lude.Text,
    -- | Do not use. A validation exception occurs if you add a @MaxResults@ parameter to a @ListTagsRequest@ call.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- * 'nextToken' - Do not use. A validation exception occurs if you add a @NextToken@ parameter to a @ListTagsRequest@ call.
-- * 'resourceARN' - The stack or layer's Amazon Resource Number (ARN).
-- * 'maxResults' - Do not use. A validation exception occurs if you add a @MaxResults@ parameter to a @ListTagsRequest@ call.
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

-- | Do not use. A validation exception occurs if you add a @NextToken@ parameter to a @ListTagsRequest@ call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTags (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTags -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTags)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The stack or layer's Amazon Resource Number (ARN).
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltResourceARN :: Lens.Lens' ListTags Lude.Text
ltResourceARN = Lens.lens (resourceARN :: ListTags -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTags)
{-# DEPRECATED ltResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Do not use. A validation exception occurs if you add a @MaxResults@ parameter to a @ListTagsRequest@ call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxResults :: Lens.Lens' ListTags (Lude.Maybe Lude.Int)
ltMaxResults = Lens.lens (maxResults :: ListTags -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListTags)
{-# DEPRECATED ltMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Lude.AWSRequest ListTags where
  type Rs ListTags = ListTagsResponse
  request = Req.postJSON opsWorksService
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
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.ListTags" :: Lude.ByteString),
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

-- | Contains the response to a @ListTags@ request.
--
-- /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to get the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | A set of key-value pairs that contain tag keys and tag values that are attached to a stack or layer.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to get the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
-- * 'tags' - A set of key-value pairs that contain tag keys and tag values that are attached to a stack or layer.
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

-- | If a paginated request does not return all of the remaining results, this parameter is set to a token that you can assign to the request object's @NextToken@ parameter to get the next set of results. If the previous paginated request returned all of the remaining results, this parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTagsResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A set of key-value pairs that contain tag keys and tag values that are attached to a stack or layer.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTags :: Lens.Lens' ListTagsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ltrsTags = Lens.lens (tags :: ListTagsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: ListTagsResponse)
{-# DEPRECATED ltrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTagsResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
