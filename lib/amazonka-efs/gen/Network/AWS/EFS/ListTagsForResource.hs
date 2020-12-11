{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags for a top-level EFS resource. You must provide the ID of the resource that you want to retrieve the tags for.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeAccessPoints@ action.
module Network.AWS.EFS.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrNextToken,
    ltfrMaxResults,
    ltfrResourceId,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsNextToken,
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    resourceId :: Lude.Text
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
-- * 'maxResults' - (Optional) Specifies the maximum number of tag objects to return in the response. The default value is 100.
-- * 'nextToken' - You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions if the response payload was paginated.
-- * 'resourceId' - Specifies the EFS resource you want to retrieve tags for. You can retrieve tags for EFS file systems and access points using this API endpoint.
mkListTagsForResource ::
  -- | 'resourceId'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceId_ =
  ListTagsForResource'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      resourceId = pResourceId_
    }

-- | You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions if the response payload was paginated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextToken :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Text)
ltfrNextToken = Lens.lens (nextToken :: ListTagsForResource -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResource)
{-# DEPRECATED ltfrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) Specifies the maximum number of tag objects to return in the response. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrMaxResults :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Natural)
ltfrMaxResults = Lens.lens (maxResults :: ListTagsForResource -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListTagsForResource)
{-# DEPRECATED ltfrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies the EFS resource you want to retrieve tags for. You can retrieve tags for EFS file systems and access points using this API endpoint.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceId :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceId = Lens.lens (resourceId :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceId = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.get efsService
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

instance Lude.ToPath ListTagsForResource where
  toPath ListTagsForResource' {..} =
    Lude.mconcat ["/2015-02-01/resource-tags/", Lude.toBS resourceId]

instance Lude.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Lude.mconcat
      ["NextToken" Lude.=: nextToken, "MaxResults" Lude.=: maxResults]

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
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

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - @NextToken@ is present if the response payload is paginated. You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions.
-- * 'responseStatus' - The response status code.
-- * 'tags' - An array of the tags for the specified EFS resource.
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

-- | @NextToken@ is present if the response payload is paginated. You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsNextToken :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe Lude.Text)
ltfrrsNextToken = Lens.lens (nextToken :: ListTagsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An array of the tags for the specified EFS resource.
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
