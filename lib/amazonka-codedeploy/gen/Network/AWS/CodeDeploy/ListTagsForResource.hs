{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags for the resource identified by a specified Amazon Resource Name (ARN). Tags are used to organize and categorize your CodeDeploy resources.
module Network.AWS.CodeDeploy.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrNextToken,
    ltfrResourceARN,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsNextToken,
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { -- | An identifier returned from the previous @ListTagsForResource@ call. It can be used to return the next set of applications in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The ARN of a CodeDeploy resource. @ListTagsForResource@ returns all the tags associated with the resource that is identified by the @ResourceArn@ .
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier returned from the previous @ListTagsForResource@ call. It can be used to return the next set of applications in the list.
-- * 'resourceARN' - The ARN of a CodeDeploy resource. @ListTagsForResource@ returns all the tags associated with the resource that is identified by the @ResourceArn@ .
mkListTagsForResource ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResourceARN_ =
  ListTagsForResource'
    { nextToken = Lude.Nothing,
      resourceARN = pResourceARN_
    }

-- | An identifier returned from the previous @ListTagsForResource@ call. It can be used to return the next set of applications in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrNextToken :: Lens.Lens' ListTagsForResource (Lude.Maybe Lude.Text)
ltfrNextToken = Lens.lens (nextToken :: ListTagsForResource -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResource)
{-# DEPRECATED ltfrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARN of a CodeDeploy resource. @ListTagsForResource@ returns all the tags associated with the resource that is identified by the @ResourceArn@ .
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResourceARN :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResourceARN = Lens.lens (resourceARN :: ListTagsForResource -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.postJSON codeDeployService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeDeploy_20141006.ListTagsForResource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForResource where
  toJSON ListTagsForResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            Lude.Just ("ResourceArn" Lude..= resourceARN)
          ]
      )

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list application revisions call to return the next set of application revisions in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of tags returned by @ListTagsForResource@ . The tags are associated with the resource identified by the input @ResourceArn@ parameter.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list application revisions call to return the next set of application revisions in the list.
-- * 'tags' - A list of tags returned by @ListTagsForResource@ . The tags are associated with the resource identified by the input @ResourceArn@ parameter.
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

-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list application revisions call to return the next set of application revisions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsNextToken :: Lens.Lens' ListTagsForResourceResponse (Lude.Maybe Lude.Text)
ltfrrsNextToken = Lens.lens (nextToken :: ListTagsForResourceResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of tags returned by @ListTagsForResource@ . The tags are associated with the resource identified by the input @ResourceArn@ parameter.
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
