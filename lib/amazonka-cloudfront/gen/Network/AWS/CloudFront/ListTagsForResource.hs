{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List tags for a CloudFront resource.
module Network.AWS.CloudFront.ListTagsForResource
  ( -- * Creating a request
    ListTagsForResource (..),
    mkListTagsForResource,

    -- ** Request lenses
    ltfrResource,

    -- * Destructuring the response
    ListTagsForResourceResponse (..),
    mkListTagsForResourceResponse,

    -- ** Response lenses
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request to list tags for a CloudFront resource.
--
-- /See:/ 'mkListTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
  { -- | An ARN of a CloudFront resource.
    resource :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- * 'resource' - An ARN of a CloudFront resource.
mkListTagsForResource ::
  -- | 'resource'
  Lude.Text ->
  ListTagsForResource
mkListTagsForResource pResource_ =
  ListTagsForResource' {resource = pResource_}

-- | An ARN of a CloudFront resource.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrResource :: Lens.Lens' ListTagsForResource Lude.Text
ltfrResource = Lens.lens (resource :: ListTagsForResource -> Lude.Text) (\s a -> s {resource = a} :: ListTagsForResource)
{-# DEPRECATED ltfrResource "Use generic-lens or generic-optics with 'resource' instead." #-}

instance Lude.AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListTagsForResourceResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTagsForResource where
  toPath = Lude.const "/2020-05-31/tagging"

instance Lude.ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    Lude.mconcat ["Resource" Lude.=: resource]

-- | The returned result of the corresponding request.
--
-- /See:/ 'mkListTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { -- | A complex type that contains zero or more @Tag@ elements.
    tags :: Tags,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- * 'tags' - A complex type that contains zero or more @Tag@ elements.
-- * 'responseStatus' - The response status code.
mkListTagsForResourceResponse ::
  -- | 'tags'
  Tags ->
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForResourceResponse
mkListTagsForResourceResponse pTags_ pResponseStatus_ =
  ListTagsForResourceResponse'
    { tags = pTags_,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains zero or more @Tag@ elements.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsTags :: Lens.Lens' ListTagsForResourceResponse Tags
ltfrrsTags = Lens.lens (tags :: ListTagsForResourceResponse -> Tags) (\s a -> s {tags = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfrrsResponseStatus :: Lens.Lens' ListTagsForResourceResponse Lude.Int
ltfrrsResponseStatus = Lens.lens (responseStatus :: ListTagsForResourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForResourceResponse)
{-# DEPRECATED ltfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
