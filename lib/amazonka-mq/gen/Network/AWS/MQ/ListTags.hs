{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.ListTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for a resource.
module Network.AWS.MQ.ListTags
  ( -- * Creating a request
    ListTags (..),
    mkListTags,

    -- ** Request lenses
    ltResourceARN,

    -- * Destructuring the response
    ListTagsResponse (..),
    mkListTagsResponse,

    -- ** Response lenses
    ltrsTags,
    ltrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTags' smart constructor.
newtype ListTags = ListTags'
  { -- | The Amazon Resource Name (ARN) of the resource tag.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTags' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource tag.
mkListTags ::
  -- | 'resourceARN'
  Lude.Text ->
  ListTags
mkListTags pResourceARN_ = ListTags' {resourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the resource tag.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltResourceARN :: Lens.Lens' ListTags Lude.Text
ltResourceARN = Lens.lens (resourceARN :: ListTags -> Lude.Text) (\s a -> s {resourceARN = a} :: ListTags)
{-# DEPRECATED ltResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest ListTags where
  type Rs ListTags = ListTagsResponse
  request = Req.get mqService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsResponse'
            Lude.<$> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListTags where
  toPath ListTags' {..} =
    Lude.mconcat ["/v1/tags/", Lude.toBS resourceARN]

instance Lude.ToQuery ListTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListTagsResponse' smart constructor.
data ListTagsResponse = ListTagsResponse'
  { -- | The key-value pair for the resource tag.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsResponse' with the minimum fields required to make a request.
--
-- * 'tags' - The key-value pair for the resource tag.
-- * 'responseStatus' - The response status code.
mkListTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsResponse
mkListTagsResponse pResponseStatus_ =
  ListTagsResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The key-value pair for the resource tag.
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
