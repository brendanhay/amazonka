{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of tags associated with a resource.
module Network.AWS.Glue.GetTags
  ( -- * Creating a request
    GetTags (..),
    mkGetTags,

    -- ** Request lenses
    gtResourceARN,

    -- * Destructuring the response
    GetTagsResponse (..),
    mkGetTagsResponse,

    -- ** Response lenses
    gtrsTags,
    gtrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTags' smart constructor.
newtype GetTags = GetTags'
  { -- | The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
    resourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
mkGetTags ::
  -- | 'resourceARN'
  Lude.Text ->
  GetTags
mkGetTags pResourceARN_ = GetTags' {resourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the resource for which to retrieve tags.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtResourceARN :: Lens.Lens' GetTags Lude.Text
gtResourceARN = Lens.lens (resourceARN :: GetTags -> Lude.Text) (\s a -> s {resourceARN = a} :: GetTags)
{-# DEPRECATED gtResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Lude.<$> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTags where
  toJSON GetTags' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ResourceArn" Lude..= resourceARN)])

instance Lude.ToPath GetTags where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The requested tags.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- * 'tags' - The requested tags.
-- * 'responseStatus' - The response status code.
mkGetTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTagsResponse
mkGetTagsResponse pResponseStatus_ =
  GetTagsResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTags :: Lens.Lens' GetTagsResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gtrsTags = Lens.lens (tags :: GetTagsResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: GetTagsResponse)
{-# DEPRECATED gtrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTagsResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTagsResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
