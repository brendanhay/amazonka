{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'Tags' collection for a given resource.
module Network.AWS.APIGateway.GetTags
  ( -- * Creating a request
    GetTags (..),
    mkGetTags,

    -- ** Request lenses
    gtLimit,
    gtPosition,
    gtResourceARN,

    -- * Destructuring the response
    GetTagsResponse (..),
    mkGetTagsResponse,

    -- ** Response lenses
    gtrsTags,
    gtrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Gets the 'Tags' collection for a given resource.
--
-- /See:/ 'mkGetTags' smart constructor.
data GetTags = GetTags'
  { limit :: Lude.Maybe Lude.Int,
    position :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- * 'limit' - (Not currently supported) The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
-- * 'position' - (Not currently supported) The current pagination position in the paged result set.
-- * 'resourceARN' - [Required] The ARN of a resource that can be tagged.
mkGetTags ::
  -- | 'resourceARN'
  Lude.Text ->
  GetTags
mkGetTags pResourceARN_ =
  GetTags'
    { limit = Lude.Nothing,
      position = Lude.Nothing,
      resourceARN = pResourceARN_
    }

-- | (Not currently supported) The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtLimit :: Lens.Lens' GetTags (Lude.Maybe Lude.Int)
gtLimit = Lens.lens (limit :: GetTags -> Lude.Maybe Lude.Int) (\s a -> s {limit = a} :: GetTags)
{-# DEPRECATED gtLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | (Not currently supported) The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtPosition :: Lens.Lens' GetTags (Lude.Maybe Lude.Text)
gtPosition = Lens.lens (position :: GetTags -> Lude.Maybe Lude.Text) (\s a -> s {position = a} :: GetTags)
{-# DEPRECATED gtPosition "Use generic-lens or generic-optics with 'position' instead." #-}

-- | [Required] The ARN of a resource that can be tagged.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtResourceARN :: Lens.Lens' GetTags Lude.Text
gtResourceARN = Lens.lens (resourceARN :: GetTags -> Lude.Text) (\s a -> s {resourceARN = a} :: GetTags)
{-# DEPRECATED gtResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

instance Lude.AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Lude.<$> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetTags where
  toPath GetTags' {..} =
    Lude.mconcat ["/tags/", Lude.toBS resourceARN]

instance Lude.ToQuery GetTags where
  toQuery GetTags' {..} =
    Lude.mconcat ["limit" Lude.=: limit, "position" Lude.=: position]

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { tags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
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

-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
mkGetTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTagsResponse
mkGetTagsResponse pResponseStatus_ =
  GetTagsResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The collection of tags. Each tag element is associated with a given resource.
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
