{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.GetTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tags that are associated with a resource group, specified by an ARN.
module Network.AWS.ResourceGroups.GetTags
  ( -- * Creating a request
    GetTags (..),
    mkGetTags,

    -- ** Request lenses
    gtARN,

    -- * Destructuring the response
    GetTagsResponse (..),
    mkGetTagsResponse,

    -- ** Response lenses
    gtrsARN,
    gtrsTags,
    gtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroups.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTags' smart constructor.
newtype GetTags = GetTags'
  { -- | The ARN of the resource group whose tags you want to retrieve.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTags' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the resource group whose tags you want to retrieve.
mkGetTags ::
  -- | 'arn'
  Lude.Text ->
  GetTags
mkGetTags pARN_ = GetTags' {arn = pARN_}

-- | The ARN of the resource group whose tags you want to retrieve.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtARN :: Lens.Lens' GetTags Lude.Text
gtARN = Lens.lens (arn :: GetTags -> Lude.Text) (\s a -> s {arn = a} :: GetTags)
{-# DEPRECATED gtARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetTags where
  type Rs GetTags = GetTagsResponse
  request = Req.get resourceGroupsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTagsResponse'
            Lude.<$> (x Lude..?> "Arn")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTags where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTags where
  toPath GetTags' {..} =
    Lude.mconcat ["/resources/", Lude.toBS arn, "/tags"]

instance Lude.ToQuery GetTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTagsResponse' smart constructor.
data GetTagsResponse = GetTagsResponse'
  { -- | The ARN of the tagged resource group.
    arn :: Lude.Maybe Lude.Text,
    -- | The tags associated with the specified resource group.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTagsResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the tagged resource group.
-- * 'tags' - The tags associated with the specified resource group.
-- * 'responseStatus' - The response status code.
mkGetTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTagsResponse
mkGetTagsResponse pResponseStatus_ =
  GetTagsResponse'
    { arn = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the tagged resource group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsARN :: Lens.Lens' GetTagsResponse (Lude.Maybe Lude.Text)
gtrsARN = Lens.lens (arn :: GetTagsResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: GetTagsResponse)
{-# DEPRECATED gtrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The tags associated with the specified resource group.
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
