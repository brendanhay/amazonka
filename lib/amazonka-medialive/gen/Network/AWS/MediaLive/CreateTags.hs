{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create tags for a resource
module Network.AWS.MediaLive.CreateTags
  ( -- * Creating a request
    CreateTags (..),
    mkCreateTags,

    -- ** Request lenses
    ctResourceARN,
    ctTags,

    -- * Destructuring the response
    CreateTagsResponse (..),
    mkCreateTagsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for CreateTagsRequest
--
-- /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { resourceARN :: Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- * 'resourceARN' -
-- * 'tags' -
mkCreateTags ::
  -- | 'resourceARN'
  Lude.Text ->
  CreateTags
mkCreateTags pResourceARN_ =
  CreateTags' {resourceARN = pResourceARN_, tags = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceARN :: Lens.Lens' CreateTags Lude.Text
ctResourceARN = Lens.lens (resourceARN :: CreateTags -> Lude.Text) (\s a -> s {resourceARN = a} :: CreateTags)
{-# DEPRECATED ctResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ctTags = Lens.lens (tags :: CreateTags -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateTags)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Req.postJSON mediaLiveService
  response = Res.receiveNull CreateTagsResponse'

instance Lude.ToHeaders CreateTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTags where
  toJSON CreateTags' {..} =
    Lude.object (Lude.catMaybes [("tags" Lude..=) Lude.<$> tags])

instance Lude.ToPath CreateTags where
  toPath CreateTags' {..} =
    Lude.mconcat ["/prod/tags/", Lude.toBS resourceARN]

instance Lude.ToQuery CreateTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
mkCreateTagsResponse ::
  CreateTagsResponse
mkCreateTagsResponse = CreateTagsResponse'
