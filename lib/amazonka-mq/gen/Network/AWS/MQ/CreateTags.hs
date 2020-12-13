{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add a tag to a resource.
module Network.AWS.MQ.CreateTags
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
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A map of the key-value pairs for the resource tag.
--
-- /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { -- | The Amazon Resource Name (ARN) of the resource tag.
    resourceARN :: Lude.Text,
    -- | The key-value pair for the resource tag.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The Amazon Resource Name (ARN) of the resource tag.
-- * 'tags' - The key-value pair for the resource tag.
mkCreateTags ::
  -- | 'resourceARN'
  Lude.Text ->
  CreateTags
mkCreateTags pResourceARN_ =
  CreateTags' {resourceARN = pResourceARN_, tags = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the resource tag.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceARN :: Lens.Lens' CreateTags Lude.Text
ctResourceARN = Lens.lens (resourceARN :: CreateTags -> Lude.Text) (\s a -> s {resourceARN = a} :: CreateTags)
{-# DEPRECATED ctResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The key-value pair for the resource tag.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ctTags = Lens.lens (tags :: CreateTags -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateTags)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Req.postJSON mqService
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
    Lude.mconcat ["/v1/tags/", Lude.toBS resourceARN]

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
