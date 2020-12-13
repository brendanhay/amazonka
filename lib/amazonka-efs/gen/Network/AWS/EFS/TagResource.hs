{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.TagResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a tag for an EFS resource. You can create tags for EFS file systems and access points using this API operation.
--
-- This operation requires permissions for the @elasticfilesystem:TagResource@ action.
module Network.AWS.EFS.TagResource
  ( -- * Creating a request
    TagResource (..),
    mkTagResource,

    -- ** Request lenses
    trResourceId,
    trTags,

    -- * Destructuring the response
    TagResourceResponse (..),
    mkTagResourceResponse,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagResource' smart constructor.
data TagResource = TagResource'
  { -- | The ID specifying the EFS resource that you want to create a tag for.
    resourceId :: Lude.Text,
    -- |
    tags :: [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID specifying the EFS resource that you want to create a tag for.
-- * 'tags' -
mkTagResource ::
  -- | 'resourceId'
  Lude.Text ->
  TagResource
mkTagResource pResourceId_ =
  TagResource' {resourceId = pResourceId_, tags = Lude.mempty}

-- | The ID specifying the EFS resource that you want to create a tag for.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trResourceId :: Lens.Lens' TagResource Lude.Text
trResourceId = Lens.lens (resourceId :: TagResource -> Lude.Text) (\s a -> s {resourceId = a} :: TagResource)
{-# DEPRECATED trResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- |
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trTags :: Lens.Lens' TagResource [Tag]
trTags = Lens.lens (tags :: TagResource -> [Tag]) (\s a -> s {tags = a} :: TagResource)
{-# DEPRECATED trTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagResource where
  type Rs TagResource = TagResourceResponse
  request = Req.postJSON efsService
  response = Res.receiveNull TagResourceResponse'

instance Lude.ToHeaders TagResource where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON TagResource where
  toJSON TagResource' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Tags" Lude..= tags)])

instance Lude.ToPath TagResource where
  toPath TagResource' {..} =
    Lude.mconcat ["/2015-02-01/resource-tags/", Lude.toBS resourceId]

instance Lude.ToQuery TagResource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagResourceResponse' smart constructor.
data TagResourceResponse = TagResourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
mkTagResourceResponse ::
  TagResourceResponse
mkTagResourceResponse = TagResourceResponse'
