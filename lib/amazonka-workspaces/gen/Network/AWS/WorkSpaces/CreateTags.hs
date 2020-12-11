{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the specified tags for the specified WorkSpaces resource.
module Network.AWS.WorkSpaces.CreateTags
  ( -- * Creating a request
    CreateTags (..),
    mkCreateTags,

    -- ** Request lenses
    ctResourceId,
    ctTags,

    -- * Destructuring the response
    CreateTagsResponse (..),
    mkCreateTagsResponse,

    -- ** Response lenses
    ctrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { resourceId :: Lude.Text,
    tags :: [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- * 'resourceId' - The identifier of the WorkSpaces resource. The supported resource types are WorkSpaces, registered directories, images, custom bundles, IP access control groups, and connection aliases.
-- * 'tags' - The tags. Each WorkSpaces resource can have a maximum of 50 tags.
mkCreateTags ::
  -- | 'resourceId'
  Lude.Text ->
  CreateTags
mkCreateTags pResourceId_ =
  CreateTags' {resourceId = pResourceId_, tags = Lude.mempty}

-- | The identifier of the WorkSpaces resource. The supported resource types are WorkSpaces, registered directories, images, custom bundles, IP access control groups, and connection aliases.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceId :: Lens.Lens' CreateTags Lude.Text
ctResourceId = Lens.lens (resourceId :: CreateTags -> Lude.Text) (\s a -> s {resourceId = a} :: CreateTags)
{-# DEPRECATED ctResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The tags. Each WorkSpaces resource can have a maximum of 50 tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Tag]
ctTags = Lens.lens (tags :: CreateTags -> [Tag]) (\s a -> s {tags = a} :: CreateTags)
{-# DEPRECATED ctTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateTags where
  type Rs CreateTags = CreateTagsResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateTagsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTags where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.CreateTags" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTags where
  toJSON CreateTags' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceId" Lude..= resourceId),
            Lude.Just ("Tags" Lude..= tags)
          ]
      )

instance Lude.ToPath CreateTags where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTags where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
newtype CreateTagsResponse = CreateTagsResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateTagsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTagsResponse
mkCreateTagsResponse pResponseStatus_ =
  CreateTagsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrsResponseStatus :: Lens.Lens' CreateTagsResponse Lude.Int
ctrsResponseStatus = Lens.lens (responseStatus :: CreateTagsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTagsResponse)
{-# DEPRECATED ctrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
