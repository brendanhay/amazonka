{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.TagProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a project.
module Network.AWS.CodeStar.TagProject
  ( -- * Creating a request
    TagProject (..),
    mkTagProject,

    -- ** Request lenses
    tpId,
    tpTags,

    -- * Destructuring the response
    TagProjectResponse (..),
    mkTagProjectResponse,

    -- ** Response lenses
    tprsTags,
    tprsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkTagProject' smart constructor.
data TagProject = TagProject'
  { id :: Lude.Text,
    tags :: Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TagProject' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the project you want to add a tag to.
-- * 'tags' - The tags you want to add to the project.
mkTagProject ::
  -- | 'id'
  Lude.Text ->
  TagProject
mkTagProject pId_ = TagProject' {id = pId_, tags = Lude.mempty}

-- | The ID of the project you want to add a tag to.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpId :: Lens.Lens' TagProject Lude.Text
tpId = Lens.lens (id :: TagProject -> Lude.Text) (\s a -> s {id = a} :: TagProject)
{-# DEPRECATED tpId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The tags you want to add to the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpTags :: Lens.Lens' TagProject (Lude.HashMap Lude.Text (Lude.Text))
tpTags = Lens.lens (tags :: TagProject -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {tags = a} :: TagProject)
{-# DEPRECATED tpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest TagProject where
  type Rs TagProject = TagProjectResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          TagProjectResponse'
            Lude.<$> (x Lude..?> "tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TagProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.TagProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TagProject where
  toJSON TagProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("id" Lude..= id), Lude.Just ("tags" Lude..= tags)]
      )

instance Lude.ToPath TagProject where
  toPath = Lude.const "/"

instance Lude.ToQuery TagProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTagProjectResponse' smart constructor.
data TagProjectResponse = TagProjectResponse'
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

-- | Creates a value of 'TagProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tags' - The tags for the project.
mkTagProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TagProjectResponse
mkTagProjectResponse pResponseStatus_ =
  TagProjectResponse'
    { tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The tags for the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tprsTags :: Lens.Lens' TagProjectResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tprsTags = Lens.lens (tags :: TagProjectResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: TagProjectResponse)
{-# DEPRECATED tprsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tprsResponseStatus :: Lens.Lens' TagProjectResponse Lude.Int
tprsResponseStatus = Lens.lens (responseStatus :: TagProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TagProjectResponse)
{-# DEPRECATED tprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
