{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UntagProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a project.
module Network.AWS.CodeStar.UntagProject
  ( -- * Creating a request
    UntagProject (..),
    mkUntagProject,

    -- ** Request lenses
    uId,
    uTags,

    -- * Destructuring the response
    UntagProjectResponse (..),
    mkUntagProjectResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUntagProject' smart constructor.
data UntagProject = UntagProject'
  { id :: Lude.Text,
    tags :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UntagProject' with the minimum fields required to make a request.
--
-- * 'id' - The ID of the project to remove tags from.
-- * 'tags' - The tags to remove from the project.
mkUntagProject ::
  -- | 'id'
  Lude.Text ->
  UntagProject
mkUntagProject pId_ = UntagProject' {id = pId_, tags = Lude.mempty}

-- | The ID of the project to remove tags from.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uId :: Lens.Lens' UntagProject Lude.Text
uId = Lens.lens (id :: UntagProject -> Lude.Text) (\s a -> s {id = a} :: UntagProject)
{-# DEPRECATED uId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The tags to remove from the project.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uTags :: Lens.Lens' UntagProject [Lude.Text]
uTags = Lens.lens (tags :: UntagProject -> [Lude.Text]) (\s a -> s {tags = a} :: UntagProject)
{-# DEPRECATED uTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest UntagProject where
  type Rs UntagProject = UntagProjectResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UntagProjectResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UntagProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.UntagProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UntagProject where
  toJSON UntagProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("id" Lude..= id), Lude.Just ("tags" Lude..= tags)]
      )

instance Lude.ToPath UntagProject where
  toPath = Lude.const "/"

instance Lude.ToQuery UntagProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUntagProjectResponse' smart constructor.
newtype UntagProjectResponse = UntagProjectResponse'
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

-- | Creates a value of 'UntagProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUntagProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UntagProjectResponse
mkUntagProjectResponse pResponseStatus_ =
  UntagProjectResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UntagProjectResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UntagProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UntagProjectResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
