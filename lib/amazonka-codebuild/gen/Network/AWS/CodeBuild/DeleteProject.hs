{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a build project. When you delete a project, its builds are not deleted.
module Network.AWS.CodeBuild.DeleteProject
  ( -- * Creating a request
    DeleteProject (..),
    mkDeleteProject,

    -- ** Request lenses
    dpName,

    -- * Destructuring the response
    DeleteProjectResponse (..),
    mkDeleteProjectResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProject' with the minimum fields required to make a request.
--
-- * 'name' - The name of the build project.
mkDeleteProject ::
  -- | 'name'
  Lude.Text ->
  DeleteProject
mkDeleteProject pName_ = DeleteProject' {name = pName_}

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpName :: Lens.Lens' DeleteProject Lude.Text
dpName = Lens.lens (name :: DeleteProject -> Lude.Text) (\s a -> s {name = a} :: DeleteProject)
{-# DEPRECATED dpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProjectResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.DeleteProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("name" Lude..= name)])

instance Lude.ToPath DeleteProject where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProjectResponse' smart constructor.
newtype DeleteProjectResponse = DeleteProjectResponse'
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

-- | Creates a value of 'DeleteProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProjectResponse
mkDeleteProjectResponse pResponseStatus_ =
  DeleteProjectResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeleteProjectResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeleteProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
