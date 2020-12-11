{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels project. To delete a project you must first delete all models associated with the project. To delete a model, see 'DeleteProjectVersion' .
--
-- This operation requires permissions to perform the @rekognition:DeleteProject@ action.
module Network.AWS.Rekognition.DeleteProject
  ( -- * Creating a request
    DeleteProject (..),
    mkDeleteProject,

    -- ** Request lenses
    dpProjectARN,

    -- * Destructuring the response
    DeleteProjectResponse (..),
    mkDeleteProjectResponse,

    -- ** Response lenses
    dprsStatus,
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject' {projectARN :: Lude.Text}
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
-- * 'projectARN' - The Amazon Resource Name (ARN) of the project that you want to delete.
mkDeleteProject ::
  -- | 'projectARN'
  Lude.Text ->
  DeleteProject
mkDeleteProject pProjectARN_ =
  DeleteProject' {projectARN = pProjectARN_}

-- | The Amazon Resource Name (ARN) of the project that you want to delete.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProjectARN :: Lens.Lens' DeleteProject Lude.Text
dpProjectARN = Lens.lens (projectARN :: DeleteProject -> Lude.Text) (\s a -> s {projectARN = a} :: DeleteProject)
{-# DEPRECATED dpProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

instance Lude.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = Req.postJSON rekognitionService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Lude.<$> (x Lude..?> "Status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("RekognitionService.DeleteProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ProjectArn" Lude..= projectARN)])

instance Lude.ToPath DeleteProject where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { status ::
      Lude.Maybe ProjectStatus,
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

-- | Creates a value of 'DeleteProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The current status of the delete project operation.
mkDeleteProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProjectResponse
mkDeleteProjectResponse pResponseStatus_ =
  DeleteProjectResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the delete project operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsStatus :: Lens.Lens' DeleteProjectResponse (Lude.Maybe ProjectStatus)
dprsStatus = Lens.lens (status :: DeleteProjectResponse -> Lude.Maybe ProjectStatus) (\s a -> s {status = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeleteProjectResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeleteProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
