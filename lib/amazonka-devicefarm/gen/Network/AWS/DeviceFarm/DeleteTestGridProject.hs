{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteTestGridProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Selenium testing project and all content generated under it.
--
-- /Important:/ You cannot undo this operation.
module Network.AWS.DeviceFarm.DeleteTestGridProject
  ( -- * Creating a request
    DeleteTestGridProject (..),
    mkDeleteTestGridProject,

    -- ** Request lenses
    dtgpProjectARN,

    -- * Destructuring the response
    DeleteTestGridProjectResponse (..),
    mkDeleteTestGridProjectResponse,

    -- ** Response lenses
    dtgprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTestGridProject' smart constructor.
newtype DeleteTestGridProject = DeleteTestGridProject'
  { projectARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTestGridProject' with the minimum fields required to make a request.
--
-- * 'projectARN' - The ARN of the project to delete, from 'CreateTestGridProject' or 'ListTestGridProjects' .
mkDeleteTestGridProject ::
  -- | 'projectARN'
  Lude.Text ->
  DeleteTestGridProject
mkDeleteTestGridProject pProjectARN_ =
  DeleteTestGridProject' {projectARN = pProjectARN_}

-- | The ARN of the project to delete, from 'CreateTestGridProject' or 'ListTestGridProjects' .
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgpProjectARN :: Lens.Lens' DeleteTestGridProject Lude.Text
dtgpProjectARN = Lens.lens (projectARN :: DeleteTestGridProject -> Lude.Text) (\s a -> s {projectARN = a} :: DeleteTestGridProject)
{-# DEPRECATED dtgpProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

instance Lude.AWSRequest DeleteTestGridProject where
  type Rs DeleteTestGridProject = DeleteTestGridProjectResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTestGridProjectResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTestGridProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.DeleteTestGridProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTestGridProject where
  toJSON DeleteTestGridProject' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("projectArn" Lude..= projectARN)])

instance Lude.ToPath DeleteTestGridProject where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTestGridProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTestGridProjectResponse' smart constructor.
newtype DeleteTestGridProjectResponse = DeleteTestGridProjectResponse'
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

-- | Creates a value of 'DeleteTestGridProjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTestGridProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTestGridProjectResponse
mkDeleteTestGridProjectResponse pResponseStatus_ =
  DeleteTestGridProjectResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgprsResponseStatus :: Lens.Lens' DeleteTestGridProjectResponse Lude.Int
dtgprsResponseStatus = Lens.lens (responseStatus :: DeleteTestGridProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTestGridProjectResponse)
{-# DEPRECATED dtgprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
