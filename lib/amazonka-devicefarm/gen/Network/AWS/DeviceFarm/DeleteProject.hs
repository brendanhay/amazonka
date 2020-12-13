{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Device Farm project, given the project ARN.
--
-- Deleting this resource does not stop an in-progress run.
module Network.AWS.DeviceFarm.DeleteProject
  ( -- * Creating a request
    DeleteProject (..),
    mkDeleteProject,

    -- ** Request lenses
    dpfArn,

    -- * Destructuring the response
    DeleteProjectResponse (..),
    mkDeleteProjectResponse,

    -- ** Response lenses
    dprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the delete project operation.
--
-- /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm project to delete.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProject' with the minimum fields required to make a request.
--
-- * 'arn' - Represents the Amazon Resource Name (ARN) of the Device Farm project to delete.
mkDeleteProject ::
  -- | 'arn'
  Lude.Text ->
  DeleteProject
mkDeleteProject pArn_ = DeleteProject' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm project to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfArn :: Lens.Lens' DeleteProject Lude.Text
dpfArn = Lens.lens (arn :: DeleteProject -> Lude.Text) (\s a -> s {arn = a} :: DeleteProject)
{-# DEPRECATED dpfArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = Req.postJSON deviceFarmService
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
              Lude.=# ("DeviceFarm_20150623.DeleteProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteProject where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProject where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a delete project request.
--
-- /See:/ 'mkDeleteProjectResponse' smart constructor.
newtype DeleteProjectResponse = DeleteProjectResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
