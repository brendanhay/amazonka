{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delets a project in AWS Mobile Hub.
module Network.AWS.Mobile.DeleteProject
  ( -- * Creating a request
    DeleteProject (..),
    mkDeleteProject,

    -- ** Request lenses
    dpProjectId,

    -- * Destructuring the response
    DeleteProjectResponse (..),
    mkDeleteProjectResponse,

    -- ** Response lenses
    dprsDeletedResources,
    dprsOrphanedResources,
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request structure used to request a project be deleted.
--
-- /See:/ 'mkDeleteProject' smart constructor.
newtype DeleteProject = DeleteProject'
  { -- | Unique project identifier.
    projectId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProject' with the minimum fields required to make a request.
--
-- * 'projectId' - Unique project identifier.
mkDeleteProject ::
  -- | 'projectId'
  Lude.Text ->
  DeleteProject
mkDeleteProject pProjectId_ =
  DeleteProject' {projectId = pProjectId_}

-- | Unique project identifier.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpProjectId :: Lens.Lens' DeleteProject Lude.Text
dpProjectId = Lens.lens (projectId :: DeleteProject -> Lude.Text) (\s a -> s {projectId = a} :: DeleteProject)
{-# DEPRECATED dpProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Lude.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = Req.delete mobileService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Lude.<$> (x Lude..?> "deletedResources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "orphanedResources" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteProject where
  toPath DeleteProject' {..} =
    Lude.mconcat ["/projects/", Lude.toBS projectId]

instance Lude.ToQuery DeleteProject where
  toQuery = Lude.const Lude.mempty

-- | Result structure used in response to request to delete a project.
--
-- /See:/ 'mkDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | Resources which were deleted.
    deletedResources :: Lude.Maybe [Resource],
    -- | Resources which were not deleted, due to a risk of losing potentially important data or files.
    orphanedResources :: Lude.Maybe [Resource],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProjectResponse' with the minimum fields required to make a request.
--
-- * 'deletedResources' - Resources which were deleted.
-- * 'orphanedResources' - Resources which were not deleted, due to a risk of losing potentially important data or files.
-- * 'responseStatus' - The response status code.
mkDeleteProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProjectResponse
mkDeleteProjectResponse pResponseStatus_ =
  DeleteProjectResponse'
    { deletedResources = Lude.Nothing,
      orphanedResources = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Resources which were deleted.
--
-- /Note:/ Consider using 'deletedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsDeletedResources :: Lens.Lens' DeleteProjectResponse (Lude.Maybe [Resource])
dprsDeletedResources = Lens.lens (deletedResources :: DeleteProjectResponse -> Lude.Maybe [Resource]) (\s a -> s {deletedResources = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsDeletedResources "Use generic-lens or generic-optics with 'deletedResources' instead." #-}

-- | Resources which were not deleted, due to a risk of losing potentially important data or files.
--
-- /Note:/ Consider using 'orphanedResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsOrphanedResources :: Lens.Lens' DeleteProjectResponse (Lude.Maybe [Resource])
dprsOrphanedResources = Lens.lens (orphanedResources :: DeleteProjectResponse -> Lude.Maybe [Resource]) (\s a -> s {orphanedResources = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsOrphanedResources "Use generic-lens or generic-optics with 'orphanedResources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DeleteProjectResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DeleteProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProjectResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
