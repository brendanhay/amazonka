{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.DeleteProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delets a project in AWS Mobile Hub.
module Network.AWS.Mobile.DeleteProject
  ( -- * Creating a Request
    DeleteProject (..),
    newDeleteProject,

    -- * Request Lenses
    deleteProject_projectId,

    -- * Destructuring the Response
    DeleteProjectResponse (..),
    newDeleteProjectResponse,

    -- * Response Lenses
    deleteProjectResponse_deletedResources,
    deleteProjectResponse_orphanedResources,
    deleteProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request a project be deleted.
--
-- /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | Unique project identifier.
    projectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectId', 'deleteProject_projectId' - Unique project identifier.
newDeleteProject ::
  -- | 'projectId'
  Core.Text ->
  DeleteProject
newDeleteProject pProjectId_ =
  DeleteProject' {projectId = pProjectId_}

-- | Unique project identifier.
deleteProject_projectId :: Lens.Lens' DeleteProject Core.Text
deleteProject_projectId = Lens.lens (\DeleteProject' {projectId} -> projectId) (\s@DeleteProject' {} a -> s {projectId = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Core.<$> (x Core..?> "deletedResources" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "orphanedResources" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProject

instance Core.NFData DeleteProject

instance Core.ToHeaders DeleteProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteProject where
  toPath DeleteProject' {..} =
    Core.mconcat ["/projects/", Core.toBS projectId]

instance Core.ToQuery DeleteProject where
  toQuery = Core.const Core.mempty

-- | Result structure used in response to request to delete a project.
--
-- /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | Resources which were deleted.
    deletedResources :: Core.Maybe [Resource],
    -- | Resources which were not deleted, due to a risk of losing potentially
    -- important data or files.
    orphanedResources :: Core.Maybe [Resource],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletedResources', 'deleteProjectResponse_deletedResources' - Resources which were deleted.
--
-- 'orphanedResources', 'deleteProjectResponse_orphanedResources' - Resources which were not deleted, due to a risk of losing potentially
-- important data or files.
--
-- 'httpStatus', 'deleteProjectResponse_httpStatus' - The response's http status code.
newDeleteProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse'
    { deletedResources =
        Core.Nothing,
      orphanedResources = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Resources which were deleted.
deleteProjectResponse_deletedResources :: Lens.Lens' DeleteProjectResponse (Core.Maybe [Resource])
deleteProjectResponse_deletedResources = Lens.lens (\DeleteProjectResponse' {deletedResources} -> deletedResources) (\s@DeleteProjectResponse' {} a -> s {deletedResources = a} :: DeleteProjectResponse) Core.. Lens.mapping Lens._Coerce

-- | Resources which were not deleted, due to a risk of losing potentially
-- important data or files.
deleteProjectResponse_orphanedResources :: Lens.Lens' DeleteProjectResponse (Core.Maybe [Resource])
deleteProjectResponse_orphanedResources = Lens.lens (\DeleteProjectResponse' {orphanedResources} -> orphanedResources) (\s@DeleteProjectResponse' {} a -> s {orphanedResources = a} :: DeleteProjectResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Core.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Core.NFData DeleteProjectResponse
