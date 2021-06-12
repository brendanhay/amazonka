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
-- Module      : Network.AWS.Rekognition.DeleteProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels project. To delete a project
-- you must first delete all models associated with the project. To delete
-- a model, see DeleteProjectVersion.
--
-- This operation requires permissions to perform the
-- @rekognition:DeleteProject@ action.
module Network.AWS.Rekognition.DeleteProject
  ( -- * Creating a Request
    DeleteProject (..),
    newDeleteProject,

    -- * Request Lenses
    deleteProject_projectArn,

    -- * Destructuring the Response
    DeleteProjectResponse (..),
    newDeleteProjectResponse,

    -- * Response Lenses
    deleteProjectResponse_status,
    deleteProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | The Amazon Resource Name (ARN) of the project that you want to delete.
    projectArn :: Core.Text
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
-- 'projectArn', 'deleteProject_projectArn' - The Amazon Resource Name (ARN) of the project that you want to delete.
newDeleteProject ::
  -- | 'projectArn'
  Core.Text ->
  DeleteProject
newDeleteProject pProjectArn_ =
  DeleteProject' {projectArn = pProjectArn_}

-- | The Amazon Resource Name (ARN) of the project that you want to delete.
deleteProject_projectArn :: Lens.Lens' DeleteProject Core.Text
deleteProject_projectArn = Lens.lens (\DeleteProject' {projectArn} -> projectArn) (\s@DeleteProject' {} a -> s {projectArn = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProject

instance Core.NFData DeleteProject

instance Core.ToHeaders DeleteProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DeleteProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ProjectArn" Core..= projectArn)]
      )

instance Core.ToPath DeleteProject where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The current status of the delete project operation.
    status :: Core.Maybe ProjectStatus,
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
-- 'status', 'deleteProjectResponse_status' - The current status of the delete project operation.
--
-- 'httpStatus', 'deleteProjectResponse_httpStatus' - The response's http status code.
newDeleteProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse'
    { status = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the delete project operation.
deleteProjectResponse_status :: Lens.Lens' DeleteProjectResponse (Core.Maybe ProjectStatus)
deleteProjectResponse_status = Lens.lens (\DeleteProjectResponse' {status} -> status) (\s@DeleteProjectResponse' {} a -> s {status = a} :: DeleteProjectResponse)

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Core.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Core.NFData DeleteProjectResponse
