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
-- Module      : Amazonka.Rekognition.DeleteProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels project. To delete a project
-- you must first delete all models associated with the project. To delete
-- a model, see DeleteProjectVersion.
--
-- @DeleteProject@ is an asynchronous operation. To check if the project is
-- deleted, call DescribeProjects. The project is deleted when the project
-- no longer appears in the response. Be aware that deleting a given
-- project will also delete any @ProjectPolicies@ associated with that
-- project.
--
-- This operation requires permissions to perform the
-- @rekognition:DeleteProject@ action.
module Amazonka.Rekognition.DeleteProject
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | The Amazon Resource Name (ARN) of the project that you want to delete.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteProject
newDeleteProject pProjectArn_ =
  DeleteProject' {projectArn = pProjectArn_}

-- | The Amazon Resource Name (ARN) of the project that you want to delete.
deleteProject_projectArn :: Lens.Lens' DeleteProject Prelude.Text
deleteProject_projectArn = Lens.lens (\DeleteProject' {projectArn} -> projectArn) (\s@DeleteProject' {} a -> s {projectArn = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProject where
  hashWithSalt _salt DeleteProject' {..} =
    _salt `Prelude.hashWithSalt` projectArn

instance Prelude.NFData DeleteProject where
  rnf DeleteProject' {..} = Prelude.rnf projectArn

instance Core.ToHeaders DeleteProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DeleteProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProjectArn" Core..= projectArn)]
      )

instance Core.ToPath DeleteProject where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The current status of the delete project operation.
    status :: Prelude.Maybe ProjectStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse'
    { status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the delete project operation.
deleteProjectResponse_status :: Lens.Lens' DeleteProjectResponse (Prelude.Maybe ProjectStatus)
deleteProjectResponse_status = Lens.lens (\DeleteProjectResponse' {status} -> status) (\s@DeleteProjectResponse' {} a -> s {status = a} :: DeleteProjectResponse)

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Prelude.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Prelude.NFData DeleteProjectResponse where
  rnf DeleteProjectResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
