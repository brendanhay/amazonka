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
-- Module      : Amazonka.Mobile.DeleteProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delets a project in AWS Mobile Hub.
module Amazonka.Mobile.DeleteProject
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request structure used to request a project be deleted.
--
-- /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | Unique project identifier.
    projectId :: Prelude.Text
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
-- 'projectId', 'deleteProject_projectId' - Unique project identifier.
newDeleteProject ::
  -- | 'projectId'
  Prelude.Text ->
  DeleteProject
newDeleteProject pProjectId_ =
  DeleteProject' {projectId = pProjectId_}

-- | Unique project identifier.
deleteProject_projectId :: Lens.Lens' DeleteProject Prelude.Text
deleteProject_projectId = Lens.lens (\DeleteProject' {projectId} -> projectId) (\s@DeleteProject' {} a -> s {projectId = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Prelude.<$> ( x Data..?> "deletedResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "orphanedResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProject where
  hashWithSalt _salt DeleteProject' {..} =
    _salt `Prelude.hashWithSalt` projectId

instance Prelude.NFData DeleteProject where
  rnf DeleteProject' {..} = Prelude.rnf projectId

instance Data.ToHeaders DeleteProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteProject where
  toPath DeleteProject' {..} =
    Prelude.mconcat ["/projects/", Data.toBS projectId]

instance Data.ToQuery DeleteProject where
  toQuery = Prelude.const Prelude.mempty

-- | Result structure used in response to request to delete a project.
--
-- /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | Resources which were deleted.
    deletedResources :: Prelude.Maybe [Resource],
    -- | Resources which were not deleted, due to a risk of losing potentially
    -- important data or files.
    orphanedResources :: Prelude.Maybe [Resource],
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
-- 'deletedResources', 'deleteProjectResponse_deletedResources' - Resources which were deleted.
--
-- 'orphanedResources', 'deleteProjectResponse_orphanedResources' - Resources which were not deleted, due to a risk of losing potentially
-- important data or files.
--
-- 'httpStatus', 'deleteProjectResponse_httpStatus' - The response's http status code.
newDeleteProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse'
    { deletedResources =
        Prelude.Nothing,
      orphanedResources = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Resources which were deleted.
deleteProjectResponse_deletedResources :: Lens.Lens' DeleteProjectResponse (Prelude.Maybe [Resource])
deleteProjectResponse_deletedResources = Lens.lens (\DeleteProjectResponse' {deletedResources} -> deletedResources) (\s@DeleteProjectResponse' {} a -> s {deletedResources = a} :: DeleteProjectResponse) Prelude.. Lens.mapping Lens.coerced

-- | Resources which were not deleted, due to a risk of losing potentially
-- important data or files.
deleteProjectResponse_orphanedResources :: Lens.Lens' DeleteProjectResponse (Prelude.Maybe [Resource])
deleteProjectResponse_orphanedResources = Lens.lens (\DeleteProjectResponse' {orphanedResources} -> orphanedResources) (\s@DeleteProjectResponse' {} a -> s {orphanedResources = a} :: DeleteProjectResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Prelude.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Prelude.NFData DeleteProjectResponse where
  rnf DeleteProjectResponse' {..} =
    Prelude.rnf deletedResources
      `Prelude.seq` Prelude.rnf orphanedResources
      `Prelude.seq` Prelude.rnf httpStatus
