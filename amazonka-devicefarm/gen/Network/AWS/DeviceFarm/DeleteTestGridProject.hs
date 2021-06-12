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
-- Module      : Network.AWS.DeviceFarm.DeleteTestGridProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Selenium testing project and all content generated under it.
--
-- You cannot undo this operation.
--
-- You cannot delete a project if it has active sessions.
module Network.AWS.DeviceFarm.DeleteTestGridProject
  ( -- * Creating a Request
    DeleteTestGridProject (..),
    newDeleteTestGridProject,

    -- * Request Lenses
    deleteTestGridProject_projectArn,

    -- * Destructuring the Response
    DeleteTestGridProjectResponse (..),
    newDeleteTestGridProjectResponse,

    -- * Response Lenses
    deleteTestGridProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTestGridProject' smart constructor.
data DeleteTestGridProject = DeleteTestGridProject'
  { -- | The ARN of the project to delete, from CreateTestGridProject or
    -- ListTestGridProjects.
    projectArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectArn', 'deleteTestGridProject_projectArn' - The ARN of the project to delete, from CreateTestGridProject or
-- ListTestGridProjects.
newDeleteTestGridProject ::
  -- | 'projectArn'
  Core.Text ->
  DeleteTestGridProject
newDeleteTestGridProject pProjectArn_ =
  DeleteTestGridProject' {projectArn = pProjectArn_}

-- | The ARN of the project to delete, from CreateTestGridProject or
-- ListTestGridProjects.
deleteTestGridProject_projectArn :: Lens.Lens' DeleteTestGridProject Core.Text
deleteTestGridProject_projectArn = Lens.lens (\DeleteTestGridProject' {projectArn} -> projectArn) (\s@DeleteTestGridProject' {} a -> s {projectArn = a} :: DeleteTestGridProject)

instance Core.AWSRequest DeleteTestGridProject where
  type
    AWSResponse DeleteTestGridProject =
      DeleteTestGridProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTestGridProjectResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTestGridProject

instance Core.NFData DeleteTestGridProject

instance Core.ToHeaders DeleteTestGridProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.DeleteTestGridProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteTestGridProject where
  toJSON DeleteTestGridProject' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("projectArn" Core..= projectArn)]
      )

instance Core.ToPath DeleteTestGridProject where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTestGridProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteTestGridProjectResponse' smart constructor.
data DeleteTestGridProjectResponse = DeleteTestGridProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTestGridProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTestGridProjectResponse_httpStatus' - The response's http status code.
newDeleteTestGridProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTestGridProjectResponse
newDeleteTestGridProjectResponse pHttpStatus_ =
  DeleteTestGridProjectResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTestGridProjectResponse_httpStatus :: Lens.Lens' DeleteTestGridProjectResponse Core.Int
deleteTestGridProjectResponse_httpStatus = Lens.lens (\DeleteTestGridProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteTestGridProjectResponse' {} a -> s {httpStatus = a} :: DeleteTestGridProjectResponse)

instance Core.NFData DeleteTestGridProjectResponse
