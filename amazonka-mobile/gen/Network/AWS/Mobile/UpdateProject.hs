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
-- Module      : Network.AWS.Mobile.UpdateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing project.
module Network.AWS.Mobile.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_contents,
    updateProject_projectId,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_details,
    updateProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used for requests to update project configuration.
--
-- /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | ZIP or YAML file which contains project configuration to be updated.
    -- This should be the contents of the file downloaded from the URL provided
    -- in an export project operation.
    contents :: Core.Maybe Core.ByteString,
    -- | Unique project identifier.
    projectId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contents', 'updateProject_contents' - ZIP or YAML file which contains project configuration to be updated.
-- This should be the contents of the file downloaded from the URL provided
-- in an export project operation.
--
-- 'projectId', 'updateProject_projectId' - Unique project identifier.
newUpdateProject ::
  -- | 'projectId'
  Core.Text ->
  UpdateProject
newUpdateProject pProjectId_ =
  UpdateProject'
    { contents = Core.Nothing,
      projectId = pProjectId_
    }

-- | ZIP or YAML file which contains project configuration to be updated.
-- This should be the contents of the file downloaded from the URL provided
-- in an export project operation.
updateProject_contents :: Lens.Lens' UpdateProject (Core.Maybe Core.ByteString)
updateProject_contents = Lens.lens (\UpdateProject' {contents} -> contents) (\s@UpdateProject' {} a -> s {contents = a} :: UpdateProject)

-- | Unique project identifier.
updateProject_projectId :: Lens.Lens' UpdateProject Core.Text
updateProject_projectId = Lens.lens (\UpdateProject' {projectId} -> projectId) (\s@UpdateProject' {} a -> s {projectId = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request = Request.postBody defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Core.<$> (x Core..?> "details")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateProject

instance Core.NFData UpdateProject

instance Core.ToBody UpdateProject where
  toBody UpdateProject' {..} = Core.toBody contents

instance Core.ToHeaders UpdateProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath UpdateProject where
  toPath = Core.const "/update"

instance Core.ToQuery UpdateProject where
  toQuery UpdateProject' {..} =
    Core.mconcat ["projectId" Core.=: projectId]

-- | Result structure used for requests to updated project configuration.
--
-- /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | Detailed information about the updated AWS Mobile Hub project.
    details :: Core.Maybe ProjectDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'updateProjectResponse_details' - Detailed information about the updated AWS Mobile Hub project.
--
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse'
    { details = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the updated AWS Mobile Hub project.
updateProjectResponse_details :: Lens.Lens' UpdateProjectResponse (Core.Maybe ProjectDetails)
updateProjectResponse_details = Lens.lens (\UpdateProjectResponse' {details} -> details) (\s@UpdateProjectResponse' {} a -> s {details = a} :: UpdateProjectResponse)

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Core.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Core.NFData UpdateProjectResponse
