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
-- Module      : Network.AWS.DeviceFarm.CreateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project.
module Network.AWS.DeviceFarm.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_defaultJobTimeoutMinutes,
    createProject_name,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_project,
    createProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the create project operation.
--
-- /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | Sets the execution timeout value (in minutes) for a project. All test
    -- runs in this project use the specified execution timeout value unless
    -- overridden when scheduling a run.
    defaultJobTimeoutMinutes :: Core.Maybe Core.Int,
    -- | The project\'s name.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultJobTimeoutMinutes', 'createProject_defaultJobTimeoutMinutes' - Sets the execution timeout value (in minutes) for a project. All test
-- runs in this project use the specified execution timeout value unless
-- overridden when scheduling a run.
--
-- 'name', 'createProject_name' - The project\'s name.
newCreateProject ::
  -- | 'name'
  Core.Text ->
  CreateProject
newCreateProject pName_ =
  CreateProject'
    { defaultJobTimeoutMinutes =
        Core.Nothing,
      name = pName_
    }

-- | Sets the execution timeout value (in minutes) for a project. All test
-- runs in this project use the specified execution timeout value unless
-- overridden when scheduling a run.
createProject_defaultJobTimeoutMinutes :: Lens.Lens' CreateProject (Core.Maybe Core.Int)
createProject_defaultJobTimeoutMinutes = Lens.lens (\CreateProject' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@CreateProject' {} a -> s {defaultJobTimeoutMinutes = a} :: CreateProject)

-- | The project\'s name.
createProject_name :: Lens.Lens' CreateProject Core.Text
createProject_name = Lens.lens (\CreateProject' {name} -> name) (\s@CreateProject' {} a -> s {name = a} :: CreateProject)

instance Core.AWSRequest CreateProject where
  type
    AWSResponse CreateProject =
      CreateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Core.<$> (x Core..?> "project")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProject

instance Core.NFData CreateProject

instance Core.ToHeaders CreateProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("defaultJobTimeoutMinutes" Core..=)
              Core.<$> defaultJobTimeoutMinutes,
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateProject where
  toPath = Core.const "/"

instance Core.ToQuery CreateProject where
  toQuery = Core.const Core.mempty

-- | Represents the result of a create project request.
--
-- /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The newly created project.
    project :: Core.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'createProjectResponse_project' - The newly created project.
--
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { project = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created project.
createProjectResponse_project :: Lens.Lens' CreateProjectResponse (Core.Maybe Project)
createProjectResponse_project = Lens.lens (\CreateProjectResponse' {project} -> project) (\s@CreateProjectResponse' {} a -> s {project = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Core.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Core.NFData CreateProjectResponse
