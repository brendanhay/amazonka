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
-- Module      : Network.AWS.DeviceFarm.UpdateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified project name, given the project ARN and a new
-- name.
module Network.AWS.DeviceFarm.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_name,
    updateProject_defaultJobTimeoutMinutes,
    updateProject_arn,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the update project operation.
--
-- /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | A string that represents the new name of the project that you are
    -- updating.
    name :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes a test run in the project executes before it times
    -- out.
    defaultJobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the project whose name to update.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateProject_name' - A string that represents the new name of the project that you are
-- updating.
--
-- 'defaultJobTimeoutMinutes', 'updateProject_defaultJobTimeoutMinutes' - The number of minutes a test run in the project executes before it times
-- out.
--
-- 'arn', 'updateProject_arn' - The Amazon Resource Name (ARN) of the project whose name to update.
newUpdateProject ::
  -- | 'arn'
  Prelude.Text ->
  UpdateProject
newUpdateProject pArn_ =
  UpdateProject'
    { name = Prelude.Nothing,
      defaultJobTimeoutMinutes = Prelude.Nothing,
      arn = pArn_
    }

-- | A string that represents the new name of the project that you are
-- updating.
updateProject_name :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_name = Lens.lens (\UpdateProject' {name} -> name) (\s@UpdateProject' {} a -> s {name = a} :: UpdateProject)

-- | The number of minutes a test run in the project executes before it times
-- out.
updateProject_defaultJobTimeoutMinutes :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Int)
updateProject_defaultJobTimeoutMinutes = Lens.lens (\UpdateProject' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@UpdateProject' {} a -> s {defaultJobTimeoutMinutes = a} :: UpdateProject)

-- | The Amazon Resource Name (ARN) of the project whose name to update.
updateProject_arn :: Lens.Lens' UpdateProject Prelude.Text
updateProject_arn = Lens.lens (\UpdateProject' {arn} -> arn) (\s@UpdateProject' {} a -> s {arn = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (x Core..?> "project")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProject

instance Prelude.NFData UpdateProject

instance Core.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("defaultJobTimeoutMinutes" Core..=)
              Prelude.<$> defaultJobTimeoutMinutes,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateProject where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of an update project request.
--
-- /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The project to update.
    project :: Prelude.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'updateProjectResponse_project' - The project to update.
--
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse'
    { project = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The project to update.
updateProjectResponse_project :: Lens.Lens' UpdateProjectResponse (Prelude.Maybe Project)
updateProjectResponse_project = Lens.lens (\UpdateProjectResponse' {project} -> project) (\s@UpdateProjectResponse' {} a -> s {project = a} :: UpdateProjectResponse)

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse
