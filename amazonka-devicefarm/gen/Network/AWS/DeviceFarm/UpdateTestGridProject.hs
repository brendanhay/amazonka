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
-- Module      : Network.AWS.DeviceFarm.UpdateTestGridProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Change details of a project.
module Network.AWS.DeviceFarm.UpdateTestGridProject
  ( -- * Creating a Request
    UpdateTestGridProject (..),
    newUpdateTestGridProject,

    -- * Request Lenses
    updateTestGridProject_name,
    updateTestGridProject_description,
    updateTestGridProject_projectArn,

    -- * Destructuring the Response
    UpdateTestGridProjectResponse (..),
    newUpdateTestGridProjectResponse,

    -- * Response Lenses
    updateTestGridProjectResponse_testGridProject,
    updateTestGridProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTestGridProject' smart constructor.
data UpdateTestGridProject = UpdateTestGridProject'
  { -- | Human-readable name for the project.
    name :: Core.Maybe Core.Text,
    -- | Human-readable description for the project.
    description :: Core.Maybe Core.Text,
    -- | ARN of the project to update.
    projectArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateTestGridProject_name' - Human-readable name for the project.
--
-- 'description', 'updateTestGridProject_description' - Human-readable description for the project.
--
-- 'projectArn', 'updateTestGridProject_projectArn' - ARN of the project to update.
newUpdateTestGridProject ::
  -- | 'projectArn'
  Core.Text ->
  UpdateTestGridProject
newUpdateTestGridProject pProjectArn_ =
  UpdateTestGridProject'
    { name = Core.Nothing,
      description = Core.Nothing,
      projectArn = pProjectArn_
    }

-- | Human-readable name for the project.
updateTestGridProject_name :: Lens.Lens' UpdateTestGridProject (Core.Maybe Core.Text)
updateTestGridProject_name = Lens.lens (\UpdateTestGridProject' {name} -> name) (\s@UpdateTestGridProject' {} a -> s {name = a} :: UpdateTestGridProject)

-- | Human-readable description for the project.
updateTestGridProject_description :: Lens.Lens' UpdateTestGridProject (Core.Maybe Core.Text)
updateTestGridProject_description = Lens.lens (\UpdateTestGridProject' {description} -> description) (\s@UpdateTestGridProject' {} a -> s {description = a} :: UpdateTestGridProject)

-- | ARN of the project to update.
updateTestGridProject_projectArn :: Lens.Lens' UpdateTestGridProject Core.Text
updateTestGridProject_projectArn = Lens.lens (\UpdateTestGridProject' {projectArn} -> projectArn) (\s@UpdateTestGridProject' {} a -> s {projectArn = a} :: UpdateTestGridProject)

instance Core.AWSRequest UpdateTestGridProject where
  type
    AWSResponse UpdateTestGridProject =
      UpdateTestGridProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTestGridProjectResponse'
            Core.<$> (x Core..?> "testGridProject")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTestGridProject

instance Core.NFData UpdateTestGridProject

instance Core.ToHeaders UpdateTestGridProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateTestGridProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTestGridProject where
  toJSON UpdateTestGridProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("description" Core..=) Core.<$> description,
            Core.Just ("projectArn" Core..= projectArn)
          ]
      )

instance Core.ToPath UpdateTestGridProject where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTestGridProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateTestGridProjectResponse' smart constructor.
data UpdateTestGridProjectResponse = UpdateTestGridProjectResponse'
  { -- | The project, including updated information.
    testGridProject :: Core.Maybe TestGridProject,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTestGridProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testGridProject', 'updateTestGridProjectResponse_testGridProject' - The project, including updated information.
--
-- 'httpStatus', 'updateTestGridProjectResponse_httpStatus' - The response's http status code.
newUpdateTestGridProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTestGridProjectResponse
newUpdateTestGridProjectResponse pHttpStatus_ =
  UpdateTestGridProjectResponse'
    { testGridProject =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The project, including updated information.
updateTestGridProjectResponse_testGridProject :: Lens.Lens' UpdateTestGridProjectResponse (Core.Maybe TestGridProject)
updateTestGridProjectResponse_testGridProject = Lens.lens (\UpdateTestGridProjectResponse' {testGridProject} -> testGridProject) (\s@UpdateTestGridProjectResponse' {} a -> s {testGridProject = a} :: UpdateTestGridProjectResponse)

-- | The response's http status code.
updateTestGridProjectResponse_httpStatus :: Lens.Lens' UpdateTestGridProjectResponse Core.Int
updateTestGridProjectResponse_httpStatus = Lens.lens (\UpdateTestGridProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateTestGridProjectResponse' {} a -> s {httpStatus = a} :: UpdateTestGridProjectResponse)

instance Core.NFData UpdateTestGridProjectResponse
