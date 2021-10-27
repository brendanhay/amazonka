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
-- Module      : Network.AWS.IoT1ClickProjects.UpdateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a project associated with your AWS account and region. With the
-- exception of device template names, you can pass just the values that
-- need to be updated because the update request will change only the
-- values that are provided. To clear a value, pass the empty string (i.e.,
-- @\"\"@).
module Network.AWS.IoT1ClickProjects.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_placementTemplate,
    updateProject_description,
    updateProject_projectName,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT1ClickProjects.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | An object defining the project update. Once a project has been created,
    -- you cannot add device template names to the project. However, for a
    -- given @placementTemplate@, you can update the associated
    -- @callbackOverrides@ for the device definition using this API.
    placementTemplate :: Prelude.Maybe PlacementTemplate,
    -- | An optional user-defined description for the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the project to be updated.
    projectName :: Prelude.Text
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
-- 'placementTemplate', 'updateProject_placementTemplate' - An object defining the project update. Once a project has been created,
-- you cannot add device template names to the project. However, for a
-- given @placementTemplate@, you can update the associated
-- @callbackOverrides@ for the device definition using this API.
--
-- 'description', 'updateProject_description' - An optional user-defined description for the project.
--
-- 'projectName', 'updateProject_projectName' - The name of the project to be updated.
newUpdateProject ::
  -- | 'projectName'
  Prelude.Text ->
  UpdateProject
newUpdateProject pProjectName_ =
  UpdateProject'
    { placementTemplate = Prelude.Nothing,
      description = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | An object defining the project update. Once a project has been created,
-- you cannot add device template names to the project. However, for a
-- given @placementTemplate@, you can update the associated
-- @callbackOverrides@ for the device definition using this API.
updateProject_placementTemplate :: Lens.Lens' UpdateProject (Prelude.Maybe PlacementTemplate)
updateProject_placementTemplate = Lens.lens (\UpdateProject' {placementTemplate} -> placementTemplate) (\s@UpdateProject' {} a -> s {placementTemplate = a} :: UpdateProject)

-- | An optional user-defined description for the project.
updateProject_description :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_description = Lens.lens (\UpdateProject' {description} -> description) (\s@UpdateProject' {} a -> s {description = a} :: UpdateProject)

-- | The name of the project to be updated.
updateProject_projectName :: Lens.Lens' UpdateProject Prelude.Text
updateProject_projectName = Lens.lens (\UpdateProject' {projectName} -> projectName) (\s@UpdateProject' {} a -> s {projectName = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProject

instance Prelude.NFData UpdateProject

instance Core.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("placementTemplate" Core..=)
              Prelude.<$> placementTemplate,
            ("description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateProject where
  toPath UpdateProject' {..} =
    Prelude.mconcat
      ["/projects/", Core.toBS projectName]

instance Core.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse
