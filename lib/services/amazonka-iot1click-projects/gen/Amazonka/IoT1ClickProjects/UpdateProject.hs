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
-- Module      : Amazonka.IoT1ClickProjects.UpdateProject
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.IoT1ClickProjects.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_description,
    updateProject_placementTemplate,
    updateProject_projectName,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickProjects.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | An optional user-defined description for the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | An object defining the project update. Once a project has been created,
    -- you cannot add device template names to the project. However, for a
    -- given @placementTemplate@, you can update the associated
    -- @callbackOverrides@ for the device definition using this API.
    placementTemplate :: Prelude.Maybe PlacementTemplate,
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
-- 'description', 'updateProject_description' - An optional user-defined description for the project.
--
-- 'placementTemplate', 'updateProject_placementTemplate' - An object defining the project update. Once a project has been created,
-- you cannot add device template names to the project. However, for a
-- given @placementTemplate@, you can update the associated
-- @callbackOverrides@ for the device definition using this API.
--
-- 'projectName', 'updateProject_projectName' - The name of the project to be updated.
newUpdateProject ::
  -- | 'projectName'
  Prelude.Text ->
  UpdateProject
newUpdateProject pProjectName_ =
  UpdateProject'
    { description = Prelude.Nothing,
      placementTemplate = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | An optional user-defined description for the project.
updateProject_description :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_description = Lens.lens (\UpdateProject' {description} -> description) (\s@UpdateProject' {} a -> s {description = a} :: UpdateProject)

-- | An object defining the project update. Once a project has been created,
-- you cannot add device template names to the project. However, for a
-- given @placementTemplate@, you can update the associated
-- @callbackOverrides@ for the device definition using this API.
updateProject_placementTemplate :: Lens.Lens' UpdateProject (Prelude.Maybe PlacementTemplate)
updateProject_placementTemplate = Lens.lens (\UpdateProject' {placementTemplate} -> placementTemplate) (\s@UpdateProject' {} a -> s {placementTemplate = a} :: UpdateProject)

-- | The name of the project to be updated.
updateProject_projectName :: Lens.Lens' UpdateProject Prelude.Text
updateProject_projectName = Lens.lens (\UpdateProject' {projectName} -> projectName) (\s@UpdateProject' {} a -> s {projectName = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt _salt UpdateProject' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` placementTemplate
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf placementTemplate
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("placementTemplate" Data..=)
              Prelude.<$> placementTemplate
          ]
      )

instance Data.ToPath UpdateProject where
  toPath UpdateProject' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS projectName]

instance Data.ToQuery UpdateProject where
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

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf httpStatus
