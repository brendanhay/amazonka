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
-- Module      : Amazonka.IoTSiteWise.UpdateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an IoT SiteWise Monitor project.
module Amazonka.IoTSiteWise.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_clientToken,
    updateProject_projectDescription,
    updateProject_projectId,
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
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | A unique case-sensitive identifier that you can provide to ensure the
    -- idempotency of the request. Don\'t reuse this client token if a new
    -- idempotent request is required.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A new description for the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the project to update.
    projectId :: Prelude.Text,
    -- | A new friendly name for the project.
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
-- 'clientToken', 'updateProject_clientToken' - A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
--
-- 'projectDescription', 'updateProject_projectDescription' - A new description for the project.
--
-- 'projectId', 'updateProject_projectId' - The ID of the project to update.
--
-- 'projectName', 'updateProject_projectName' - A new friendly name for the project.
newUpdateProject ::
  -- | 'projectId'
  Prelude.Text ->
  -- | 'projectName'
  Prelude.Text ->
  UpdateProject
newUpdateProject pProjectId_ pProjectName_ =
  UpdateProject'
    { clientToken = Prelude.Nothing,
      projectDescription = Prelude.Nothing,
      projectId = pProjectId_,
      projectName = pProjectName_
    }

-- | A unique case-sensitive identifier that you can provide to ensure the
-- idempotency of the request. Don\'t reuse this client token if a new
-- idempotent request is required.
updateProject_clientToken :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_clientToken = Lens.lens (\UpdateProject' {clientToken} -> clientToken) (\s@UpdateProject' {} a -> s {clientToken = a} :: UpdateProject)

-- | A new description for the project.
updateProject_projectDescription :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_projectDescription = Lens.lens (\UpdateProject' {projectDescription} -> projectDescription) (\s@UpdateProject' {} a -> s {projectDescription = a} :: UpdateProject)

-- | The ID of the project to update.
updateProject_projectId :: Lens.Lens' UpdateProject Prelude.Text
updateProject_projectId = Lens.lens (\UpdateProject' {projectId} -> projectId) (\s@UpdateProject' {} a -> s {projectId = a} :: UpdateProject)

-- | A new friendly name for the project.
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
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` projectDescription
      `Prelude.hashWithSalt` projectId
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf projectDescription
      `Prelude.seq` Prelude.rnf projectId
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
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("projectDescription" Data..=)
              Prelude.<$> projectDescription,
            Prelude.Just ("projectName" Data..= projectName)
          ]
      )

instance Data.ToPath UpdateProject where
  toPath UpdateProject' {..} =
    Prelude.mconcat ["/projects/", Data.toBS projectId]

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
