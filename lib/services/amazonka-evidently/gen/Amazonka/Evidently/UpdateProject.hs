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
-- Module      : Amazonka.Evidently.UpdateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description of an existing project.
--
-- To create a new project, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_CreateProject.html CreateProject>.
--
-- Don\'t use this operation to update the data storage options of a
-- project. Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_UpdateProjectDataDelivery.html UpdateProjectDataDelivery>.
--
-- Don\'t use this operation to update the tags of a project. Instead, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_TagResource.html TagResource>.
module Amazonka.Evidently.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_appConfigResource,
    updateProject_description,
    updateProject_project,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_httpStatus,
    updateProjectResponse_project,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | Use this parameter if the project will use client-side evaluation
    -- powered by AppConfig. Client-side evaluation allows your application to
    -- assign variations to user sessions locally instead of by calling the
    -- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
    -- operation. This mitigates the latency and availability risks that come
    -- with an API call. allows you to
    --
    -- This parameter is a structure that contains information about the
    -- AppConfig application that will be used for client-side evaluation.
    appConfigResource :: Prelude.Maybe ProjectAppConfigResourceConfig,
    -- | An optional description of the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the project to update.
    project :: Prelude.Text
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
-- 'appConfigResource', 'updateProject_appConfigResource' - Use this parameter if the project will use client-side evaluation
-- powered by AppConfig. Client-side evaluation allows your application to
-- assign variations to user sessions locally instead of by calling the
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
-- operation. This mitigates the latency and availability risks that come
-- with an API call. allows you to
--
-- This parameter is a structure that contains information about the
-- AppConfig application that will be used for client-side evaluation.
--
-- 'description', 'updateProject_description' - An optional description of the project.
--
-- 'project', 'updateProject_project' - The name or ARN of the project to update.
newUpdateProject ::
  -- | 'project'
  Prelude.Text ->
  UpdateProject
newUpdateProject pProject_ =
  UpdateProject'
    { appConfigResource = Prelude.Nothing,
      description = Prelude.Nothing,
      project = pProject_
    }

-- | Use this parameter if the project will use client-side evaluation
-- powered by AppConfig. Client-side evaluation allows your application to
-- assign variations to user sessions locally instead of by calling the
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
-- operation. This mitigates the latency and availability risks that come
-- with an API call. allows you to
--
-- This parameter is a structure that contains information about the
-- AppConfig application that will be used for client-side evaluation.
updateProject_appConfigResource :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectAppConfigResourceConfig)
updateProject_appConfigResource = Lens.lens (\UpdateProject' {appConfigResource} -> appConfigResource) (\s@UpdateProject' {} a -> s {appConfigResource = a} :: UpdateProject)

-- | An optional description of the project.
updateProject_description :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_description = Lens.lens (\UpdateProject' {description} -> description) (\s@UpdateProject' {} a -> s {description = a} :: UpdateProject)

-- | The name or ARN of the project to update.
updateProject_project :: Lens.Lens' UpdateProject Prelude.Text
updateProject_project = Lens.lens (\UpdateProject' {project} -> project) (\s@UpdateProject' {} a -> s {project = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "project")
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt _salt UpdateProject' {..} =
    _salt
      `Prelude.hashWithSalt` appConfigResource
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` project

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf appConfigResource
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf project

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
          [ ("appConfigResource" Data..=)
              Prelude.<$> appConfigResource,
            ("description" Data..=) Prelude.<$> description
          ]
      )

instance Data.ToPath UpdateProject where
  toPath UpdateProject' {..} =
    Prelude.mconcat ["/projects/", Data.toBS project]

instance Data.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing information about the updated project.
    project :: Project
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
--
-- 'project', 'updateProjectResponse_project' - A structure containing information about the updated project.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'project'
  Project ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ pProject_ =
  UpdateProjectResponse'
    { httpStatus = pHttpStatus_,
      project = pProject_
    }

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

-- | A structure containing information about the updated project.
updateProjectResponse_project :: Lens.Lens' UpdateProjectResponse Project
updateProjectResponse_project = Lens.lens (\UpdateProjectResponse' {project} -> project) (\s@UpdateProjectResponse' {} a -> s {project = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf project
