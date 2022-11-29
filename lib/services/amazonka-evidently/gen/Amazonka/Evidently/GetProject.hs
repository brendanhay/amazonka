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
-- Module      : Amazonka.Evidently.GetProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details about one launch. You must already know the project
-- name. To retrieve a list of projects in your account, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_ListProjects.html ListProjects>.
module Amazonka.Evidently.GetProject
  ( -- * Creating a Request
    GetProject (..),
    newGetProject,

    -- * Request Lenses
    getProject_project,

    -- * Destructuring the Response
    GetProjectResponse (..),
    newGetProjectResponse,

    -- * Response Lenses
    getProjectResponse_httpStatus,
    getProjectResponse_project,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetProject' smart constructor.
data GetProject = GetProject'
  { -- | The name or ARN of the project that you want to see the details of.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'getProject_project' - The name or ARN of the project that you want to see the details of.
newGetProject ::
  -- | 'project'
  Prelude.Text ->
  GetProject
newGetProject pProject_ =
  GetProject' {project = pProject_}

-- | The name or ARN of the project that you want to see the details of.
getProject_project :: Lens.Lens' GetProject Prelude.Text
getProject_project = Lens.lens (\GetProject' {project} -> project) (\s@GetProject' {} a -> s {project = a} :: GetProject)

instance Core.AWSRequest GetProject where
  type AWSResponse GetProject = GetProjectResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "project")
      )

instance Prelude.Hashable GetProject where
  hashWithSalt _salt GetProject' {..} =
    _salt `Prelude.hashWithSalt` project

instance Prelude.NFData GetProject where
  rnf GetProject' {..} = Prelude.rnf project

instance Core.ToHeaders GetProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetProject where
  toPath GetProject' {..} =
    Prelude.mconcat ["/projects/", Core.toBS project]

instance Core.ToQuery GetProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProjectResponse' smart constructor.
data GetProjectResponse = GetProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing the configuration details of the project.
    project :: Project
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getProjectResponse_httpStatus' - The response's http status code.
--
-- 'project', 'getProjectResponse_project' - A structure containing the configuration details of the project.
newGetProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'project'
  Project ->
  GetProjectResponse
newGetProjectResponse pHttpStatus_ pProject_ =
  GetProjectResponse'
    { httpStatus = pHttpStatus_,
      project = pProject_
    }

-- | The response's http status code.
getProjectResponse_httpStatus :: Lens.Lens' GetProjectResponse Prelude.Int
getProjectResponse_httpStatus = Lens.lens (\GetProjectResponse' {httpStatus} -> httpStatus) (\s@GetProjectResponse' {} a -> s {httpStatus = a} :: GetProjectResponse)

-- | A structure containing the configuration details of the project.
getProjectResponse_project :: Lens.Lens' GetProjectResponse Project
getProjectResponse_project = Lens.lens (\GetProjectResponse' {project} -> project) (\s@GetProjectResponse' {} a -> s {project = a} :: GetProjectResponse)

instance Prelude.NFData GetProjectResponse where
  rnf GetProjectResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf project
