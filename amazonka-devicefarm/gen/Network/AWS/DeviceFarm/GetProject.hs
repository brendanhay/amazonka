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
-- Module      : Network.AWS.DeviceFarm.GetProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a project.
module Network.AWS.DeviceFarm.GetProject
  ( -- * Creating a Request
    GetProject (..),
    newGetProject,

    -- * Request Lenses
    getProject_arn,

    -- * Destructuring the Response
    GetProjectResponse (..),
    newGetProjectResponse,

    -- * Response Lenses
    getProjectResponse_project,
    getProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get project operation.
--
-- /See:/ 'newGetProject' smart constructor.
data GetProject = GetProject'
  { -- | The project\'s ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getProject_arn' - The project\'s ARN.
newGetProject ::
  -- | 'arn'
  Core.Text ->
  GetProject
newGetProject pArn_ = GetProject' {arn = pArn_}

-- | The project\'s ARN.
getProject_arn :: Lens.Lens' GetProject Core.Text
getProject_arn = Lens.lens (\GetProject' {arn} -> arn) (\s@GetProject' {} a -> s {arn = a} :: GetProject)

instance Core.AWSRequest GetProject where
  type AWSResponse GetProject = GetProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProjectResponse'
            Core.<$> (x Core..?> "project")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetProject

instance Core.NFData GetProject

instance Core.ToHeaders GetProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetProject where
  toJSON GetProject' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetProject where
  toPath = Core.const "/"

instance Core.ToQuery GetProject where
  toQuery = Core.const Core.mempty

-- | Represents the result of a get project request.
--
-- /See:/ 'newGetProjectResponse' smart constructor.
data GetProjectResponse = GetProjectResponse'
  { -- | The project to get information about.
    project :: Core.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'getProjectResponse_project' - The project to get information about.
--
-- 'httpStatus', 'getProjectResponse_httpStatus' - The response's http status code.
newGetProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetProjectResponse
newGetProjectResponse pHttpStatus_ =
  GetProjectResponse'
    { project = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The project to get information about.
getProjectResponse_project :: Lens.Lens' GetProjectResponse (Core.Maybe Project)
getProjectResponse_project = Lens.lens (\GetProjectResponse' {project} -> project) (\s@GetProjectResponse' {} a -> s {project = a} :: GetProjectResponse)

-- | The response's http status code.
getProjectResponse_httpStatus :: Lens.Lens' GetProjectResponse Core.Int
getProjectResponse_httpStatus = Lens.lens (\GetProjectResponse' {httpStatus} -> httpStatus) (\s@GetProjectResponse' {} a -> s {httpStatus = a} :: GetProjectResponse)

instance Core.NFData GetProjectResponse
