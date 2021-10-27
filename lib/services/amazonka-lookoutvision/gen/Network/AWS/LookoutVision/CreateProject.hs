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
-- Module      : Network.AWS.LookoutVision.CreateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty Amazon Lookout for Vision project. After you create the
-- project, add a dataset by calling CreateDataset.
--
-- This operation requires permissions to perform the
-- @lookoutvision:CreateProject@ operation.
module Network.AWS.LookoutVision.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_clientToken,
    createProject_projectName,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_projectMetadata,
    createProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutVision.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | ClientToken is an idempotency token that ensures a call to
    -- @CreateProject@ completes only once. You choose the value to pass. For
    -- example, An issue, such as an network outage, might prevent you from
    -- getting a response from @CreateProject@. In this case, safely retry your
    -- call to @CreateProject@ by using the same @ClientToken@ parameter value.
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @CreateProject@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name for the project.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createProject_clientToken' - ClientToken is an idempotency token that ensures a call to
-- @CreateProject@ completes only once. You choose the value to pass. For
-- example, An issue, such as an network outage, might prevent you from
-- getting a response from @CreateProject@. In this case, safely retry your
-- call to @CreateProject@ by using the same @ClientToken@ parameter value.
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @CreateProject@. An idempotency token is active for 8 hours.
--
-- 'projectName', 'createProject_projectName' - The name for the project.
newCreateProject ::
  -- | 'projectName'
  Prelude.Text ->
  CreateProject
newCreateProject pProjectName_ =
  CreateProject'
    { clientToken = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | ClientToken is an idempotency token that ensures a call to
-- @CreateProject@ completes only once. You choose the value to pass. For
-- example, An issue, such as an network outage, might prevent you from
-- getting a response from @CreateProject@. In this case, safely retry your
-- call to @CreateProject@ by using the same @ClientToken@ parameter value.
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @CreateProject@. An idempotency token is active for 8 hours.
createProject_clientToken :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_clientToken = Lens.lens (\CreateProject' {clientToken} -> clientToken) (\s@CreateProject' {} a -> s {clientToken = a} :: CreateProject)

-- | The name for the project.
createProject_projectName :: Lens.Lens' CreateProject Prelude.Text
createProject_projectName = Lens.lens (\CreateProject' {projectName} -> projectName) (\s@CreateProject' {} a -> s {projectName = a} :: CreateProject)

instance Core.AWSRequest CreateProject where
  type
    AWSResponse CreateProject =
      CreateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Prelude.<$> (x Core..?> "ProjectMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProject

instance Prelude.NFData CreateProject

instance Core.ToHeaders CreateProject where
  toHeaders CreateProject' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProjectName" Core..= projectName)]
      )

instance Core.ToPath CreateProject where
  toPath = Prelude.const "/2020-11-20/projects"

instance Core.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Information about the project.
    projectMetadata :: Prelude.Maybe ProjectMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectMetadata', 'createProjectResponse_projectMetadata' - Information about the project.
--
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { projectMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the project.
createProjectResponse_projectMetadata :: Lens.Lens' CreateProjectResponse (Prelude.Maybe ProjectMetadata)
createProjectResponse_projectMetadata = Lens.lens (\CreateProjectResponse' {projectMetadata} -> projectMetadata) (\s@CreateProjectResponse' {} a -> s {projectMetadata = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse
