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
-- Module      : Network.AWS.Rekognition.CreateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Rekognition Custom Labels project. A project is a
-- logical grouping of resources (images, Labels, models) and operations
-- (training, evaluation and detection).
--
-- This operation requires permissions to perform the
-- @rekognition:CreateProject@ action.
module Network.AWS.Rekognition.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_projectName,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_projectArn,
    createProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | The name of the project to create.
    projectName :: Core.Text
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
-- 'projectName', 'createProject_projectName' - The name of the project to create.
newCreateProject ::
  -- | 'projectName'
  Core.Text ->
  CreateProject
newCreateProject pProjectName_ =
  CreateProject' {projectName = pProjectName_}

-- | The name of the project to create.
createProject_projectName :: Lens.Lens' CreateProject Core.Text
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
            Core.<$> (x Core..?> "ProjectArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProject

instance Core.NFData CreateProject

instance Core.ToHeaders CreateProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.CreateProject" ::
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
          [Core.Just ("ProjectName" Core..= projectName)]
      )

instance Core.ToPath CreateProject where
  toPath = Core.const "/"

instance Core.ToQuery CreateProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the new project. You can use the ARN
    -- to configure IAM access to the project.
    projectArn :: Core.Maybe Core.Text,
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
-- 'projectArn', 'createProjectResponse_projectArn' - The Amazon Resource Name (ARN) of the new project. You can use the ARN
-- to configure IAM access to the project.
--
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { projectArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new project. You can use the ARN
-- to configure IAM access to the project.
createProjectResponse_projectArn :: Lens.Lens' CreateProjectResponse (Core.Maybe Core.Text)
createProjectResponse_projectArn = Lens.lens (\CreateProjectResponse' {projectArn} -> projectArn) (\s@CreateProjectResponse' {} a -> s {projectArn = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Core.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Core.NFData CreateProjectResponse
