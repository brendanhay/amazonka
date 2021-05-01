{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | The name of the project to create.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateProject
newCreateProject pProjectName_ =
  CreateProject' {projectName = pProjectName_}

-- | The name of the project to create.
createProject_projectName :: Lens.Lens' CreateProject Prelude.Text
createProject_projectName = Lens.lens (\CreateProject' {projectName} -> projectName) (\s@CreateProject' {} a -> s {projectName = a} :: CreateProject)

instance Prelude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Prelude.<$> (x Prelude..?> "ProjectArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProject

instance Prelude.NFData CreateProject

instance Prelude.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.CreateProject" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProjectName" Prelude..= projectName)
          ]
      )

instance Prelude.ToPath CreateProject where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the new project. You can use the ARN
    -- to configure IAM access to the project.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { projectArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new project. You can use the ARN
-- to configure IAM access to the project.
createProjectResponse_projectArn :: Lens.Lens' CreateProjectResponse (Prelude.Maybe Prelude.Text)
createProjectResponse_projectArn = Lens.lens (\CreateProjectResponse' {projectArn} -> projectArn) (\s@CreateProjectResponse' {} a -> s {projectArn = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse
