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
-- Module      : Network.AWS.CodeStar.CreateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project, including project resources. This action creates a
-- project based on a submitted project request. A set of source code files
-- and a toolchain template file can be included with the project request.
-- If these are not provided, an empty project is created.
module Network.AWS.CodeStar.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_sourceCode,
    createProject_tags,
    createProject_description,
    createProject_clientRequestToken,
    createProject_toolchain,
    createProject_name,
    createProject_id,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_clientRequestToken,
    createProjectResponse_projectTemplateId,
    createProjectResponse_httpStatus,
    createProjectResponse_id,
    createProjectResponse_arn,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | A list of the Code objects submitted with the project request. If this
    -- parameter is specified, the request must also include the toolchain
    -- parameter.
    sourceCode :: Prelude.Maybe [Code],
    -- | The tags created for the project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the project, if any.
    description :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | A user- or system-generated token that identifies the entity that
    -- requested project creation. This token can be used to repeat the
    -- request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the toolchain template file submitted with the project
    -- request. If this parameter is specified, the request must also include
    -- the sourceCode parameter.
    toolchain :: Prelude.Maybe Toolchain,
    -- | The display name for the project to be created in AWS CodeStar.
    name :: Prelude.Sensitive Prelude.Text,
    -- | The ID of the project to be created in AWS CodeStar.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceCode', 'createProject_sourceCode' - A list of the Code objects submitted with the project request. If this
-- parameter is specified, the request must also include the toolchain
-- parameter.
--
-- 'tags', 'createProject_tags' - The tags created for the project.
--
-- 'description', 'createProject_description' - The description of the project, if any.
--
-- 'clientRequestToken', 'createProject_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested project creation. This token can be used to repeat the
-- request.
--
-- 'toolchain', 'createProject_toolchain' - The name of the toolchain template file submitted with the project
-- request. If this parameter is specified, the request must also include
-- the sourceCode parameter.
--
-- 'name', 'createProject_name' - The display name for the project to be created in AWS CodeStar.
--
-- 'id', 'createProject_id' - The ID of the project to be created in AWS CodeStar.
newCreateProject ::
  -- | 'name'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  CreateProject
newCreateProject pName_ pId_ =
  CreateProject'
    { sourceCode = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      toolchain = Prelude.Nothing,
      name = Prelude._Sensitive Lens.# pName_,
      id = pId_
    }

-- | A list of the Code objects submitted with the project request. If this
-- parameter is specified, the request must also include the toolchain
-- parameter.
createProject_sourceCode :: Lens.Lens' CreateProject (Prelude.Maybe [Code])
createProject_sourceCode = Lens.lens (\CreateProject' {sourceCode} -> sourceCode) (\s@CreateProject' {} a -> s {sourceCode = a} :: CreateProject) Prelude.. Lens.mapping Prelude._Coerce

-- | The tags created for the project.
createProject_tags :: Lens.Lens' CreateProject (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProject_tags = Lens.lens (\CreateProject' {tags} -> tags) (\s@CreateProject' {} a -> s {tags = a} :: CreateProject) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the project, if any.
createProject_description :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_description = Lens.lens (\CreateProject' {description} -> description) (\s@CreateProject' {} a -> s {description = a} :: CreateProject) Prelude.. Lens.mapping Prelude._Sensitive

-- | A user- or system-generated token that identifies the entity that
-- requested project creation. This token can be used to repeat the
-- request.
createProject_clientRequestToken :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_clientRequestToken = Lens.lens (\CreateProject' {clientRequestToken} -> clientRequestToken) (\s@CreateProject' {} a -> s {clientRequestToken = a} :: CreateProject)

-- | The name of the toolchain template file submitted with the project
-- request. If this parameter is specified, the request must also include
-- the sourceCode parameter.
createProject_toolchain :: Lens.Lens' CreateProject (Prelude.Maybe Toolchain)
createProject_toolchain = Lens.lens (\CreateProject' {toolchain} -> toolchain) (\s@CreateProject' {} a -> s {toolchain = a} :: CreateProject)

-- | The display name for the project to be created in AWS CodeStar.
createProject_name :: Lens.Lens' CreateProject Prelude.Text
createProject_name = Lens.lens (\CreateProject' {name} -> name) (\s@CreateProject' {} a -> s {name = a} :: CreateProject) Prelude.. Prelude._Sensitive

-- | The ID of the project to be created in AWS CodeStar.
createProject_id :: Lens.Lens' CreateProject Prelude.Text
createProject_id = Lens.lens (\CreateProject' {id} -> id) (\s@CreateProject' {} a -> s {id = a} :: CreateProject)

instance Prelude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Prelude.<$> (x Prelude..?> "clientRequestToken")
            Prelude.<*> (x Prelude..?> "projectTemplateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "id")
            Prelude.<*> (x Prelude..:> "arn")
      )

instance Prelude.Hashable CreateProject

instance Prelude.NFData CreateProject

instance Prelude.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.CreateProject" ::
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
          [ ("sourceCode" Prelude..=) Prelude.<$> sourceCode,
            ("tags" Prelude..=) Prelude.<$> tags,
            ("description" Prelude..=) Prelude.<$> description,
            ("clientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            ("toolchain" Prelude..=) Prelude.<$> toolchain,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("id" Prelude..= id)
          ]
      )

instance Prelude.ToPath CreateProject where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | A user- or system-generated token that identifies the entity that
    -- requested project creation.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    projectTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the project.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the created project.
    arn :: Prelude.Text
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
-- 'clientRequestToken', 'createProjectResponse_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested project creation.
--
-- 'projectTemplateId', 'createProjectResponse_projectTemplateId' - Reserved for future use.
--
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
--
-- 'id', 'createProjectResponse_id' - The ID of the project.
--
-- 'arn', 'createProjectResponse_arn' - The Amazon Resource Name (ARN) of the created project.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ pId_ pArn_ =
  CreateProjectResponse'
    { clientRequestToken =
        Prelude.Nothing,
      projectTemplateId = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      id = pId_,
      arn = pArn_
    }

-- | A user- or system-generated token that identifies the entity that
-- requested project creation.
createProjectResponse_clientRequestToken :: Lens.Lens' CreateProjectResponse (Prelude.Maybe Prelude.Text)
createProjectResponse_clientRequestToken = Lens.lens (\CreateProjectResponse' {clientRequestToken} -> clientRequestToken) (\s@CreateProjectResponse' {} a -> s {clientRequestToken = a} :: CreateProjectResponse)

-- | Reserved for future use.
createProjectResponse_projectTemplateId :: Lens.Lens' CreateProjectResponse (Prelude.Maybe Prelude.Text)
createProjectResponse_projectTemplateId = Lens.lens (\CreateProjectResponse' {projectTemplateId} -> projectTemplateId) (\s@CreateProjectResponse' {} a -> s {projectTemplateId = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

-- | The ID of the project.
createProjectResponse_id :: Lens.Lens' CreateProjectResponse Prelude.Text
createProjectResponse_id = Lens.lens (\CreateProjectResponse' {id} -> id) (\s@CreateProjectResponse' {} a -> s {id = a} :: CreateProjectResponse)

-- | The Amazon Resource Name (ARN) of the created project.
createProjectResponse_arn :: Lens.Lens' CreateProjectResponse Prelude.Text
createProjectResponse_arn = Lens.lens (\CreateProjectResponse' {arn} -> arn) (\s@CreateProjectResponse' {} a -> s {arn = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse
