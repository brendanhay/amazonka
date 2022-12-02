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
-- Module      : Amazonka.IoT1ClickProjects.CreateProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty project with a placement template. A project contains
-- zero or more placements that adhere to the placement template defined in
-- the project.
module Amazonka.IoT1ClickProjects.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_tags,
    createProject_description,
    createProject_placementTemplate,
    createProject_projectName,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickProjects.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | Optional tags (metadata key\/value pairs) to be associated with the
    -- project. For example,
    -- @{ {\"key1\": \"value1\", \"key2\": \"value2\"} }@. For more
    -- information, see
    -- <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An optional description for the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | The schema defining the placement to be created. A placement template
    -- defines placement default attributes and device templates. You cannot
    -- add or remove device templates after the project has been created.
    -- However, you can update @callbackOverrides@ for the device templates
    -- using the @UpdateProject@ API.
    placementTemplate :: Prelude.Maybe PlacementTemplate,
    -- | The name of the project to create.
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
-- 'tags', 'createProject_tags' - Optional tags (metadata key\/value pairs) to be associated with the
-- project. For example,
-- @{ {\"key1\": \"value1\", \"key2\": \"value2\"} }@. For more
-- information, see
-- <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies>.
--
-- 'description', 'createProject_description' - An optional description for the project.
--
-- 'placementTemplate', 'createProject_placementTemplate' - The schema defining the placement to be created. A placement template
-- defines placement default attributes and device templates. You cannot
-- add or remove device templates after the project has been created.
-- However, you can update @callbackOverrides@ for the device templates
-- using the @UpdateProject@ API.
--
-- 'projectName', 'createProject_projectName' - The name of the project to create.
newCreateProject ::
  -- | 'projectName'
  Prelude.Text ->
  CreateProject
newCreateProject pProjectName_ =
  CreateProject'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      placementTemplate = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | Optional tags (metadata key\/value pairs) to be associated with the
-- project. For example,
-- @{ {\"key1\": \"value1\", \"key2\": \"value2\"} }@. For more
-- information, see
-- <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ AWS Tagging Strategies>.
createProject_tags :: Lens.Lens' CreateProject (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProject_tags = Lens.lens (\CreateProject' {tags} -> tags) (\s@CreateProject' {} a -> s {tags = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

-- | An optional description for the project.
createProject_description :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_description = Lens.lens (\CreateProject' {description} -> description) (\s@CreateProject' {} a -> s {description = a} :: CreateProject)

-- | The schema defining the placement to be created. A placement template
-- defines placement default attributes and device templates. You cannot
-- add or remove device templates after the project has been created.
-- However, you can update @callbackOverrides@ for the device templates
-- using the @UpdateProject@ API.
createProject_placementTemplate :: Lens.Lens' CreateProject (Prelude.Maybe PlacementTemplate)
createProject_placementTemplate = Lens.lens (\CreateProject' {placementTemplate} -> placementTemplate) (\s@CreateProject' {} a -> s {placementTemplate = a} :: CreateProject)

-- | The name of the project to create.
createProject_projectName :: Lens.Lens' CreateProject Prelude.Text
createProject_projectName = Lens.lens (\CreateProject' {projectName} -> projectName) (\s@CreateProject' {} a -> s {projectName = a} :: CreateProject)

instance Core.AWSRequest CreateProject where
  type
    AWSResponse CreateProject =
      CreateProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProject where
  hashWithSalt _salt CreateProject' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` placementTemplate
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData CreateProject where
  rnf CreateProject' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf placementTemplate
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("description" Data..=) Prelude.<$> description,
            ("placementTemplate" Data..=)
              Prelude.<$> placementTemplate,
            Prelude.Just ("projectName" Data..= projectName)
          ]
      )

instance Data.ToPath CreateProject where
  toPath = Prelude.const "/projects"

instance Data.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse where
  rnf CreateProjectResponse' {..} =
    Prelude.rnf httpStatus
