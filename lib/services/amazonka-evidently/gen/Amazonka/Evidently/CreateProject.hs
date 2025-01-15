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
-- Module      : Amazonka.Evidently.CreateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project, which is the logical object in Evidently that can
-- contain features, launches, and experiments. Use projects to group
-- similar features together.
--
-- To update an existing project, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_UpdateProject.html UpdateProject>.
module Amazonka.Evidently.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_appConfigResource,
    createProject_dataDelivery,
    createProject_description,
    createProject_tags,
    createProject_name,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_httpStatus,
    createProjectResponse_project,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | Use this parameter if the project will use /client-side evaluation
    -- powered by AppConfig/. Client-side evaluation allows your application to
    -- assign variations to user sessions locally instead of by calling the
    -- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
    -- operation. This mitigates the latency and availability risks that come
    -- with an API call. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-client-side-evaluation.html Client-side evaluation - powered by AppConfig.>
    --
    -- This parameter is a structure that contains information about the
    -- AppConfig application and environment that will be used as for
    -- client-side evaluation.
    --
    -- To create a project that uses client-side evaluation, you must have the
    -- @evidently:ExportProjectAsConfiguration@ permission.
    appConfigResource :: Prelude.Maybe ProjectAppConfigResourceConfig,
    -- | A structure that contains information about where Evidently is to store
    -- evaluation events for longer term storage, if you choose to do so. If
    -- you choose not to store these events, Evidently deletes them after using
    -- them to produce metrics and other experiment results that you can view.
    dataDelivery :: Prelude.Maybe ProjectDataDeliveryConfig,
    -- | An optional description of the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | Assigns one or more tags (key-value pairs) to the project.
    --
    -- Tags can help you organize and categorize your resources. You can also
    -- use them to scope user permissions by granting a user permission to
    -- access or change only resources with certain tag values.
    --
    -- Tags don\'t have any semantic meaning to Amazon Web Services and are
    -- interpreted strictly as strings of characters.
    --
    -- >  <p>You can associate as many as 50 tags with a project.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name for the project.
    name :: Prelude.Text
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
-- 'appConfigResource', 'createProject_appConfigResource' - Use this parameter if the project will use /client-side evaluation
-- powered by AppConfig/. Client-side evaluation allows your application to
-- assign variations to user sessions locally instead of by calling the
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
-- operation. This mitigates the latency and availability risks that come
-- with an API call. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-client-side-evaluation.html Client-side evaluation - powered by AppConfig.>
--
-- This parameter is a structure that contains information about the
-- AppConfig application and environment that will be used as for
-- client-side evaluation.
--
-- To create a project that uses client-side evaluation, you must have the
-- @evidently:ExportProjectAsConfiguration@ permission.
--
-- 'dataDelivery', 'createProject_dataDelivery' - A structure that contains information about where Evidently is to store
-- evaluation events for longer term storage, if you choose to do so. If
-- you choose not to store these events, Evidently deletes them after using
-- them to produce metrics and other experiment results that you can view.
--
-- 'description', 'createProject_description' - An optional description of the project.
--
-- 'tags', 'createProject_tags' - Assigns one or more tags (key-value pairs) to the project.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with a project.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
--
-- 'name', 'createProject_name' - The name for the project.
newCreateProject ::
  -- | 'name'
  Prelude.Text ->
  CreateProject
newCreateProject pName_ =
  CreateProject'
    { appConfigResource = Prelude.Nothing,
      dataDelivery = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | Use this parameter if the project will use /client-side evaluation
-- powered by AppConfig/. Client-side evaluation allows your application to
-- assign variations to user sessions locally instead of by calling the
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
-- operation. This mitigates the latency and availability risks that come
-- with an API call. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-client-side-evaluation.html Client-side evaluation - powered by AppConfig.>
--
-- This parameter is a structure that contains information about the
-- AppConfig application and environment that will be used as for
-- client-side evaluation.
--
-- To create a project that uses client-side evaluation, you must have the
-- @evidently:ExportProjectAsConfiguration@ permission.
createProject_appConfigResource :: Lens.Lens' CreateProject (Prelude.Maybe ProjectAppConfigResourceConfig)
createProject_appConfigResource = Lens.lens (\CreateProject' {appConfigResource} -> appConfigResource) (\s@CreateProject' {} a -> s {appConfigResource = a} :: CreateProject)

-- | A structure that contains information about where Evidently is to store
-- evaluation events for longer term storage, if you choose to do so. If
-- you choose not to store these events, Evidently deletes them after using
-- them to produce metrics and other experiment results that you can view.
createProject_dataDelivery :: Lens.Lens' CreateProject (Prelude.Maybe ProjectDataDeliveryConfig)
createProject_dataDelivery = Lens.lens (\CreateProject' {dataDelivery} -> dataDelivery) (\s@CreateProject' {} a -> s {dataDelivery = a} :: CreateProject)

-- | An optional description of the project.
createProject_description :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_description = Lens.lens (\CreateProject' {description} -> description) (\s@CreateProject' {} a -> s {description = a} :: CreateProject)

-- | Assigns one or more tags (key-value pairs) to the project.
--
-- Tags can help you organize and categorize your resources. You can also
-- use them to scope user permissions by granting a user permission to
-- access or change only resources with certain tag values.
--
-- Tags don\'t have any semantic meaning to Amazon Web Services and are
-- interpreted strictly as strings of characters.
--
-- >  <p>You can associate as many as 50 tags with a project.</p> <p>For more information, see <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services resources</a>.</p>
createProject_tags :: Lens.Lens' CreateProject (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProject_tags = Lens.lens (\CreateProject' {tags} -> tags) (\s@CreateProject' {} a -> s {tags = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

-- | The name for the project.
createProject_name :: Lens.Lens' CreateProject Prelude.Text
createProject_name = Lens.lens (\CreateProject' {name} -> name) (\s@CreateProject' {} a -> s {name = a} :: CreateProject)

instance Core.AWSRequest CreateProject where
  type
    AWSResponse CreateProject =
      CreateProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "project")
      )

instance Prelude.Hashable CreateProject where
  hashWithSalt _salt CreateProject' {..} =
    _salt
      `Prelude.hashWithSalt` appConfigResource
      `Prelude.hashWithSalt` dataDelivery
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateProject where
  rnf CreateProject' {..} =
    Prelude.rnf appConfigResource `Prelude.seq`
      Prelude.rnf dataDelivery `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf name

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
          [ ("appConfigResource" Data..=)
              Prelude.<$> appConfigResource,
            ("dataDelivery" Data..=) Prelude.<$> dataDelivery,
            ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateProject where
  toPath = Prelude.const "/projects"

instance Data.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains information about the created project.
    project :: Project
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
--
-- 'project', 'createProjectResponse_project' - A structure that contains information about the created project.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'project'
  Project ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ pProject_ =
  CreateProjectResponse'
    { httpStatus = pHttpStatus_,
      project = pProject_
    }

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

-- | A structure that contains information about the created project.
createProjectResponse_project :: Lens.Lens' CreateProjectResponse Project
createProjectResponse_project = Lens.lens (\CreateProjectResponse' {project} -> project) (\s@CreateProjectResponse' {} a -> s {project = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse where
  rnf CreateProjectResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf project
