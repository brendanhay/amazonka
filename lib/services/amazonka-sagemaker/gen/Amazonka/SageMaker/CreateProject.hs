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
-- Module      : Amazonka.SageMaker.CreateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a machine learning (ML) project that can contain one or more
-- templates that set up an ML pipeline from training to deploying an
-- approved model.
module Amazonka.SageMaker.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_projectDescription,
    createProject_tags,
    createProject_projectName,
    createProject_serviceCatalogProvisioningDetails,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_httpStatus,
    createProjectResponse_projectArn,
    createProjectResponse_projectId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | A description for the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    -- | An array of key-value pairs that you want to use to organize and track
    -- your Amazon Web Services resource costs. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the project.
    projectName :: Prelude.Text,
    -- | The product ID and provisioning artifact ID to provision a service
    -- catalog. The provisioning artifact ID will default to the latest
    -- provisioning artifact ID of the product, if you don\'t provide the
    -- provisioning artifact ID. For more information, see
    -- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
    serviceCatalogProvisioningDetails :: ServiceCatalogProvisioningDetails
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
-- 'projectDescription', 'createProject_projectDescription' - A description for the project.
--
-- 'tags', 'createProject_tags' - An array of key-value pairs that you want to use to organize and track
-- your Amazon Web Services resource costs. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'projectName', 'createProject_projectName' - The name of the project.
--
-- 'serviceCatalogProvisioningDetails', 'createProject_serviceCatalogProvisioningDetails' - The product ID and provisioning artifact ID to provision a service
-- catalog. The provisioning artifact ID will default to the latest
-- provisioning artifact ID of the product, if you don\'t provide the
-- provisioning artifact ID. For more information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
newCreateProject ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'serviceCatalogProvisioningDetails'
  ServiceCatalogProvisioningDetails ->
  CreateProject
newCreateProject
  pProjectName_
  pServiceCatalogProvisioningDetails_ =
    CreateProject'
      { projectDescription =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        projectName = pProjectName_,
        serviceCatalogProvisioningDetails =
          pServiceCatalogProvisioningDetails_
      }

-- | A description for the project.
createProject_projectDescription :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_projectDescription = Lens.lens (\CreateProject' {projectDescription} -> projectDescription) (\s@CreateProject' {} a -> s {projectDescription = a} :: CreateProject)

-- | An array of key-value pairs that you want to use to organize and track
-- your Amazon Web Services resource costs. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
createProject_tags :: Lens.Lens' CreateProject (Prelude.Maybe [Tag])
createProject_tags = Lens.lens (\CreateProject' {tags} -> tags) (\s@CreateProject' {} a -> s {tags = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

-- | The name of the project.
createProject_projectName :: Lens.Lens' CreateProject Prelude.Text
createProject_projectName = Lens.lens (\CreateProject' {projectName} -> projectName) (\s@CreateProject' {} a -> s {projectName = a} :: CreateProject)

-- | The product ID and provisioning artifact ID to provision a service
-- catalog. The provisioning artifact ID will default to the latest
-- provisioning artifact ID of the product, if you don\'t provide the
-- provisioning artifact ID. For more information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
createProject_serviceCatalogProvisioningDetails :: Lens.Lens' CreateProject ServiceCatalogProvisioningDetails
createProject_serviceCatalogProvisioningDetails = Lens.lens (\CreateProject' {serviceCatalogProvisioningDetails} -> serviceCatalogProvisioningDetails) (\s@CreateProject' {} a -> s {serviceCatalogProvisioningDetails = a} :: CreateProject)

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
            Prelude.<*> (x Data..:> "ProjectArn")
            Prelude.<*> (x Data..:> "ProjectId")
      )

instance Prelude.Hashable CreateProject where
  hashWithSalt _salt CreateProject' {..} =
    _salt
      `Prelude.hashWithSalt` projectDescription
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` serviceCatalogProvisioningDetails

instance Prelude.NFData CreateProject where
  rnf CreateProject' {..} =
    Prelude.rnf projectDescription
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf serviceCatalogProvisioningDetails

instance Data.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.CreateProject" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ProjectDescription" Data..=)
              Prelude.<$> projectDescription,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ProjectName" Data..= projectName),
            Prelude.Just
              ( "ServiceCatalogProvisioningDetails"
                  Data..= serviceCatalogProvisioningDetails
              )
          ]
      )

instance Data.ToPath CreateProject where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Text,
    -- | The ID of the new project.
    projectId :: Prelude.Text
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
-- 'projectArn', 'createProjectResponse_projectArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'projectId', 'createProjectResponse_projectId' - The ID of the new project.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'projectId'
  Prelude.Text ->
  CreateProjectResponse
newCreateProjectResponse
  pHttpStatus_
  pProjectArn_
  pProjectId_ =
    CreateProjectResponse'
      { httpStatus = pHttpStatus_,
        projectArn = pProjectArn_,
        projectId = pProjectId_
      }

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

-- | The Amazon Resource Name (ARN) of the project.
createProjectResponse_projectArn :: Lens.Lens' CreateProjectResponse Prelude.Text
createProjectResponse_projectArn = Lens.lens (\CreateProjectResponse' {projectArn} -> projectArn) (\s@CreateProjectResponse' {} a -> s {projectArn = a} :: CreateProjectResponse)

-- | The ID of the new project.
createProjectResponse_projectId :: Lens.Lens' CreateProjectResponse Prelude.Text
createProjectResponse_projectId = Lens.lens (\CreateProjectResponse' {projectId} -> projectId) (\s@CreateProjectResponse' {} a -> s {projectId = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse where
  rnf CreateProjectResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf projectId
