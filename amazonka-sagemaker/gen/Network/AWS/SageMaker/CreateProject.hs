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
-- Module      : Network.AWS.SageMaker.CreateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a machine learning (ML) project that can contain one or more
-- templates that set up an ML pipeline from training to deploying an
-- approved model.
module Network.AWS.SageMaker.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_tags,
    createProject_projectDescription,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | An array of key-value pairs that you want to use to organize and track
    -- your AWS resource costs. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
    -- in the /AWS General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | A description for the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Text,
    -- | The product ID and provisioning artifact ID to provision a service
    -- catalog. For information, see
    -- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
    serviceCatalogProvisioningDetails :: ServiceCatalogProvisioningDetails
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
-- 'tags', 'createProject_tags' - An array of key-value pairs that you want to use to organize and track
-- your AWS resource costs. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
--
-- 'projectDescription', 'createProject_projectDescription' - A description for the project.
--
-- 'projectName', 'createProject_projectName' - The name of the project.
--
-- 'serviceCatalogProvisioningDetails', 'createProject_serviceCatalogProvisioningDetails' - The product ID and provisioning artifact ID to provision a service
-- catalog. For information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
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
      { tags = Prelude.Nothing,
        projectDescription = Prelude.Nothing,
        projectName = pProjectName_,
        serviceCatalogProvisioningDetails =
          pServiceCatalogProvisioningDetails_
      }

-- | An array of key-value pairs that you want to use to organize and track
-- your AWS resource costs. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS resources>
-- in the /AWS General Reference Guide/.
createProject_tags :: Lens.Lens' CreateProject (Prelude.Maybe [Tag])
createProject_tags = Lens.lens (\CreateProject' {tags} -> tags) (\s@CreateProject' {} a -> s {tags = a} :: CreateProject) Prelude.. Lens.mapping Prelude._Coerce

-- | A description for the project.
createProject_projectDescription :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_projectDescription = Lens.lens (\CreateProject' {projectDescription} -> projectDescription) (\s@CreateProject' {} a -> s {projectDescription = a} :: CreateProject)

-- | The name of the project.
createProject_projectName :: Lens.Lens' CreateProject Prelude.Text
createProject_projectName = Lens.lens (\CreateProject' {projectName} -> projectName) (\s@CreateProject' {} a -> s {projectName = a} :: CreateProject)

-- | The product ID and provisioning artifact ID to provision a service
-- catalog. For information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
createProject_serviceCatalogProvisioningDetails :: Lens.Lens' CreateProject ServiceCatalogProvisioningDetails
createProject_serviceCatalogProvisioningDetails = Lens.lens (\CreateProject' {serviceCatalogProvisioningDetails} -> serviceCatalogProvisioningDetails) (\s@CreateProject' {} a -> s {serviceCatalogProvisioningDetails = a} :: CreateProject)

instance Prelude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "ProjectArn")
            Prelude.<*> (x Prelude..:> "ProjectId")
      )

instance Prelude.Hashable CreateProject

instance Prelude.NFData CreateProject

instance Prelude.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.CreateProject" :: Prelude.ByteString),
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
          [ ("Tags" Prelude..=) Prelude.<$> tags,
            ("ProjectDescription" Prelude..=)
              Prelude.<$> projectDescription,
            Prelude.Just ("ProjectName" Prelude..= projectName),
            Prelude.Just
              ( "ServiceCatalogProvisioningDetails"
                  Prelude..= serviceCatalogProvisioningDetails
              )
          ]
      )

instance Prelude.ToPath CreateProject where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProject where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateProjectResponse
