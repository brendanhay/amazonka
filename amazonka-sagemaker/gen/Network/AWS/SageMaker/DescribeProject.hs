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
-- Module      : Network.AWS.SageMaker.DescribeProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a project.
module Network.AWS.SageMaker.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_projectName,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_serviceCatalogProvisionedProductDetails,
    describeProjectResponse_projectDescription,
    describeProjectResponse_createdBy,
    describeProjectResponse_httpStatus,
    describeProjectResponse_projectArn,
    describeProjectResponse_projectName,
    describeProjectResponse_projectId,
    describeProjectResponse_serviceCatalogProvisioningDetails,
    describeProjectResponse_projectStatus,
    describeProjectResponse_creationTime,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | The name of the project to describe.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'describeProject_projectName' - The name of the project to describe.
newDescribeProject ::
  -- | 'projectName'
  Prelude.Text ->
  DescribeProject
newDescribeProject pProjectName_ =
  DescribeProject' {projectName = pProjectName_}

-- | The name of the project to describe.
describeProject_projectName :: Lens.Lens' DescribeProject Prelude.Text
describeProject_projectName = Lens.lens (\DescribeProject' {projectName} -> projectName) (\s@DescribeProject' {} a -> s {projectName = a} :: DescribeProject)

instance Core.AWSRequest DescribeProject where
  type
    AWSResponse DescribeProject =
      DescribeProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Prelude.<$> ( x
                            Core..?> "ServiceCatalogProvisionedProductDetails"
                        )
            Prelude.<*> (x Core..?> "ProjectDescription")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ProjectArn")
            Prelude.<*> (x Core..:> "ProjectName")
            Prelude.<*> (x Core..:> "ProjectId")
            Prelude.<*> (x Core..:> "ServiceCatalogProvisioningDetails")
            Prelude.<*> (x Core..:> "ProjectStatus")
            Prelude.<*> (x Core..:> "CreationTime")
      )

instance Prelude.Hashable DescribeProject

instance Prelude.NFData DescribeProject

instance Core.ToHeaders DescribeProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeProject" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProject where
  toJSON DescribeProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProjectName" Core..= projectName)]
      )

instance Core.ToPath DescribeProject where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | Information about a provisioned service catalog product.
    serviceCatalogProvisionedProductDetails :: Prelude.Maybe ServiceCatalogProvisionedProductDetails,
    -- | The description of the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Text,
    -- | The ID of the project.
    projectId :: Prelude.Text,
    -- | Information used to provision a service catalog product. For
    -- information, see
    -- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
    serviceCatalogProvisioningDetails :: ServiceCatalogProvisioningDetails,
    -- | The status of the project.
    projectStatus :: ProjectStatus,
    -- | The time when the project was created.
    creationTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCatalogProvisionedProductDetails', 'describeProjectResponse_serviceCatalogProvisionedProductDetails' - Information about a provisioned service catalog product.
--
-- 'projectDescription', 'describeProjectResponse_projectDescription' - The description of the project.
--
-- 'createdBy', 'describeProjectResponse_createdBy' - Undocumented member.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
--
-- 'projectArn', 'describeProjectResponse_projectArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'projectName', 'describeProjectResponse_projectName' - The name of the project.
--
-- 'projectId', 'describeProjectResponse_projectId' - The ID of the project.
--
-- 'serviceCatalogProvisioningDetails', 'describeProjectResponse_serviceCatalogProvisioningDetails' - Information used to provision a service catalog product. For
-- information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
--
-- 'projectStatus', 'describeProjectResponse_projectStatus' - The status of the project.
--
-- 'creationTime', 'describeProjectResponse_creationTime' - The time when the project was created.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'projectName'
  Prelude.Text ->
  -- | 'projectId'
  Prelude.Text ->
  -- | 'serviceCatalogProvisioningDetails'
  ServiceCatalogProvisioningDetails ->
  -- | 'projectStatus'
  ProjectStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  DescribeProjectResponse
newDescribeProjectResponse
  pHttpStatus_
  pProjectArn_
  pProjectName_
  pProjectId_
  pServiceCatalogProvisioningDetails_
  pProjectStatus_
  pCreationTime_ =
    DescribeProjectResponse'
      { serviceCatalogProvisionedProductDetails =
          Prelude.Nothing,
        projectDescription = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        projectArn = pProjectArn_,
        projectName = pProjectName_,
        projectId = pProjectId_,
        serviceCatalogProvisioningDetails =
          pServiceCatalogProvisioningDetails_,
        projectStatus = pProjectStatus_,
        creationTime = Core._Time Lens.# pCreationTime_
      }

-- | Information about a provisioned service catalog product.
describeProjectResponse_serviceCatalogProvisionedProductDetails :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe ServiceCatalogProvisionedProductDetails)
describeProjectResponse_serviceCatalogProvisionedProductDetails = Lens.lens (\DescribeProjectResponse' {serviceCatalogProvisionedProductDetails} -> serviceCatalogProvisionedProductDetails) (\s@DescribeProjectResponse' {} a -> s {serviceCatalogProvisionedProductDetails = a} :: DescribeProjectResponse)

-- | The description of the project.
describeProjectResponse_projectDescription :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_projectDescription = Lens.lens (\DescribeProjectResponse' {projectDescription} -> projectDescription) (\s@DescribeProjectResponse' {} a -> s {projectDescription = a} :: DescribeProjectResponse)

-- | Undocumented member.
describeProjectResponse_createdBy :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe UserContext)
describeProjectResponse_createdBy = Lens.lens (\DescribeProjectResponse' {createdBy} -> createdBy) (\s@DescribeProjectResponse' {} a -> s {createdBy = a} :: DescribeProjectResponse)

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Prelude.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

-- | The Amazon Resource Name (ARN) of the project.
describeProjectResponse_projectArn :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_projectArn = Lens.lens (\DescribeProjectResponse' {projectArn} -> projectArn) (\s@DescribeProjectResponse' {} a -> s {projectArn = a} :: DescribeProjectResponse)

-- | The name of the project.
describeProjectResponse_projectName :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_projectName = Lens.lens (\DescribeProjectResponse' {projectName} -> projectName) (\s@DescribeProjectResponse' {} a -> s {projectName = a} :: DescribeProjectResponse)

-- | The ID of the project.
describeProjectResponse_projectId :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_projectId = Lens.lens (\DescribeProjectResponse' {projectId} -> projectId) (\s@DescribeProjectResponse' {} a -> s {projectId = a} :: DescribeProjectResponse)

-- | Information used to provision a service catalog product. For
-- information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is AWS Service Catalog>.
describeProjectResponse_serviceCatalogProvisioningDetails :: Lens.Lens' DescribeProjectResponse ServiceCatalogProvisioningDetails
describeProjectResponse_serviceCatalogProvisioningDetails = Lens.lens (\DescribeProjectResponse' {serviceCatalogProvisioningDetails} -> serviceCatalogProvisioningDetails) (\s@DescribeProjectResponse' {} a -> s {serviceCatalogProvisioningDetails = a} :: DescribeProjectResponse)

-- | The status of the project.
describeProjectResponse_projectStatus :: Lens.Lens' DescribeProjectResponse ProjectStatus
describeProjectResponse_projectStatus = Lens.lens (\DescribeProjectResponse' {projectStatus} -> projectStatus) (\s@DescribeProjectResponse' {} a -> s {projectStatus = a} :: DescribeProjectResponse)

-- | The time when the project was created.
describeProjectResponse_creationTime :: Lens.Lens' DescribeProjectResponse Prelude.UTCTime
describeProjectResponse_creationTime = Lens.lens (\DescribeProjectResponse' {creationTime} -> creationTime) (\s@DescribeProjectResponse' {} a -> s {creationTime = a} :: DescribeProjectResponse) Prelude.. Core._Time

instance Prelude.NFData DescribeProjectResponse
