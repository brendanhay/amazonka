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
-- Module      : Amazonka.SageMaker.DescribeProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the details of a project.
module Amazonka.SageMaker.DescribeProject
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
    describeProjectResponse_lastModifiedTime,
    describeProjectResponse_projectDescription,
    describeProjectResponse_lastModifiedBy,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Prelude.<$> ( x
                            Data..?> "ServiceCatalogProvisionedProductDetails"
                        )
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "ProjectDescription")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProjectArn")
            Prelude.<*> (x Data..:> "ProjectName")
            Prelude.<*> (x Data..:> "ProjectId")
            Prelude.<*> (x Data..:> "ServiceCatalogProvisioningDetails")
            Prelude.<*> (x Data..:> "ProjectStatus")
            Prelude.<*> (x Data..:> "CreationTime")
      )

instance Prelude.Hashable DescribeProject where
  hashWithSalt _salt DescribeProject' {..} =
    _salt `Prelude.hashWithSalt` projectName

instance Prelude.NFData DescribeProject where
  rnf DescribeProject' {..} = Prelude.rnf projectName

instance Data.ToHeaders DescribeProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DescribeProject" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProject where
  toJSON DescribeProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProjectName" Data..= projectName)]
      )

instance Data.ToPath DescribeProject where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | Information about a provisioned service catalog product.
    serviceCatalogProvisionedProductDetails :: Prelude.Maybe ServiceCatalogProvisionedProductDetails,
    -- | The timestamp when project was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
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
    -- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
    serviceCatalogProvisioningDetails :: ServiceCatalogProvisioningDetails,
    -- | The status of the project.
    projectStatus :: ProjectStatus,
    -- | The time when the project was created.
    creationTime :: Data.POSIX
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
-- 'lastModifiedTime', 'describeProjectResponse_lastModifiedTime' - The timestamp when project was last modified.
--
-- 'projectDescription', 'describeProjectResponse_projectDescription' - The description of the project.
--
-- 'lastModifiedBy', 'describeProjectResponse_lastModifiedBy' - Undocumented member.
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
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
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
        lastModifiedTime = Prelude.Nothing,
        projectDescription = Prelude.Nothing,
        lastModifiedBy = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        projectArn = pProjectArn_,
        projectName = pProjectName_,
        projectId = pProjectId_,
        serviceCatalogProvisioningDetails =
          pServiceCatalogProvisioningDetails_,
        projectStatus = pProjectStatus_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | Information about a provisioned service catalog product.
describeProjectResponse_serviceCatalogProvisionedProductDetails :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe ServiceCatalogProvisionedProductDetails)
describeProjectResponse_serviceCatalogProvisionedProductDetails = Lens.lens (\DescribeProjectResponse' {serviceCatalogProvisionedProductDetails} -> serviceCatalogProvisionedProductDetails) (\s@DescribeProjectResponse' {} a -> s {serviceCatalogProvisionedProductDetails = a} :: DescribeProjectResponse)

-- | The timestamp when project was last modified.
describeProjectResponse_lastModifiedTime :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.UTCTime)
describeProjectResponse_lastModifiedTime = Lens.lens (\DescribeProjectResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeProjectResponse' {} a -> s {lastModifiedTime = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the project.
describeProjectResponse_projectDescription :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_projectDescription = Lens.lens (\DescribeProjectResponse' {projectDescription} -> projectDescription) (\s@DescribeProjectResponse' {} a -> s {projectDescription = a} :: DescribeProjectResponse)

-- | Undocumented member.
describeProjectResponse_lastModifiedBy :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe UserContext)
describeProjectResponse_lastModifiedBy = Lens.lens (\DescribeProjectResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeProjectResponse' {} a -> s {lastModifiedBy = a} :: DescribeProjectResponse)

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
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
describeProjectResponse_serviceCatalogProvisioningDetails :: Lens.Lens' DescribeProjectResponse ServiceCatalogProvisioningDetails
describeProjectResponse_serviceCatalogProvisioningDetails = Lens.lens (\DescribeProjectResponse' {serviceCatalogProvisioningDetails} -> serviceCatalogProvisioningDetails) (\s@DescribeProjectResponse' {} a -> s {serviceCatalogProvisioningDetails = a} :: DescribeProjectResponse)

-- | The status of the project.
describeProjectResponse_projectStatus :: Lens.Lens' DescribeProjectResponse ProjectStatus
describeProjectResponse_projectStatus = Lens.lens (\DescribeProjectResponse' {projectStatus} -> projectStatus) (\s@DescribeProjectResponse' {} a -> s {projectStatus = a} :: DescribeProjectResponse)

-- | The time when the project was created.
describeProjectResponse_creationTime :: Lens.Lens' DescribeProjectResponse Prelude.UTCTime
describeProjectResponse_creationTime = Lens.lens (\DescribeProjectResponse' {creationTime} -> creationTime) (\s@DescribeProjectResponse' {} a -> s {creationTime = a} :: DescribeProjectResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeProjectResponse where
  rnf DescribeProjectResponse' {..} =
    Prelude.rnf serviceCatalogProvisionedProductDetails
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf projectDescription
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf projectId
      `Prelude.seq` Prelude.rnf serviceCatalogProvisioningDetails
      `Prelude.seq` Prelude.rnf projectStatus
      `Prelude.seq` Prelude.rnf creationTime
