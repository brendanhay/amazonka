{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.Project
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Project where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProjectStatus
import Amazonka.SageMaker.Types.ServiceCatalogProvisionedProductDetails
import Amazonka.SageMaker.Types.ServiceCatalogProvisioningDetails
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | The properties of a project as returned by the Search API.
--
-- /See:/ 'newProject' smart constructor.
data Project = Project'
  { -- | Who created the project.
    createdBy :: Prelude.Maybe UserContext,
    -- | A timestamp specifying when the project was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | A timestamp container for when the project was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the project.
    projectId :: Prelude.Maybe Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The status of the project.
    projectStatus :: Prelude.Maybe ProjectStatus,
    serviceCatalogProvisionedProductDetails :: Prelude.Maybe ServiceCatalogProvisionedProductDetails,
    serviceCatalogProvisioningDetails :: Prelude.Maybe ServiceCatalogProvisioningDetails,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Project' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBy', 'project_createdBy' - Who created the project.
--
-- 'creationTime', 'project_creationTime' - A timestamp specifying when the project was created.
--
-- 'lastModifiedBy', 'project_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'project_lastModifiedTime' - A timestamp container for when the project was last modified.
--
-- 'projectArn', 'project_projectArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'projectDescription', 'project_projectDescription' - The description of the project.
--
-- 'projectId', 'project_projectId' - The ID of the project.
--
-- 'projectName', 'project_projectName' - The name of the project.
--
-- 'projectStatus', 'project_projectStatus' - The status of the project.
--
-- 'serviceCatalogProvisionedProductDetails', 'project_serviceCatalogProvisionedProductDetails' - Undocumented member.
--
-- 'serviceCatalogProvisioningDetails', 'project_serviceCatalogProvisioningDetails' - Undocumented member.
--
-- 'tags', 'project_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
newProject ::
  Project
newProject =
  Project'
    { createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      projectArn = Prelude.Nothing,
      projectDescription = Prelude.Nothing,
      projectId = Prelude.Nothing,
      projectName = Prelude.Nothing,
      projectStatus = Prelude.Nothing,
      serviceCatalogProvisionedProductDetails =
        Prelude.Nothing,
      serviceCatalogProvisioningDetails = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Who created the project.
project_createdBy :: Lens.Lens' Project (Prelude.Maybe UserContext)
project_createdBy = Lens.lens (\Project' {createdBy} -> createdBy) (\s@Project' {} a -> s {createdBy = a} :: Project)

-- | A timestamp specifying when the project was created.
project_creationTime :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_creationTime = Lens.lens (\Project' {creationTime} -> creationTime) (\s@Project' {} a -> s {creationTime = a} :: Project) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
project_lastModifiedBy :: Lens.Lens' Project (Prelude.Maybe UserContext)
project_lastModifiedBy = Lens.lens (\Project' {lastModifiedBy} -> lastModifiedBy) (\s@Project' {} a -> s {lastModifiedBy = a} :: Project)

-- | A timestamp container for when the project was last modified.
project_lastModifiedTime :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_lastModifiedTime = Lens.lens (\Project' {lastModifiedTime} -> lastModifiedTime) (\s@Project' {} a -> s {lastModifiedTime = a} :: Project) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the project.
project_projectArn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectArn = Lens.lens (\Project' {projectArn} -> projectArn) (\s@Project' {} a -> s {projectArn = a} :: Project)

-- | The description of the project.
project_projectDescription :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectDescription = Lens.lens (\Project' {projectDescription} -> projectDescription) (\s@Project' {} a -> s {projectDescription = a} :: Project)

-- | The ID of the project.
project_projectId :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectId = Lens.lens (\Project' {projectId} -> projectId) (\s@Project' {} a -> s {projectId = a} :: Project)

-- | The name of the project.
project_projectName :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectName = Lens.lens (\Project' {projectName} -> projectName) (\s@Project' {} a -> s {projectName = a} :: Project)

-- | The status of the project.
project_projectStatus :: Lens.Lens' Project (Prelude.Maybe ProjectStatus)
project_projectStatus = Lens.lens (\Project' {projectStatus} -> projectStatus) (\s@Project' {} a -> s {projectStatus = a} :: Project)

-- | Undocumented member.
project_serviceCatalogProvisionedProductDetails :: Lens.Lens' Project (Prelude.Maybe ServiceCatalogProvisionedProductDetails)
project_serviceCatalogProvisionedProductDetails = Lens.lens (\Project' {serviceCatalogProvisionedProductDetails} -> serviceCatalogProvisionedProductDetails) (\s@Project' {} a -> s {serviceCatalogProvisionedProductDetails = a} :: Project)

-- | Undocumented member.
project_serviceCatalogProvisioningDetails :: Lens.Lens' Project (Prelude.Maybe ServiceCatalogProvisioningDetails)
project_serviceCatalogProvisioningDetails = Lens.lens (\Project' {serviceCatalogProvisioningDetails} -> serviceCatalogProvisioningDetails) (\s@Project' {} a -> s {serviceCatalogProvisioningDetails = a} :: Project)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
project_tags :: Lens.Lens' Project (Prelude.Maybe [Tag])
project_tags = Lens.lens (\Project' {tags} -> tags) (\s@Project' {} a -> s {tags = a} :: Project) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Project where
  parseJSON =
    Data.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ProjectArn")
            Prelude.<*> (x Data..:? "ProjectDescription")
            Prelude.<*> (x Data..:? "ProjectId")
            Prelude.<*> (x Data..:? "ProjectName")
            Prelude.<*> (x Data..:? "ProjectStatus")
            Prelude.<*> ( x
                            Data..:? "ServiceCatalogProvisionedProductDetails"
                        )
            Prelude.<*> (x Data..:? "ServiceCatalogProvisioningDetails")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Project where
  hashWithSalt _salt Project' {..} =
    _salt `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` projectArn
      `Prelude.hashWithSalt` projectDescription
      `Prelude.hashWithSalt` projectId
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` projectStatus
      `Prelude.hashWithSalt` serviceCatalogProvisionedProductDetails
      `Prelude.hashWithSalt` serviceCatalogProvisioningDetails
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Project where
  rnf Project' {..} =
    Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf projectDescription
      `Prelude.seq` Prelude.rnf projectId
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf projectStatus
      `Prelude.seq` Prelude.rnf
        serviceCatalogProvisionedProductDetails
      `Prelude.seq` Prelude.rnf serviceCatalogProvisioningDetails
      `Prelude.seq` Prelude.rnf tags
