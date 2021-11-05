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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Project where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | A timestamp specifying when the project was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    serviceCatalogProvisionedProductDetails :: Prelude.Maybe ServiceCatalogProvisionedProductDetails,
    -- | Who created the project.
    createdBy :: Prelude.Maybe UserContext,
    -- | The status of the project.
    projectStatus :: Prelude.Maybe ProjectStatus,
    -- | The name of the project.
    projectName :: Prelude.Maybe Prelude.Text,
    serviceCatalogProvisioningDetails :: Prelude.Maybe ServiceCatalogProvisioningDetails,
    -- | The ID of the project.
    projectId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
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
-- 'creationTime', 'project_creationTime' - A timestamp specifying when the project was created.
--
-- 'serviceCatalogProvisionedProductDetails', 'project_serviceCatalogProvisionedProductDetails' - Undocumented member.
--
-- 'createdBy', 'project_createdBy' - Who created the project.
--
-- 'projectStatus', 'project_projectStatus' - The status of the project.
--
-- 'projectName', 'project_projectName' - The name of the project.
--
-- 'serviceCatalogProvisioningDetails', 'project_serviceCatalogProvisioningDetails' - Undocumented member.
--
-- 'projectId', 'project_projectId' - The ID of the project.
--
-- 'projectArn', 'project_projectArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'projectDescription', 'project_projectDescription' - The description of the project.
--
-- 'tags', 'project_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
newProject ::
  Project
newProject =
  Project'
    { creationTime = Prelude.Nothing,
      serviceCatalogProvisionedProductDetails =
        Prelude.Nothing,
      createdBy = Prelude.Nothing,
      projectStatus = Prelude.Nothing,
      projectName = Prelude.Nothing,
      serviceCatalogProvisioningDetails = Prelude.Nothing,
      projectId = Prelude.Nothing,
      projectArn = Prelude.Nothing,
      projectDescription = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A timestamp specifying when the project was created.
project_creationTime :: Lens.Lens' Project (Prelude.Maybe Prelude.UTCTime)
project_creationTime = Lens.lens (\Project' {creationTime} -> creationTime) (\s@Project' {} a -> s {creationTime = a} :: Project) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
project_serviceCatalogProvisionedProductDetails :: Lens.Lens' Project (Prelude.Maybe ServiceCatalogProvisionedProductDetails)
project_serviceCatalogProvisionedProductDetails = Lens.lens (\Project' {serviceCatalogProvisionedProductDetails} -> serviceCatalogProvisionedProductDetails) (\s@Project' {} a -> s {serviceCatalogProvisionedProductDetails = a} :: Project)

-- | Who created the project.
project_createdBy :: Lens.Lens' Project (Prelude.Maybe UserContext)
project_createdBy = Lens.lens (\Project' {createdBy} -> createdBy) (\s@Project' {} a -> s {createdBy = a} :: Project)

-- | The status of the project.
project_projectStatus :: Lens.Lens' Project (Prelude.Maybe ProjectStatus)
project_projectStatus = Lens.lens (\Project' {projectStatus} -> projectStatus) (\s@Project' {} a -> s {projectStatus = a} :: Project)

-- | The name of the project.
project_projectName :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectName = Lens.lens (\Project' {projectName} -> projectName) (\s@Project' {} a -> s {projectName = a} :: Project)

-- | Undocumented member.
project_serviceCatalogProvisioningDetails :: Lens.Lens' Project (Prelude.Maybe ServiceCatalogProvisioningDetails)
project_serviceCatalogProvisioningDetails = Lens.lens (\Project' {serviceCatalogProvisioningDetails} -> serviceCatalogProvisioningDetails) (\s@Project' {} a -> s {serviceCatalogProvisioningDetails = a} :: Project)

-- | The ID of the project.
project_projectId :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectId = Lens.lens (\Project' {projectId} -> projectId) (\s@Project' {} a -> s {projectId = a} :: Project)

-- | The Amazon Resource Name (ARN) of the project.
project_projectArn :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectArn = Lens.lens (\Project' {projectArn} -> projectArn) (\s@Project' {} a -> s {projectArn = a} :: Project)

-- | The description of the project.
project_projectDescription :: Lens.Lens' Project (Prelude.Maybe Prelude.Text)
project_projectDescription = Lens.lens (\Project' {projectDescription} -> projectDescription) (\s@Project' {} a -> s {projectDescription = a} :: Project)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
project_tags :: Lens.Lens' Project (Prelude.Maybe [Tag])
project_tags = Lens.lens (\Project' {tags} -> tags) (\s@Project' {} a -> s {tags = a} :: Project) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Project where
  parseJSON =
    Core.withObject
      "Project"
      ( \x ->
          Project'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> ( x
                            Core..:? "ServiceCatalogProvisionedProductDetails"
                        )
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "ProjectStatus")
            Prelude.<*> (x Core..:? "ProjectName")
            Prelude.<*> (x Core..:? "ServiceCatalogProvisioningDetails")
            Prelude.<*> (x Core..:? "ProjectId")
            Prelude.<*> (x Core..:? "ProjectArn")
            Prelude.<*> (x Core..:? "ProjectDescription")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Project

instance Prelude.NFData Project
