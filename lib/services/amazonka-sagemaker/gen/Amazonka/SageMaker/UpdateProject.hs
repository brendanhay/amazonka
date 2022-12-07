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
-- Module      : Amazonka.SageMaker.UpdateProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a machine learning (ML) project that is created from a template
-- that sets up an ML pipeline from training to deploying an approved
-- model.
--
-- You must not update a project that is in use. If you update the
-- @ServiceCatalogProvisioningUpdateDetails@ of a project that is active or
-- being created, or updated, you may lose resources already created by the
-- project.
module Amazonka.SageMaker.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_tags,
    updateProject_projectDescription,
    updateProject_serviceCatalogProvisioningUpdateDetails,
    updateProject_projectName,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_httpStatus,
    updateProjectResponse_projectArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    -- In addition, the project must have tag update constraints set in order
    -- to include this parameter in the request. For more information, see
    -- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/constraints-resourceupdate.html Amazon Web Services Service Catalog Tag Update Constraints>.
    tags :: Prelude.Maybe [Tag],
    -- | The description for the project.
    projectDescription :: Prelude.Maybe Prelude.Text,
    -- | The product ID and provisioning artifact ID to provision a service
    -- catalog. The provisioning artifact ID will default to the latest
    -- provisioning artifact ID of the product, if you don\'t provide the
    -- provisioning artifact ID. For more information, see
    -- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
    serviceCatalogProvisioningUpdateDetails :: Prelude.Maybe ServiceCatalogProvisioningUpdateDetails,
    -- | The name of the project.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateProject_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
-- In addition, the project must have tag update constraints set in order
-- to include this parameter in the request. For more information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/constraints-resourceupdate.html Amazon Web Services Service Catalog Tag Update Constraints>.
--
-- 'projectDescription', 'updateProject_projectDescription' - The description for the project.
--
-- 'serviceCatalogProvisioningUpdateDetails', 'updateProject_serviceCatalogProvisioningUpdateDetails' - The product ID and provisioning artifact ID to provision a service
-- catalog. The provisioning artifact ID will default to the latest
-- provisioning artifact ID of the product, if you don\'t provide the
-- provisioning artifact ID. For more information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
--
-- 'projectName', 'updateProject_projectName' - The name of the project.
newUpdateProject ::
  -- | 'projectName'
  Prelude.Text ->
  UpdateProject
newUpdateProject pProjectName_ =
  UpdateProject'
    { tags = Prelude.Nothing,
      projectDescription = Prelude.Nothing,
      serviceCatalogProvisioningUpdateDetails =
        Prelude.Nothing,
      projectName = pProjectName_
    }

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
-- In addition, the project must have tag update constraints set in order
-- to include this parameter in the request. For more information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/constraints-resourceupdate.html Amazon Web Services Service Catalog Tag Update Constraints>.
updateProject_tags :: Lens.Lens' UpdateProject (Prelude.Maybe [Tag])
updateProject_tags = Lens.lens (\UpdateProject' {tags} -> tags) (\s@UpdateProject' {} a -> s {tags = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | The description for the project.
updateProject_projectDescription :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_projectDescription = Lens.lens (\UpdateProject' {projectDescription} -> projectDescription) (\s@UpdateProject' {} a -> s {projectDescription = a} :: UpdateProject)

-- | The product ID and provisioning artifact ID to provision a service
-- catalog. The provisioning artifact ID will default to the latest
-- provisioning artifact ID of the product, if you don\'t provide the
-- provisioning artifact ID. For more information, see
-- <https://docs.aws.amazon.com/servicecatalog/latest/adminguide/introduction.html What is Amazon Web Services Service Catalog>.
updateProject_serviceCatalogProvisioningUpdateDetails :: Lens.Lens' UpdateProject (Prelude.Maybe ServiceCatalogProvisioningUpdateDetails)
updateProject_serviceCatalogProvisioningUpdateDetails = Lens.lens (\UpdateProject' {serviceCatalogProvisioningUpdateDetails} -> serviceCatalogProvisioningUpdateDetails) (\s@UpdateProject' {} a -> s {serviceCatalogProvisioningUpdateDetails = a} :: UpdateProject)

-- | The name of the project.
updateProject_projectName :: Lens.Lens' UpdateProject Prelude.Text
updateProject_projectName = Lens.lens (\UpdateProject' {projectName} -> projectName) (\s@UpdateProject' {} a -> s {projectName = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProjectArn")
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt _salt UpdateProject' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` projectDescription
      `Prelude.hashWithSalt` serviceCatalogProvisioningUpdateDetails
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf projectDescription
      `Prelude.seq` Prelude.rnf serviceCatalogProvisioningUpdateDetails
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.UpdateProject" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ProjectDescription" Data..=)
              Prelude.<$> projectDescription,
            ("ServiceCatalogProvisioningUpdateDetails" Data..=)
              Prelude.<$> serviceCatalogProvisioningUpdateDetails,
            Prelude.Just ("ProjectName" Data..= projectName)
          ]
      )

instance Data.ToPath UpdateProject where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the project.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
--
-- 'projectArn', 'updateProjectResponse_projectArn' - The Amazon Resource Name (ARN) of the project.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'projectArn'
  Prelude.Text ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ pProjectArn_ =
  UpdateProjectResponse'
    { httpStatus = pHttpStatus_,
      projectArn = pProjectArn_
    }

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

-- | The Amazon Resource Name (ARN) of the project.
updateProjectResponse_projectArn :: Lens.Lens' UpdateProjectResponse Prelude.Text
updateProjectResponse_projectArn = Lens.lens (\UpdateProjectResponse' {projectArn} -> projectArn) (\s@UpdateProjectResponse' {} a -> s {projectArn = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf projectArn
