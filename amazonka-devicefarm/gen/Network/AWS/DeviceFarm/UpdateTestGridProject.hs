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
-- Module      : Network.AWS.DeviceFarm.UpdateTestGridProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Change details of a project.
module Network.AWS.DeviceFarm.UpdateTestGridProject
  ( -- * Creating a Request
    UpdateTestGridProject (..),
    newUpdateTestGridProject,

    -- * Request Lenses
    updateTestGridProject_vpcConfig,
    updateTestGridProject_name,
    updateTestGridProject_description,
    updateTestGridProject_projectArn,

    -- * Destructuring the Response
    UpdateTestGridProjectResponse (..),
    newUpdateTestGridProjectResponse,

    -- * Response Lenses
    updateTestGridProjectResponse_testGridProject,
    updateTestGridProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTestGridProject' smart constructor.
data UpdateTestGridProject = UpdateTestGridProject'
  { -- | The VPC security groups and subnets that are attached to a project.
    vpcConfig :: Prelude.Maybe TestGridVpcConfig,
    -- | Human-readable name for the project.
    name :: Prelude.Maybe Prelude.Text,
    -- | Human-readable description for the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | ARN of the project to update.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'updateTestGridProject_vpcConfig' - The VPC security groups and subnets that are attached to a project.
--
-- 'name', 'updateTestGridProject_name' - Human-readable name for the project.
--
-- 'description', 'updateTestGridProject_description' - Human-readable description for the project.
--
-- 'projectArn', 'updateTestGridProject_projectArn' - ARN of the project to update.
newUpdateTestGridProject ::
  -- | 'projectArn'
  Prelude.Text ->
  UpdateTestGridProject
newUpdateTestGridProject pProjectArn_ =
  UpdateTestGridProject'
    { vpcConfig = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      projectArn = pProjectArn_
    }

-- | The VPC security groups and subnets that are attached to a project.
updateTestGridProject_vpcConfig :: Lens.Lens' UpdateTestGridProject (Prelude.Maybe TestGridVpcConfig)
updateTestGridProject_vpcConfig = Lens.lens (\UpdateTestGridProject' {vpcConfig} -> vpcConfig) (\s@UpdateTestGridProject' {} a -> s {vpcConfig = a} :: UpdateTestGridProject)

-- | Human-readable name for the project.
updateTestGridProject_name :: Lens.Lens' UpdateTestGridProject (Prelude.Maybe Prelude.Text)
updateTestGridProject_name = Lens.lens (\UpdateTestGridProject' {name} -> name) (\s@UpdateTestGridProject' {} a -> s {name = a} :: UpdateTestGridProject)

-- | Human-readable description for the project.
updateTestGridProject_description :: Lens.Lens' UpdateTestGridProject (Prelude.Maybe Prelude.Text)
updateTestGridProject_description = Lens.lens (\UpdateTestGridProject' {description} -> description) (\s@UpdateTestGridProject' {} a -> s {description = a} :: UpdateTestGridProject)

-- | ARN of the project to update.
updateTestGridProject_projectArn :: Lens.Lens' UpdateTestGridProject Prelude.Text
updateTestGridProject_projectArn = Lens.lens (\UpdateTestGridProject' {projectArn} -> projectArn) (\s@UpdateTestGridProject' {} a -> s {projectArn = a} :: UpdateTestGridProject)

instance Core.AWSRequest UpdateTestGridProject where
  type
    AWSResponse UpdateTestGridProject =
      UpdateTestGridProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTestGridProjectResponse'
            Prelude.<$> (x Core..?> "testGridProject")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTestGridProject

instance Prelude.NFData UpdateTestGridProject

instance Core.ToHeaders UpdateTestGridProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.UpdateTestGridProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTestGridProject where
  toJSON UpdateTestGridProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("vpcConfig" Core..=) Prelude.<$> vpcConfig,
            ("name" Core..=) Prelude.<$> name,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("projectArn" Core..= projectArn)
          ]
      )

instance Core.ToPath UpdateTestGridProject where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateTestGridProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTestGridProjectResponse' smart constructor.
data UpdateTestGridProjectResponse = UpdateTestGridProjectResponse'
  { -- | The project, including updated information.
    testGridProject :: Prelude.Maybe TestGridProject,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTestGridProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testGridProject', 'updateTestGridProjectResponse_testGridProject' - The project, including updated information.
--
-- 'httpStatus', 'updateTestGridProjectResponse_httpStatus' - The response's http status code.
newUpdateTestGridProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTestGridProjectResponse
newUpdateTestGridProjectResponse pHttpStatus_ =
  UpdateTestGridProjectResponse'
    { testGridProject =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The project, including updated information.
updateTestGridProjectResponse_testGridProject :: Lens.Lens' UpdateTestGridProjectResponse (Prelude.Maybe TestGridProject)
updateTestGridProjectResponse_testGridProject = Lens.lens (\UpdateTestGridProjectResponse' {testGridProject} -> testGridProject) (\s@UpdateTestGridProjectResponse' {} a -> s {testGridProject = a} :: UpdateTestGridProjectResponse)

-- | The response's http status code.
updateTestGridProjectResponse_httpStatus :: Lens.Lens' UpdateTestGridProjectResponse Prelude.Int
updateTestGridProjectResponse_httpStatus = Lens.lens (\UpdateTestGridProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateTestGridProjectResponse' {} a -> s {httpStatus = a} :: UpdateTestGridProjectResponse)

instance Prelude.NFData UpdateTestGridProjectResponse
