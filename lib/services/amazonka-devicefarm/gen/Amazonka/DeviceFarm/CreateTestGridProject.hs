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
-- Module      : Amazonka.DeviceFarm.CreateTestGridProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Selenium testing project. Projects are used to track
-- TestGridSession instances.
module Amazonka.DeviceFarm.CreateTestGridProject
  ( -- * Creating a Request
    CreateTestGridProject (..),
    newCreateTestGridProject,

    -- * Request Lenses
    createTestGridProject_vpcConfig,
    createTestGridProject_description,
    createTestGridProject_name,

    -- * Destructuring the Response
    CreateTestGridProjectResponse (..),
    newCreateTestGridProjectResponse,

    -- * Response Lenses
    createTestGridProjectResponse_testGridProject,
    createTestGridProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTestGridProject' smart constructor.
data CreateTestGridProject = CreateTestGridProject'
  { -- | The VPC security groups and subnets that are attached to a project.
    vpcConfig :: Prelude.Maybe TestGridVpcConfig,
    -- | Human-readable description of the project.
    description :: Prelude.Maybe Prelude.Text,
    -- | Human-readable name of the Selenium testing project.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'createTestGridProject_vpcConfig' - The VPC security groups and subnets that are attached to a project.
--
-- 'description', 'createTestGridProject_description' - Human-readable description of the project.
--
-- 'name', 'createTestGridProject_name' - Human-readable name of the Selenium testing project.
newCreateTestGridProject ::
  -- | 'name'
  Prelude.Text ->
  CreateTestGridProject
newCreateTestGridProject pName_ =
  CreateTestGridProject'
    { vpcConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | The VPC security groups and subnets that are attached to a project.
createTestGridProject_vpcConfig :: Lens.Lens' CreateTestGridProject (Prelude.Maybe TestGridVpcConfig)
createTestGridProject_vpcConfig = Lens.lens (\CreateTestGridProject' {vpcConfig} -> vpcConfig) (\s@CreateTestGridProject' {} a -> s {vpcConfig = a} :: CreateTestGridProject)

-- | Human-readable description of the project.
createTestGridProject_description :: Lens.Lens' CreateTestGridProject (Prelude.Maybe Prelude.Text)
createTestGridProject_description = Lens.lens (\CreateTestGridProject' {description} -> description) (\s@CreateTestGridProject' {} a -> s {description = a} :: CreateTestGridProject)

-- | Human-readable name of the Selenium testing project.
createTestGridProject_name :: Lens.Lens' CreateTestGridProject Prelude.Text
createTestGridProject_name = Lens.lens (\CreateTestGridProject' {name} -> name) (\s@CreateTestGridProject' {} a -> s {name = a} :: CreateTestGridProject)

instance Core.AWSRequest CreateTestGridProject where
  type
    AWSResponse CreateTestGridProject =
      CreateTestGridProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTestGridProjectResponse'
            Prelude.<$> (x Data..?> "testGridProject")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTestGridProject where
  hashWithSalt _salt CreateTestGridProject' {..} =
    _salt `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateTestGridProject where
  rnf CreateTestGridProject' {..} =
    Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateTestGridProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.CreateTestGridProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTestGridProject where
  toJSON CreateTestGridProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("vpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateTestGridProject where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTestGridProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTestGridProjectResponse' smart constructor.
data CreateTestGridProjectResponse = CreateTestGridProjectResponse'
  { -- | ARN of the Selenium testing project that was created.
    testGridProject :: Prelude.Maybe TestGridProject,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTestGridProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testGridProject', 'createTestGridProjectResponse_testGridProject' - ARN of the Selenium testing project that was created.
--
-- 'httpStatus', 'createTestGridProjectResponse_httpStatus' - The response's http status code.
newCreateTestGridProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTestGridProjectResponse
newCreateTestGridProjectResponse pHttpStatus_ =
  CreateTestGridProjectResponse'
    { testGridProject =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ARN of the Selenium testing project that was created.
createTestGridProjectResponse_testGridProject :: Lens.Lens' CreateTestGridProjectResponse (Prelude.Maybe TestGridProject)
createTestGridProjectResponse_testGridProject = Lens.lens (\CreateTestGridProjectResponse' {testGridProject} -> testGridProject) (\s@CreateTestGridProjectResponse' {} a -> s {testGridProject = a} :: CreateTestGridProjectResponse)

-- | The response's http status code.
createTestGridProjectResponse_httpStatus :: Lens.Lens' CreateTestGridProjectResponse Prelude.Int
createTestGridProjectResponse_httpStatus = Lens.lens (\CreateTestGridProjectResponse' {httpStatus} -> httpStatus) (\s@CreateTestGridProjectResponse' {} a -> s {httpStatus = a} :: CreateTestGridProjectResponse)

instance Prelude.NFData CreateTestGridProjectResponse where
  rnf CreateTestGridProjectResponse' {..} =
    Prelude.rnf testGridProject
      `Prelude.seq` Prelude.rnf httpStatus
