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
-- Module      : Amazonka.DeviceFarm.CreateProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project.
module Amazonka.DeviceFarm.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_vpcConfig,
    createProject_defaultJobTimeoutMinutes,
    createProject_name,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_project,
    createProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the create project operation.
--
-- /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | The VPC security groups and subnets that are attached to a project.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | Sets the execution timeout value (in minutes) for a project. All test
    -- runs in this project use the specified execution timeout value unless
    -- overridden when scheduling a run.
    defaultJobTimeoutMinutes :: Prelude.Maybe Prelude.Int,
    -- | The project\'s name.
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
-- 'vpcConfig', 'createProject_vpcConfig' - The VPC security groups and subnets that are attached to a project.
--
-- 'defaultJobTimeoutMinutes', 'createProject_defaultJobTimeoutMinutes' - Sets the execution timeout value (in minutes) for a project. All test
-- runs in this project use the specified execution timeout value unless
-- overridden when scheduling a run.
--
-- 'name', 'createProject_name' - The project\'s name.
newCreateProject ::
  -- | 'name'
  Prelude.Text ->
  CreateProject
newCreateProject pName_ =
  CreateProject'
    { vpcConfig = Prelude.Nothing,
      defaultJobTimeoutMinutes = Prelude.Nothing,
      name = pName_
    }

-- | The VPC security groups and subnets that are attached to a project.
createProject_vpcConfig :: Lens.Lens' CreateProject (Prelude.Maybe VpcConfig)
createProject_vpcConfig = Lens.lens (\CreateProject' {vpcConfig} -> vpcConfig) (\s@CreateProject' {} a -> s {vpcConfig = a} :: CreateProject)

-- | Sets the execution timeout value (in minutes) for a project. All test
-- runs in this project use the specified execution timeout value unless
-- overridden when scheduling a run.
createProject_defaultJobTimeoutMinutes :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Int)
createProject_defaultJobTimeoutMinutes = Lens.lens (\CreateProject' {defaultJobTimeoutMinutes} -> defaultJobTimeoutMinutes) (\s@CreateProject' {} a -> s {defaultJobTimeoutMinutes = a} :: CreateProject)

-- | The project\'s name.
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
            Prelude.<$> (x Data..?> "project")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProject where
  hashWithSalt _salt CreateProject' {..} =
    _salt `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` defaultJobTimeoutMinutes
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateProject where
  rnf CreateProject' {..} =
    Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf defaultJobTimeoutMinutes
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.CreateProject" ::
                          Prelude.ByteString
                      ),
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
          [ ("vpcConfig" Data..=) Prelude.<$> vpcConfig,
            ("defaultJobTimeoutMinutes" Data..=)
              Prelude.<$> defaultJobTimeoutMinutes,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateProject where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a create project request.
--
-- /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | The newly created project.
    project :: Prelude.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'project', 'createProjectResponse_project' - The newly created project.
--
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { project = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The newly created project.
createProjectResponse_project :: Lens.Lens' CreateProjectResponse (Prelude.Maybe Project)
createProjectResponse_project = Lens.lens (\CreateProjectResponse' {project} -> project) (\s@CreateProjectResponse' {} a -> s {project = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse where
  rnf CreateProjectResponse' {..} =
    Prelude.rnf project
      `Prelude.seq` Prelude.rnf httpStatus
