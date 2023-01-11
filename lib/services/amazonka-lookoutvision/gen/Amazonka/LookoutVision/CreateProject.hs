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
-- Module      : Amazonka.LookoutVision.CreateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an empty Amazon Lookout for Vision project. After you create the
-- project, add a dataset by calling CreateDataset.
--
-- This operation requires permissions to perform the
-- @lookoutvision:CreateProject@ operation.
module Amazonka.LookoutVision.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_clientToken,
    createProject_projectName,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_projectMetadata,
    createProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | ClientToken is an idempotency token that ensures a call to
    -- @CreateProject@ completes only once. You choose the value to pass. For
    -- example, An issue might prevent you from getting a response from
    -- @CreateProject@. In this case, safely retry your call to @CreateProject@
    -- by using the same @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple project creation requests. You\'ll need to
    -- provide your own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @CreateProject@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name for the project.
    projectName :: Prelude.Text
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
-- 'clientToken', 'createProject_clientToken' - ClientToken is an idempotency token that ensures a call to
-- @CreateProject@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @CreateProject@. In this case, safely retry your call to @CreateProject@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple project creation requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @CreateProject@. An idempotency token is active for 8 hours.
--
-- 'projectName', 'createProject_projectName' - The name for the project.
newCreateProject ::
  -- | 'projectName'
  Prelude.Text ->
  CreateProject
newCreateProject pProjectName_ =
  CreateProject'
    { clientToken = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | ClientToken is an idempotency token that ensures a call to
-- @CreateProject@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @CreateProject@. In this case, safely retry your call to @CreateProject@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple project creation requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @CreateProject@. An idempotency token is active for 8 hours.
createProject_clientToken :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_clientToken = Lens.lens (\CreateProject' {clientToken} -> clientToken) (\s@CreateProject' {} a -> s {clientToken = a} :: CreateProject)

-- | The name for the project.
createProject_projectName :: Lens.Lens' CreateProject Prelude.Text
createProject_projectName = Lens.lens (\CreateProject' {projectName} -> projectName) (\s@CreateProject' {} a -> s {projectName = a} :: CreateProject)

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
            Prelude.<$> (x Data..?> "ProjectMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProject where
  hashWithSalt _salt CreateProject' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData CreateProject where
  rnf CreateProject' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders CreateProject where
  toHeaders CreateProject' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProjectName" Data..= projectName)]
      )

instance Data.ToPath CreateProject where
  toPath = Prelude.const "/2020-11-20/projects"

instance Data.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Information about the project.
    projectMetadata :: Prelude.Maybe ProjectMetadata,
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
-- 'projectMetadata', 'createProjectResponse_projectMetadata' - Information about the project.
--
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { projectMetadata =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the project.
createProjectResponse_projectMetadata :: Lens.Lens' CreateProjectResponse (Prelude.Maybe ProjectMetadata)
createProjectResponse_projectMetadata = Lens.lens (\CreateProjectResponse' {projectMetadata} -> projectMetadata) (\s@CreateProjectResponse' {} a -> s {projectMetadata = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse where
  rnf CreateProjectResponse' {..} =
    Prelude.rnf projectMetadata
      `Prelude.seq` Prelude.rnf httpStatus
