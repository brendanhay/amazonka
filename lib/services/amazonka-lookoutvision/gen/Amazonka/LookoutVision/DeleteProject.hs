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
-- Module      : Amazonka.LookoutVision.DeleteProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Lookout for Vision project.
--
-- To delete a project, you must first delete each version of the model
-- associated with the project. To delete a model use the DeleteModel
-- operation.
--
-- You also have to delete the dataset(s) associated with the model. For
-- more information, see DeleteDataset. The images referenced by the
-- training and test datasets aren\'t deleted.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DeleteProject@ operation.
module Amazonka.LookoutVision.DeleteProject
  ( -- * Creating a Request
    DeleteProject (..),
    newDeleteProject,

    -- * Request Lenses
    deleteProject_clientToken,
    deleteProject_projectName,

    -- * Destructuring the Response
    DeleteProjectResponse (..),
    newDeleteProjectResponse,

    -- * Response Lenses
    deleteProjectResponse_projectArn,
    deleteProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | ClientToken is an idempotency token that ensures a call to
    -- @DeleteProject@ completes only once. You choose the value to pass. For
    -- example, An issue might prevent you from getting a response from
    -- @DeleteProject@. In this case, safely retry your call to @DeleteProject@
    -- by using the same @ClientToken@ parameter value.
    --
    -- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
    -- using inserts a value for you. This prevents retries after a network
    -- error from making multiple project deletion requests. You\'ll need to
    -- provide your own value for other use cases.
    --
    -- An error occurs if the other input parameters are not the same as in the
    -- first request. Using a different value for @ClientToken@ is considered a
    -- new call to @DeleteProject@. An idempotency token is active for 8 hours.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the project to delete.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteProject_clientToken' - ClientToken is an idempotency token that ensures a call to
-- @DeleteProject@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @DeleteProject@. In this case, safely retry your call to @DeleteProject@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple project deletion requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @DeleteProject@. An idempotency token is active for 8 hours.
--
-- 'projectName', 'deleteProject_projectName' - The name of the project to delete.
newDeleteProject ::
  -- | 'projectName'
  Prelude.Text ->
  DeleteProject
newDeleteProject pProjectName_ =
  DeleteProject'
    { clientToken = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | ClientToken is an idempotency token that ensures a call to
-- @DeleteProject@ completes only once. You choose the value to pass. For
-- example, An issue might prevent you from getting a response from
-- @DeleteProject@. In this case, safely retry your call to @DeleteProject@
-- by using the same @ClientToken@ parameter value.
--
-- If you don\'t supply a value for @ClientToken@, the AWS SDK you are
-- using inserts a value for you. This prevents retries after a network
-- error from making multiple project deletion requests. You\'ll need to
-- provide your own value for other use cases.
--
-- An error occurs if the other input parameters are not the same as in the
-- first request. Using a different value for @ClientToken@ is considered a
-- new call to @DeleteProject@. An idempotency token is active for 8 hours.
deleteProject_clientToken :: Lens.Lens' DeleteProject (Prelude.Maybe Prelude.Text)
deleteProject_clientToken = Lens.lens (\DeleteProject' {clientToken} -> clientToken) (\s@DeleteProject' {} a -> s {clientToken = a} :: DeleteProject)

-- | The name of the project to delete.
deleteProject_projectName :: Lens.Lens' DeleteProject Prelude.Text
deleteProject_projectName = Lens.lens (\DeleteProject' {projectName} -> projectName) (\s@DeleteProject' {} a -> s {projectName = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Prelude.<$> (x Data..?> "ProjectArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProject where
  hashWithSalt _salt DeleteProject' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData DeleteProject where
  rnf DeleteProject' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders DeleteProject where
  toHeaders DeleteProject' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteProject where
  toPath DeleteProject' {..} =
    Prelude.mconcat
      ["/2020-11-20/projects/", Data.toBS projectName]

instance Data.ToQuery DeleteProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the project that was deleted.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectArn', 'deleteProjectResponse_projectArn' - The Amazon Resource Name (ARN) of the project that was deleted.
--
-- 'httpStatus', 'deleteProjectResponse_httpStatus' - The response's http status code.
newDeleteProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse'
    { projectArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the project that was deleted.
deleteProjectResponse_projectArn :: Lens.Lens' DeleteProjectResponse (Prelude.Maybe Prelude.Text)
deleteProjectResponse_projectArn = Lens.lens (\DeleteProjectResponse' {projectArn} -> projectArn) (\s@DeleteProjectResponse' {} a -> s {projectArn = a} :: DeleteProjectResponse)

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Prelude.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Prelude.NFData DeleteProjectResponse where
  rnf DeleteProjectResponse' {..} =
    Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf httpStatus
