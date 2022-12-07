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
-- Module      : Amazonka.Glue.UpdateJobFromSourceControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes a job from the source control repository. This operation
-- takes the job artifacts that are located in the remote repository and
-- updates the Glue internal stores with these artifacts.
--
-- This API supports optional parameters which take in the repository
-- information.
module Amazonka.Glue.UpdateJobFromSourceControl
  ( -- * Creating a Request
    UpdateJobFromSourceControl (..),
    newUpdateJobFromSourceControl,

    -- * Request Lenses
    updateJobFromSourceControl_repositoryOwner,
    updateJobFromSourceControl_commitId,
    updateJobFromSourceControl_branchName,
    updateJobFromSourceControl_folder,
    updateJobFromSourceControl_jobName,
    updateJobFromSourceControl_repositoryName,
    updateJobFromSourceControl_authToken,
    updateJobFromSourceControl_provider,
    updateJobFromSourceControl_authStrategy,

    -- * Destructuring the Response
    UpdateJobFromSourceControlResponse (..),
    newUpdateJobFromSourceControlResponse,

    -- * Response Lenses
    updateJobFromSourceControlResponse_jobName,
    updateJobFromSourceControlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateJobFromSourceControl' smart constructor.
data UpdateJobFromSourceControl = UpdateJobFromSourceControl'
  { -- | The owner of the remote repository that contains the job artifacts.
    repositoryOwner :: Prelude.Maybe Prelude.Text,
    -- | A commit ID for a commit in the remote repository.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | An optional branch in the remote repository.
    branchName :: Prelude.Maybe Prelude.Text,
    -- | An optional folder in the remote repository.
    folder :: Prelude.Maybe Prelude.Text,
    -- | The name of the Glue job to be synchronized to or from the remote
    -- repository.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The name of the remote repository that contains the job artifacts.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The value of the authorization token.
    authToken :: Prelude.Maybe Prelude.Text,
    -- | The provider for the remote repository.
    provider :: Prelude.Maybe SourceControlProvider,
    -- | The type of authentication, which can be an authentication token stored
    -- in Amazon Web Services Secrets Manager, or a personal access token.
    authStrategy :: Prelude.Maybe SourceControlAuthStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobFromSourceControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryOwner', 'updateJobFromSourceControl_repositoryOwner' - The owner of the remote repository that contains the job artifacts.
--
-- 'commitId', 'updateJobFromSourceControl_commitId' - A commit ID for a commit in the remote repository.
--
-- 'branchName', 'updateJobFromSourceControl_branchName' - An optional branch in the remote repository.
--
-- 'folder', 'updateJobFromSourceControl_folder' - An optional folder in the remote repository.
--
-- 'jobName', 'updateJobFromSourceControl_jobName' - The name of the Glue job to be synchronized to or from the remote
-- repository.
--
-- 'repositoryName', 'updateJobFromSourceControl_repositoryName' - The name of the remote repository that contains the job artifacts.
--
-- 'authToken', 'updateJobFromSourceControl_authToken' - The value of the authorization token.
--
-- 'provider', 'updateJobFromSourceControl_provider' - The provider for the remote repository.
--
-- 'authStrategy', 'updateJobFromSourceControl_authStrategy' - The type of authentication, which can be an authentication token stored
-- in Amazon Web Services Secrets Manager, or a personal access token.
newUpdateJobFromSourceControl ::
  UpdateJobFromSourceControl
newUpdateJobFromSourceControl =
  UpdateJobFromSourceControl'
    { repositoryOwner =
        Prelude.Nothing,
      commitId = Prelude.Nothing,
      branchName = Prelude.Nothing,
      folder = Prelude.Nothing,
      jobName = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      authToken = Prelude.Nothing,
      provider = Prelude.Nothing,
      authStrategy = Prelude.Nothing
    }

-- | The owner of the remote repository that contains the job artifacts.
updateJobFromSourceControl_repositoryOwner :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe Prelude.Text)
updateJobFromSourceControl_repositoryOwner = Lens.lens (\UpdateJobFromSourceControl' {repositoryOwner} -> repositoryOwner) (\s@UpdateJobFromSourceControl' {} a -> s {repositoryOwner = a} :: UpdateJobFromSourceControl)

-- | A commit ID for a commit in the remote repository.
updateJobFromSourceControl_commitId :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe Prelude.Text)
updateJobFromSourceControl_commitId = Lens.lens (\UpdateJobFromSourceControl' {commitId} -> commitId) (\s@UpdateJobFromSourceControl' {} a -> s {commitId = a} :: UpdateJobFromSourceControl)

-- | An optional branch in the remote repository.
updateJobFromSourceControl_branchName :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe Prelude.Text)
updateJobFromSourceControl_branchName = Lens.lens (\UpdateJobFromSourceControl' {branchName} -> branchName) (\s@UpdateJobFromSourceControl' {} a -> s {branchName = a} :: UpdateJobFromSourceControl)

-- | An optional folder in the remote repository.
updateJobFromSourceControl_folder :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe Prelude.Text)
updateJobFromSourceControl_folder = Lens.lens (\UpdateJobFromSourceControl' {folder} -> folder) (\s@UpdateJobFromSourceControl' {} a -> s {folder = a} :: UpdateJobFromSourceControl)

-- | The name of the Glue job to be synchronized to or from the remote
-- repository.
updateJobFromSourceControl_jobName :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe Prelude.Text)
updateJobFromSourceControl_jobName = Lens.lens (\UpdateJobFromSourceControl' {jobName} -> jobName) (\s@UpdateJobFromSourceControl' {} a -> s {jobName = a} :: UpdateJobFromSourceControl)

-- | The name of the remote repository that contains the job artifacts.
updateJobFromSourceControl_repositoryName :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe Prelude.Text)
updateJobFromSourceControl_repositoryName = Lens.lens (\UpdateJobFromSourceControl' {repositoryName} -> repositoryName) (\s@UpdateJobFromSourceControl' {} a -> s {repositoryName = a} :: UpdateJobFromSourceControl)

-- | The value of the authorization token.
updateJobFromSourceControl_authToken :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe Prelude.Text)
updateJobFromSourceControl_authToken = Lens.lens (\UpdateJobFromSourceControl' {authToken} -> authToken) (\s@UpdateJobFromSourceControl' {} a -> s {authToken = a} :: UpdateJobFromSourceControl)

-- | The provider for the remote repository.
updateJobFromSourceControl_provider :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe SourceControlProvider)
updateJobFromSourceControl_provider = Lens.lens (\UpdateJobFromSourceControl' {provider} -> provider) (\s@UpdateJobFromSourceControl' {} a -> s {provider = a} :: UpdateJobFromSourceControl)

-- | The type of authentication, which can be an authentication token stored
-- in Amazon Web Services Secrets Manager, or a personal access token.
updateJobFromSourceControl_authStrategy :: Lens.Lens' UpdateJobFromSourceControl (Prelude.Maybe SourceControlAuthStrategy)
updateJobFromSourceControl_authStrategy = Lens.lens (\UpdateJobFromSourceControl' {authStrategy} -> authStrategy) (\s@UpdateJobFromSourceControl' {} a -> s {authStrategy = a} :: UpdateJobFromSourceControl)

instance Core.AWSRequest UpdateJobFromSourceControl where
  type
    AWSResponse UpdateJobFromSourceControl =
      UpdateJobFromSourceControlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateJobFromSourceControlResponse'
            Prelude.<$> (x Data..?> "JobName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateJobFromSourceControl where
  hashWithSalt _salt UpdateJobFromSourceControl' {..} =
    _salt `Prelude.hashWithSalt` repositoryOwner
      `Prelude.hashWithSalt` commitId
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` folder
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` authToken
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` authStrategy

instance Prelude.NFData UpdateJobFromSourceControl where
  rnf UpdateJobFromSourceControl' {..} =
    Prelude.rnf repositoryOwner
      `Prelude.seq` Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf folder
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf authToken
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf authStrategy

instance Data.ToHeaders UpdateJobFromSourceControl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.UpdateJobFromSourceControl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateJobFromSourceControl where
  toJSON UpdateJobFromSourceControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RepositoryOwner" Data..=)
              Prelude.<$> repositoryOwner,
            ("CommitId" Data..=) Prelude.<$> commitId,
            ("BranchName" Data..=) Prelude.<$> branchName,
            ("Folder" Data..=) Prelude.<$> folder,
            ("JobName" Data..=) Prelude.<$> jobName,
            ("RepositoryName" Data..=)
              Prelude.<$> repositoryName,
            ("AuthToken" Data..=) Prelude.<$> authToken,
            ("Provider" Data..=) Prelude.<$> provider,
            ("AuthStrategy" Data..=) Prelude.<$> authStrategy
          ]
      )

instance Data.ToPath UpdateJobFromSourceControl where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateJobFromSourceControl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateJobFromSourceControlResponse' smart constructor.
data UpdateJobFromSourceControlResponse = UpdateJobFromSourceControlResponse'
  { -- | The name of the Glue job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateJobFromSourceControlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'updateJobFromSourceControlResponse_jobName' - The name of the Glue job.
--
-- 'httpStatus', 'updateJobFromSourceControlResponse_httpStatus' - The response's http status code.
newUpdateJobFromSourceControlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateJobFromSourceControlResponse
newUpdateJobFromSourceControlResponse pHttpStatus_ =
  UpdateJobFromSourceControlResponse'
    { jobName =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the Glue job.
updateJobFromSourceControlResponse_jobName :: Lens.Lens' UpdateJobFromSourceControlResponse (Prelude.Maybe Prelude.Text)
updateJobFromSourceControlResponse_jobName = Lens.lens (\UpdateJobFromSourceControlResponse' {jobName} -> jobName) (\s@UpdateJobFromSourceControlResponse' {} a -> s {jobName = a} :: UpdateJobFromSourceControlResponse)

-- | The response's http status code.
updateJobFromSourceControlResponse_httpStatus :: Lens.Lens' UpdateJobFromSourceControlResponse Prelude.Int
updateJobFromSourceControlResponse_httpStatus = Lens.lens (\UpdateJobFromSourceControlResponse' {httpStatus} -> httpStatus) (\s@UpdateJobFromSourceControlResponse' {} a -> s {httpStatus = a} :: UpdateJobFromSourceControlResponse)

instance
  Prelude.NFData
    UpdateJobFromSourceControlResponse
  where
  rnf UpdateJobFromSourceControlResponse' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf httpStatus
