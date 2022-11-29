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
-- Module      : Amazonka.Glue.UpdateSourceControlFromJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes a job to the source control repository. This operation
-- takes the job artifacts from the Glue internal stores and makes a commit
-- to the remote repository that is configured on the job.
--
-- This API supports optional parameters which take in the repository
-- information.
module Amazonka.Glue.UpdateSourceControlFromJob
  ( -- * Creating a Request
    UpdateSourceControlFromJob (..),
    newUpdateSourceControlFromJob,

    -- * Request Lenses
    updateSourceControlFromJob_repositoryOwner,
    updateSourceControlFromJob_commitId,
    updateSourceControlFromJob_branchName,
    updateSourceControlFromJob_folder,
    updateSourceControlFromJob_jobName,
    updateSourceControlFromJob_repositoryName,
    updateSourceControlFromJob_authToken,
    updateSourceControlFromJob_provider,
    updateSourceControlFromJob_authStrategy,

    -- * Destructuring the Response
    UpdateSourceControlFromJobResponse (..),
    newUpdateSourceControlFromJobResponse,

    -- * Response Lenses
    updateSourceControlFromJobResponse_jobName,
    updateSourceControlFromJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSourceControlFromJob' smart constructor.
data UpdateSourceControlFromJob = UpdateSourceControlFromJob'
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
-- Create a value of 'UpdateSourceControlFromJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryOwner', 'updateSourceControlFromJob_repositoryOwner' - The owner of the remote repository that contains the job artifacts.
--
-- 'commitId', 'updateSourceControlFromJob_commitId' - A commit ID for a commit in the remote repository.
--
-- 'branchName', 'updateSourceControlFromJob_branchName' - An optional branch in the remote repository.
--
-- 'folder', 'updateSourceControlFromJob_folder' - An optional folder in the remote repository.
--
-- 'jobName', 'updateSourceControlFromJob_jobName' - The name of the Glue job to be synchronized to or from the remote
-- repository.
--
-- 'repositoryName', 'updateSourceControlFromJob_repositoryName' - The name of the remote repository that contains the job artifacts.
--
-- 'authToken', 'updateSourceControlFromJob_authToken' - The value of the authorization token.
--
-- 'provider', 'updateSourceControlFromJob_provider' - The provider for the remote repository.
--
-- 'authStrategy', 'updateSourceControlFromJob_authStrategy' - The type of authentication, which can be an authentication token stored
-- in Amazon Web Services Secrets Manager, or a personal access token.
newUpdateSourceControlFromJob ::
  UpdateSourceControlFromJob
newUpdateSourceControlFromJob =
  UpdateSourceControlFromJob'
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
updateSourceControlFromJob_repositoryOwner :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe Prelude.Text)
updateSourceControlFromJob_repositoryOwner = Lens.lens (\UpdateSourceControlFromJob' {repositoryOwner} -> repositoryOwner) (\s@UpdateSourceControlFromJob' {} a -> s {repositoryOwner = a} :: UpdateSourceControlFromJob)

-- | A commit ID for a commit in the remote repository.
updateSourceControlFromJob_commitId :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe Prelude.Text)
updateSourceControlFromJob_commitId = Lens.lens (\UpdateSourceControlFromJob' {commitId} -> commitId) (\s@UpdateSourceControlFromJob' {} a -> s {commitId = a} :: UpdateSourceControlFromJob)

-- | An optional branch in the remote repository.
updateSourceControlFromJob_branchName :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe Prelude.Text)
updateSourceControlFromJob_branchName = Lens.lens (\UpdateSourceControlFromJob' {branchName} -> branchName) (\s@UpdateSourceControlFromJob' {} a -> s {branchName = a} :: UpdateSourceControlFromJob)

-- | An optional folder in the remote repository.
updateSourceControlFromJob_folder :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe Prelude.Text)
updateSourceControlFromJob_folder = Lens.lens (\UpdateSourceControlFromJob' {folder} -> folder) (\s@UpdateSourceControlFromJob' {} a -> s {folder = a} :: UpdateSourceControlFromJob)

-- | The name of the Glue job to be synchronized to or from the remote
-- repository.
updateSourceControlFromJob_jobName :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe Prelude.Text)
updateSourceControlFromJob_jobName = Lens.lens (\UpdateSourceControlFromJob' {jobName} -> jobName) (\s@UpdateSourceControlFromJob' {} a -> s {jobName = a} :: UpdateSourceControlFromJob)

-- | The name of the remote repository that contains the job artifacts.
updateSourceControlFromJob_repositoryName :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe Prelude.Text)
updateSourceControlFromJob_repositoryName = Lens.lens (\UpdateSourceControlFromJob' {repositoryName} -> repositoryName) (\s@UpdateSourceControlFromJob' {} a -> s {repositoryName = a} :: UpdateSourceControlFromJob)

-- | The value of the authorization token.
updateSourceControlFromJob_authToken :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe Prelude.Text)
updateSourceControlFromJob_authToken = Lens.lens (\UpdateSourceControlFromJob' {authToken} -> authToken) (\s@UpdateSourceControlFromJob' {} a -> s {authToken = a} :: UpdateSourceControlFromJob)

-- | The provider for the remote repository.
updateSourceControlFromJob_provider :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe SourceControlProvider)
updateSourceControlFromJob_provider = Lens.lens (\UpdateSourceControlFromJob' {provider} -> provider) (\s@UpdateSourceControlFromJob' {} a -> s {provider = a} :: UpdateSourceControlFromJob)

-- | The type of authentication, which can be an authentication token stored
-- in Amazon Web Services Secrets Manager, or a personal access token.
updateSourceControlFromJob_authStrategy :: Lens.Lens' UpdateSourceControlFromJob (Prelude.Maybe SourceControlAuthStrategy)
updateSourceControlFromJob_authStrategy = Lens.lens (\UpdateSourceControlFromJob' {authStrategy} -> authStrategy) (\s@UpdateSourceControlFromJob' {} a -> s {authStrategy = a} :: UpdateSourceControlFromJob)

instance Core.AWSRequest UpdateSourceControlFromJob where
  type
    AWSResponse UpdateSourceControlFromJob =
      UpdateSourceControlFromJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSourceControlFromJobResponse'
            Prelude.<$> (x Core..?> "JobName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSourceControlFromJob where
  hashWithSalt _salt UpdateSourceControlFromJob' {..} =
    _salt `Prelude.hashWithSalt` repositoryOwner
      `Prelude.hashWithSalt` commitId
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` folder
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` authToken
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` authStrategy

instance Prelude.NFData UpdateSourceControlFromJob where
  rnf UpdateSourceControlFromJob' {..} =
    Prelude.rnf repositoryOwner
      `Prelude.seq` Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf folder
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf authToken
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf authStrategy

instance Core.ToHeaders UpdateSourceControlFromJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.UpdateSourceControlFromJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSourceControlFromJob where
  toJSON UpdateSourceControlFromJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RepositoryOwner" Core..=)
              Prelude.<$> repositoryOwner,
            ("CommitId" Core..=) Prelude.<$> commitId,
            ("BranchName" Core..=) Prelude.<$> branchName,
            ("Folder" Core..=) Prelude.<$> folder,
            ("JobName" Core..=) Prelude.<$> jobName,
            ("RepositoryName" Core..=)
              Prelude.<$> repositoryName,
            ("AuthToken" Core..=) Prelude.<$> authToken,
            ("Provider" Core..=) Prelude.<$> provider,
            ("AuthStrategy" Core..=) Prelude.<$> authStrategy
          ]
      )

instance Core.ToPath UpdateSourceControlFromJob where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSourceControlFromJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSourceControlFromJobResponse' smart constructor.
data UpdateSourceControlFromJobResponse = UpdateSourceControlFromJobResponse'
  { -- | The name of the Glue job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSourceControlFromJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'updateSourceControlFromJobResponse_jobName' - The name of the Glue job.
--
-- 'httpStatus', 'updateSourceControlFromJobResponse_httpStatus' - The response's http status code.
newUpdateSourceControlFromJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSourceControlFromJobResponse
newUpdateSourceControlFromJobResponse pHttpStatus_ =
  UpdateSourceControlFromJobResponse'
    { jobName =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the Glue job.
updateSourceControlFromJobResponse_jobName :: Lens.Lens' UpdateSourceControlFromJobResponse (Prelude.Maybe Prelude.Text)
updateSourceControlFromJobResponse_jobName = Lens.lens (\UpdateSourceControlFromJobResponse' {jobName} -> jobName) (\s@UpdateSourceControlFromJobResponse' {} a -> s {jobName = a} :: UpdateSourceControlFromJobResponse)

-- | The response's http status code.
updateSourceControlFromJobResponse_httpStatus :: Lens.Lens' UpdateSourceControlFromJobResponse Prelude.Int
updateSourceControlFromJobResponse_httpStatus = Lens.lens (\UpdateSourceControlFromJobResponse' {httpStatus} -> httpStatus) (\s@UpdateSourceControlFromJobResponse' {} a -> s {httpStatus = a} :: UpdateSourceControlFromJobResponse)

instance
  Prelude.NFData
    UpdateSourceControlFromJobResponse
  where
  rnf UpdateSourceControlFromJobResponse' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf httpStatus
