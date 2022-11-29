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
-- Module      : Amazonka.Amplify.UpdateBranch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a branch for an Amplify app.
module Amazonka.Amplify.UpdateBranch
  ( -- * Creating a Request
    UpdateBranch (..),
    newUpdateBranch,

    -- * Request Lenses
    updateBranch_enablePerformanceMode,
    updateBranch_ttl,
    updateBranch_basicAuthCredentials,
    updateBranch_displayName,
    updateBranch_description,
    updateBranch_environmentVariables,
    updateBranch_pullRequestEnvironmentName,
    updateBranch_backendEnvironmentArn,
    updateBranch_stage,
    updateBranch_enableAutoBuild,
    updateBranch_enableNotification,
    updateBranch_enableBasicAuth,
    updateBranch_framework,
    updateBranch_buildSpec,
    updateBranch_enablePullRequestPreview,
    updateBranch_appId,
    updateBranch_branchName,

    -- * Destructuring the Response
    UpdateBranchResponse (..),
    newUpdateBranchResponse,

    -- * Response Lenses
    updateBranchResponse_httpStatus,
    updateBranchResponse_branch,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the update branch request.
--
-- /See:/ 'newUpdateBranch' smart constructor.
data UpdateBranch = UpdateBranch'
  { -- | Enables performance mode for the branch.
    --
    -- Performance mode optimizes for faster hosting performance by keeping
    -- content cached at the edge for a longer interval. When performance mode
    -- is enabled, hosting configuration or code changes can take up to 10
    -- minutes to roll out.
    enablePerformanceMode :: Prelude.Maybe Prelude.Bool,
    -- | The content Time to Live (TTL) for the website in seconds.
    ttl :: Prelude.Maybe Prelude.Text,
    -- | The basic authorization credentials for the branch. You must
    -- base64-encode the authorization credentials and provide them in the
    -- format @user:password@.
    basicAuthCredentials :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The display name for a branch. This is used as the default domain
    -- prefix.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The description for the branch.
    description :: Prelude.Maybe Prelude.Text,
    -- | The environment variables for the branch.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amplify environment name for the pull request.
    pullRequestEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for a backend environment that is part of
    -- an Amplify app.
    backendEnvironmentArn :: Prelude.Maybe Prelude.Text,
    -- | Describes the current stage for the branch.
    stage :: Prelude.Maybe Stage,
    -- | Enables auto building for the branch.
    enableAutoBuild :: Prelude.Maybe Prelude.Bool,
    -- | Enables notifications for the branch.
    enableNotification :: Prelude.Maybe Prelude.Bool,
    -- | Enables basic authorization for the branch.
    enableBasicAuth :: Prelude.Maybe Prelude.Bool,
    -- | The framework for the branch.
    framework :: Prelude.Maybe Prelude.Text,
    -- | The build specification (build spec) for the branch.
    buildSpec :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Enables pull request previews for this branch.
    enablePullRequestPreview :: Prelude.Maybe Prelude.Bool,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the branch.
    branchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enablePerformanceMode', 'updateBranch_enablePerformanceMode' - Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
--
-- 'ttl', 'updateBranch_ttl' - The content Time to Live (TTL) for the website in seconds.
--
-- 'basicAuthCredentials', 'updateBranch_basicAuthCredentials' - The basic authorization credentials for the branch. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
--
-- 'displayName', 'updateBranch_displayName' - The display name for a branch. This is used as the default domain
-- prefix.
--
-- 'description', 'updateBranch_description' - The description for the branch.
--
-- 'environmentVariables', 'updateBranch_environmentVariables' - The environment variables for the branch.
--
-- 'pullRequestEnvironmentName', 'updateBranch_pullRequestEnvironmentName' - The Amplify environment name for the pull request.
--
-- 'backendEnvironmentArn', 'updateBranch_backendEnvironmentArn' - The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
--
-- 'stage', 'updateBranch_stage' - Describes the current stage for the branch.
--
-- 'enableAutoBuild', 'updateBranch_enableAutoBuild' - Enables auto building for the branch.
--
-- 'enableNotification', 'updateBranch_enableNotification' - Enables notifications for the branch.
--
-- 'enableBasicAuth', 'updateBranch_enableBasicAuth' - Enables basic authorization for the branch.
--
-- 'framework', 'updateBranch_framework' - The framework for the branch.
--
-- 'buildSpec', 'updateBranch_buildSpec' - The build specification (build spec) for the branch.
--
-- 'enablePullRequestPreview', 'updateBranch_enablePullRequestPreview' - Enables pull request previews for this branch.
--
-- 'appId', 'updateBranch_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'updateBranch_branchName' - The name for the branch.
newUpdateBranch ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  UpdateBranch
newUpdateBranch pAppId_ pBranchName_ =
  UpdateBranch'
    { enablePerformanceMode =
        Prelude.Nothing,
      ttl = Prelude.Nothing,
      basicAuthCredentials = Prelude.Nothing,
      displayName = Prelude.Nothing,
      description = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      pullRequestEnvironmentName = Prelude.Nothing,
      backendEnvironmentArn = Prelude.Nothing,
      stage = Prelude.Nothing,
      enableAutoBuild = Prelude.Nothing,
      enableNotification = Prelude.Nothing,
      enableBasicAuth = Prelude.Nothing,
      framework = Prelude.Nothing,
      buildSpec = Prelude.Nothing,
      enablePullRequestPreview = Prelude.Nothing,
      appId = pAppId_,
      branchName = pBranchName_
    }

-- | Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
updateBranch_enablePerformanceMode :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Bool)
updateBranch_enablePerformanceMode = Lens.lens (\UpdateBranch' {enablePerformanceMode} -> enablePerformanceMode) (\s@UpdateBranch' {} a -> s {enablePerformanceMode = a} :: UpdateBranch)

-- | The content Time to Live (TTL) for the website in seconds.
updateBranch_ttl :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_ttl = Lens.lens (\UpdateBranch' {ttl} -> ttl) (\s@UpdateBranch' {} a -> s {ttl = a} :: UpdateBranch)

-- | The basic authorization credentials for the branch. You must
-- base64-encode the authorization credentials and provide them in the
-- format @user:password@.
updateBranch_basicAuthCredentials :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_basicAuthCredentials = Lens.lens (\UpdateBranch' {basicAuthCredentials} -> basicAuthCredentials) (\s@UpdateBranch' {} a -> s {basicAuthCredentials = a} :: UpdateBranch) Prelude.. Lens.mapping Core._Sensitive

-- | The display name for a branch. This is used as the default domain
-- prefix.
updateBranch_displayName :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_displayName = Lens.lens (\UpdateBranch' {displayName} -> displayName) (\s@UpdateBranch' {} a -> s {displayName = a} :: UpdateBranch)

-- | The description for the branch.
updateBranch_description :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_description = Lens.lens (\UpdateBranch' {description} -> description) (\s@UpdateBranch' {} a -> s {description = a} :: UpdateBranch)

-- | The environment variables for the branch.
updateBranch_environmentVariables :: Lens.Lens' UpdateBranch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateBranch_environmentVariables = Lens.lens (\UpdateBranch' {environmentVariables} -> environmentVariables) (\s@UpdateBranch' {} a -> s {environmentVariables = a} :: UpdateBranch) Prelude.. Lens.mapping Lens.coerced

-- | The Amplify environment name for the pull request.
updateBranch_pullRequestEnvironmentName :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_pullRequestEnvironmentName = Lens.lens (\UpdateBranch' {pullRequestEnvironmentName} -> pullRequestEnvironmentName) (\s@UpdateBranch' {} a -> s {pullRequestEnvironmentName = a} :: UpdateBranch)

-- | The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
updateBranch_backendEnvironmentArn :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_backendEnvironmentArn = Lens.lens (\UpdateBranch' {backendEnvironmentArn} -> backendEnvironmentArn) (\s@UpdateBranch' {} a -> s {backendEnvironmentArn = a} :: UpdateBranch)

-- | Describes the current stage for the branch.
updateBranch_stage :: Lens.Lens' UpdateBranch (Prelude.Maybe Stage)
updateBranch_stage = Lens.lens (\UpdateBranch' {stage} -> stage) (\s@UpdateBranch' {} a -> s {stage = a} :: UpdateBranch)

-- | Enables auto building for the branch.
updateBranch_enableAutoBuild :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Bool)
updateBranch_enableAutoBuild = Lens.lens (\UpdateBranch' {enableAutoBuild} -> enableAutoBuild) (\s@UpdateBranch' {} a -> s {enableAutoBuild = a} :: UpdateBranch)

-- | Enables notifications for the branch.
updateBranch_enableNotification :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Bool)
updateBranch_enableNotification = Lens.lens (\UpdateBranch' {enableNotification} -> enableNotification) (\s@UpdateBranch' {} a -> s {enableNotification = a} :: UpdateBranch)

-- | Enables basic authorization for the branch.
updateBranch_enableBasicAuth :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Bool)
updateBranch_enableBasicAuth = Lens.lens (\UpdateBranch' {enableBasicAuth} -> enableBasicAuth) (\s@UpdateBranch' {} a -> s {enableBasicAuth = a} :: UpdateBranch)

-- | The framework for the branch.
updateBranch_framework :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_framework = Lens.lens (\UpdateBranch' {framework} -> framework) (\s@UpdateBranch' {} a -> s {framework = a} :: UpdateBranch)

-- | The build specification (build spec) for the branch.
updateBranch_buildSpec :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Text)
updateBranch_buildSpec = Lens.lens (\UpdateBranch' {buildSpec} -> buildSpec) (\s@UpdateBranch' {} a -> s {buildSpec = a} :: UpdateBranch) Prelude.. Lens.mapping Core._Sensitive

-- | Enables pull request previews for this branch.
updateBranch_enablePullRequestPreview :: Lens.Lens' UpdateBranch (Prelude.Maybe Prelude.Bool)
updateBranch_enablePullRequestPreview = Lens.lens (\UpdateBranch' {enablePullRequestPreview} -> enablePullRequestPreview) (\s@UpdateBranch' {} a -> s {enablePullRequestPreview = a} :: UpdateBranch)

-- | The unique ID for an Amplify app.
updateBranch_appId :: Lens.Lens' UpdateBranch Prelude.Text
updateBranch_appId = Lens.lens (\UpdateBranch' {appId} -> appId) (\s@UpdateBranch' {} a -> s {appId = a} :: UpdateBranch)

-- | The name for the branch.
updateBranch_branchName :: Lens.Lens' UpdateBranch Prelude.Text
updateBranch_branchName = Lens.lens (\UpdateBranch' {branchName} -> branchName) (\s@UpdateBranch' {} a -> s {branchName = a} :: UpdateBranch)

instance Core.AWSRequest UpdateBranch where
  type AWSResponse UpdateBranch = UpdateBranchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBranchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "branch")
      )

instance Prelude.Hashable UpdateBranch where
  hashWithSalt _salt UpdateBranch' {..} =
    _salt `Prelude.hashWithSalt` enablePerformanceMode
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` basicAuthCredentials
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` pullRequestEnvironmentName
      `Prelude.hashWithSalt` backendEnvironmentArn
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` enableAutoBuild
      `Prelude.hashWithSalt` enableNotification
      `Prelude.hashWithSalt` enableBasicAuth
      `Prelude.hashWithSalt` framework
      `Prelude.hashWithSalt` buildSpec
      `Prelude.hashWithSalt` enablePullRequestPreview
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` branchName

instance Prelude.NFData UpdateBranch where
  rnf UpdateBranch' {..} =
    Prelude.rnf enablePerformanceMode
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf basicAuthCredentials
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf pullRequestEnvironmentName
      `Prelude.seq` Prelude.rnf backendEnvironmentArn
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf enableAutoBuild
      `Prelude.seq` Prelude.rnf enableNotification
      `Prelude.seq` Prelude.rnf enableBasicAuth
      `Prelude.seq` Prelude.rnf framework
      `Prelude.seq` Prelude.rnf buildSpec
      `Prelude.seq` Prelude.rnf enablePullRequestPreview
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf branchName

instance Core.ToHeaders UpdateBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBranch where
  toJSON UpdateBranch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("enablePerformanceMode" Core..=)
              Prelude.<$> enablePerformanceMode,
            ("ttl" Core..=) Prelude.<$> ttl,
            ("basicAuthCredentials" Core..=)
              Prelude.<$> basicAuthCredentials,
            ("displayName" Core..=) Prelude.<$> displayName,
            ("description" Core..=) Prelude.<$> description,
            ("environmentVariables" Core..=)
              Prelude.<$> environmentVariables,
            ("pullRequestEnvironmentName" Core..=)
              Prelude.<$> pullRequestEnvironmentName,
            ("backendEnvironmentArn" Core..=)
              Prelude.<$> backendEnvironmentArn,
            ("stage" Core..=) Prelude.<$> stage,
            ("enableAutoBuild" Core..=)
              Prelude.<$> enableAutoBuild,
            ("enableNotification" Core..=)
              Prelude.<$> enableNotification,
            ("enableBasicAuth" Core..=)
              Prelude.<$> enableBasicAuth,
            ("framework" Core..=) Prelude.<$> framework,
            ("buildSpec" Core..=) Prelude.<$> buildSpec,
            ("enablePullRequestPreview" Core..=)
              Prelude.<$> enablePullRequestPreview
          ]
      )

instance Core.ToPath UpdateBranch where
  toPath UpdateBranch' {..} =
    Prelude.mconcat
      [ "/apps/",
        Core.toBS appId,
        "/branches/",
        Core.toBS branchName
      ]

instance Core.ToQuery UpdateBranch where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for the update branch request.
--
-- /See:/ 'newUpdateBranchResponse' smart constructor.
data UpdateBranchResponse = UpdateBranchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The branch for an Amplify app, which maps to a third-party repository
    -- branch.
    branch :: Branch
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateBranchResponse_httpStatus' - The response's http status code.
--
-- 'branch', 'updateBranchResponse_branch' - The branch for an Amplify app, which maps to a third-party repository
-- branch.
newUpdateBranchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'branch'
  Branch ->
  UpdateBranchResponse
newUpdateBranchResponse pHttpStatus_ pBranch_ =
  UpdateBranchResponse'
    { httpStatus = pHttpStatus_,
      branch = pBranch_
    }

-- | The response's http status code.
updateBranchResponse_httpStatus :: Lens.Lens' UpdateBranchResponse Prelude.Int
updateBranchResponse_httpStatus = Lens.lens (\UpdateBranchResponse' {httpStatus} -> httpStatus) (\s@UpdateBranchResponse' {} a -> s {httpStatus = a} :: UpdateBranchResponse)

-- | The branch for an Amplify app, which maps to a third-party repository
-- branch.
updateBranchResponse_branch :: Lens.Lens' UpdateBranchResponse Branch
updateBranchResponse_branch = Lens.lens (\UpdateBranchResponse' {branch} -> branch) (\s@UpdateBranchResponse' {} a -> s {branch = a} :: UpdateBranchResponse)

instance Prelude.NFData UpdateBranchResponse where
  rnf UpdateBranchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf branch
