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
-- Module      : Network.AWS.Amplify.CreateBranch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new branch for an Amplify app.
module Network.AWS.Amplify.CreateBranch
  ( -- * Creating a Request
    CreateBranch (..),
    newCreateBranch,

    -- * Request Lenses
    createBranch_framework,
    createBranch_ttl,
    createBranch_enableNotification,
    createBranch_stage,
    createBranch_backendEnvironmentArn,
    createBranch_enablePullRequestPreview,
    createBranch_basicAuthCredentials,
    createBranch_buildSpec,
    createBranch_enablePerformanceMode,
    createBranch_displayName,
    createBranch_environmentVariables,
    createBranch_enableAutoBuild,
    createBranch_enableBasicAuth,
    createBranch_pullRequestEnvironmentName,
    createBranch_description,
    createBranch_tags,
    createBranch_appId,
    createBranch_branchName,

    -- * Destructuring the Response
    CreateBranchResponse (..),
    newCreateBranchResponse,

    -- * Response Lenses
    createBranchResponse_httpStatus,
    createBranchResponse_branch,
  )
where

import Network.AWS.Amplify.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request structure for the create branch request.
--
-- /See:/ 'newCreateBranch' smart constructor.
data CreateBranch = CreateBranch'
  { -- | The framework for the branch.
    framework :: Prelude.Maybe Prelude.Text,
    -- | The content Time To Live (TTL) for the website in seconds.
    ttl :: Prelude.Maybe Prelude.Text,
    -- | Enables notifications for the branch.
    enableNotification :: Prelude.Maybe Prelude.Bool,
    -- | Describes the current stage for the branch.
    stage :: Prelude.Maybe Stage,
    -- | The Amazon Resource Name (ARN) for a backend environment that is part of
    -- an Amplify app.
    backendEnvironmentArn :: Prelude.Maybe Prelude.Text,
    -- | Enables pull request previews for this branch.
    enablePullRequestPreview :: Prelude.Maybe Prelude.Bool,
    -- | The basic authorization credentials for the branch.
    basicAuthCredentials :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The build specification (build spec) for the branch.
    buildSpec :: Prelude.Maybe Prelude.Text,
    -- | Enables performance mode for the branch.
    --
    -- Performance mode optimizes for faster hosting performance by keeping
    -- content cached at the edge for a longer interval. When performance mode
    -- is enabled, hosting configuration or code changes can take up to 10
    -- minutes to roll out.
    enablePerformanceMode :: Prelude.Maybe Prelude.Bool,
    -- | The display name for a branch. This is used as the default domain
    -- prefix.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The environment variables for the branch.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Enables auto building for the branch.
    enableAutoBuild :: Prelude.Maybe Prelude.Bool,
    -- | Enables basic authorization for the branch.
    enableBasicAuth :: Prelude.Maybe Prelude.Bool,
    -- | The Amplify environment name for the pull request.
    pullRequestEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The description for the branch.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tag for the branch.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name for the branch.
    branchName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framework', 'createBranch_framework' - The framework for the branch.
--
-- 'ttl', 'createBranch_ttl' - The content Time To Live (TTL) for the website in seconds.
--
-- 'enableNotification', 'createBranch_enableNotification' - Enables notifications for the branch.
--
-- 'stage', 'createBranch_stage' - Describes the current stage for the branch.
--
-- 'backendEnvironmentArn', 'createBranch_backendEnvironmentArn' - The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
--
-- 'enablePullRequestPreview', 'createBranch_enablePullRequestPreview' - Enables pull request previews for this branch.
--
-- 'basicAuthCredentials', 'createBranch_basicAuthCredentials' - The basic authorization credentials for the branch.
--
-- 'buildSpec', 'createBranch_buildSpec' - The build specification (build spec) for the branch.
--
-- 'enablePerformanceMode', 'createBranch_enablePerformanceMode' - Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
--
-- 'displayName', 'createBranch_displayName' - The display name for a branch. This is used as the default domain
-- prefix.
--
-- 'environmentVariables', 'createBranch_environmentVariables' - The environment variables for the branch.
--
-- 'enableAutoBuild', 'createBranch_enableAutoBuild' - Enables auto building for the branch.
--
-- 'enableBasicAuth', 'createBranch_enableBasicAuth' - Enables basic authorization for the branch.
--
-- 'pullRequestEnvironmentName', 'createBranch_pullRequestEnvironmentName' - The Amplify environment name for the pull request.
--
-- 'description', 'createBranch_description' - The description for the branch.
--
-- 'tags', 'createBranch_tags' - The tag for the branch.
--
-- 'appId', 'createBranch_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'createBranch_branchName' - The name for the branch.
newCreateBranch ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  CreateBranch
newCreateBranch pAppId_ pBranchName_ =
  CreateBranch'
    { framework = Prelude.Nothing,
      ttl = Prelude.Nothing,
      enableNotification = Prelude.Nothing,
      stage = Prelude.Nothing,
      backendEnvironmentArn = Prelude.Nothing,
      enablePullRequestPreview = Prelude.Nothing,
      basicAuthCredentials = Prelude.Nothing,
      buildSpec = Prelude.Nothing,
      enablePerformanceMode = Prelude.Nothing,
      displayName = Prelude.Nothing,
      environmentVariables = Prelude.Nothing,
      enableAutoBuild = Prelude.Nothing,
      enableBasicAuth = Prelude.Nothing,
      pullRequestEnvironmentName = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      appId = pAppId_,
      branchName = pBranchName_
    }

-- | The framework for the branch.
createBranch_framework :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_framework = Lens.lens (\CreateBranch' {framework} -> framework) (\s@CreateBranch' {} a -> s {framework = a} :: CreateBranch)

-- | The content Time To Live (TTL) for the website in seconds.
createBranch_ttl :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_ttl = Lens.lens (\CreateBranch' {ttl} -> ttl) (\s@CreateBranch' {} a -> s {ttl = a} :: CreateBranch)

-- | Enables notifications for the branch.
createBranch_enableNotification :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Bool)
createBranch_enableNotification = Lens.lens (\CreateBranch' {enableNotification} -> enableNotification) (\s@CreateBranch' {} a -> s {enableNotification = a} :: CreateBranch)

-- | Describes the current stage for the branch.
createBranch_stage :: Lens.Lens' CreateBranch (Prelude.Maybe Stage)
createBranch_stage = Lens.lens (\CreateBranch' {stage} -> stage) (\s@CreateBranch' {} a -> s {stage = a} :: CreateBranch)

-- | The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
createBranch_backendEnvironmentArn :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_backendEnvironmentArn = Lens.lens (\CreateBranch' {backendEnvironmentArn} -> backendEnvironmentArn) (\s@CreateBranch' {} a -> s {backendEnvironmentArn = a} :: CreateBranch)

-- | Enables pull request previews for this branch.
createBranch_enablePullRequestPreview :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Bool)
createBranch_enablePullRequestPreview = Lens.lens (\CreateBranch' {enablePullRequestPreview} -> enablePullRequestPreview) (\s@CreateBranch' {} a -> s {enablePullRequestPreview = a} :: CreateBranch)

-- | The basic authorization credentials for the branch.
createBranch_basicAuthCredentials :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_basicAuthCredentials = Lens.lens (\CreateBranch' {basicAuthCredentials} -> basicAuthCredentials) (\s@CreateBranch' {} a -> s {basicAuthCredentials = a} :: CreateBranch) Prelude.. Lens.mapping Core._Sensitive

-- | The build specification (build spec) for the branch.
createBranch_buildSpec :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_buildSpec = Lens.lens (\CreateBranch' {buildSpec} -> buildSpec) (\s@CreateBranch' {} a -> s {buildSpec = a} :: CreateBranch)

-- | Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
createBranch_enablePerformanceMode :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Bool)
createBranch_enablePerformanceMode = Lens.lens (\CreateBranch' {enablePerformanceMode} -> enablePerformanceMode) (\s@CreateBranch' {} a -> s {enablePerformanceMode = a} :: CreateBranch)

-- | The display name for a branch. This is used as the default domain
-- prefix.
createBranch_displayName :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_displayName = Lens.lens (\CreateBranch' {displayName} -> displayName) (\s@CreateBranch' {} a -> s {displayName = a} :: CreateBranch)

-- | The environment variables for the branch.
createBranch_environmentVariables :: Lens.Lens' CreateBranch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBranch_environmentVariables = Lens.lens (\CreateBranch' {environmentVariables} -> environmentVariables) (\s@CreateBranch' {} a -> s {environmentVariables = a} :: CreateBranch) Prelude.. Lens.mapping Lens.coerced

-- | Enables auto building for the branch.
createBranch_enableAutoBuild :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Bool)
createBranch_enableAutoBuild = Lens.lens (\CreateBranch' {enableAutoBuild} -> enableAutoBuild) (\s@CreateBranch' {} a -> s {enableAutoBuild = a} :: CreateBranch)

-- | Enables basic authorization for the branch.
createBranch_enableBasicAuth :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Bool)
createBranch_enableBasicAuth = Lens.lens (\CreateBranch' {enableBasicAuth} -> enableBasicAuth) (\s@CreateBranch' {} a -> s {enableBasicAuth = a} :: CreateBranch)

-- | The Amplify environment name for the pull request.
createBranch_pullRequestEnvironmentName :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_pullRequestEnvironmentName = Lens.lens (\CreateBranch' {pullRequestEnvironmentName} -> pullRequestEnvironmentName) (\s@CreateBranch' {} a -> s {pullRequestEnvironmentName = a} :: CreateBranch)

-- | The description for the branch.
createBranch_description :: Lens.Lens' CreateBranch (Prelude.Maybe Prelude.Text)
createBranch_description = Lens.lens (\CreateBranch' {description} -> description) (\s@CreateBranch' {} a -> s {description = a} :: CreateBranch)

-- | The tag for the branch.
createBranch_tags :: Lens.Lens' CreateBranch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBranch_tags = Lens.lens (\CreateBranch' {tags} -> tags) (\s@CreateBranch' {} a -> s {tags = a} :: CreateBranch) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID for an Amplify app.
createBranch_appId :: Lens.Lens' CreateBranch Prelude.Text
createBranch_appId = Lens.lens (\CreateBranch' {appId} -> appId) (\s@CreateBranch' {} a -> s {appId = a} :: CreateBranch)

-- | The name for the branch.
createBranch_branchName :: Lens.Lens' CreateBranch Prelude.Text
createBranch_branchName = Lens.lens (\CreateBranch' {branchName} -> branchName) (\s@CreateBranch' {} a -> s {branchName = a} :: CreateBranch)

instance Core.AWSRequest CreateBranch where
  type AWSResponse CreateBranch = CreateBranchResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBranchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "branch")
      )

instance Prelude.Hashable CreateBranch

instance Prelude.NFData CreateBranch

instance Core.ToHeaders CreateBranch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBranch where
  toJSON CreateBranch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("framework" Core..=) Prelude.<$> framework,
            ("ttl" Core..=) Prelude.<$> ttl,
            ("enableNotification" Core..=)
              Prelude.<$> enableNotification,
            ("stage" Core..=) Prelude.<$> stage,
            ("backendEnvironmentArn" Core..=)
              Prelude.<$> backendEnvironmentArn,
            ("enablePullRequestPreview" Core..=)
              Prelude.<$> enablePullRequestPreview,
            ("basicAuthCredentials" Core..=)
              Prelude.<$> basicAuthCredentials,
            ("buildSpec" Core..=) Prelude.<$> buildSpec,
            ("enablePerformanceMode" Core..=)
              Prelude.<$> enablePerformanceMode,
            ("displayName" Core..=) Prelude.<$> displayName,
            ("environmentVariables" Core..=)
              Prelude.<$> environmentVariables,
            ("enableAutoBuild" Core..=)
              Prelude.<$> enableAutoBuild,
            ("enableBasicAuth" Core..=)
              Prelude.<$> enableBasicAuth,
            ("pullRequestEnvironmentName" Core..=)
              Prelude.<$> pullRequestEnvironmentName,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("branchName" Core..= branchName)
          ]
      )

instance Core.ToPath CreateBranch where
  toPath CreateBranch' {..} =
    Prelude.mconcat
      ["/apps/", Core.toBS appId, "/branches"]

instance Core.ToQuery CreateBranch where
  toQuery = Prelude.const Prelude.mempty

-- | The result structure for create branch request.
--
-- /See:/ 'newCreateBranchResponse' smart constructor.
data CreateBranchResponse = CreateBranchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Describes the branch for an Amplify app, which maps to a third-party
    -- repository branch.
    branch :: Branch
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBranchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBranchResponse_httpStatus' - The response's http status code.
--
-- 'branch', 'createBranchResponse_branch' - Describes the branch for an Amplify app, which maps to a third-party
-- repository branch.
newCreateBranchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'branch'
  Branch ->
  CreateBranchResponse
newCreateBranchResponse pHttpStatus_ pBranch_ =
  CreateBranchResponse'
    { httpStatus = pHttpStatus_,
      branch = pBranch_
    }

-- | The response's http status code.
createBranchResponse_httpStatus :: Lens.Lens' CreateBranchResponse Prelude.Int
createBranchResponse_httpStatus = Lens.lens (\CreateBranchResponse' {httpStatus} -> httpStatus) (\s@CreateBranchResponse' {} a -> s {httpStatus = a} :: CreateBranchResponse)

-- | Describes the branch for an Amplify app, which maps to a third-party
-- repository branch.
createBranchResponse_branch :: Lens.Lens' CreateBranchResponse Branch
createBranchResponse_branch = Lens.lens (\CreateBranchResponse' {branch} -> branch) (\s@CreateBranchResponse' {} a -> s {branch = a} :: CreateBranchResponse)

instance Prelude.NFData CreateBranchResponse
