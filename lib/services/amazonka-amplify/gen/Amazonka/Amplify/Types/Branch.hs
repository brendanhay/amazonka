{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Amplify.Types.Branch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.Branch where

import Amazonka.Amplify.Types.Stage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The branch for an Amplify app, which maps to a third-party repository
-- branch.
--
-- /See:/ 'newBranch' smart constructor.
data Branch = Branch'
  { -- | The tag for the branch of an Amplify app.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The thumbnail URL for the branch of an Amplify app.
    thumbnailUrl :: Prelude.Maybe Prelude.Text,
    -- | Enables performance mode for the branch.
    --
    -- Performance mode optimizes for faster hosting performance by keeping
    -- content cached at the edge for a longer interval. When performance mode
    -- is enabled, hosting configuration or code changes can take up to 10
    -- minutes to roll out.
    enablePerformanceMode :: Prelude.Maybe Prelude.Bool,
    -- | The destination branch if the branch is a pull request branch.
    destinationBranch :: Prelude.Maybe Prelude.Text,
    -- | A list of custom resources that are linked to this branch.
    associatedResources :: Prelude.Maybe [Prelude.Text],
    -- | The basic authorization credentials for a branch of an Amplify app. You
    -- must base64-encode the authorization credentials and provide them in the
    -- format @user:password@.
    basicAuthCredentials :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The source branch if the branch is a pull request branch.
    sourceBranch :: Prelude.Maybe Prelude.Text,
    -- | The Amplify environment name for the pull request.
    pullRequestEnvironmentName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for a backend environment that is part of
    -- an Amplify app.
    backendEnvironmentArn :: Prelude.Maybe Prelude.Text,
    -- | The build specification (build spec) content for the branch of an
    -- Amplify app.
    buildSpec :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) for a branch that is part of an Amplify
    -- app.
    branchArn :: Prelude.Text,
    -- | The name for the branch that is part of an Amplify app.
    branchName :: Prelude.Text,
    -- | The description for the branch that is part of an Amplify app.
    description :: Prelude.Text,
    -- | The current stage for the branch that is part of an Amplify app.
    stage :: Stage,
    -- | The display name for the branch. This is used as the default domain
    -- prefix.
    displayName :: Prelude.Text,
    -- | Enables notifications for a branch that is part of an Amplify app.
    enableNotification :: Prelude.Bool,
    -- | The creation date and time for a branch that is part of an Amplify app.
    createTime :: Data.POSIX,
    -- | The last updated date and time for a branch that is part of an Amplify
    -- app.
    updateTime :: Data.POSIX,
    -- | The environment variables specific to a branch of an Amplify app.
    environmentVariables :: Prelude.HashMap Prelude.Text Prelude.Text,
    -- | Enables auto-building on push for a branch of an Amplify app.
    enableAutoBuild :: Prelude.Bool,
    -- | The custom domains for a branch of an Amplify app.
    customDomains :: [Prelude.Text],
    -- | The framework for a branch of an Amplify app.
    framework :: Prelude.Text,
    -- | The ID of the active job for a branch of an Amplify app.
    activeJobId :: Prelude.Text,
    -- | The total number of jobs that are part of an Amplify app.
    totalNumberOfJobs :: Prelude.Text,
    -- | Enables basic authorization for a branch of an Amplify app.
    enableBasicAuth :: Prelude.Bool,
    -- | The content Time to Live (TTL) for the website in seconds.
    ttl :: Prelude.Text,
    -- | Enables pull request previews for the branch.
    enablePullRequestPreview :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Branch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'branch_tags' - The tag for the branch of an Amplify app.
--
-- 'thumbnailUrl', 'branch_thumbnailUrl' - The thumbnail URL for the branch of an Amplify app.
--
-- 'enablePerformanceMode', 'branch_enablePerformanceMode' - Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
--
-- 'destinationBranch', 'branch_destinationBranch' - The destination branch if the branch is a pull request branch.
--
-- 'associatedResources', 'branch_associatedResources' - A list of custom resources that are linked to this branch.
--
-- 'basicAuthCredentials', 'branch_basicAuthCredentials' - The basic authorization credentials for a branch of an Amplify app. You
-- must base64-encode the authorization credentials and provide them in the
-- format @user:password@.
--
-- 'sourceBranch', 'branch_sourceBranch' - The source branch if the branch is a pull request branch.
--
-- 'pullRequestEnvironmentName', 'branch_pullRequestEnvironmentName' - The Amplify environment name for the pull request.
--
-- 'backendEnvironmentArn', 'branch_backendEnvironmentArn' - The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
--
-- 'buildSpec', 'branch_buildSpec' - The build specification (build spec) content for the branch of an
-- Amplify app.
--
-- 'branchArn', 'branch_branchArn' - The Amazon Resource Name (ARN) for a branch that is part of an Amplify
-- app.
--
-- 'branchName', 'branch_branchName' - The name for the branch that is part of an Amplify app.
--
-- 'description', 'branch_description' - The description for the branch that is part of an Amplify app.
--
-- 'stage', 'branch_stage' - The current stage for the branch that is part of an Amplify app.
--
-- 'displayName', 'branch_displayName' - The display name for the branch. This is used as the default domain
-- prefix.
--
-- 'enableNotification', 'branch_enableNotification' - Enables notifications for a branch that is part of an Amplify app.
--
-- 'createTime', 'branch_createTime' - The creation date and time for a branch that is part of an Amplify app.
--
-- 'updateTime', 'branch_updateTime' - The last updated date and time for a branch that is part of an Amplify
-- app.
--
-- 'environmentVariables', 'branch_environmentVariables' - The environment variables specific to a branch of an Amplify app.
--
-- 'enableAutoBuild', 'branch_enableAutoBuild' - Enables auto-building on push for a branch of an Amplify app.
--
-- 'customDomains', 'branch_customDomains' - The custom domains for a branch of an Amplify app.
--
-- 'framework', 'branch_framework' - The framework for a branch of an Amplify app.
--
-- 'activeJobId', 'branch_activeJobId' - The ID of the active job for a branch of an Amplify app.
--
-- 'totalNumberOfJobs', 'branch_totalNumberOfJobs' - The total number of jobs that are part of an Amplify app.
--
-- 'enableBasicAuth', 'branch_enableBasicAuth' - Enables basic authorization for a branch of an Amplify app.
--
-- 'ttl', 'branch_ttl' - The content Time to Live (TTL) for the website in seconds.
--
-- 'enablePullRequestPreview', 'branch_enablePullRequestPreview' - Enables pull request previews for the branch.
newBranch ::
  -- | 'branchArn'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'stage'
  Stage ->
  -- | 'displayName'
  Prelude.Text ->
  -- | 'enableNotification'
  Prelude.Bool ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'enableAutoBuild'
  Prelude.Bool ->
  -- | 'framework'
  Prelude.Text ->
  -- | 'activeJobId'
  Prelude.Text ->
  -- | 'totalNumberOfJobs'
  Prelude.Text ->
  -- | 'enableBasicAuth'
  Prelude.Bool ->
  -- | 'ttl'
  Prelude.Text ->
  -- | 'enablePullRequestPreview'
  Prelude.Bool ->
  Branch
newBranch
  pBranchArn_
  pBranchName_
  pDescription_
  pStage_
  pDisplayName_
  pEnableNotification_
  pCreateTime_
  pUpdateTime_
  pEnableAutoBuild_
  pFramework_
  pActiveJobId_
  pTotalNumberOfJobs_
  pEnableBasicAuth_
  pTtl_
  pEnablePullRequestPreview_ =
    Branch'
      { tags = Prelude.Nothing,
        thumbnailUrl = Prelude.Nothing,
        enablePerformanceMode = Prelude.Nothing,
        destinationBranch = Prelude.Nothing,
        associatedResources = Prelude.Nothing,
        basicAuthCredentials = Prelude.Nothing,
        sourceBranch = Prelude.Nothing,
        pullRequestEnvironmentName = Prelude.Nothing,
        backendEnvironmentArn = Prelude.Nothing,
        buildSpec = Prelude.Nothing,
        branchArn = pBranchArn_,
        branchName = pBranchName_,
        description = pDescription_,
        stage = pStage_,
        displayName = pDisplayName_,
        enableNotification = pEnableNotification_,
        createTime = Data._Time Lens.# pCreateTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        environmentVariables = Prelude.mempty,
        enableAutoBuild = pEnableAutoBuild_,
        customDomains = Prelude.mempty,
        framework = pFramework_,
        activeJobId = pActiveJobId_,
        totalNumberOfJobs = pTotalNumberOfJobs_,
        enableBasicAuth = pEnableBasicAuth_,
        ttl = pTtl_,
        enablePullRequestPreview =
          pEnablePullRequestPreview_
      }

-- | The tag for the branch of an Amplify app.
branch_tags :: Lens.Lens' Branch (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
branch_tags = Lens.lens (\Branch' {tags} -> tags) (\s@Branch' {} a -> s {tags = a} :: Branch) Prelude.. Lens.mapping Lens.coerced

-- | The thumbnail URL for the branch of an Amplify app.
branch_thumbnailUrl :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_thumbnailUrl = Lens.lens (\Branch' {thumbnailUrl} -> thumbnailUrl) (\s@Branch' {} a -> s {thumbnailUrl = a} :: Branch)

-- | Enables performance mode for the branch.
--
-- Performance mode optimizes for faster hosting performance by keeping
-- content cached at the edge for a longer interval. When performance mode
-- is enabled, hosting configuration or code changes can take up to 10
-- minutes to roll out.
branch_enablePerformanceMode :: Lens.Lens' Branch (Prelude.Maybe Prelude.Bool)
branch_enablePerformanceMode = Lens.lens (\Branch' {enablePerformanceMode} -> enablePerformanceMode) (\s@Branch' {} a -> s {enablePerformanceMode = a} :: Branch)

-- | The destination branch if the branch is a pull request branch.
branch_destinationBranch :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_destinationBranch = Lens.lens (\Branch' {destinationBranch} -> destinationBranch) (\s@Branch' {} a -> s {destinationBranch = a} :: Branch)

-- | A list of custom resources that are linked to this branch.
branch_associatedResources :: Lens.Lens' Branch (Prelude.Maybe [Prelude.Text])
branch_associatedResources = Lens.lens (\Branch' {associatedResources} -> associatedResources) (\s@Branch' {} a -> s {associatedResources = a} :: Branch) Prelude.. Lens.mapping Lens.coerced

-- | The basic authorization credentials for a branch of an Amplify app. You
-- must base64-encode the authorization credentials and provide them in the
-- format @user:password@.
branch_basicAuthCredentials :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_basicAuthCredentials = Lens.lens (\Branch' {basicAuthCredentials} -> basicAuthCredentials) (\s@Branch' {} a -> s {basicAuthCredentials = a} :: Branch) Prelude.. Lens.mapping Data._Sensitive

-- | The source branch if the branch is a pull request branch.
branch_sourceBranch :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_sourceBranch = Lens.lens (\Branch' {sourceBranch} -> sourceBranch) (\s@Branch' {} a -> s {sourceBranch = a} :: Branch)

-- | The Amplify environment name for the pull request.
branch_pullRequestEnvironmentName :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_pullRequestEnvironmentName = Lens.lens (\Branch' {pullRequestEnvironmentName} -> pullRequestEnvironmentName) (\s@Branch' {} a -> s {pullRequestEnvironmentName = a} :: Branch)

-- | The Amazon Resource Name (ARN) for a backend environment that is part of
-- an Amplify app.
branch_backendEnvironmentArn :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_backendEnvironmentArn = Lens.lens (\Branch' {backendEnvironmentArn} -> backendEnvironmentArn) (\s@Branch' {} a -> s {backendEnvironmentArn = a} :: Branch)

-- | The build specification (build spec) content for the branch of an
-- Amplify app.
branch_buildSpec :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_buildSpec = Lens.lens (\Branch' {buildSpec} -> buildSpec) (\s@Branch' {} a -> s {buildSpec = a} :: Branch) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) for a branch that is part of an Amplify
-- app.
branch_branchArn :: Lens.Lens' Branch Prelude.Text
branch_branchArn = Lens.lens (\Branch' {branchArn} -> branchArn) (\s@Branch' {} a -> s {branchArn = a} :: Branch)

-- | The name for the branch that is part of an Amplify app.
branch_branchName :: Lens.Lens' Branch Prelude.Text
branch_branchName = Lens.lens (\Branch' {branchName} -> branchName) (\s@Branch' {} a -> s {branchName = a} :: Branch)

-- | The description for the branch that is part of an Amplify app.
branch_description :: Lens.Lens' Branch Prelude.Text
branch_description = Lens.lens (\Branch' {description} -> description) (\s@Branch' {} a -> s {description = a} :: Branch)

-- | The current stage for the branch that is part of an Amplify app.
branch_stage :: Lens.Lens' Branch Stage
branch_stage = Lens.lens (\Branch' {stage} -> stage) (\s@Branch' {} a -> s {stage = a} :: Branch)

-- | The display name for the branch. This is used as the default domain
-- prefix.
branch_displayName :: Lens.Lens' Branch Prelude.Text
branch_displayName = Lens.lens (\Branch' {displayName} -> displayName) (\s@Branch' {} a -> s {displayName = a} :: Branch)

-- | Enables notifications for a branch that is part of an Amplify app.
branch_enableNotification :: Lens.Lens' Branch Prelude.Bool
branch_enableNotification = Lens.lens (\Branch' {enableNotification} -> enableNotification) (\s@Branch' {} a -> s {enableNotification = a} :: Branch)

-- | The creation date and time for a branch that is part of an Amplify app.
branch_createTime :: Lens.Lens' Branch Prelude.UTCTime
branch_createTime = Lens.lens (\Branch' {createTime} -> createTime) (\s@Branch' {} a -> s {createTime = a} :: Branch) Prelude.. Data._Time

-- | The last updated date and time for a branch that is part of an Amplify
-- app.
branch_updateTime :: Lens.Lens' Branch Prelude.UTCTime
branch_updateTime = Lens.lens (\Branch' {updateTime} -> updateTime) (\s@Branch' {} a -> s {updateTime = a} :: Branch) Prelude.. Data._Time

-- | The environment variables specific to a branch of an Amplify app.
branch_environmentVariables :: Lens.Lens' Branch (Prelude.HashMap Prelude.Text Prelude.Text)
branch_environmentVariables = Lens.lens (\Branch' {environmentVariables} -> environmentVariables) (\s@Branch' {} a -> s {environmentVariables = a} :: Branch) Prelude.. Lens.coerced

-- | Enables auto-building on push for a branch of an Amplify app.
branch_enableAutoBuild :: Lens.Lens' Branch Prelude.Bool
branch_enableAutoBuild = Lens.lens (\Branch' {enableAutoBuild} -> enableAutoBuild) (\s@Branch' {} a -> s {enableAutoBuild = a} :: Branch)

-- | The custom domains for a branch of an Amplify app.
branch_customDomains :: Lens.Lens' Branch [Prelude.Text]
branch_customDomains = Lens.lens (\Branch' {customDomains} -> customDomains) (\s@Branch' {} a -> s {customDomains = a} :: Branch) Prelude.. Lens.coerced

-- | The framework for a branch of an Amplify app.
branch_framework :: Lens.Lens' Branch Prelude.Text
branch_framework = Lens.lens (\Branch' {framework} -> framework) (\s@Branch' {} a -> s {framework = a} :: Branch)

-- | The ID of the active job for a branch of an Amplify app.
branch_activeJobId :: Lens.Lens' Branch Prelude.Text
branch_activeJobId = Lens.lens (\Branch' {activeJobId} -> activeJobId) (\s@Branch' {} a -> s {activeJobId = a} :: Branch)

-- | The total number of jobs that are part of an Amplify app.
branch_totalNumberOfJobs :: Lens.Lens' Branch Prelude.Text
branch_totalNumberOfJobs = Lens.lens (\Branch' {totalNumberOfJobs} -> totalNumberOfJobs) (\s@Branch' {} a -> s {totalNumberOfJobs = a} :: Branch)

-- | Enables basic authorization for a branch of an Amplify app.
branch_enableBasicAuth :: Lens.Lens' Branch Prelude.Bool
branch_enableBasicAuth = Lens.lens (\Branch' {enableBasicAuth} -> enableBasicAuth) (\s@Branch' {} a -> s {enableBasicAuth = a} :: Branch)

-- | The content Time to Live (TTL) for the website in seconds.
branch_ttl :: Lens.Lens' Branch Prelude.Text
branch_ttl = Lens.lens (\Branch' {ttl} -> ttl) (\s@Branch' {} a -> s {ttl = a} :: Branch)

-- | Enables pull request previews for the branch.
branch_enablePullRequestPreview :: Lens.Lens' Branch Prelude.Bool
branch_enablePullRequestPreview = Lens.lens (\Branch' {enablePullRequestPreview} -> enablePullRequestPreview) (\s@Branch' {} a -> s {enablePullRequestPreview = a} :: Branch)

instance Data.FromJSON Branch where
  parseJSON =
    Data.withObject
      "Branch"
      ( \x ->
          Branch'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "thumbnailUrl")
            Prelude.<*> (x Data..:? "enablePerformanceMode")
            Prelude.<*> (x Data..:? "destinationBranch")
            Prelude.<*> ( x Data..:? "associatedResources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "basicAuthCredentials")
            Prelude.<*> (x Data..:? "sourceBranch")
            Prelude.<*> (x Data..:? "pullRequestEnvironmentName")
            Prelude.<*> (x Data..:? "backendEnvironmentArn")
            Prelude.<*> (x Data..:? "buildSpec")
            Prelude.<*> (x Data..: "branchArn")
            Prelude.<*> (x Data..: "branchName")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "stage")
            Prelude.<*> (x Data..: "displayName")
            Prelude.<*> (x Data..: "enableNotification")
            Prelude.<*> (x Data..: "createTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> ( x Data..:? "environmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "enableAutoBuild")
            Prelude.<*> (x Data..:? "customDomains" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "framework")
            Prelude.<*> (x Data..: "activeJobId")
            Prelude.<*> (x Data..: "totalNumberOfJobs")
            Prelude.<*> (x Data..: "enableBasicAuth")
            Prelude.<*> (x Data..: "ttl")
            Prelude.<*> (x Data..: "enablePullRequestPreview")
      )

instance Prelude.Hashable Branch where
  hashWithSalt _salt Branch' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` thumbnailUrl
      `Prelude.hashWithSalt` enablePerformanceMode
      `Prelude.hashWithSalt` destinationBranch
      `Prelude.hashWithSalt` associatedResources
      `Prelude.hashWithSalt` basicAuthCredentials
      `Prelude.hashWithSalt` sourceBranch
      `Prelude.hashWithSalt` pullRequestEnvironmentName
      `Prelude.hashWithSalt` backendEnvironmentArn
      `Prelude.hashWithSalt` buildSpec
      `Prelude.hashWithSalt` branchArn
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` enableNotification
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` enableAutoBuild
      `Prelude.hashWithSalt` customDomains
      `Prelude.hashWithSalt` framework
      `Prelude.hashWithSalt` activeJobId
      `Prelude.hashWithSalt` totalNumberOfJobs
      `Prelude.hashWithSalt` enableBasicAuth
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` enablePullRequestPreview

instance Prelude.NFData Branch where
  rnf Branch' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf thumbnailUrl
      `Prelude.seq` Prelude.rnf enablePerformanceMode
      `Prelude.seq` Prelude.rnf destinationBranch
      `Prelude.seq` Prelude.rnf associatedResources
      `Prelude.seq` Prelude.rnf basicAuthCredentials
      `Prelude.seq` Prelude.rnf sourceBranch
      `Prelude.seq` Prelude.rnf pullRequestEnvironmentName
      `Prelude.seq` Prelude.rnf backendEnvironmentArn
      `Prelude.seq` Prelude.rnf buildSpec
      `Prelude.seq` Prelude.rnf branchArn
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf enableNotification
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf enableAutoBuild
      `Prelude.seq` Prelude.rnf customDomains
      `Prelude.seq` Prelude.rnf framework
      `Prelude.seq` Prelude.rnf activeJobId
      `Prelude.seq` Prelude.rnf
        totalNumberOfJobs
      `Prelude.seq` Prelude.rnf
        enableBasicAuth
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf
        enablePullRequestPreview
