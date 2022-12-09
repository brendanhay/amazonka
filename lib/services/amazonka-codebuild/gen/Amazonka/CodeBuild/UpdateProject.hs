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
-- Module      : Amazonka.CodeBuild.UpdateProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of a build project.
module Amazonka.CodeBuild.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_artifacts,
    updateProject_badgeEnabled,
    updateProject_buildBatchConfig,
    updateProject_cache,
    updateProject_concurrentBuildLimit,
    updateProject_description,
    updateProject_encryptionKey,
    updateProject_environment,
    updateProject_fileSystemLocations,
    updateProject_logsConfig,
    updateProject_queuedTimeoutInMinutes,
    updateProject_secondaryArtifacts,
    updateProject_secondarySourceVersions,
    updateProject_secondarySources,
    updateProject_serviceRole,
    updateProject_source,
    updateProject_sourceVersion,
    updateProject_tags,
    updateProject_timeoutInMinutes,
    updateProject_vpcConfig,
    updateProject_name,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | Information to be changed about the build output artifacts for the build
    -- project.
    artifacts :: Prelude.Maybe ProjectArtifacts,
    -- | Set this to true to generate a publicly accessible URL for your
    -- project\'s build badge.
    badgeEnabled :: Prelude.Maybe Prelude.Bool,
    buildBatchConfig :: Prelude.Maybe ProjectBuildBatchConfig,
    -- | Stores recently used information so that it can be quickly accessed at a
    -- later time.
    cache :: Prelude.Maybe ProjectCache,
    -- | The maximum number of concurrent builds that are allowed for this
    -- project.
    --
    -- New builds are only started if the current number of builds is less than
    -- or equal to this limit. If the current build count meets this limit, new
    -- builds are throttled and are not run.
    --
    -- To remove this limit, set this value to -1.
    concurrentBuildLimit :: Prelude.Maybe Prelude.Int,
    -- | A new or replacement description of the build project.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Key Management Service customer master key (CMK) to be used for
    -- encrypting the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | Information to be changed about the build environment for the build
    -- project.
    environment :: Prelude.Maybe ProjectEnvironment,
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    -- | Information about logs for the build project. A project can create logs
    -- in CloudWatch Logs, logs in an S3 bucket, or both.
    logsConfig :: Prelude.Maybe LogsConfig,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | An array of @ProjectArtifact@ objects.
    secondaryArtifacts :: Prelude.Maybe [ProjectArtifacts],
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
    -- is specified at the build level, then they take over these
    -- @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Prelude.Maybe [ProjectSourceVersion],
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Prelude.Maybe [ProjectSource],
    -- | The replacement ARN of the IAM role that enables CodeBuild to interact
    -- with dependent Amazon Web Services services on behalf of the Amazon Web
    -- Services account.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Information to be changed about the build input source code for the
    -- build project.
    source :: Prelude.Maybe ProjectSource,
    -- | A version of the build input to be built for this project. If not
    -- specified, the latest version is used. If specified, it must be one of:
    --
    -- -   For CodeCommit: the commit ID, branch, or Git tag to use.
    --
    -- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
    --     that corresponds to the version of the source code you want to
    --     build. If a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- -   For Bitbucket: the commit ID, branch name, or tag name that
    --     corresponds to the version of the source code you want to build. If
    --     a branch name is specified, the branch\'s HEAD commit ID is used. If
    --     not specified, the default branch\'s HEAD commit ID is used.
    --
    -- -   For Amazon S3: the version ID of the object that represents the
    --     build input ZIP file to use.
    --
    -- If @sourceVersion@ is specified at the build level, then that version
    -- takes precedence over this @sourceVersion@ (at the project level).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | An updated list of tag key and value pairs associated with this build
    -- project.
    --
    -- These tags are available for use by Amazon Web Services services that
    -- support CodeBuild build project tags.
    tags :: Prelude.Maybe [Tag],
    -- | The replacement value in minutes, from 5 to 480 (8 hours), for CodeBuild
    -- to wait before timing out any related build that did not get marked as
    -- completed.
    timeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | VpcConfig enables CodeBuild to access resources in an Amazon VPC.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name of the build project.
    --
    -- You cannot change a build project\'s name.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifacts', 'updateProject_artifacts' - Information to be changed about the build output artifacts for the build
-- project.
--
-- 'badgeEnabled', 'updateProject_badgeEnabled' - Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
--
-- 'buildBatchConfig', 'updateProject_buildBatchConfig' - Undocumented member.
--
-- 'cache', 'updateProject_cache' - Stores recently used information so that it can be quickly accessed at a
-- later time.
--
-- 'concurrentBuildLimit', 'updateProject_concurrentBuildLimit' - The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
--
-- To remove this limit, set this value to -1.
--
-- 'description', 'updateProject_description' - A new or replacement description of the build project.
--
-- 'encryptionKey', 'updateProject_encryptionKey' - The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'environment', 'updateProject_environment' - Information to be changed about the build environment for the build
-- project.
--
-- 'fileSystemLocations', 'updateProject_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'logsConfig', 'updateProject_logsConfig' - Information about logs for the build project. A project can create logs
-- in CloudWatch Logs, logs in an S3 bucket, or both.
--
-- 'queuedTimeoutInMinutes', 'updateProject_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'secondaryArtifacts', 'updateProject_secondaryArtifacts' - An array of @ProjectArtifact@ objects.
--
-- 'secondarySourceVersions', 'updateProject_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take over these
-- @secondarySourceVersions@ (at the project level).
--
-- 'secondarySources', 'updateProject_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'serviceRole', 'updateProject_serviceRole' - The replacement ARN of the IAM role that enables CodeBuild to interact
-- with dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
--
-- 'source', 'updateProject_source' - Information to be changed about the build input source code for the
-- build project.
--
-- 'sourceVersion', 'updateProject_sourceVersion' - A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- If @sourceVersion@ is specified at the build level, then that version
-- takes precedence over this @sourceVersion@ (at the project level).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
--
-- 'tags', 'updateProject_tags' - An updated list of tag key and value pairs associated with this build
-- project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
--
-- 'timeoutInMinutes', 'updateProject_timeoutInMinutes' - The replacement value in minutes, from 5 to 480 (8 hours), for CodeBuild
-- to wait before timing out any related build that did not get marked as
-- completed.
--
-- 'vpcConfig', 'updateProject_vpcConfig' - VpcConfig enables CodeBuild to access resources in an Amazon VPC.
--
-- 'name', 'updateProject_name' - The name of the build project.
--
-- You cannot change a build project\'s name.
newUpdateProject ::
  -- | 'name'
  Prelude.Text ->
  UpdateProject
newUpdateProject pName_ =
  UpdateProject'
    { artifacts = Prelude.Nothing,
      badgeEnabled = Prelude.Nothing,
      buildBatchConfig = Prelude.Nothing,
      cache = Prelude.Nothing,
      concurrentBuildLimit = Prelude.Nothing,
      description = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      environment = Prelude.Nothing,
      fileSystemLocations = Prelude.Nothing,
      logsConfig = Prelude.Nothing,
      queuedTimeoutInMinutes = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing,
      secondarySourceVersions = Prelude.Nothing,
      secondarySources = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      tags = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      name = pName_
    }

-- | Information to be changed about the build output artifacts for the build
-- project.
updateProject_artifacts :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectArtifacts)
updateProject_artifacts = Lens.lens (\UpdateProject' {artifacts} -> artifacts) (\s@UpdateProject' {} a -> s {artifacts = a} :: UpdateProject)

-- | Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
updateProject_badgeEnabled :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Bool)
updateProject_badgeEnabled = Lens.lens (\UpdateProject' {badgeEnabled} -> badgeEnabled) (\s@UpdateProject' {} a -> s {badgeEnabled = a} :: UpdateProject)

-- | Undocumented member.
updateProject_buildBatchConfig :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectBuildBatchConfig)
updateProject_buildBatchConfig = Lens.lens (\UpdateProject' {buildBatchConfig} -> buildBatchConfig) (\s@UpdateProject' {} a -> s {buildBatchConfig = a} :: UpdateProject)

-- | Stores recently used information so that it can be quickly accessed at a
-- later time.
updateProject_cache :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectCache)
updateProject_cache = Lens.lens (\UpdateProject' {cache} -> cache) (\s@UpdateProject' {} a -> s {cache = a} :: UpdateProject)

-- | The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
--
-- To remove this limit, set this value to -1.
updateProject_concurrentBuildLimit :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Int)
updateProject_concurrentBuildLimit = Lens.lens (\UpdateProject' {concurrentBuildLimit} -> concurrentBuildLimit) (\s@UpdateProject' {} a -> s {concurrentBuildLimit = a} :: UpdateProject)

-- | A new or replacement description of the build project.
updateProject_description :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_description = Lens.lens (\UpdateProject' {description} -> description) (\s@UpdateProject' {} a -> s {description = a} :: UpdateProject)

-- | The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
updateProject_encryptionKey :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_encryptionKey = Lens.lens (\UpdateProject' {encryptionKey} -> encryptionKey) (\s@UpdateProject' {} a -> s {encryptionKey = a} :: UpdateProject)

-- | Information to be changed about the build environment for the build
-- project.
updateProject_environment :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectEnvironment)
updateProject_environment = Lens.lens (\UpdateProject' {environment} -> environment) (\s@UpdateProject' {} a -> s {environment = a} :: UpdateProject)

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
updateProject_fileSystemLocations :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectFileSystemLocation])
updateProject_fileSystemLocations = Lens.lens (\UpdateProject' {fileSystemLocations} -> fileSystemLocations) (\s@UpdateProject' {} a -> s {fileSystemLocations = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | Information about logs for the build project. A project can create logs
-- in CloudWatch Logs, logs in an S3 bucket, or both.
updateProject_logsConfig :: Lens.Lens' UpdateProject (Prelude.Maybe LogsConfig)
updateProject_logsConfig = Lens.lens (\UpdateProject' {logsConfig} -> logsConfig) (\s@UpdateProject' {} a -> s {logsConfig = a} :: UpdateProject)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
updateProject_queuedTimeoutInMinutes :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Natural)
updateProject_queuedTimeoutInMinutes = Lens.lens (\UpdateProject' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@UpdateProject' {} a -> s {queuedTimeoutInMinutes = a} :: UpdateProject)

-- | An array of @ProjectArtifact@ objects.
updateProject_secondaryArtifacts :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectArtifacts])
updateProject_secondaryArtifacts = Lens.lens (\UpdateProject' {secondaryArtifacts} -> secondaryArtifacts) (\s@UpdateProject' {} a -> s {secondaryArtifacts = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take over these
-- @secondarySourceVersions@ (at the project level).
updateProject_secondarySourceVersions :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectSourceVersion])
updateProject_secondarySourceVersions = Lens.lens (\UpdateProject' {secondarySourceVersions} -> secondarySourceVersions) (\s@UpdateProject' {} a -> s {secondarySourceVersions = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectSource@ objects.
updateProject_secondarySources :: Lens.Lens' UpdateProject (Prelude.Maybe [ProjectSource])
updateProject_secondarySources = Lens.lens (\UpdateProject' {secondarySources} -> secondarySources) (\s@UpdateProject' {} a -> s {secondarySources = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | The replacement ARN of the IAM role that enables CodeBuild to interact
-- with dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
updateProject_serviceRole :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_serviceRole = Lens.lens (\UpdateProject' {serviceRole} -> serviceRole) (\s@UpdateProject' {} a -> s {serviceRole = a} :: UpdateProject)

-- | Information to be changed about the build input source code for the
-- build project.
updateProject_source :: Lens.Lens' UpdateProject (Prelude.Maybe ProjectSource)
updateProject_source = Lens.lens (\UpdateProject' {source} -> source) (\s@UpdateProject' {} a -> s {source = a} :: UpdateProject)

-- | A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- If @sourceVersion@ is specified at the build level, then that version
-- takes precedence over this @sourceVersion@ (at the project level).
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
updateProject_sourceVersion :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_sourceVersion = Lens.lens (\UpdateProject' {sourceVersion} -> sourceVersion) (\s@UpdateProject' {} a -> s {sourceVersion = a} :: UpdateProject)

-- | An updated list of tag key and value pairs associated with this build
-- project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
updateProject_tags :: Lens.Lens' UpdateProject (Prelude.Maybe [Tag])
updateProject_tags = Lens.lens (\UpdateProject' {tags} -> tags) (\s@UpdateProject' {} a -> s {tags = a} :: UpdateProject) Prelude.. Lens.mapping Lens.coerced

-- | The replacement value in minutes, from 5 to 480 (8 hours), for CodeBuild
-- to wait before timing out any related build that did not get marked as
-- completed.
updateProject_timeoutInMinutes :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Natural)
updateProject_timeoutInMinutes = Lens.lens (\UpdateProject' {timeoutInMinutes} -> timeoutInMinutes) (\s@UpdateProject' {} a -> s {timeoutInMinutes = a} :: UpdateProject)

-- | VpcConfig enables CodeBuild to access resources in an Amazon VPC.
updateProject_vpcConfig :: Lens.Lens' UpdateProject (Prelude.Maybe VpcConfig)
updateProject_vpcConfig = Lens.lens (\UpdateProject' {vpcConfig} -> vpcConfig) (\s@UpdateProject' {} a -> s {vpcConfig = a} :: UpdateProject)

-- | The name of the build project.
--
-- You cannot change a build project\'s name.
updateProject_name :: Lens.Lens' UpdateProject Prelude.Text
updateProject_name = Lens.lens (\UpdateProject' {name} -> name) (\s@UpdateProject' {} a -> s {name = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (x Data..?> "project")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt _salt UpdateProject' {..} =
    _salt `Prelude.hashWithSalt` artifacts
      `Prelude.hashWithSalt` badgeEnabled
      `Prelude.hashWithSalt` buildBatchConfig
      `Prelude.hashWithSalt` cache
      `Prelude.hashWithSalt` concurrentBuildLimit
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` fileSystemLocations
      `Prelude.hashWithSalt` logsConfig
      `Prelude.hashWithSalt` queuedTimeoutInMinutes
      `Prelude.hashWithSalt` secondaryArtifacts
      `Prelude.hashWithSalt` secondarySourceVersions
      `Prelude.hashWithSalt` secondarySources
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeoutInMinutes
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf badgeEnabled
      `Prelude.seq` Prelude.rnf buildBatchConfig
      `Prelude.seq` Prelude.rnf cache
      `Prelude.seq` Prelude.rnf concurrentBuildLimit
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf fileSystemLocations
      `Prelude.seq` Prelude.rnf logsConfig
      `Prelude.seq` Prelude.rnf queuedTimeoutInMinutes
      `Prelude.seq` Prelude.rnf secondaryArtifacts
      `Prelude.seq` Prelude.rnf secondarySourceVersions
      `Prelude.seq` Prelude.rnf secondarySources
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeoutInMinutes
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.UpdateProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("artifacts" Data..=) Prelude.<$> artifacts,
            ("badgeEnabled" Data..=) Prelude.<$> badgeEnabled,
            ("buildBatchConfig" Data..=)
              Prelude.<$> buildBatchConfig,
            ("cache" Data..=) Prelude.<$> cache,
            ("concurrentBuildLimit" Data..=)
              Prelude.<$> concurrentBuildLimit,
            ("description" Data..=) Prelude.<$> description,
            ("encryptionKey" Data..=) Prelude.<$> encryptionKey,
            ("environment" Data..=) Prelude.<$> environment,
            ("fileSystemLocations" Data..=)
              Prelude.<$> fileSystemLocations,
            ("logsConfig" Data..=) Prelude.<$> logsConfig,
            ("queuedTimeoutInMinutes" Data..=)
              Prelude.<$> queuedTimeoutInMinutes,
            ("secondaryArtifacts" Data..=)
              Prelude.<$> secondaryArtifacts,
            ("secondarySourceVersions" Data..=)
              Prelude.<$> secondarySourceVersions,
            ("secondarySources" Data..=)
              Prelude.<$> secondarySources,
            ("serviceRole" Data..=) Prelude.<$> serviceRole,
            ("source" Data..=) Prelude.<$> source,
            ("sourceVersion" Data..=) Prelude.<$> sourceVersion,
            ("tags" Data..=) Prelude.<$> tags,
            ("timeoutInMinutes" Data..=)
              Prelude.<$> timeoutInMinutes,
            ("vpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath UpdateProject where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | Information about the build project that was changed.
    project :: Prelude.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'updateProjectResponse_project' - Information about the build project that was changed.
--
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse'
    { project = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the build project that was changed.
updateProjectResponse_project :: Lens.Lens' UpdateProjectResponse (Prelude.Maybe Project)
updateProjectResponse_project = Lens.lens (\UpdateProjectResponse' {project} -> project) (\s@UpdateProjectResponse' {} a -> s {project = a} :: UpdateProjectResponse)

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf project
      `Prelude.seq` Prelude.rnf httpStatus
