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
-- Module      : Amazonka.CodeBuild.CreateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a build project.
module Amazonka.CodeBuild.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_badgeEnabled,
    createProject_buildBatchConfig,
    createProject_cache,
    createProject_concurrentBuildLimit,
    createProject_description,
    createProject_encryptionKey,
    createProject_fileSystemLocations,
    createProject_logsConfig,
    createProject_queuedTimeoutInMinutes,
    createProject_secondaryArtifacts,
    createProject_secondarySourceVersions,
    createProject_secondarySources,
    createProject_sourceVersion,
    createProject_tags,
    createProject_timeoutInMinutes,
    createProject_vpcConfig,
    createProject_name,
    createProject_source,
    createProject_artifacts,
    createProject_environment,
    createProject_serviceRole,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_project,
    createProjectResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | Set this to true to generate a publicly accessible URL for your
    -- project\'s build badge.
    badgeEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A ProjectBuildBatchConfig object that defines the batch build options
    -- for the project.
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
    concurrentBuildLimit :: Prelude.Maybe Prelude.Int,
    -- | A description that makes the build project easy to identify.
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
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    -- | Information about logs for the build project. These can be logs in
    -- CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
    logsConfig :: Prelude.Maybe LogsConfig,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Prelude.Maybe [ProjectArtifacts],
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
    -- is specified at the build level, then they take precedence over these
    -- @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Prelude.Maybe [ProjectSourceVersion],
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Prelude.Maybe [ProjectSource],
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
    -- | A list of tag key and value pairs associated with this build project.
    --
    -- These tags are available for use by Amazon Web Services services that
    -- support CodeBuild build project tags.
    tags :: Prelude.Maybe [Tag],
    -- | How long, in minutes, from 5 to 480 (8 hours), for CodeBuild to wait
    -- before it times out any build that has not been marked as completed. The
    -- default is 60 minutes.
    timeoutInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | VpcConfig enables CodeBuild to access resources in an Amazon VPC.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The name of the build project.
    name :: Prelude.Text,
    -- | Information about the build input source code for the build project.
    source :: ProjectSource,
    -- | Information about the build output artifacts for the build project.
    artifacts :: ProjectArtifacts,
    -- | Information about the build environment for the build project.
    environment :: ProjectEnvironment,
    -- | The ARN of the IAM role that enables CodeBuild to interact with
    -- dependent Amazon Web Services services on behalf of the Amazon Web
    -- Services account.
    serviceRole :: Prelude.Text
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
-- 'badgeEnabled', 'createProject_badgeEnabled' - Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
--
-- 'buildBatchConfig', 'createProject_buildBatchConfig' - A ProjectBuildBatchConfig object that defines the batch build options
-- for the project.
--
-- 'cache', 'createProject_cache' - Stores recently used information so that it can be quickly accessed at a
-- later time.
--
-- 'concurrentBuildLimit', 'createProject_concurrentBuildLimit' - The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
--
-- 'description', 'createProject_description' - A description that makes the build project easy to identify.
--
-- 'encryptionKey', 'createProject_encryptionKey' - The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'fileSystemLocations', 'createProject_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'logsConfig', 'createProject_logsConfig' - Information about logs for the build project. These can be logs in
-- CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
--
-- 'queuedTimeoutInMinutes', 'createProject_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'secondaryArtifacts', 'createProject_secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
--
-- 'secondarySourceVersions', 'createProject_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take precedence over these
-- @secondarySourceVersions@ (at the project level).
--
-- 'secondarySources', 'createProject_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'sourceVersion', 'createProject_sourceVersion' - A version of the build input to be built for this project. If not
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
-- 'tags', 'createProject_tags' - A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
--
-- 'timeoutInMinutes', 'createProject_timeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for CodeBuild to wait
-- before it times out any build that has not been marked as completed. The
-- default is 60 minutes.
--
-- 'vpcConfig', 'createProject_vpcConfig' - VpcConfig enables CodeBuild to access resources in an Amazon VPC.
--
-- 'name', 'createProject_name' - The name of the build project.
--
-- 'source', 'createProject_source' - Information about the build input source code for the build project.
--
-- 'artifacts', 'createProject_artifacts' - Information about the build output artifacts for the build project.
--
-- 'environment', 'createProject_environment' - Information about the build environment for the build project.
--
-- 'serviceRole', 'createProject_serviceRole' - The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
newCreateProject ::
  -- | 'name'
  Prelude.Text ->
  -- | 'source'
  ProjectSource ->
  -- | 'artifacts'
  ProjectArtifacts ->
  -- | 'environment'
  ProjectEnvironment ->
  -- | 'serviceRole'
  Prelude.Text ->
  CreateProject
newCreateProject
  pName_
  pSource_
  pArtifacts_
  pEnvironment_
  pServiceRole_ =
    CreateProject'
      { badgeEnabled = Prelude.Nothing,
        buildBatchConfig = Prelude.Nothing,
        cache = Prelude.Nothing,
        concurrentBuildLimit = Prelude.Nothing,
        description = Prelude.Nothing,
        encryptionKey = Prelude.Nothing,
        fileSystemLocations = Prelude.Nothing,
        logsConfig = Prelude.Nothing,
        queuedTimeoutInMinutes = Prelude.Nothing,
        secondaryArtifacts = Prelude.Nothing,
        secondarySourceVersions = Prelude.Nothing,
        secondarySources = Prelude.Nothing,
        sourceVersion = Prelude.Nothing,
        tags = Prelude.Nothing,
        timeoutInMinutes = Prelude.Nothing,
        vpcConfig = Prelude.Nothing,
        name = pName_,
        source = pSource_,
        artifacts = pArtifacts_,
        environment = pEnvironment_,
        serviceRole = pServiceRole_
      }

-- | Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
createProject_badgeEnabled :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Bool)
createProject_badgeEnabled = Lens.lens (\CreateProject' {badgeEnabled} -> badgeEnabled) (\s@CreateProject' {} a -> s {badgeEnabled = a} :: CreateProject)

-- | A ProjectBuildBatchConfig object that defines the batch build options
-- for the project.
createProject_buildBatchConfig :: Lens.Lens' CreateProject (Prelude.Maybe ProjectBuildBatchConfig)
createProject_buildBatchConfig = Lens.lens (\CreateProject' {buildBatchConfig} -> buildBatchConfig) (\s@CreateProject' {} a -> s {buildBatchConfig = a} :: CreateProject)

-- | Stores recently used information so that it can be quickly accessed at a
-- later time.
createProject_cache :: Lens.Lens' CreateProject (Prelude.Maybe ProjectCache)
createProject_cache = Lens.lens (\CreateProject' {cache} -> cache) (\s@CreateProject' {} a -> s {cache = a} :: CreateProject)

-- | The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
createProject_concurrentBuildLimit :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Int)
createProject_concurrentBuildLimit = Lens.lens (\CreateProject' {concurrentBuildLimit} -> concurrentBuildLimit) (\s@CreateProject' {} a -> s {concurrentBuildLimit = a} :: CreateProject)

-- | A description that makes the build project easy to identify.
createProject_description :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_description = Lens.lens (\CreateProject' {description} -> description) (\s@CreateProject' {} a -> s {description = a} :: CreateProject)

-- | The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
createProject_encryptionKey :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_encryptionKey = Lens.lens (\CreateProject' {encryptionKey} -> encryptionKey) (\s@CreateProject' {} a -> s {encryptionKey = a} :: CreateProject)

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
createProject_fileSystemLocations :: Lens.Lens' CreateProject (Prelude.Maybe [ProjectFileSystemLocation])
createProject_fileSystemLocations = Lens.lens (\CreateProject' {fileSystemLocations} -> fileSystemLocations) (\s@CreateProject' {} a -> s {fileSystemLocations = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

-- | Information about logs for the build project. These can be logs in
-- CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
createProject_logsConfig :: Lens.Lens' CreateProject (Prelude.Maybe LogsConfig)
createProject_logsConfig = Lens.lens (\CreateProject' {logsConfig} -> logsConfig) (\s@CreateProject' {} a -> s {logsConfig = a} :: CreateProject)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
createProject_queuedTimeoutInMinutes :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Natural)
createProject_queuedTimeoutInMinutes = Lens.lens (\CreateProject' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@CreateProject' {} a -> s {queuedTimeoutInMinutes = a} :: CreateProject)

-- | An array of @ProjectArtifacts@ objects.
createProject_secondaryArtifacts :: Lens.Lens' CreateProject (Prelude.Maybe [ProjectArtifacts])
createProject_secondaryArtifacts = Lens.lens (\CreateProject' {secondaryArtifacts} -> secondaryArtifacts) (\s@CreateProject' {} a -> s {secondaryArtifacts = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take precedence over these
-- @secondarySourceVersions@ (at the project level).
createProject_secondarySourceVersions :: Lens.Lens' CreateProject (Prelude.Maybe [ProjectSourceVersion])
createProject_secondarySourceVersions = Lens.lens (\CreateProject' {secondarySourceVersions} -> secondarySourceVersions) (\s@CreateProject' {} a -> s {secondarySourceVersions = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectSource@ objects.
createProject_secondarySources :: Lens.Lens' CreateProject (Prelude.Maybe [ProjectSource])
createProject_secondarySources = Lens.lens (\CreateProject' {secondarySources} -> secondarySources) (\s@CreateProject' {} a -> s {secondarySources = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

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
createProject_sourceVersion :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_sourceVersion = Lens.lens (\CreateProject' {sourceVersion} -> sourceVersion) (\s@CreateProject' {} a -> s {sourceVersion = a} :: CreateProject)

-- | A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild build project tags.
createProject_tags :: Lens.Lens' CreateProject (Prelude.Maybe [Tag])
createProject_tags = Lens.lens (\CreateProject' {tags} -> tags) (\s@CreateProject' {} a -> s {tags = a} :: CreateProject) Prelude.. Lens.mapping Lens.coerced

-- | How long, in minutes, from 5 to 480 (8 hours), for CodeBuild to wait
-- before it times out any build that has not been marked as completed. The
-- default is 60 minutes.
createProject_timeoutInMinutes :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Natural)
createProject_timeoutInMinutes = Lens.lens (\CreateProject' {timeoutInMinutes} -> timeoutInMinutes) (\s@CreateProject' {} a -> s {timeoutInMinutes = a} :: CreateProject)

-- | VpcConfig enables CodeBuild to access resources in an Amazon VPC.
createProject_vpcConfig :: Lens.Lens' CreateProject (Prelude.Maybe VpcConfig)
createProject_vpcConfig = Lens.lens (\CreateProject' {vpcConfig} -> vpcConfig) (\s@CreateProject' {} a -> s {vpcConfig = a} :: CreateProject)

-- | The name of the build project.
createProject_name :: Lens.Lens' CreateProject Prelude.Text
createProject_name = Lens.lens (\CreateProject' {name} -> name) (\s@CreateProject' {} a -> s {name = a} :: CreateProject)

-- | Information about the build input source code for the build project.
createProject_source :: Lens.Lens' CreateProject ProjectSource
createProject_source = Lens.lens (\CreateProject' {source} -> source) (\s@CreateProject' {} a -> s {source = a} :: CreateProject)

-- | Information about the build output artifacts for the build project.
createProject_artifacts :: Lens.Lens' CreateProject ProjectArtifacts
createProject_artifacts = Lens.lens (\CreateProject' {artifacts} -> artifacts) (\s@CreateProject' {} a -> s {artifacts = a} :: CreateProject)

-- | Information about the build environment for the build project.
createProject_environment :: Lens.Lens' CreateProject ProjectEnvironment
createProject_environment = Lens.lens (\CreateProject' {environment} -> environment) (\s@CreateProject' {} a -> s {environment = a} :: CreateProject)

-- | The ARN of the IAM role that enables CodeBuild to interact with
-- dependent Amazon Web Services services on behalf of the Amazon Web
-- Services account.
createProject_serviceRole :: Lens.Lens' CreateProject Prelude.Text
createProject_serviceRole = Lens.lens (\CreateProject' {serviceRole} -> serviceRole) (\s@CreateProject' {} a -> s {serviceRole = a} :: CreateProject)

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
    _salt
      `Prelude.hashWithSalt` badgeEnabled
      `Prelude.hashWithSalt` buildBatchConfig
      `Prelude.hashWithSalt` cache
      `Prelude.hashWithSalt` concurrentBuildLimit
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` fileSystemLocations
      `Prelude.hashWithSalt` logsConfig
      `Prelude.hashWithSalt` queuedTimeoutInMinutes
      `Prelude.hashWithSalt` secondaryArtifacts
      `Prelude.hashWithSalt` secondarySourceVersions
      `Prelude.hashWithSalt` secondarySources
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeoutInMinutes
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` artifacts
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` serviceRole

instance Prelude.NFData CreateProject where
  rnf CreateProject' {..} =
    Prelude.rnf badgeEnabled
      `Prelude.seq` Prelude.rnf buildBatchConfig
      `Prelude.seq` Prelude.rnf cache
      `Prelude.seq` Prelude.rnf concurrentBuildLimit
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf fileSystemLocations
      `Prelude.seq` Prelude.rnf logsConfig
      `Prelude.seq` Prelude.rnf queuedTimeoutInMinutes
      `Prelude.seq` Prelude.rnf secondaryArtifacts
      `Prelude.seq` Prelude.rnf secondarySourceVersions
      `Prelude.seq` Prelude.rnf secondarySources
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeoutInMinutes
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf serviceRole

instance Data.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.CreateProject" ::
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
          [ ("badgeEnabled" Data..=) Prelude.<$> badgeEnabled,
            ("buildBatchConfig" Data..=)
              Prelude.<$> buildBatchConfig,
            ("cache" Data..=) Prelude.<$> cache,
            ("concurrentBuildLimit" Data..=)
              Prelude.<$> concurrentBuildLimit,
            ("description" Data..=) Prelude.<$> description,
            ("encryptionKey" Data..=) Prelude.<$> encryptionKey,
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
            ("sourceVersion" Data..=) Prelude.<$> sourceVersion,
            ("tags" Data..=) Prelude.<$> tags,
            ("timeoutInMinutes" Data..=)
              Prelude.<$> timeoutInMinutes,
            ("vpcConfig" Data..=) Prelude.<$> vpcConfig,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("source" Data..= source),
            Prelude.Just ("artifacts" Data..= artifacts),
            Prelude.Just ("environment" Data..= environment),
            Prelude.Just ("serviceRole" Data..= serviceRole)
          ]
      )

instance Data.ToPath CreateProject where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Information about the build project that was created.
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
-- 'project', 'createProjectResponse_project' - Information about the build project that was created.
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

-- | Information about the build project that was created.
createProjectResponse_project :: Lens.Lens' CreateProjectResponse (Prelude.Maybe Project)
createProjectResponse_project = Lens.lens (\CreateProjectResponse' {project} -> project) (\s@CreateProjectResponse' {} a -> s {project = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse where
  rnf CreateProjectResponse' {..} =
    Prelude.rnf project
      `Prelude.seq` Prelude.rnf httpStatus
