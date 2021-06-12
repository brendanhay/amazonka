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
-- Module      : Network.AWS.CodeBuild.CreateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a build project.
module Network.AWS.CodeBuild.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_vpcConfig,
    createProject_secondaryArtifacts,
    createProject_sourceVersion,
    createProject_cache,
    createProject_secondarySourceVersions,
    createProject_encryptionKey,
    createProject_badgeEnabled,
    createProject_concurrentBuildLimit,
    createProject_logsConfig,
    createProject_queuedTimeoutInMinutes,
    createProject_secondarySources,
    createProject_tags,
    createProject_timeoutInMinutes,
    createProject_description,
    createProject_buildBatchConfig,
    createProject_fileSystemLocations,
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

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
    vpcConfig :: Core.Maybe VpcConfig,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Core.Maybe [ProjectArtifacts],
    -- | A version of the build input to be built for this project. If not
    -- specified, the latest version is used. If specified, it must be one of:
    --
    -- -   For AWS CodeCommit: the commit ID, branch, or Git tag to use.
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
    -- in the /AWS CodeBuild User Guide/.
    sourceVersion :: Core.Maybe Core.Text,
    -- | Stores recently used information so that it can be quickly accessed at a
    -- later time.
    cache :: Core.Maybe ProjectCache,
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
    -- is specified at the build level, then they take precedence over these
    -- @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Core.Maybe [ProjectSourceVersion],
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
    -- used for encrypting the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Core.Maybe Core.Text,
    -- | Set this to true to generate a publicly accessible URL for your
    -- project\'s build badge.
    badgeEnabled :: Core.Maybe Core.Bool,
    -- | The maximum number of concurrent builds that are allowed for this
    -- project.
    --
    -- New builds are only started if the current number of builds is less than
    -- or equal to this limit. If the current build count meets this limit, new
    -- builds are throttled and are not run.
    concurrentBuildLimit :: Core.Maybe Core.Int,
    -- | Information about logs for the build project. These can be logs in
    -- Amazon CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
    logsConfig :: Core.Maybe LogsConfig,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Core.Maybe Core.Natural,
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Core.Maybe [ProjectSource],
    -- | A list of tag key and value pairs associated with this build project.
    --
    -- These tags are available for use by AWS services that support AWS
    -- CodeBuild build project tags.
    tags :: Core.Maybe [Tag],
    -- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait
    -- before it times out any build that has not been marked as completed. The
    -- default is 60 minutes.
    timeoutInMinutes :: Core.Maybe Core.Natural,
    -- | A description that makes the build project easy to identify.
    description :: Core.Maybe Core.Text,
    -- | A ProjectBuildBatchConfig object that defines the batch build options
    -- for the project.
    buildBatchConfig :: Core.Maybe ProjectBuildBatchConfig,
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Core.Maybe [ProjectFileSystemLocation],
    -- | The name of the build project.
    name :: Core.Text,
    -- | Information about the build input source code for the build project.
    source :: ProjectSource,
    -- | Information about the build output artifacts for the build project.
    artifacts :: ProjectArtifacts,
    -- | Information about the build environment for the build project.
    environment :: ProjectEnvironment,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that
    -- enables AWS CodeBuild to interact with dependent AWS services on behalf
    -- of the AWS account.
    serviceRole :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'createProject_vpcConfig' - VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- 'secondaryArtifacts', 'createProject_secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
--
-- 'sourceVersion', 'createProject_sourceVersion' - A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For AWS CodeCommit: the commit ID, branch, or Git tag to use.
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
-- in the /AWS CodeBuild User Guide/.
--
-- 'cache', 'createProject_cache' - Stores recently used information so that it can be quickly accessed at a
-- later time.
--
-- 'secondarySourceVersions', 'createProject_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take precedence over these
-- @secondarySourceVersions@ (at the project level).
--
-- 'encryptionKey', 'createProject_encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
-- used for encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'badgeEnabled', 'createProject_badgeEnabled' - Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
--
-- 'concurrentBuildLimit', 'createProject_concurrentBuildLimit' - The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
--
-- 'logsConfig', 'createProject_logsConfig' - Information about logs for the build project. These can be logs in
-- Amazon CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
--
-- 'queuedTimeoutInMinutes', 'createProject_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'secondarySources', 'createProject_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'tags', 'createProject_tags' - A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS
-- CodeBuild build project tags.
--
-- 'timeoutInMinutes', 'createProject_timeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait
-- before it times out any build that has not been marked as completed. The
-- default is 60 minutes.
--
-- 'description', 'createProject_description' - A description that makes the build project easy to identify.
--
-- 'buildBatchConfig', 'createProject_buildBatchConfig' - A ProjectBuildBatchConfig object that defines the batch build options
-- for the project.
--
-- 'fileSystemLocations', 'createProject_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'name', 'createProject_name' - The name of the build project.
--
-- 'source', 'createProject_source' - Information about the build input source code for the build project.
--
-- 'artifacts', 'createProject_artifacts' - Information about the build output artifacts for the build project.
--
-- 'environment', 'createProject_environment' - Information about the build environment for the build project.
--
-- 'serviceRole', 'createProject_serviceRole' - The ARN of the AWS Identity and Access Management (IAM) role that
-- enables AWS CodeBuild to interact with dependent AWS services on behalf
-- of the AWS account.
newCreateProject ::
  -- | 'name'
  Core.Text ->
  -- | 'source'
  ProjectSource ->
  -- | 'artifacts'
  ProjectArtifacts ->
  -- | 'environment'
  ProjectEnvironment ->
  -- | 'serviceRole'
  Core.Text ->
  CreateProject
newCreateProject
  pName_
  pSource_
  pArtifacts_
  pEnvironment_
  pServiceRole_ =
    CreateProject'
      { vpcConfig = Core.Nothing,
        secondaryArtifacts = Core.Nothing,
        sourceVersion = Core.Nothing,
        cache = Core.Nothing,
        secondarySourceVersions = Core.Nothing,
        encryptionKey = Core.Nothing,
        badgeEnabled = Core.Nothing,
        concurrentBuildLimit = Core.Nothing,
        logsConfig = Core.Nothing,
        queuedTimeoutInMinutes = Core.Nothing,
        secondarySources = Core.Nothing,
        tags = Core.Nothing,
        timeoutInMinutes = Core.Nothing,
        description = Core.Nothing,
        buildBatchConfig = Core.Nothing,
        fileSystemLocations = Core.Nothing,
        name = pName_,
        source = pSource_,
        artifacts = pArtifacts_,
        environment = pEnvironment_,
        serviceRole = pServiceRole_
      }

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
createProject_vpcConfig :: Lens.Lens' CreateProject (Core.Maybe VpcConfig)
createProject_vpcConfig = Lens.lens (\CreateProject' {vpcConfig} -> vpcConfig) (\s@CreateProject' {} a -> s {vpcConfig = a} :: CreateProject)

-- | An array of @ProjectArtifacts@ objects.
createProject_secondaryArtifacts :: Lens.Lens' CreateProject (Core.Maybe [ProjectArtifacts])
createProject_secondaryArtifacts = Lens.lens (\CreateProject' {secondaryArtifacts} -> secondaryArtifacts) (\s@CreateProject' {} a -> s {secondaryArtifacts = a} :: CreateProject) Core.. Lens.mapping Lens._Coerce

-- | A version of the build input to be built for this project. If not
-- specified, the latest version is used. If specified, it must be one of:
--
-- -   For AWS CodeCommit: the commit ID, branch, or Git tag to use.
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
-- in the /AWS CodeBuild User Guide/.
createProject_sourceVersion :: Lens.Lens' CreateProject (Core.Maybe Core.Text)
createProject_sourceVersion = Lens.lens (\CreateProject' {sourceVersion} -> sourceVersion) (\s@CreateProject' {} a -> s {sourceVersion = a} :: CreateProject)

-- | Stores recently used information so that it can be quickly accessed at a
-- later time.
createProject_cache :: Lens.Lens' CreateProject (Core.Maybe ProjectCache)
createProject_cache = Lens.lens (\CreateProject' {cache} -> cache) (\s@CreateProject' {} a -> s {cache = a} :: CreateProject)

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@
-- is specified at the build level, then they take precedence over these
-- @secondarySourceVersions@ (at the project level).
createProject_secondarySourceVersions :: Lens.Lens' CreateProject (Core.Maybe [ProjectSourceVersion])
createProject_secondarySourceVersions = Lens.lens (\CreateProject' {secondarySourceVersions} -> secondarySourceVersions) (\s@CreateProject' {} a -> s {secondarySourceVersions = a} :: CreateProject) Core.. Lens.mapping Lens._Coerce

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
-- used for encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
createProject_encryptionKey :: Lens.Lens' CreateProject (Core.Maybe Core.Text)
createProject_encryptionKey = Lens.lens (\CreateProject' {encryptionKey} -> encryptionKey) (\s@CreateProject' {} a -> s {encryptionKey = a} :: CreateProject)

-- | Set this to true to generate a publicly accessible URL for your
-- project\'s build badge.
createProject_badgeEnabled :: Lens.Lens' CreateProject (Core.Maybe Core.Bool)
createProject_badgeEnabled = Lens.lens (\CreateProject' {badgeEnabled} -> badgeEnabled) (\s@CreateProject' {} a -> s {badgeEnabled = a} :: CreateProject)

-- | The maximum number of concurrent builds that are allowed for this
-- project.
--
-- New builds are only started if the current number of builds is less than
-- or equal to this limit. If the current build count meets this limit, new
-- builds are throttled and are not run.
createProject_concurrentBuildLimit :: Lens.Lens' CreateProject (Core.Maybe Core.Int)
createProject_concurrentBuildLimit = Lens.lens (\CreateProject' {concurrentBuildLimit} -> concurrentBuildLimit) (\s@CreateProject' {} a -> s {concurrentBuildLimit = a} :: CreateProject)

-- | Information about logs for the build project. These can be logs in
-- Amazon CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
createProject_logsConfig :: Lens.Lens' CreateProject (Core.Maybe LogsConfig)
createProject_logsConfig = Lens.lens (\CreateProject' {logsConfig} -> logsConfig) (\s@CreateProject' {} a -> s {logsConfig = a} :: CreateProject)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
createProject_queuedTimeoutInMinutes :: Lens.Lens' CreateProject (Core.Maybe Core.Natural)
createProject_queuedTimeoutInMinutes = Lens.lens (\CreateProject' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@CreateProject' {} a -> s {queuedTimeoutInMinutes = a} :: CreateProject)

-- | An array of @ProjectSource@ objects.
createProject_secondarySources :: Lens.Lens' CreateProject (Core.Maybe [ProjectSource])
createProject_secondarySources = Lens.lens (\CreateProject' {secondarySources} -> secondarySources) (\s@CreateProject' {} a -> s {secondarySources = a} :: CreateProject) Core.. Lens.mapping Lens._Coerce

-- | A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS
-- CodeBuild build project tags.
createProject_tags :: Lens.Lens' CreateProject (Core.Maybe [Tag])
createProject_tags = Lens.lens (\CreateProject' {tags} -> tags) (\s@CreateProject' {} a -> s {tags = a} :: CreateProject) Core.. Lens.mapping Lens._Coerce

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait
-- before it times out any build that has not been marked as completed. The
-- default is 60 minutes.
createProject_timeoutInMinutes :: Lens.Lens' CreateProject (Core.Maybe Core.Natural)
createProject_timeoutInMinutes = Lens.lens (\CreateProject' {timeoutInMinutes} -> timeoutInMinutes) (\s@CreateProject' {} a -> s {timeoutInMinutes = a} :: CreateProject)

-- | A description that makes the build project easy to identify.
createProject_description :: Lens.Lens' CreateProject (Core.Maybe Core.Text)
createProject_description = Lens.lens (\CreateProject' {description} -> description) (\s@CreateProject' {} a -> s {description = a} :: CreateProject)

-- | A ProjectBuildBatchConfig object that defines the batch build options
-- for the project.
createProject_buildBatchConfig :: Lens.Lens' CreateProject (Core.Maybe ProjectBuildBatchConfig)
createProject_buildBatchConfig = Lens.lens (\CreateProject' {buildBatchConfig} -> buildBatchConfig) (\s@CreateProject' {} a -> s {buildBatchConfig = a} :: CreateProject)

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
createProject_fileSystemLocations :: Lens.Lens' CreateProject (Core.Maybe [ProjectFileSystemLocation])
createProject_fileSystemLocations = Lens.lens (\CreateProject' {fileSystemLocations} -> fileSystemLocations) (\s@CreateProject' {} a -> s {fileSystemLocations = a} :: CreateProject) Core.. Lens.mapping Lens._Coerce

-- | The name of the build project.
createProject_name :: Lens.Lens' CreateProject Core.Text
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

-- | The ARN of the AWS Identity and Access Management (IAM) role that
-- enables AWS CodeBuild to interact with dependent AWS services on behalf
-- of the AWS account.
createProject_serviceRole :: Lens.Lens' CreateProject Core.Text
createProject_serviceRole = Lens.lens (\CreateProject' {serviceRole} -> serviceRole) (\s@CreateProject' {} a -> s {serviceRole = a} :: CreateProject)

instance Core.AWSRequest CreateProject where
  type
    AWSResponse CreateProject =
      CreateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Core.<$> (x Core..?> "project")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProject

instance Core.NFData CreateProject

instance Core.ToHeaders CreateProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.CreateProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("vpcConfig" Core..=) Core.<$> vpcConfig,
            ("secondaryArtifacts" Core..=)
              Core.<$> secondaryArtifacts,
            ("sourceVersion" Core..=) Core.<$> sourceVersion,
            ("cache" Core..=) Core.<$> cache,
            ("secondarySourceVersions" Core..=)
              Core.<$> secondarySourceVersions,
            ("encryptionKey" Core..=) Core.<$> encryptionKey,
            ("badgeEnabled" Core..=) Core.<$> badgeEnabled,
            ("concurrentBuildLimit" Core..=)
              Core.<$> concurrentBuildLimit,
            ("logsConfig" Core..=) Core.<$> logsConfig,
            ("queuedTimeoutInMinutes" Core..=)
              Core.<$> queuedTimeoutInMinutes,
            ("secondarySources" Core..=)
              Core.<$> secondarySources,
            ("tags" Core..=) Core.<$> tags,
            ("timeoutInMinutes" Core..=)
              Core.<$> timeoutInMinutes,
            ("description" Core..=) Core.<$> description,
            ("buildBatchConfig" Core..=)
              Core.<$> buildBatchConfig,
            ("fileSystemLocations" Core..=)
              Core.<$> fileSystemLocations,
            Core.Just ("name" Core..= name),
            Core.Just ("source" Core..= source),
            Core.Just ("artifacts" Core..= artifacts),
            Core.Just ("environment" Core..= environment),
            Core.Just ("serviceRole" Core..= serviceRole)
          ]
      )

instance Core.ToPath CreateProject where
  toPath = Core.const "/"

instance Core.ToQuery CreateProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Information about the build project that was created.
    project :: Core.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { project = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the build project that was created.
createProjectResponse_project :: Lens.Lens' CreateProjectResponse (Core.Maybe Project)
createProjectResponse_project = Lens.lens (\CreateProjectResponse' {project} -> project) (\s@CreateProjectResponse' {} a -> s {project = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Core.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Core.NFData CreateProjectResponse
