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
-- Module      : Network.AWS.CodeBuild.Types.BuildBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatch where

import Network.AWS.CodeBuild.Types.BuildArtifacts
import Network.AWS.CodeBuild.Types.BuildBatchPhase
import Network.AWS.CodeBuild.Types.BuildGroup
import Network.AWS.CodeBuild.Types.LogsConfig
import Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.CodeBuild.Types.VpcConfig
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about a batch build.
--
-- /See:/ 'newBuildBatch' smart constructor.
data BuildBatch = BuildBatch'
  { vpcConfig :: Core.Maybe VpcConfig,
    -- | The identifier of the resolved version of this batch build\'s source
    -- code.
    --
    -- -   For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the
    --     commit ID.
    --
    -- -   For AWS CodePipeline, the source revision provided by AWS
    --     CodePipeline.
    --
    -- -   For Amazon S3, this does not apply.
    resolvedSourceVersion :: Core.Maybe Core.Text,
    -- | An array of @BuildArtifacts@ objects the define the build artifacts for
    -- this batch build.
    secondaryArtifacts :: Core.Maybe [BuildArtifacts],
    -- | The identifier of the version of the source code to be built.
    sourceVersion :: Core.Maybe Core.Text,
    -- | An array of @BuildBatchPhase@ objects the specify the phases of the
    -- batch build.
    phases :: Core.Maybe [BuildBatchPhase],
    cache :: Core.Maybe ProjectCache,
    -- | The name of a service role used for builds in the batch.
    serviceRole :: Core.Maybe Core.Text,
    -- | The number of the batch build. For each project, the @buildBatchNumber@
    -- of its first batch build is @1@. The @buildBatchNumber@ of each
    -- subsequent batch build is incremented by @1@. If a batch build is
    -- deleted, the @buildBatchNumber@ of other batch builds does not change.
    buildBatchNumber :: Core.Maybe Core.Integer,
    -- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
    -- must be one of:
    --
    -- -   For AWS CodeCommit: the commit ID, branch, or Git tag to use.
    --
    -- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
    --     that corresponds to the version of the source code you want to
    --     build. If a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
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
    secondarySourceVersions :: Core.Maybe [ProjectSourceVersion],
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
    -- used for encrypting the batch build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Core.Maybe Core.Text,
    -- | A @BuildArtifacts@ object the defines the build artifacts for this batch
    -- build.
    artifacts :: Core.Maybe BuildArtifacts,
    -- | The date and time that the batch build started.
    startTime :: Core.Maybe Core.POSIX,
    -- | The identifier of the batch build.
    id :: Core.Maybe Core.Text,
    environment :: Core.Maybe ProjectEnvironment,
    source :: Core.Maybe ProjectSource,
    -- | The ARN of the batch build.
    arn :: Core.Maybe Core.Text,
    -- | The name of the batch build project.
    projectName :: Core.Maybe Core.Text,
    -- | The date and time that the batch build ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | An array of @BuildGroup@ objects that define the build groups for the
    -- batch build.
    buildGroups :: Core.Maybe [BuildGroup],
    -- | Specifies the maximum amount of time, in minutes, that the build in a
    -- batch must be completed in.
    buildTimeoutInMinutes :: Core.Maybe Core.Int,
    -- | Specifies the amount of time, in minutes, that the batch build is
    -- allowed to be queued before it times out.
    queuedTimeoutInMinutes :: Core.Maybe Core.Int,
    -- | An array of @ProjectSource@ objects that define the sources for the
    -- batch build.
    secondarySources :: Core.Maybe [ProjectSource],
    -- | Indicates if the batch build is complete.
    complete :: Core.Maybe Core.Bool,
    logConfig :: Core.Maybe LogsConfig,
    -- | The current phase of the batch build.
    currentPhase :: Core.Maybe Core.Text,
    -- | The status of the batch build.
    buildBatchStatus :: Core.Maybe StatusType,
    -- | The entity that started the batch build. Valid values include:
    --
    -- -   If AWS CodePipeline started the build, the pipeline\'s name (for
    --     example, @codepipeline\/my-demo-pipeline@).
    --
    -- -   If an AWS Identity and Access Management (IAM) user started the
    --     build, the user\'s name.
    --
    -- -   If the Jenkins plugin for AWS CodeBuild started the build, the
    --     string @CodeBuild-Jenkins-Plugin@.
    initiator :: Core.Maybe Core.Text,
    buildBatchConfig :: Core.Maybe ProjectBuildBatchConfig,
    -- | An array of @ProjectFileSystemLocation@ objects for the batch build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Core.Maybe [ProjectFileSystemLocation],
    -- | Specifies if session debugging is enabled for this batch build. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
    -- Batch session debugging is not supported for matrix batch builds.
    debugSessionEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuildBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'buildBatch_vpcConfig' - Undocumented member.
--
-- 'resolvedSourceVersion', 'buildBatch_resolvedSourceVersion' - The identifier of the resolved version of this batch build\'s source
-- code.
--
-- -   For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the
--     commit ID.
--
-- -   For AWS CodePipeline, the source revision provided by AWS
--     CodePipeline.
--
-- -   For Amazon S3, this does not apply.
--
-- 'secondaryArtifacts', 'buildBatch_secondaryArtifacts' - An array of @BuildArtifacts@ objects the define the build artifacts for
-- this batch build.
--
-- 'sourceVersion', 'buildBatch_sourceVersion' - The identifier of the version of the source code to be built.
--
-- 'phases', 'buildBatch_phases' - An array of @BuildBatchPhase@ objects the specify the phases of the
-- batch build.
--
-- 'cache', 'buildBatch_cache' - Undocumented member.
--
-- 'serviceRole', 'buildBatch_serviceRole' - The name of a service role used for builds in the batch.
--
-- 'buildBatchNumber', 'buildBatch_buildBatchNumber' - The number of the batch build. For each project, the @buildBatchNumber@
-- of its first batch build is @1@. The @buildBatchNumber@ of each
-- subsequent batch build is incremented by @1@. If a batch build is
-- deleted, the @buildBatchNumber@ of other batch builds does not change.
--
-- 'secondarySourceVersions', 'buildBatch_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
-- must be one of:
--
-- -   For AWS CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
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
-- 'encryptionKey', 'buildBatch_encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
-- used for encrypting the batch build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'artifacts', 'buildBatch_artifacts' - A @BuildArtifacts@ object the defines the build artifacts for this batch
-- build.
--
-- 'startTime', 'buildBatch_startTime' - The date and time that the batch build started.
--
-- 'id', 'buildBatch_id' - The identifier of the batch build.
--
-- 'environment', 'buildBatch_environment' - Undocumented member.
--
-- 'source', 'buildBatch_source' - Undocumented member.
--
-- 'arn', 'buildBatch_arn' - The ARN of the batch build.
--
-- 'projectName', 'buildBatch_projectName' - The name of the batch build project.
--
-- 'endTime', 'buildBatch_endTime' - The date and time that the batch build ended.
--
-- 'buildGroups', 'buildBatch_buildGroups' - An array of @BuildGroup@ objects that define the build groups for the
-- batch build.
--
-- 'buildTimeoutInMinutes', 'buildBatch_buildTimeoutInMinutes' - Specifies the maximum amount of time, in minutes, that the build in a
-- batch must be completed in.
--
-- 'queuedTimeoutInMinutes', 'buildBatch_queuedTimeoutInMinutes' - Specifies the amount of time, in minutes, that the batch build is
-- allowed to be queued before it times out.
--
-- 'secondarySources', 'buildBatch_secondarySources' - An array of @ProjectSource@ objects that define the sources for the
-- batch build.
--
-- 'complete', 'buildBatch_complete' - Indicates if the batch build is complete.
--
-- 'logConfig', 'buildBatch_logConfig' - Undocumented member.
--
-- 'currentPhase', 'buildBatch_currentPhase' - The current phase of the batch build.
--
-- 'buildBatchStatus', 'buildBatch_buildBatchStatus' - The status of the batch build.
--
-- 'initiator', 'buildBatch_initiator' - The entity that started the batch build. Valid values include:
--
-- -   If AWS CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an AWS Identity and Access Management (IAM) user started the
--     build, the user\'s name.
--
-- -   If the Jenkins plugin for AWS CodeBuild started the build, the
--     string @CodeBuild-Jenkins-Plugin@.
--
-- 'buildBatchConfig', 'buildBatch_buildBatchConfig' - Undocumented member.
--
-- 'fileSystemLocations', 'buildBatch_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for the batch build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'debugSessionEnabled', 'buildBatch_debugSessionEnabled' - Specifies if session debugging is enabled for this batch build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
-- Batch session debugging is not supported for matrix batch builds.
newBuildBatch ::
  BuildBatch
newBuildBatch =
  BuildBatch'
    { vpcConfig = Core.Nothing,
      resolvedSourceVersion = Core.Nothing,
      secondaryArtifacts = Core.Nothing,
      sourceVersion = Core.Nothing,
      phases = Core.Nothing,
      cache = Core.Nothing,
      serviceRole = Core.Nothing,
      buildBatchNumber = Core.Nothing,
      secondarySourceVersions = Core.Nothing,
      encryptionKey = Core.Nothing,
      artifacts = Core.Nothing,
      startTime = Core.Nothing,
      id = Core.Nothing,
      environment = Core.Nothing,
      source = Core.Nothing,
      arn = Core.Nothing,
      projectName = Core.Nothing,
      endTime = Core.Nothing,
      buildGroups = Core.Nothing,
      buildTimeoutInMinutes = Core.Nothing,
      queuedTimeoutInMinutes = Core.Nothing,
      secondarySources = Core.Nothing,
      complete = Core.Nothing,
      logConfig = Core.Nothing,
      currentPhase = Core.Nothing,
      buildBatchStatus = Core.Nothing,
      initiator = Core.Nothing,
      buildBatchConfig = Core.Nothing,
      fileSystemLocations = Core.Nothing,
      debugSessionEnabled = Core.Nothing
    }

-- | Undocumented member.
buildBatch_vpcConfig :: Lens.Lens' BuildBatch (Core.Maybe VpcConfig)
buildBatch_vpcConfig = Lens.lens (\BuildBatch' {vpcConfig} -> vpcConfig) (\s@BuildBatch' {} a -> s {vpcConfig = a} :: BuildBatch)

-- | The identifier of the resolved version of this batch build\'s source
-- code.
--
-- -   For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the
--     commit ID.
--
-- -   For AWS CodePipeline, the source revision provided by AWS
--     CodePipeline.
--
-- -   For Amazon S3, this does not apply.
buildBatch_resolvedSourceVersion :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_resolvedSourceVersion = Lens.lens (\BuildBatch' {resolvedSourceVersion} -> resolvedSourceVersion) (\s@BuildBatch' {} a -> s {resolvedSourceVersion = a} :: BuildBatch)

-- | An array of @BuildArtifacts@ objects the define the build artifacts for
-- this batch build.
buildBatch_secondaryArtifacts :: Lens.Lens' BuildBatch (Core.Maybe [BuildArtifacts])
buildBatch_secondaryArtifacts = Lens.lens (\BuildBatch' {secondaryArtifacts} -> secondaryArtifacts) (\s@BuildBatch' {} a -> s {secondaryArtifacts = a} :: BuildBatch) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the version of the source code to be built.
buildBatch_sourceVersion :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_sourceVersion = Lens.lens (\BuildBatch' {sourceVersion} -> sourceVersion) (\s@BuildBatch' {} a -> s {sourceVersion = a} :: BuildBatch)

-- | An array of @BuildBatchPhase@ objects the specify the phases of the
-- batch build.
buildBatch_phases :: Lens.Lens' BuildBatch (Core.Maybe [BuildBatchPhase])
buildBatch_phases = Lens.lens (\BuildBatch' {phases} -> phases) (\s@BuildBatch' {} a -> s {phases = a} :: BuildBatch) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
buildBatch_cache :: Lens.Lens' BuildBatch (Core.Maybe ProjectCache)
buildBatch_cache = Lens.lens (\BuildBatch' {cache} -> cache) (\s@BuildBatch' {} a -> s {cache = a} :: BuildBatch)

-- | The name of a service role used for builds in the batch.
buildBatch_serviceRole :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_serviceRole = Lens.lens (\BuildBatch' {serviceRole} -> serviceRole) (\s@BuildBatch' {} a -> s {serviceRole = a} :: BuildBatch)

-- | The number of the batch build. For each project, the @buildBatchNumber@
-- of its first batch build is @1@. The @buildBatchNumber@ of each
-- subsequent batch build is incremented by @1@. If a batch build is
-- deleted, the @buildBatchNumber@ of other batch builds does not change.
buildBatch_buildBatchNumber :: Lens.Lens' BuildBatch (Core.Maybe Core.Integer)
buildBatch_buildBatchNumber = Lens.lens (\BuildBatch' {buildBatchNumber} -> buildBatchNumber) (\s@BuildBatch' {} a -> s {buildBatchNumber = a} :: BuildBatch)

-- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
-- must be one of:
--
-- -   For AWS CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
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
buildBatch_secondarySourceVersions :: Lens.Lens' BuildBatch (Core.Maybe [ProjectSourceVersion])
buildBatch_secondarySourceVersions = Lens.lens (\BuildBatch' {secondarySourceVersions} -> secondarySourceVersions) (\s@BuildBatch' {} a -> s {secondarySourceVersions = a} :: BuildBatch) Core.. Lens.mapping Lens._Coerce

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
-- used for encrypting the batch build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
buildBatch_encryptionKey :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_encryptionKey = Lens.lens (\BuildBatch' {encryptionKey} -> encryptionKey) (\s@BuildBatch' {} a -> s {encryptionKey = a} :: BuildBatch)

-- | A @BuildArtifacts@ object the defines the build artifacts for this batch
-- build.
buildBatch_artifacts :: Lens.Lens' BuildBatch (Core.Maybe BuildArtifacts)
buildBatch_artifacts = Lens.lens (\BuildBatch' {artifacts} -> artifacts) (\s@BuildBatch' {} a -> s {artifacts = a} :: BuildBatch)

-- | The date and time that the batch build started.
buildBatch_startTime :: Lens.Lens' BuildBatch (Core.Maybe Core.UTCTime)
buildBatch_startTime = Lens.lens (\BuildBatch' {startTime} -> startTime) (\s@BuildBatch' {} a -> s {startTime = a} :: BuildBatch) Core.. Lens.mapping Core._Time

-- | The identifier of the batch build.
buildBatch_id :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_id = Lens.lens (\BuildBatch' {id} -> id) (\s@BuildBatch' {} a -> s {id = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_environment :: Lens.Lens' BuildBatch (Core.Maybe ProjectEnvironment)
buildBatch_environment = Lens.lens (\BuildBatch' {environment} -> environment) (\s@BuildBatch' {} a -> s {environment = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_source :: Lens.Lens' BuildBatch (Core.Maybe ProjectSource)
buildBatch_source = Lens.lens (\BuildBatch' {source} -> source) (\s@BuildBatch' {} a -> s {source = a} :: BuildBatch)

-- | The ARN of the batch build.
buildBatch_arn :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_arn = Lens.lens (\BuildBatch' {arn} -> arn) (\s@BuildBatch' {} a -> s {arn = a} :: BuildBatch)

-- | The name of the batch build project.
buildBatch_projectName :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_projectName = Lens.lens (\BuildBatch' {projectName} -> projectName) (\s@BuildBatch' {} a -> s {projectName = a} :: BuildBatch)

-- | The date and time that the batch build ended.
buildBatch_endTime :: Lens.Lens' BuildBatch (Core.Maybe Core.UTCTime)
buildBatch_endTime = Lens.lens (\BuildBatch' {endTime} -> endTime) (\s@BuildBatch' {} a -> s {endTime = a} :: BuildBatch) Core.. Lens.mapping Core._Time

-- | An array of @BuildGroup@ objects that define the build groups for the
-- batch build.
buildBatch_buildGroups :: Lens.Lens' BuildBatch (Core.Maybe [BuildGroup])
buildBatch_buildGroups = Lens.lens (\BuildBatch' {buildGroups} -> buildGroups) (\s@BuildBatch' {} a -> s {buildGroups = a} :: BuildBatch) Core.. Lens.mapping Lens._Coerce

-- | Specifies the maximum amount of time, in minutes, that the build in a
-- batch must be completed in.
buildBatch_buildTimeoutInMinutes :: Lens.Lens' BuildBatch (Core.Maybe Core.Int)
buildBatch_buildTimeoutInMinutes = Lens.lens (\BuildBatch' {buildTimeoutInMinutes} -> buildTimeoutInMinutes) (\s@BuildBatch' {} a -> s {buildTimeoutInMinutes = a} :: BuildBatch)

-- | Specifies the amount of time, in minutes, that the batch build is
-- allowed to be queued before it times out.
buildBatch_queuedTimeoutInMinutes :: Lens.Lens' BuildBatch (Core.Maybe Core.Int)
buildBatch_queuedTimeoutInMinutes = Lens.lens (\BuildBatch' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@BuildBatch' {} a -> s {queuedTimeoutInMinutes = a} :: BuildBatch)

-- | An array of @ProjectSource@ objects that define the sources for the
-- batch build.
buildBatch_secondarySources :: Lens.Lens' BuildBatch (Core.Maybe [ProjectSource])
buildBatch_secondarySources = Lens.lens (\BuildBatch' {secondarySources} -> secondarySources) (\s@BuildBatch' {} a -> s {secondarySources = a} :: BuildBatch) Core.. Lens.mapping Lens._Coerce

-- | Indicates if the batch build is complete.
buildBatch_complete :: Lens.Lens' BuildBatch (Core.Maybe Core.Bool)
buildBatch_complete = Lens.lens (\BuildBatch' {complete} -> complete) (\s@BuildBatch' {} a -> s {complete = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_logConfig :: Lens.Lens' BuildBatch (Core.Maybe LogsConfig)
buildBatch_logConfig = Lens.lens (\BuildBatch' {logConfig} -> logConfig) (\s@BuildBatch' {} a -> s {logConfig = a} :: BuildBatch)

-- | The current phase of the batch build.
buildBatch_currentPhase :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_currentPhase = Lens.lens (\BuildBatch' {currentPhase} -> currentPhase) (\s@BuildBatch' {} a -> s {currentPhase = a} :: BuildBatch)

-- | The status of the batch build.
buildBatch_buildBatchStatus :: Lens.Lens' BuildBatch (Core.Maybe StatusType)
buildBatch_buildBatchStatus = Lens.lens (\BuildBatch' {buildBatchStatus} -> buildBatchStatus) (\s@BuildBatch' {} a -> s {buildBatchStatus = a} :: BuildBatch)

-- | The entity that started the batch build. Valid values include:
--
-- -   If AWS CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an AWS Identity and Access Management (IAM) user started the
--     build, the user\'s name.
--
-- -   If the Jenkins plugin for AWS CodeBuild started the build, the
--     string @CodeBuild-Jenkins-Plugin@.
buildBatch_initiator :: Lens.Lens' BuildBatch (Core.Maybe Core.Text)
buildBatch_initiator = Lens.lens (\BuildBatch' {initiator} -> initiator) (\s@BuildBatch' {} a -> s {initiator = a} :: BuildBatch)

-- | Undocumented member.
buildBatch_buildBatchConfig :: Lens.Lens' BuildBatch (Core.Maybe ProjectBuildBatchConfig)
buildBatch_buildBatchConfig = Lens.lens (\BuildBatch' {buildBatchConfig} -> buildBatchConfig) (\s@BuildBatch' {} a -> s {buildBatchConfig = a} :: BuildBatch)

-- | An array of @ProjectFileSystemLocation@ objects for the batch build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
buildBatch_fileSystemLocations :: Lens.Lens' BuildBatch (Core.Maybe [ProjectFileSystemLocation])
buildBatch_fileSystemLocations = Lens.lens (\BuildBatch' {fileSystemLocations} -> fileSystemLocations) (\s@BuildBatch' {} a -> s {fileSystemLocations = a} :: BuildBatch) Core.. Lens.mapping Lens._Coerce

-- | Specifies if session debugging is enabled for this batch build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
-- Batch session debugging is not supported for matrix batch builds.
buildBatch_debugSessionEnabled :: Lens.Lens' BuildBatch (Core.Maybe Core.Bool)
buildBatch_debugSessionEnabled = Lens.lens (\BuildBatch' {debugSessionEnabled} -> debugSessionEnabled) (\s@BuildBatch' {} a -> s {debugSessionEnabled = a} :: BuildBatch)

instance Core.FromJSON BuildBatch where
  parseJSON =
    Core.withObject
      "BuildBatch"
      ( \x ->
          BuildBatch'
            Core.<$> (x Core..:? "vpcConfig")
            Core.<*> (x Core..:? "resolvedSourceVersion")
            Core.<*> ( x Core..:? "secondaryArtifacts"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "sourceVersion")
            Core.<*> (x Core..:? "phases" Core..!= Core.mempty)
            Core.<*> (x Core..:? "cache")
            Core.<*> (x Core..:? "serviceRole")
            Core.<*> (x Core..:? "buildBatchNumber")
            Core.<*> ( x Core..:? "secondarySourceVersions"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "encryptionKey")
            Core.<*> (x Core..:? "artifacts")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "environment")
            Core.<*> (x Core..:? "source")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "projectName")
            Core.<*> (x Core..:? "endTime")
            Core.<*> (x Core..:? "buildGroups" Core..!= Core.mempty)
            Core.<*> (x Core..:? "buildTimeoutInMinutes")
            Core.<*> (x Core..:? "queuedTimeoutInMinutes")
            Core.<*> (x Core..:? "secondarySources" Core..!= Core.mempty)
            Core.<*> (x Core..:? "complete")
            Core.<*> (x Core..:? "logConfig")
            Core.<*> (x Core..:? "currentPhase")
            Core.<*> (x Core..:? "buildBatchStatus")
            Core.<*> (x Core..:? "initiator")
            Core.<*> (x Core..:? "buildBatchConfig")
            Core.<*> ( x Core..:? "fileSystemLocations"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "debugSessionEnabled")
      )

instance Core.Hashable BuildBatch

instance Core.NFData BuildBatch
