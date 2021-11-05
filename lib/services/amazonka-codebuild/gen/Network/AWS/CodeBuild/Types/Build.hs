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
-- Module      : Amazonka.CodeBuild.Types.Build
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.Build where

import Amazonka.CodeBuild.Types.BuildArtifacts
import Amazonka.CodeBuild.Types.BuildPhase
import Amazonka.CodeBuild.Types.DebugSession
import Amazonka.CodeBuild.Types.ExportedEnvironmentVariable
import Amazonka.CodeBuild.Types.LogsLocation
import Amazonka.CodeBuild.Types.NetworkInterface
import Amazonka.CodeBuild.Types.ProjectCache
import Amazonka.CodeBuild.Types.ProjectEnvironment
import Amazonka.CodeBuild.Types.ProjectFileSystemLocation
import Amazonka.CodeBuild.Types.ProjectSource
import Amazonka.CodeBuild.Types.ProjectSourceVersion
import Amazonka.CodeBuild.Types.StatusType
import Amazonka.CodeBuild.Types.VpcConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a build.
--
-- /See:/ 'newBuild' smart constructor.
data Build = Build'
  { -- | Information about all previous build phases that are complete and
    -- information about any current build phase that is not yet complete.
    phases :: Prelude.Maybe [BuildPhase],
    -- | Whether the build is complete. True if complete; otherwise, false.
    buildComplete :: Prelude.Maybe Prelude.Bool,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Prelude.Maybe [BuildArtifacts],
    -- | The Amazon Resource Name (ARN) of the build.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A list of exported environment variables for this build.
    --
    -- Exported environment variables are used in conjunction with CodePipeline
    -- to export environment variables from the current build stage to
    -- subsequent stages in the pipeline. For more information, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/actions-variables.html Working with variables>
    -- in the /CodePipeline User Guide/.
    exportedEnvironmentVariables :: Prelude.Maybe [ExportedEnvironmentVariable],
    -- | The number of the build. For each project, the @buildNumber@ of its
    -- first build is @1@. The @buildNumber@ of each subsequent build is
    -- incremented by @1@. If a build is deleted, the @buildNumber@ of other
    -- builds does not change.
    buildNumber :: Prelude.Maybe Prelude.Integer,
    -- | When the build process started, expressed in Unix time format.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Information about the output artifacts for the build.
    artifacts :: Prelude.Maybe BuildArtifacts,
    -- | Information about the build environment for this build.
    environment :: Prelude.Maybe ProjectEnvironment,
    -- | The entity that started the build. Valid values include:
    --
    -- -   If CodePipeline started the build, the pipeline\'s name (for
    --     example, @codepipeline\/my-demo-pipeline@).
    --
    -- -   If an IAM user started the build, the user\'s name (for example,
    --     @MyUserName@).
    --
    -- -   If the Jenkins plugin for CodeBuild started the build, the string
    --     @CodeBuild-Jenkins-Plugin@.
    initiator :: Prelude.Maybe Prelude.Text,
    -- | Describes a network interface.
    networkInterface :: Prelude.Maybe NetworkInterface,
    -- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
    -- must be one of:
    --
    -- -   For CodeCommit: the commit ID, branch, or Git tag to use.
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
    secondarySourceVersions :: Prelude.Maybe [ProjectSourceVersion],
    -- | The current build phase.
    currentPhase :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | Information about the cache for the build.
    cache :: Prelude.Maybe ProjectCache,
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Prelude.Maybe [ProjectSource],
    -- | Contains information about the debug session for this build.
    debugSession :: Prelude.Maybe DebugSession,
    -- | Any version identifier for the version of the source code to be built.
    -- If @sourceVersion@ is specified at the project level, then this
    -- @sourceVersion@ (at the build level) takes precedence.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the batch build that this build is a member of, if
    -- applicable.
    buildBatchArn :: Prelude.Maybe Prelude.Text,
    -- | Information about the build\'s logs in CloudWatch Logs.
    logs :: Prelude.Maybe LogsLocation,
    -- | An identifier for the version of this build\'s source code.
    --
    -- -   For CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit
    --     ID.
    --
    -- -   For CodePipeline, the source revision provided by CodePipeline.
    --
    -- -   For Amazon S3, this does not apply.
    resolvedSourceVersion :: Prelude.Maybe Prelude.Text,
    -- | If your CodeBuild project accesses resources in an Amazon VPC, you
    -- provide this parameter that identifies the VPC ID and the list of
    -- security group IDs and subnet IDs. The security groups and subnets must
    -- belong to the same VPC. You must provide at least one security group and
    -- one subnet ID.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | When the build process ended, expressed in Unix time format.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the CodeBuild project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the build. Valid values include:
    --
    -- -   @FAILED@: The build failed.
    --
    -- -   @FAULT@: The build faulted.
    --
    -- -   @IN_PROGRESS@: The build is still in progress.
    --
    -- -   @STOPPED@: The build stopped.
    --
    -- -   @SUCCEEDED@: The build succeeded.
    --
    -- -   @TIMED_OUT@: The build timed out.
    buildStatus :: Prelude.Maybe StatusType,
    -- | Information about the source code to be built.
    source :: Prelude.Maybe ProjectSource,
    -- | The unique ID for the build.
    id :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    -- | An array of the ARNs associated with this build\'s reports.
    reportArns :: Prelude.Maybe [Prelude.Text],
    -- | The Key Management Service customer master key (CMK) to be used for
    -- encrypting the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The name of a service role used for this build.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | How long, in minutes, for CodeBuild to wait before timing out this build
    -- if it does not get marked as completed.
    timeoutInMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Build' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phases', 'build_phases' - Information about all previous build phases that are complete and
-- information about any current build phase that is not yet complete.
--
-- 'buildComplete', 'build_buildComplete' - Whether the build is complete. True if complete; otherwise, false.
--
-- 'secondaryArtifacts', 'build_secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
--
-- 'arn', 'build_arn' - The Amazon Resource Name (ARN) of the build.
--
-- 'exportedEnvironmentVariables', 'build_exportedEnvironmentVariables' - A list of exported environment variables for this build.
--
-- Exported environment variables are used in conjunction with CodePipeline
-- to export environment variables from the current build stage to
-- subsequent stages in the pipeline. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/actions-variables.html Working with variables>
-- in the /CodePipeline User Guide/.
--
-- 'buildNumber', 'build_buildNumber' - The number of the build. For each project, the @buildNumber@ of its
-- first build is @1@. The @buildNumber@ of each subsequent build is
-- incremented by @1@. If a build is deleted, the @buildNumber@ of other
-- builds does not change.
--
-- 'startTime', 'build_startTime' - When the build process started, expressed in Unix time format.
--
-- 'artifacts', 'build_artifacts' - Information about the output artifacts for the build.
--
-- 'environment', 'build_environment' - Information about the build environment for this build.
--
-- 'initiator', 'build_initiator' - The entity that started the build. Valid values include:
--
-- -   If CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an IAM user started the build, the user\'s name (for example,
--     @MyUserName@).
--
-- -   If the Jenkins plugin for CodeBuild started the build, the string
--     @CodeBuild-Jenkins-Plugin@.
--
-- 'networkInterface', 'build_networkInterface' - Describes a network interface.
--
-- 'secondarySourceVersions', 'build_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
-- must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
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
-- 'currentPhase', 'build_currentPhase' - The current build phase.
--
-- 'queuedTimeoutInMinutes', 'build_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'cache', 'build_cache' - Information about the cache for the build.
--
-- 'secondarySources', 'build_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'debugSession', 'build_debugSession' - Contains information about the debug session for this build.
--
-- 'sourceVersion', 'build_sourceVersion' - Any version identifier for the version of the source code to be built.
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
--
-- 'buildBatchArn', 'build_buildBatchArn' - The ARN of the batch build that this build is a member of, if
-- applicable.
--
-- 'logs', 'build_logs' - Information about the build\'s logs in CloudWatch Logs.
--
-- 'resolvedSourceVersion', 'build_resolvedSourceVersion' - An identifier for the version of this build\'s source code.
--
-- -   For CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit
--     ID.
--
-- -   For CodePipeline, the source revision provided by CodePipeline.
--
-- -   For Amazon S3, this does not apply.
--
-- 'vpcConfig', 'build_vpcConfig' - If your CodeBuild project accesses resources in an Amazon VPC, you
-- provide this parameter that identifies the VPC ID and the list of
-- security group IDs and subnet IDs. The security groups and subnets must
-- belong to the same VPC. You must provide at least one security group and
-- one subnet ID.
--
-- 'endTime', 'build_endTime' - When the build process ended, expressed in Unix time format.
--
-- 'projectName', 'build_projectName' - The name of the CodeBuild project.
--
-- 'buildStatus', 'build_buildStatus' - The current status of the build. Valid values include:
--
-- -   @FAILED@: The build failed.
--
-- -   @FAULT@: The build faulted.
--
-- -   @IN_PROGRESS@: The build is still in progress.
--
-- -   @STOPPED@: The build stopped.
--
-- -   @SUCCEEDED@: The build succeeded.
--
-- -   @TIMED_OUT@: The build timed out.
--
-- 'source', 'build_source' - Information about the source code to be built.
--
-- 'id', 'build_id' - The unique ID for the build.
--
-- 'fileSystemLocations', 'build_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'reportArns', 'build_reportArns' - An array of the ARNs associated with this build\'s reports.
--
-- 'encryptionKey', 'build_encryptionKey' - The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'serviceRole', 'build_serviceRole' - The name of a service role used for this build.
--
-- 'timeoutInMinutes', 'build_timeoutInMinutes' - How long, in minutes, for CodeBuild to wait before timing out this build
-- if it does not get marked as completed.
newBuild ::
  Build
newBuild =
  Build'
    { phases = Prelude.Nothing,
      buildComplete = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing,
      arn = Prelude.Nothing,
      exportedEnvironmentVariables = Prelude.Nothing,
      buildNumber = Prelude.Nothing,
      startTime = Prelude.Nothing,
      artifacts = Prelude.Nothing,
      environment = Prelude.Nothing,
      initiator = Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      secondarySourceVersions = Prelude.Nothing,
      currentPhase = Prelude.Nothing,
      queuedTimeoutInMinutes = Prelude.Nothing,
      cache = Prelude.Nothing,
      secondarySources = Prelude.Nothing,
      debugSession = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      buildBatchArn = Prelude.Nothing,
      logs = Prelude.Nothing,
      resolvedSourceVersion = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      projectName = Prelude.Nothing,
      buildStatus = Prelude.Nothing,
      source = Prelude.Nothing,
      id = Prelude.Nothing,
      fileSystemLocations = Prelude.Nothing,
      reportArns = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing
    }

-- | Information about all previous build phases that are complete and
-- information about any current build phase that is not yet complete.
build_phases :: Lens.Lens' Build (Prelude.Maybe [BuildPhase])
build_phases = Lens.lens (\Build' {phases} -> phases) (\s@Build' {} a -> s {phases = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | Whether the build is complete. True if complete; otherwise, false.
build_buildComplete :: Lens.Lens' Build (Prelude.Maybe Prelude.Bool)
build_buildComplete = Lens.lens (\Build' {buildComplete} -> buildComplete) (\s@Build' {} a -> s {buildComplete = a} :: Build)

-- | An array of @ProjectArtifacts@ objects.
build_secondaryArtifacts :: Lens.Lens' Build (Prelude.Maybe [BuildArtifacts])
build_secondaryArtifacts = Lens.lens (\Build' {secondaryArtifacts} -> secondaryArtifacts) (\s@Build' {} a -> s {secondaryArtifacts = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the build.
build_arn :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_arn = Lens.lens (\Build' {arn} -> arn) (\s@Build' {} a -> s {arn = a} :: Build)

-- | A list of exported environment variables for this build.
--
-- Exported environment variables are used in conjunction with CodePipeline
-- to export environment variables from the current build stage to
-- subsequent stages in the pipeline. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/actions-variables.html Working with variables>
-- in the /CodePipeline User Guide/.
build_exportedEnvironmentVariables :: Lens.Lens' Build (Prelude.Maybe [ExportedEnvironmentVariable])
build_exportedEnvironmentVariables = Lens.lens (\Build' {exportedEnvironmentVariables} -> exportedEnvironmentVariables) (\s@Build' {} a -> s {exportedEnvironmentVariables = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | The number of the build. For each project, the @buildNumber@ of its
-- first build is @1@. The @buildNumber@ of each subsequent build is
-- incremented by @1@. If a build is deleted, the @buildNumber@ of other
-- builds does not change.
build_buildNumber :: Lens.Lens' Build (Prelude.Maybe Prelude.Integer)
build_buildNumber = Lens.lens (\Build' {buildNumber} -> buildNumber) (\s@Build' {} a -> s {buildNumber = a} :: Build)

-- | When the build process started, expressed in Unix time format.
build_startTime :: Lens.Lens' Build (Prelude.Maybe Prelude.UTCTime)
build_startTime = Lens.lens (\Build' {startTime} -> startTime) (\s@Build' {} a -> s {startTime = a} :: Build) Prelude.. Lens.mapping Core._Time

-- | Information about the output artifacts for the build.
build_artifacts :: Lens.Lens' Build (Prelude.Maybe BuildArtifacts)
build_artifacts = Lens.lens (\Build' {artifacts} -> artifacts) (\s@Build' {} a -> s {artifacts = a} :: Build)

-- | Information about the build environment for this build.
build_environment :: Lens.Lens' Build (Prelude.Maybe ProjectEnvironment)
build_environment = Lens.lens (\Build' {environment} -> environment) (\s@Build' {} a -> s {environment = a} :: Build)

-- | The entity that started the build. Valid values include:
--
-- -   If CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an IAM user started the build, the user\'s name (for example,
--     @MyUserName@).
--
-- -   If the Jenkins plugin for CodeBuild started the build, the string
--     @CodeBuild-Jenkins-Plugin@.
build_initiator :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_initiator = Lens.lens (\Build' {initiator} -> initiator) (\s@Build' {} a -> s {initiator = a} :: Build)

-- | Describes a network interface.
build_networkInterface :: Lens.Lens' Build (Prelude.Maybe NetworkInterface)
build_networkInterface = Lens.lens (\Build' {networkInterface} -> networkInterface) (\s@Build' {} a -> s {networkInterface = a} :: Build)

-- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
-- must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
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
build_secondarySourceVersions :: Lens.Lens' Build (Prelude.Maybe [ProjectSourceVersion])
build_secondarySourceVersions = Lens.lens (\Build' {secondarySourceVersions} -> secondarySourceVersions) (\s@Build' {} a -> s {secondarySourceVersions = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | The current build phase.
build_currentPhase :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_currentPhase = Lens.lens (\Build' {currentPhase} -> currentPhase) (\s@Build' {} a -> s {currentPhase = a} :: Build)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
build_queuedTimeoutInMinutes :: Lens.Lens' Build (Prelude.Maybe Prelude.Int)
build_queuedTimeoutInMinutes = Lens.lens (\Build' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@Build' {} a -> s {queuedTimeoutInMinutes = a} :: Build)

-- | Information about the cache for the build.
build_cache :: Lens.Lens' Build (Prelude.Maybe ProjectCache)
build_cache = Lens.lens (\Build' {cache} -> cache) (\s@Build' {} a -> s {cache = a} :: Build)

-- | An array of @ProjectSource@ objects.
build_secondarySources :: Lens.Lens' Build (Prelude.Maybe [ProjectSource])
build_secondarySources = Lens.lens (\Build' {secondarySources} -> secondarySources) (\s@Build' {} a -> s {secondarySources = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the debug session for this build.
build_debugSession :: Lens.Lens' Build (Prelude.Maybe DebugSession)
build_debugSession = Lens.lens (\Build' {debugSession} -> debugSession) (\s@Build' {} a -> s {debugSession = a} :: Build)

-- | Any version identifier for the version of the source code to be built.
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
build_sourceVersion :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_sourceVersion = Lens.lens (\Build' {sourceVersion} -> sourceVersion) (\s@Build' {} a -> s {sourceVersion = a} :: Build)

-- | The ARN of the batch build that this build is a member of, if
-- applicable.
build_buildBatchArn :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_buildBatchArn = Lens.lens (\Build' {buildBatchArn} -> buildBatchArn) (\s@Build' {} a -> s {buildBatchArn = a} :: Build)

-- | Information about the build\'s logs in CloudWatch Logs.
build_logs :: Lens.Lens' Build (Prelude.Maybe LogsLocation)
build_logs = Lens.lens (\Build' {logs} -> logs) (\s@Build' {} a -> s {logs = a} :: Build)

-- | An identifier for the version of this build\'s source code.
--
-- -   For CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit
--     ID.
--
-- -   For CodePipeline, the source revision provided by CodePipeline.
--
-- -   For Amazon S3, this does not apply.
build_resolvedSourceVersion :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_resolvedSourceVersion = Lens.lens (\Build' {resolvedSourceVersion} -> resolvedSourceVersion) (\s@Build' {} a -> s {resolvedSourceVersion = a} :: Build)

-- | If your CodeBuild project accesses resources in an Amazon VPC, you
-- provide this parameter that identifies the VPC ID and the list of
-- security group IDs and subnet IDs. The security groups and subnets must
-- belong to the same VPC. You must provide at least one security group and
-- one subnet ID.
build_vpcConfig :: Lens.Lens' Build (Prelude.Maybe VpcConfig)
build_vpcConfig = Lens.lens (\Build' {vpcConfig} -> vpcConfig) (\s@Build' {} a -> s {vpcConfig = a} :: Build)

-- | When the build process ended, expressed in Unix time format.
build_endTime :: Lens.Lens' Build (Prelude.Maybe Prelude.UTCTime)
build_endTime = Lens.lens (\Build' {endTime} -> endTime) (\s@Build' {} a -> s {endTime = a} :: Build) Prelude.. Lens.mapping Core._Time

-- | The name of the CodeBuild project.
build_projectName :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_projectName = Lens.lens (\Build' {projectName} -> projectName) (\s@Build' {} a -> s {projectName = a} :: Build)

-- | The current status of the build. Valid values include:
--
-- -   @FAILED@: The build failed.
--
-- -   @FAULT@: The build faulted.
--
-- -   @IN_PROGRESS@: The build is still in progress.
--
-- -   @STOPPED@: The build stopped.
--
-- -   @SUCCEEDED@: The build succeeded.
--
-- -   @TIMED_OUT@: The build timed out.
build_buildStatus :: Lens.Lens' Build (Prelude.Maybe StatusType)
build_buildStatus = Lens.lens (\Build' {buildStatus} -> buildStatus) (\s@Build' {} a -> s {buildStatus = a} :: Build)

-- | Information about the source code to be built.
build_source :: Lens.Lens' Build (Prelude.Maybe ProjectSource)
build_source = Lens.lens (\Build' {source} -> source) (\s@Build' {} a -> s {source = a} :: Build)

-- | The unique ID for the build.
build_id :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_id = Lens.lens (\Build' {id} -> id) (\s@Build' {} a -> s {id = a} :: Build)

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
build_fileSystemLocations :: Lens.Lens' Build (Prelude.Maybe [ProjectFileSystemLocation])
build_fileSystemLocations = Lens.lens (\Build' {fileSystemLocations} -> fileSystemLocations) (\s@Build' {} a -> s {fileSystemLocations = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | An array of the ARNs associated with this build\'s reports.
build_reportArns :: Lens.Lens' Build (Prelude.Maybe [Prelude.Text])
build_reportArns = Lens.lens (\Build' {reportArns} -> reportArns) (\s@Build' {} a -> s {reportArns = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | The Key Management Service customer master key (CMK) to be used for
-- encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
build_encryptionKey :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_encryptionKey = Lens.lens (\Build' {encryptionKey} -> encryptionKey) (\s@Build' {} a -> s {encryptionKey = a} :: Build)

-- | The name of a service role used for this build.
build_serviceRole :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_serviceRole = Lens.lens (\Build' {serviceRole} -> serviceRole) (\s@Build' {} a -> s {serviceRole = a} :: Build)

-- | How long, in minutes, for CodeBuild to wait before timing out this build
-- if it does not get marked as completed.
build_timeoutInMinutes :: Lens.Lens' Build (Prelude.Maybe Prelude.Int)
build_timeoutInMinutes = Lens.lens (\Build' {timeoutInMinutes} -> timeoutInMinutes) (\s@Build' {} a -> s {timeoutInMinutes = a} :: Build)

instance Core.FromJSON Build where
  parseJSON =
    Core.withObject
      "Build"
      ( \x ->
          Build'
            Prelude.<$> (x Core..:? "phases" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "buildComplete")
            Prelude.<*> ( x Core..:? "secondaryArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> ( x Core..:? "exportedEnvironmentVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "buildNumber")
            Prelude.<*> (x Core..:? "startTime")
            Prelude.<*> (x Core..:? "artifacts")
            Prelude.<*> (x Core..:? "environment")
            Prelude.<*> (x Core..:? "initiator")
            Prelude.<*> (x Core..:? "networkInterface")
            Prelude.<*> ( x Core..:? "secondarySourceVersions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "currentPhase")
            Prelude.<*> (x Core..:? "queuedTimeoutInMinutes")
            Prelude.<*> (x Core..:? "cache")
            Prelude.<*> ( x Core..:? "secondarySources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "debugSession")
            Prelude.<*> (x Core..:? "sourceVersion")
            Prelude.<*> (x Core..:? "buildBatchArn")
            Prelude.<*> (x Core..:? "logs")
            Prelude.<*> (x Core..:? "resolvedSourceVersion")
            Prelude.<*> (x Core..:? "vpcConfig")
            Prelude.<*> (x Core..:? "endTime")
            Prelude.<*> (x Core..:? "projectName")
            Prelude.<*> (x Core..:? "buildStatus")
            Prelude.<*> (x Core..:? "source")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> ( x Core..:? "fileSystemLocations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "reportArns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "encryptionKey")
            Prelude.<*> (x Core..:? "serviceRole")
            Prelude.<*> (x Core..:? "timeoutInMinutes")
      )

instance Prelude.Hashable Build

instance Prelude.NFData Build
