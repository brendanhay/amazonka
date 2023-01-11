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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a build.
--
-- /See:/ 'newBuild' smart constructor.
data Build = Build'
  { -- | The Amazon Resource Name (ARN) of the build.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about the output artifacts for the build.
    artifacts :: Prelude.Maybe BuildArtifacts,
    -- | The ARN of the batch build that this build is a member of, if
    -- applicable.
    buildBatchArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the build is complete. True if complete; otherwise, false.
    buildComplete :: Prelude.Maybe Prelude.Bool,
    -- | The number of the build. For each project, the @buildNumber@ of its
    -- first build is @1@. The @buildNumber@ of each subsequent build is
    -- incremented by @1@. If a build is deleted, the @buildNumber@ of other
    -- builds does not change.
    buildNumber :: Prelude.Maybe Prelude.Integer,
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
    -- | Information about the cache for the build.
    cache :: Prelude.Maybe ProjectCache,
    -- | The current build phase.
    currentPhase :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the debug session for this build.
    debugSession :: Prelude.Maybe DebugSession,
    -- | The Key Management Service customer master key (CMK) to be used for
    -- encrypting the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | When the build process ended, expressed in Unix time format.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Information about the build environment for this build.
    environment :: Prelude.Maybe ProjectEnvironment,
    -- | A list of exported environment variables for this build.
    --
    -- Exported environment variables are used in conjunction with CodePipeline
    -- to export environment variables from the current build stage to
    -- subsequent stages in the pipeline. For more information, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/actions-variables.html Working with variables>
    -- in the /CodePipeline User Guide/.
    exportedEnvironmentVariables :: Prelude.Maybe [ExportedEnvironmentVariable],
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    -- | The unique ID for the build.
    id :: Prelude.Maybe Prelude.Text,
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
    -- | Information about the build\'s logs in CloudWatch Logs.
    logs :: Prelude.Maybe LogsLocation,
    -- | Describes a network interface.
    networkInterface :: Prelude.Maybe NetworkInterface,
    -- | Information about all previous build phases that are complete and
    -- information about any current build phase that is not yet complete.
    phases :: Prelude.Maybe [BuildPhase],
    -- | The name of the CodeBuild project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | An array of the ARNs associated with this build\'s reports.
    reportArns :: Prelude.Maybe [Prelude.Text],
    -- | An identifier for the version of this build\'s source code.
    --
    -- -   For CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit
    --     ID.
    --
    -- -   For CodePipeline, the source revision provided by CodePipeline.
    --
    -- -   For Amazon S3, this does not apply.
    resolvedSourceVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Prelude.Maybe [BuildArtifacts],
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
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Prelude.Maybe [ProjectSource],
    -- | The name of a service role used for this build.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Information about the source code to be built.
    source :: Prelude.Maybe ProjectSource,
    -- | Any version identifier for the version of the source code to be built.
    -- If @sourceVersion@ is specified at the project level, then this
    -- @sourceVersion@ (at the build level) takes precedence.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | When the build process started, expressed in Unix time format.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | How long, in minutes, for CodeBuild to wait before timing out this build
    -- if it does not get marked as completed.
    timeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | If your CodeBuild project accesses resources in an Amazon VPC, you
    -- provide this parameter that identifies the VPC ID and the list of
    -- security group IDs and subnet IDs. The security groups and subnets must
    -- belong to the same VPC. You must provide at least one security group and
    -- one subnet ID.
    vpcConfig :: Prelude.Maybe VpcConfig
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
-- 'arn', 'build_arn' - The Amazon Resource Name (ARN) of the build.
--
-- 'artifacts', 'build_artifacts' - Information about the output artifacts for the build.
--
-- 'buildBatchArn', 'build_buildBatchArn' - The ARN of the batch build that this build is a member of, if
-- applicable.
--
-- 'buildComplete', 'build_buildComplete' - Whether the build is complete. True if complete; otherwise, false.
--
-- 'buildNumber', 'build_buildNumber' - The number of the build. For each project, the @buildNumber@ of its
-- first build is @1@. The @buildNumber@ of each subsequent build is
-- incremented by @1@. If a build is deleted, the @buildNumber@ of other
-- builds does not change.
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
-- 'cache', 'build_cache' - Information about the cache for the build.
--
-- 'currentPhase', 'build_currentPhase' - The current build phase.
--
-- 'debugSession', 'build_debugSession' - Contains information about the debug session for this build.
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
-- 'endTime', 'build_endTime' - When the build process ended, expressed in Unix time format.
--
-- 'environment', 'build_environment' - Information about the build environment for this build.
--
-- 'exportedEnvironmentVariables', 'build_exportedEnvironmentVariables' - A list of exported environment variables for this build.
--
-- Exported environment variables are used in conjunction with CodePipeline
-- to export environment variables from the current build stage to
-- subsequent stages in the pipeline. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/actions-variables.html Working with variables>
-- in the /CodePipeline User Guide/.
--
-- 'fileSystemLocations', 'build_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'id', 'build_id' - The unique ID for the build.
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
-- 'logs', 'build_logs' - Information about the build\'s logs in CloudWatch Logs.
--
-- 'networkInterface', 'build_networkInterface' - Describes a network interface.
--
-- 'phases', 'build_phases' - Information about all previous build phases that are complete and
-- information about any current build phase that is not yet complete.
--
-- 'projectName', 'build_projectName' - The name of the CodeBuild project.
--
-- 'queuedTimeoutInMinutes', 'build_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'reportArns', 'build_reportArns' - An array of the ARNs associated with this build\'s reports.
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
-- 'secondaryArtifacts', 'build_secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
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
-- 'secondarySources', 'build_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'serviceRole', 'build_serviceRole' - The name of a service role used for this build.
--
-- 'source', 'build_source' - Information about the source code to be built.
--
-- 'sourceVersion', 'build_sourceVersion' - Any version identifier for the version of the source code to be built.
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
--
-- 'startTime', 'build_startTime' - When the build process started, expressed in Unix time format.
--
-- 'timeoutInMinutes', 'build_timeoutInMinutes' - How long, in minutes, for CodeBuild to wait before timing out this build
-- if it does not get marked as completed.
--
-- 'vpcConfig', 'build_vpcConfig' - If your CodeBuild project accesses resources in an Amazon VPC, you
-- provide this parameter that identifies the VPC ID and the list of
-- security group IDs and subnet IDs. The security groups and subnets must
-- belong to the same VPC. You must provide at least one security group and
-- one subnet ID.
newBuild ::
  Build
newBuild =
  Build'
    { arn = Prelude.Nothing,
      artifacts = Prelude.Nothing,
      buildBatchArn = Prelude.Nothing,
      buildComplete = Prelude.Nothing,
      buildNumber = Prelude.Nothing,
      buildStatus = Prelude.Nothing,
      cache = Prelude.Nothing,
      currentPhase = Prelude.Nothing,
      debugSession = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      endTime = Prelude.Nothing,
      environment = Prelude.Nothing,
      exportedEnvironmentVariables = Prelude.Nothing,
      fileSystemLocations = Prelude.Nothing,
      id = Prelude.Nothing,
      initiator = Prelude.Nothing,
      logs = Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      phases = Prelude.Nothing,
      projectName = Prelude.Nothing,
      queuedTimeoutInMinutes = Prelude.Nothing,
      reportArns = Prelude.Nothing,
      resolvedSourceVersion = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing,
      secondarySourceVersions = Prelude.Nothing,
      secondarySources = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      startTime = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the build.
build_arn :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_arn = Lens.lens (\Build' {arn} -> arn) (\s@Build' {} a -> s {arn = a} :: Build)

-- | Information about the output artifacts for the build.
build_artifacts :: Lens.Lens' Build (Prelude.Maybe BuildArtifacts)
build_artifacts = Lens.lens (\Build' {artifacts} -> artifacts) (\s@Build' {} a -> s {artifacts = a} :: Build)

-- | The ARN of the batch build that this build is a member of, if
-- applicable.
build_buildBatchArn :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_buildBatchArn = Lens.lens (\Build' {buildBatchArn} -> buildBatchArn) (\s@Build' {} a -> s {buildBatchArn = a} :: Build)

-- | Whether the build is complete. True if complete; otherwise, false.
build_buildComplete :: Lens.Lens' Build (Prelude.Maybe Prelude.Bool)
build_buildComplete = Lens.lens (\Build' {buildComplete} -> buildComplete) (\s@Build' {} a -> s {buildComplete = a} :: Build)

-- | The number of the build. For each project, the @buildNumber@ of its
-- first build is @1@. The @buildNumber@ of each subsequent build is
-- incremented by @1@. If a build is deleted, the @buildNumber@ of other
-- builds does not change.
build_buildNumber :: Lens.Lens' Build (Prelude.Maybe Prelude.Integer)
build_buildNumber = Lens.lens (\Build' {buildNumber} -> buildNumber) (\s@Build' {} a -> s {buildNumber = a} :: Build)

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

-- | Information about the cache for the build.
build_cache :: Lens.Lens' Build (Prelude.Maybe ProjectCache)
build_cache = Lens.lens (\Build' {cache} -> cache) (\s@Build' {} a -> s {cache = a} :: Build)

-- | The current build phase.
build_currentPhase :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_currentPhase = Lens.lens (\Build' {currentPhase} -> currentPhase) (\s@Build' {} a -> s {currentPhase = a} :: Build)

-- | Contains information about the debug session for this build.
build_debugSession :: Lens.Lens' Build (Prelude.Maybe DebugSession)
build_debugSession = Lens.lens (\Build' {debugSession} -> debugSession) (\s@Build' {} a -> s {debugSession = a} :: Build)

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

-- | When the build process ended, expressed in Unix time format.
build_endTime :: Lens.Lens' Build (Prelude.Maybe Prelude.UTCTime)
build_endTime = Lens.lens (\Build' {endTime} -> endTime) (\s@Build' {} a -> s {endTime = a} :: Build) Prelude.. Lens.mapping Data._Time

-- | Information about the build environment for this build.
build_environment :: Lens.Lens' Build (Prelude.Maybe ProjectEnvironment)
build_environment = Lens.lens (\Build' {environment} -> environment) (\s@Build' {} a -> s {environment = a} :: Build)

-- | A list of exported environment variables for this build.
--
-- Exported environment variables are used in conjunction with CodePipeline
-- to export environment variables from the current build stage to
-- subsequent stages in the pipeline. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/actions-variables.html Working with variables>
-- in the /CodePipeline User Guide/.
build_exportedEnvironmentVariables :: Lens.Lens' Build (Prelude.Maybe [ExportedEnvironmentVariable])
build_exportedEnvironmentVariables = Lens.lens (\Build' {exportedEnvironmentVariables} -> exportedEnvironmentVariables) (\s@Build' {} a -> s {exportedEnvironmentVariables = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
build_fileSystemLocations :: Lens.Lens' Build (Prelude.Maybe [ProjectFileSystemLocation])
build_fileSystemLocations = Lens.lens (\Build' {fileSystemLocations} -> fileSystemLocations) (\s@Build' {} a -> s {fileSystemLocations = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID for the build.
build_id :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_id = Lens.lens (\Build' {id} -> id) (\s@Build' {} a -> s {id = a} :: Build)

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

-- | Information about the build\'s logs in CloudWatch Logs.
build_logs :: Lens.Lens' Build (Prelude.Maybe LogsLocation)
build_logs = Lens.lens (\Build' {logs} -> logs) (\s@Build' {} a -> s {logs = a} :: Build)

-- | Describes a network interface.
build_networkInterface :: Lens.Lens' Build (Prelude.Maybe NetworkInterface)
build_networkInterface = Lens.lens (\Build' {networkInterface} -> networkInterface) (\s@Build' {} a -> s {networkInterface = a} :: Build)

-- | Information about all previous build phases that are complete and
-- information about any current build phase that is not yet complete.
build_phases :: Lens.Lens' Build (Prelude.Maybe [BuildPhase])
build_phases = Lens.lens (\Build' {phases} -> phases) (\s@Build' {} a -> s {phases = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | The name of the CodeBuild project.
build_projectName :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_projectName = Lens.lens (\Build' {projectName} -> projectName) (\s@Build' {} a -> s {projectName = a} :: Build)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
build_queuedTimeoutInMinutes :: Lens.Lens' Build (Prelude.Maybe Prelude.Int)
build_queuedTimeoutInMinutes = Lens.lens (\Build' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@Build' {} a -> s {queuedTimeoutInMinutes = a} :: Build)

-- | An array of the ARNs associated with this build\'s reports.
build_reportArns :: Lens.Lens' Build (Prelude.Maybe [Prelude.Text])
build_reportArns = Lens.lens (\Build' {reportArns} -> reportArns) (\s@Build' {} a -> s {reportArns = a} :: Build) Prelude.. Lens.mapping Lens.coerced

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

-- | An array of @ProjectArtifacts@ objects.
build_secondaryArtifacts :: Lens.Lens' Build (Prelude.Maybe [BuildArtifacts])
build_secondaryArtifacts = Lens.lens (\Build' {secondaryArtifacts} -> secondaryArtifacts) (\s@Build' {} a -> s {secondaryArtifacts = a} :: Build) Prelude.. Lens.mapping Lens.coerced

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

-- | An array of @ProjectSource@ objects.
build_secondarySources :: Lens.Lens' Build (Prelude.Maybe [ProjectSource])
build_secondarySources = Lens.lens (\Build' {secondarySources} -> secondarySources) (\s@Build' {} a -> s {secondarySources = a} :: Build) Prelude.. Lens.mapping Lens.coerced

-- | The name of a service role used for this build.
build_serviceRole :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_serviceRole = Lens.lens (\Build' {serviceRole} -> serviceRole) (\s@Build' {} a -> s {serviceRole = a} :: Build)

-- | Information about the source code to be built.
build_source :: Lens.Lens' Build (Prelude.Maybe ProjectSource)
build_source = Lens.lens (\Build' {source} -> source) (\s@Build' {} a -> s {source = a} :: Build)

-- | Any version identifier for the version of the source code to be built.
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
build_sourceVersion :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_sourceVersion = Lens.lens (\Build' {sourceVersion} -> sourceVersion) (\s@Build' {} a -> s {sourceVersion = a} :: Build)

-- | When the build process started, expressed in Unix time format.
build_startTime :: Lens.Lens' Build (Prelude.Maybe Prelude.UTCTime)
build_startTime = Lens.lens (\Build' {startTime} -> startTime) (\s@Build' {} a -> s {startTime = a} :: Build) Prelude.. Lens.mapping Data._Time

-- | How long, in minutes, for CodeBuild to wait before timing out this build
-- if it does not get marked as completed.
build_timeoutInMinutes :: Lens.Lens' Build (Prelude.Maybe Prelude.Int)
build_timeoutInMinutes = Lens.lens (\Build' {timeoutInMinutes} -> timeoutInMinutes) (\s@Build' {} a -> s {timeoutInMinutes = a} :: Build)

-- | If your CodeBuild project accesses resources in an Amazon VPC, you
-- provide this parameter that identifies the VPC ID and the list of
-- security group IDs and subnet IDs. The security groups and subnets must
-- belong to the same VPC. You must provide at least one security group and
-- one subnet ID.
build_vpcConfig :: Lens.Lens' Build (Prelude.Maybe VpcConfig)
build_vpcConfig = Lens.lens (\Build' {vpcConfig} -> vpcConfig) (\s@Build' {} a -> s {vpcConfig = a} :: Build)

instance Data.FromJSON Build where
  parseJSON =
    Data.withObject
      "Build"
      ( \x ->
          Build'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "artifacts")
            Prelude.<*> (x Data..:? "buildBatchArn")
            Prelude.<*> (x Data..:? "buildComplete")
            Prelude.<*> (x Data..:? "buildNumber")
            Prelude.<*> (x Data..:? "buildStatus")
            Prelude.<*> (x Data..:? "cache")
            Prelude.<*> (x Data..:? "currentPhase")
            Prelude.<*> (x Data..:? "debugSession")
            Prelude.<*> (x Data..:? "encryptionKey")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "environment")
            Prelude.<*> ( x Data..:? "exportedEnvironmentVariables"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "fileSystemLocations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "initiator")
            Prelude.<*> (x Data..:? "logs")
            Prelude.<*> (x Data..:? "networkInterface")
            Prelude.<*> (x Data..:? "phases" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "projectName")
            Prelude.<*> (x Data..:? "queuedTimeoutInMinutes")
            Prelude.<*> (x Data..:? "reportArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resolvedSourceVersion")
            Prelude.<*> ( x Data..:? "secondaryArtifacts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "secondarySourceVersions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "secondarySources"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "serviceRole")
            Prelude.<*> (x Data..:? "source")
            Prelude.<*> (x Data..:? "sourceVersion")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "timeoutInMinutes")
            Prelude.<*> (x Data..:? "vpcConfig")
      )

instance Prelude.Hashable Build where
  hashWithSalt _salt Build' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` artifacts
      `Prelude.hashWithSalt` buildBatchArn
      `Prelude.hashWithSalt` buildComplete
      `Prelude.hashWithSalt` buildNumber
      `Prelude.hashWithSalt` buildStatus
      `Prelude.hashWithSalt` cache
      `Prelude.hashWithSalt` currentPhase
      `Prelude.hashWithSalt` debugSession
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` exportedEnvironmentVariables
      `Prelude.hashWithSalt` fileSystemLocations
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` initiator
      `Prelude.hashWithSalt` logs
      `Prelude.hashWithSalt` networkInterface
      `Prelude.hashWithSalt` phases
      `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` queuedTimeoutInMinutes
      `Prelude.hashWithSalt` reportArns
      `Prelude.hashWithSalt` resolvedSourceVersion
      `Prelude.hashWithSalt` secondaryArtifacts
      `Prelude.hashWithSalt` secondarySourceVersions
      `Prelude.hashWithSalt` secondarySources
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` timeoutInMinutes
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData Build where
  rnf Build' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf artifacts
      `Prelude.seq` Prelude.rnf buildBatchArn
      `Prelude.seq` Prelude.rnf buildComplete
      `Prelude.seq` Prelude.rnf buildNumber
      `Prelude.seq` Prelude.rnf buildStatus
      `Prelude.seq` Prelude.rnf cache
      `Prelude.seq` Prelude.rnf currentPhase
      `Prelude.seq` Prelude.rnf debugSession
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf exportedEnvironmentVariables
      `Prelude.seq` Prelude.rnf fileSystemLocations
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf initiator
      `Prelude.seq` Prelude.rnf logs
      `Prelude.seq` Prelude.rnf networkInterface
      `Prelude.seq` Prelude.rnf phases
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf
        queuedTimeoutInMinutes
      `Prelude.seq` Prelude.rnf reportArns
      `Prelude.seq` Prelude.rnf
        resolvedSourceVersion
      `Prelude.seq` Prelude.rnf
        secondaryArtifacts
      `Prelude.seq` Prelude.rnf
        secondarySourceVersions
      `Prelude.seq` Prelude.rnf
        secondarySources
      `Prelude.seq` Prelude.rnf
        serviceRole
      `Prelude.seq` Prelude.rnf
        source
      `Prelude.seq` Prelude.rnf
        sourceVersion
      `Prelude.seq` Prelude.rnf
        startTime
      `Prelude.seq` Prelude.rnf
        timeoutInMinutes
      `Prelude.seq` Prelude.rnf
        vpcConfig
