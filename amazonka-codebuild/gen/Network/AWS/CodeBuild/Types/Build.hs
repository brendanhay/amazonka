{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeBuild.Types.Build
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Build where

import Network.AWS.CodeBuild.Types.BuildArtifacts
import Network.AWS.CodeBuild.Types.BuildPhase
import Network.AWS.CodeBuild.Types.DebugSession
import Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable
import Network.AWS.CodeBuild.Types.LogsLocation
import Network.AWS.CodeBuild.Types.NetworkInterface
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.StatusType
import Network.AWS.CodeBuild.Types.VpcConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a build.
--
-- /See:/ 'newBuild' smart constructor.
data Build = Build'
  { -- | If your AWS CodeBuild project accesses resources in an Amazon VPC, you
    -- provide this parameter that identifies the VPC ID and the list of
    -- security group IDs and subnet IDs. The security groups and subnets must
    -- belong to the same VPC. You must provide at least one security group and
    -- one subnet ID.
    vpcConfig :: Prelude.Maybe VpcConfig,
    -- | The ARN of the batch build that this build is a member of, if
    -- applicable.
    buildBatchArn :: Prelude.Maybe Prelude.Text,
    -- | An identifier for the version of this build\'s source code.
    --
    -- -   For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the
    --     commit ID.
    --
    -- -   For AWS CodePipeline, the source revision provided by AWS
    --     CodePipeline.
    --
    -- -   For Amazon S3, this does not apply.
    resolvedSourceVersion :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Prelude.Maybe [BuildArtifacts],
    -- | Any version identifier for the version of the source code to be built.
    -- If @sourceVersion@ is specified at the project level, then this
    -- @sourceVersion@ (at the build level) takes precedence.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /AWS CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about all previous build phases that are complete and
    -- information about any current build phase that is not yet complete.
    phases :: Prelude.Maybe [BuildPhase],
    -- | Information about the cache for the build.
    cache :: Prelude.Maybe ProjectCache,
    -- | The name of a service role used for this build.
    serviceRole :: Prelude.Maybe Prelude.Text,
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
    secondarySourceVersions :: Prelude.Maybe [ProjectSourceVersion],
    -- | Describes a network interface.
    networkInterface :: Prelude.Maybe NetworkInterface,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
    -- used for encrypting the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | Information about the output artifacts for the build.
    artifacts :: Prelude.Maybe BuildArtifacts,
    -- | The number of the build. For each project, the @buildNumber@ of its
    -- first build is @1@. The @buildNumber@ of each subsequent build is
    -- incremented by @1@. If a build is deleted, the @buildNumber@ of other
    -- builds does not change.
    buildNumber :: Prelude.Maybe Prelude.Integer,
    -- | When the build process started, expressed in Unix time format.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The unique ID for the build.
    id :: Prelude.Maybe Prelude.Text,
    -- | Information about the build environment for this build.
    environment :: Prelude.Maybe ProjectEnvironment,
    -- | Information about the source code to be built.
    source :: Prelude.Maybe ProjectSource,
    -- | The Amazon Resource Name (ARN) of the build.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS CodeBuild project.
    projectName :: Prelude.Maybe Prelude.Text,
    -- | When the build process ended, expressed in Unix time format.
    endTime :: Prelude.Maybe Prelude.POSIX,
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
    -- | Information about the build\'s logs in Amazon CloudWatch Logs.
    logs :: Prelude.Maybe LogsLocation,
    -- | Whether the build is complete. True if complete; otherwise, false.
    buildComplete :: Prelude.Maybe Prelude.Bool,
    -- | Contains information about the debug session for this build.
    debugSession :: Prelude.Maybe DebugSession,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Prelude.Maybe [ProjectSource],
    -- | How long, in minutes, for AWS CodeBuild to wait before timing out this
    -- build if it does not get marked as completed.
    timeoutInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The current build phase.
    currentPhase :: Prelude.Maybe Prelude.Text,
    -- | The entity that started the build. Valid values include:
    --
    -- -   If AWS CodePipeline started the build, the pipeline\'s name (for
    --     example, @codepipeline\/my-demo-pipeline@).
    --
    -- -   If an AWS Identity and Access Management (IAM) user started the
    --     build, the user\'s name (for example, @MyUserName@).
    --
    -- -   If the Jenkins plugin for AWS CodeBuild started the build, the
    --     string @CodeBuild-Jenkins-Plugin@.
    initiator :: Prelude.Maybe Prelude.Text,
    -- | An array of the ARNs associated with this build\'s reports.
    reportArns :: Prelude.Maybe [Prelude.Text],
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
    -- project. A @ProjectFileSystemLocation@ object specifies the
    -- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
    -- file system created using Amazon Elastic File System.
    fileSystemLocations :: Prelude.Maybe [ProjectFileSystemLocation],
    -- | A list of exported environment variables for this build.
    exportedEnvironmentVariables :: Prelude.Maybe [ExportedEnvironmentVariable]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Build' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConfig', 'build_vpcConfig' - If your AWS CodeBuild project accesses resources in an Amazon VPC, you
-- provide this parameter that identifies the VPC ID and the list of
-- security group IDs and subnet IDs. The security groups and subnets must
-- belong to the same VPC. You must provide at least one security group and
-- one subnet ID.
--
-- 'buildBatchArn', 'build_buildBatchArn' - The ARN of the batch build that this build is a member of, if
-- applicable.
--
-- 'resolvedSourceVersion', 'build_resolvedSourceVersion' - An identifier for the version of this build\'s source code.
--
-- -   For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the
--     commit ID.
--
-- -   For AWS CodePipeline, the source revision provided by AWS
--     CodePipeline.
--
-- -   For Amazon S3, this does not apply.
--
-- 'secondaryArtifacts', 'build_secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
--
-- 'sourceVersion', 'build_sourceVersion' - Any version identifier for the version of the source code to be built.
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /AWS CodeBuild User Guide/.
--
-- 'phases', 'build_phases' - Information about all previous build phases that are complete and
-- information about any current build phase that is not yet complete.
--
-- 'cache', 'build_cache' - Information about the cache for the build.
--
-- 'serviceRole', 'build_serviceRole' - The name of a service role used for this build.
--
-- 'secondarySourceVersions', 'build_secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@
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
-- 'networkInterface', 'build_networkInterface' - Describes a network interface.
--
-- 'encryptionKey', 'build_encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
-- used for encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'artifacts', 'build_artifacts' - Information about the output artifacts for the build.
--
-- 'buildNumber', 'build_buildNumber' - The number of the build. For each project, the @buildNumber@ of its
-- first build is @1@. The @buildNumber@ of each subsequent build is
-- incremented by @1@. If a build is deleted, the @buildNumber@ of other
-- builds does not change.
--
-- 'startTime', 'build_startTime' - When the build process started, expressed in Unix time format.
--
-- 'id', 'build_id' - The unique ID for the build.
--
-- 'environment', 'build_environment' - Information about the build environment for this build.
--
-- 'source', 'build_source' - Information about the source code to be built.
--
-- 'arn', 'build_arn' - The Amazon Resource Name (ARN) of the build.
--
-- 'projectName', 'build_projectName' - The name of the AWS CodeBuild project.
--
-- 'endTime', 'build_endTime' - When the build process ended, expressed in Unix time format.
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
-- 'logs', 'build_logs' - Information about the build\'s logs in Amazon CloudWatch Logs.
--
-- 'buildComplete', 'build_buildComplete' - Whether the build is complete. True if complete; otherwise, false.
--
-- 'debugSession', 'build_debugSession' - Contains information about the debug session for this build.
--
-- 'queuedTimeoutInMinutes', 'build_queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'secondarySources', 'build_secondarySources' - An array of @ProjectSource@ objects.
--
-- 'timeoutInMinutes', 'build_timeoutInMinutes' - How long, in minutes, for AWS CodeBuild to wait before timing out this
-- build if it does not get marked as completed.
--
-- 'currentPhase', 'build_currentPhase' - The current build phase.
--
-- 'initiator', 'build_initiator' - The entity that started the build. Valid values include:
--
-- -   If AWS CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an AWS Identity and Access Management (IAM) user started the
--     build, the user\'s name (for example, @MyUserName@).
--
-- -   If the Jenkins plugin for AWS CodeBuild started the build, the
--     string @CodeBuild-Jenkins-Plugin@.
--
-- 'reportArns', 'build_reportArns' - An array of the ARNs associated with this build\'s reports.
--
-- 'fileSystemLocations', 'build_fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
--
-- 'exportedEnvironmentVariables', 'build_exportedEnvironmentVariables' - A list of exported environment variables for this build.
newBuild ::
  Build
newBuild =
  Build'
    { vpcConfig = Prelude.Nothing,
      buildBatchArn = Prelude.Nothing,
      resolvedSourceVersion = Prelude.Nothing,
      secondaryArtifacts = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      phases = Prelude.Nothing,
      cache = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      secondarySourceVersions = Prelude.Nothing,
      networkInterface = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      artifacts = Prelude.Nothing,
      buildNumber = Prelude.Nothing,
      startTime = Prelude.Nothing,
      id = Prelude.Nothing,
      environment = Prelude.Nothing,
      source = Prelude.Nothing,
      arn = Prelude.Nothing,
      projectName = Prelude.Nothing,
      endTime = Prelude.Nothing,
      buildStatus = Prelude.Nothing,
      logs = Prelude.Nothing,
      buildComplete = Prelude.Nothing,
      debugSession = Prelude.Nothing,
      queuedTimeoutInMinutes = Prelude.Nothing,
      secondarySources = Prelude.Nothing,
      timeoutInMinutes = Prelude.Nothing,
      currentPhase = Prelude.Nothing,
      initiator = Prelude.Nothing,
      reportArns = Prelude.Nothing,
      fileSystemLocations = Prelude.Nothing,
      exportedEnvironmentVariables = Prelude.Nothing
    }

-- | If your AWS CodeBuild project accesses resources in an Amazon VPC, you
-- provide this parameter that identifies the VPC ID and the list of
-- security group IDs and subnet IDs. The security groups and subnets must
-- belong to the same VPC. You must provide at least one security group and
-- one subnet ID.
build_vpcConfig :: Lens.Lens' Build (Prelude.Maybe VpcConfig)
build_vpcConfig = Lens.lens (\Build' {vpcConfig} -> vpcConfig) (\s@Build' {} a -> s {vpcConfig = a} :: Build)

-- | The ARN of the batch build that this build is a member of, if
-- applicable.
build_buildBatchArn :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_buildBatchArn = Lens.lens (\Build' {buildBatchArn} -> buildBatchArn) (\s@Build' {} a -> s {buildBatchArn = a} :: Build)

-- | An identifier for the version of this build\'s source code.
--
-- -   For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the
--     commit ID.
--
-- -   For AWS CodePipeline, the source revision provided by AWS
--     CodePipeline.
--
-- -   For Amazon S3, this does not apply.
build_resolvedSourceVersion :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_resolvedSourceVersion = Lens.lens (\Build' {resolvedSourceVersion} -> resolvedSourceVersion) (\s@Build' {} a -> s {resolvedSourceVersion = a} :: Build)

-- | An array of @ProjectArtifacts@ objects.
build_secondaryArtifacts :: Lens.Lens' Build (Prelude.Maybe [BuildArtifacts])
build_secondaryArtifacts = Lens.lens (\Build' {secondaryArtifacts} -> secondaryArtifacts) (\s@Build' {} a -> s {secondaryArtifacts = a} :: Build) Prelude.. Lens.mapping Prelude._Coerce

-- | Any version identifier for the version of the source code to be built.
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /AWS CodeBuild User Guide/.
build_sourceVersion :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_sourceVersion = Lens.lens (\Build' {sourceVersion} -> sourceVersion) (\s@Build' {} a -> s {sourceVersion = a} :: Build)

-- | Information about all previous build phases that are complete and
-- information about any current build phase that is not yet complete.
build_phases :: Lens.Lens' Build (Prelude.Maybe [BuildPhase])
build_phases = Lens.lens (\Build' {phases} -> phases) (\s@Build' {} a -> s {phases = a} :: Build) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the cache for the build.
build_cache :: Lens.Lens' Build (Prelude.Maybe ProjectCache)
build_cache = Lens.lens (\Build' {cache} -> cache) (\s@Build' {} a -> s {cache = a} :: Build)

-- | The name of a service role used for this build.
build_serviceRole :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_serviceRole = Lens.lens (\Build' {serviceRole} -> serviceRole) (\s@Build' {} a -> s {serviceRole = a} :: Build)

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
build_secondarySourceVersions :: Lens.Lens' Build (Prelude.Maybe [ProjectSourceVersion])
build_secondarySourceVersions = Lens.lens (\Build' {secondarySourceVersions} -> secondarySourceVersions) (\s@Build' {} a -> s {secondarySourceVersions = a} :: Build) Prelude.. Lens.mapping Prelude._Coerce

-- | Describes a network interface.
build_networkInterface :: Lens.Lens' Build (Prelude.Maybe NetworkInterface)
build_networkInterface = Lens.lens (\Build' {networkInterface} -> networkInterface) (\s@Build' {} a -> s {networkInterface = a} :: Build)

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be
-- used for encrypting the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
build_encryptionKey :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_encryptionKey = Lens.lens (\Build' {encryptionKey} -> encryptionKey) (\s@Build' {} a -> s {encryptionKey = a} :: Build)

-- | Information about the output artifacts for the build.
build_artifacts :: Lens.Lens' Build (Prelude.Maybe BuildArtifacts)
build_artifacts = Lens.lens (\Build' {artifacts} -> artifacts) (\s@Build' {} a -> s {artifacts = a} :: Build)

-- | The number of the build. For each project, the @buildNumber@ of its
-- first build is @1@. The @buildNumber@ of each subsequent build is
-- incremented by @1@. If a build is deleted, the @buildNumber@ of other
-- builds does not change.
build_buildNumber :: Lens.Lens' Build (Prelude.Maybe Prelude.Integer)
build_buildNumber = Lens.lens (\Build' {buildNumber} -> buildNumber) (\s@Build' {} a -> s {buildNumber = a} :: Build)

-- | When the build process started, expressed in Unix time format.
build_startTime :: Lens.Lens' Build (Prelude.Maybe Prelude.UTCTime)
build_startTime = Lens.lens (\Build' {startTime} -> startTime) (\s@Build' {} a -> s {startTime = a} :: Build) Prelude.. Lens.mapping Prelude._Time

-- | The unique ID for the build.
build_id :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_id = Lens.lens (\Build' {id} -> id) (\s@Build' {} a -> s {id = a} :: Build)

-- | Information about the build environment for this build.
build_environment :: Lens.Lens' Build (Prelude.Maybe ProjectEnvironment)
build_environment = Lens.lens (\Build' {environment} -> environment) (\s@Build' {} a -> s {environment = a} :: Build)

-- | Information about the source code to be built.
build_source :: Lens.Lens' Build (Prelude.Maybe ProjectSource)
build_source = Lens.lens (\Build' {source} -> source) (\s@Build' {} a -> s {source = a} :: Build)

-- | The Amazon Resource Name (ARN) of the build.
build_arn :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_arn = Lens.lens (\Build' {arn} -> arn) (\s@Build' {} a -> s {arn = a} :: Build)

-- | The name of the AWS CodeBuild project.
build_projectName :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_projectName = Lens.lens (\Build' {projectName} -> projectName) (\s@Build' {} a -> s {projectName = a} :: Build)

-- | When the build process ended, expressed in Unix time format.
build_endTime :: Lens.Lens' Build (Prelude.Maybe Prelude.UTCTime)
build_endTime = Lens.lens (\Build' {endTime} -> endTime) (\s@Build' {} a -> s {endTime = a} :: Build) Prelude.. Lens.mapping Prelude._Time

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

-- | Information about the build\'s logs in Amazon CloudWatch Logs.
build_logs :: Lens.Lens' Build (Prelude.Maybe LogsLocation)
build_logs = Lens.lens (\Build' {logs} -> logs) (\s@Build' {} a -> s {logs = a} :: Build)

-- | Whether the build is complete. True if complete; otherwise, false.
build_buildComplete :: Lens.Lens' Build (Prelude.Maybe Prelude.Bool)
build_buildComplete = Lens.lens (\Build' {buildComplete} -> buildComplete) (\s@Build' {} a -> s {buildComplete = a} :: Build)

-- | Contains information about the debug session for this build.
build_debugSession :: Lens.Lens' Build (Prelude.Maybe DebugSession)
build_debugSession = Lens.lens (\Build' {debugSession} -> debugSession) (\s@Build' {} a -> s {debugSession = a} :: Build)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
build_queuedTimeoutInMinutes :: Lens.Lens' Build (Prelude.Maybe Prelude.Int)
build_queuedTimeoutInMinutes = Lens.lens (\Build' {queuedTimeoutInMinutes} -> queuedTimeoutInMinutes) (\s@Build' {} a -> s {queuedTimeoutInMinutes = a} :: Build)

-- | An array of @ProjectSource@ objects.
build_secondarySources :: Lens.Lens' Build (Prelude.Maybe [ProjectSource])
build_secondarySources = Lens.lens (\Build' {secondarySources} -> secondarySources) (\s@Build' {} a -> s {secondarySources = a} :: Build) Prelude.. Lens.mapping Prelude._Coerce

-- | How long, in minutes, for AWS CodeBuild to wait before timing out this
-- build if it does not get marked as completed.
build_timeoutInMinutes :: Lens.Lens' Build (Prelude.Maybe Prelude.Int)
build_timeoutInMinutes = Lens.lens (\Build' {timeoutInMinutes} -> timeoutInMinutes) (\s@Build' {} a -> s {timeoutInMinutes = a} :: Build)

-- | The current build phase.
build_currentPhase :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_currentPhase = Lens.lens (\Build' {currentPhase} -> currentPhase) (\s@Build' {} a -> s {currentPhase = a} :: Build)

-- | The entity that started the build. Valid values include:
--
-- -   If AWS CodePipeline started the build, the pipeline\'s name (for
--     example, @codepipeline\/my-demo-pipeline@).
--
-- -   If an AWS Identity and Access Management (IAM) user started the
--     build, the user\'s name (for example, @MyUserName@).
--
-- -   If the Jenkins plugin for AWS CodeBuild started the build, the
--     string @CodeBuild-Jenkins-Plugin@.
build_initiator :: Lens.Lens' Build (Prelude.Maybe Prelude.Text)
build_initiator = Lens.lens (\Build' {initiator} -> initiator) (\s@Build' {} a -> s {initiator = a} :: Build)

-- | An array of the ARNs associated with this build\'s reports.
build_reportArns :: Lens.Lens' Build (Prelude.Maybe [Prelude.Text])
build_reportArns = Lens.lens (\Build' {reportArns} -> reportArns) (\s@Build' {} a -> s {reportArns = a} :: Build) Prelude.. Lens.mapping Prelude._Coerce

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build
-- project. A @ProjectFileSystemLocation@ object specifies the
-- @identifier@, @location@, @mountOptions@, @mountPoint@, and @type@ of a
-- file system created using Amazon Elastic File System.
build_fileSystemLocations :: Lens.Lens' Build (Prelude.Maybe [ProjectFileSystemLocation])
build_fileSystemLocations = Lens.lens (\Build' {fileSystemLocations} -> fileSystemLocations) (\s@Build' {} a -> s {fileSystemLocations = a} :: Build) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of exported environment variables for this build.
build_exportedEnvironmentVariables :: Lens.Lens' Build (Prelude.Maybe [ExportedEnvironmentVariable])
build_exportedEnvironmentVariables = Lens.lens (\Build' {exportedEnvironmentVariables} -> exportedEnvironmentVariables) (\s@Build' {} a -> s {exportedEnvironmentVariables = a} :: Build) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Build where
  parseJSON =
    Prelude.withObject
      "Build"
      ( \x ->
          Build'
            Prelude.<$> (x Prelude..:? "vpcConfig")
            Prelude.<*> (x Prelude..:? "buildBatchArn")
            Prelude.<*> (x Prelude..:? "resolvedSourceVersion")
            Prelude.<*> ( x Prelude..:? "secondaryArtifacts"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "sourceVersion")
            Prelude.<*> (x Prelude..:? "phases" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "cache")
            Prelude.<*> (x Prelude..:? "serviceRole")
            Prelude.<*> ( x Prelude..:? "secondarySourceVersions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "networkInterface")
            Prelude.<*> (x Prelude..:? "encryptionKey")
            Prelude.<*> (x Prelude..:? "artifacts")
            Prelude.<*> (x Prelude..:? "buildNumber")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "environment")
            Prelude.<*> (x Prelude..:? "source")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "projectName")
            Prelude.<*> (x Prelude..:? "endTime")
            Prelude.<*> (x Prelude..:? "buildStatus")
            Prelude.<*> (x Prelude..:? "logs")
            Prelude.<*> (x Prelude..:? "buildComplete")
            Prelude.<*> (x Prelude..:? "debugSession")
            Prelude.<*> (x Prelude..:? "queuedTimeoutInMinutes")
            Prelude.<*> ( x Prelude..:? "secondarySources"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "timeoutInMinutes")
            Prelude.<*> (x Prelude..:? "currentPhase")
            Prelude.<*> (x Prelude..:? "initiator")
            Prelude.<*> ( x Prelude..:? "reportArns"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "fileSystemLocations"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "exportedEnvironmentVariables"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Build

instance Prelude.NFData Build
