{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Build
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Build
  ( Build (..),

    -- * Smart constructor
    mkBuild,

    -- * Lenses
    bPhases,
    bBuildComplete,
    bSecondaryArtifacts,
    bArn,
    bExportedEnvironmentVariables,
    bBuildNumber,
    bStartTime,
    bArtifacts,
    bEnvironment,
    bInitiator,
    bNetworkInterface,
    bSecondarySourceVersions,
    bCurrentPhase,
    bQueuedTimeoutInMinutes,
    bCache,
    bSecondarySources,
    bDebugSession,
    bSourceVersion,
    bBuildBatchARN,
    bLogs,
    bResolvedSourceVersion,
    bVpcConfig,
    bEndTime,
    bProjectName,
    bBuildStatus,
    bSource,
    bId,
    bFileSystemLocations,
    bReportARNs,
    bEncryptionKey,
    bServiceRole,
    bTimeoutInMinutes,
  )
where

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
import Network.AWS.CodeBuild.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a build.
--
-- /See:/ 'mkBuild' smart constructor.
data Build = Build'
  { phases :: Lude.Maybe [BuildPhase],
    buildComplete :: Lude.Maybe Lude.Bool,
    secondaryArtifacts :: Lude.Maybe [BuildArtifacts],
    arn :: Lude.Maybe Lude.Text,
    exportedEnvironmentVariables ::
      Lude.Maybe [ExportedEnvironmentVariable],
    buildNumber :: Lude.Maybe Lude.Integer,
    startTime :: Lude.Maybe Lude.Timestamp,
    artifacts :: Lude.Maybe BuildArtifacts,
    environment :: Lude.Maybe ProjectEnvironment,
    initiator :: Lude.Maybe Lude.Text,
    networkInterface :: Lude.Maybe NetworkInterface,
    secondarySourceVersions :: Lude.Maybe [ProjectSourceVersion],
    currentPhase :: Lude.Maybe Lude.Text,
    queuedTimeoutInMinutes :: Lude.Maybe Lude.Int,
    cache :: Lude.Maybe ProjectCache,
    secondarySources :: Lude.Maybe [ProjectSource],
    debugSession :: Lude.Maybe DebugSession,
    sourceVersion :: Lude.Maybe Lude.Text,
    buildBatchARN :: Lude.Maybe Lude.Text,
    logs :: Lude.Maybe LogsLocation,
    resolvedSourceVersion :: Lude.Maybe Lude.Text,
    vpcConfig :: Lude.Maybe VPCConfig,
    endTime :: Lude.Maybe Lude.Timestamp,
    projectName :: Lude.Maybe Lude.Text,
    buildStatus :: Lude.Maybe StatusType,
    source :: Lude.Maybe ProjectSource,
    id :: Lude.Maybe Lude.Text,
    fileSystemLocations :: Lude.Maybe [ProjectFileSystemLocation],
    reportARNs :: Lude.Maybe [Lude.Text],
    encryptionKey :: Lude.Maybe Lude.Text,
    serviceRole :: Lude.Maybe Lude.Text,
    timeoutInMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Build' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the build.
-- * 'artifacts' - Information about the output artifacts for the build.
-- * 'buildBatchARN' - The ARN of the batch build that this build is a member of, if applicable.
-- * 'buildComplete' - Whether the build is complete. True if complete; otherwise, false.
-- * 'buildNumber' - The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
-- * 'buildStatus' - The current status of the build. Valid values include:
--
--
--     * @FAILED@ : The build failed.
--
--
--     * @FAULT@ : The build faulted.
--
--
--     * @IN_PROGRESS@ : The build is still in progress.
--
--
--     * @STOPPED@ : The build stopped.
--
--
--     * @SUCCEEDED@ : The build succeeded.
--
--
--     * @TIMED_OUT@ : The build timed out.
--
--
-- * 'cache' - Information about the cache for the build.
-- * 'currentPhase' - The current build phase.
-- * 'debugSession' - Contains information about the debug session for this build.
-- * 'encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
-- * 'endTime' - When the build process ended, expressed in Unix time format.
-- * 'environment' - Information about the build environment for this build.
-- * 'exportedEnvironmentVariables' - A list of exported environment variables for this build.
-- * 'fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
-- * 'id' - The unique ID for the build.
-- * 'initiator' - The entity that started the build. Valid values include:
--
--
--     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).
--
--
--     * If an AWS Identity and Access Management (IAM) user started the build, the user's name (for example, @MyUserName@ ).
--
--
--     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
--
--
-- * 'logs' - Information about the build's logs in Amazon CloudWatch Logs.
-- * 'networkInterface' - Describes a network interface.
-- * 'phases' - Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
-- * 'projectName' - The name of the AWS CodeBuild project.
-- * 'queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times out.
-- * 'reportARNs' - An array of the ARNs associated with this build's reports.
-- * 'resolvedSourceVersion' - An identifier for the version of this build's source code.
--
--
--     * For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit ID.
--
--
--     * For AWS CodePipeline, the source revision provided by AWS CodePipeline.
--
--
--     * For Amazon Simple Storage Service (Amazon S3), this does not apply.
--
--
-- * 'secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
-- * 'secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@ must be one of:
--
--
--     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.
--
--
--     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
--
--
-- * 'secondarySources' - An array of @ProjectSource@ objects.
-- * 'serviceRole' - The name of a service role used for this build.
-- * 'source' - Information about the source code to be built.
-- * 'sourceVersion' - Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
-- * 'startTime' - When the build process started, expressed in Unix time format.
-- * 'timeoutInMinutes' - How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
-- * 'vpcConfig' - If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
mkBuild ::
  Build
mkBuild =
  Build'
    { phases = Lude.Nothing,
      buildComplete = Lude.Nothing,
      secondaryArtifacts = Lude.Nothing,
      arn = Lude.Nothing,
      exportedEnvironmentVariables = Lude.Nothing,
      buildNumber = Lude.Nothing,
      startTime = Lude.Nothing,
      artifacts = Lude.Nothing,
      environment = Lude.Nothing,
      initiator = Lude.Nothing,
      networkInterface = Lude.Nothing,
      secondarySourceVersions = Lude.Nothing,
      currentPhase = Lude.Nothing,
      queuedTimeoutInMinutes = Lude.Nothing,
      cache = Lude.Nothing,
      secondarySources = Lude.Nothing,
      debugSession = Lude.Nothing,
      sourceVersion = Lude.Nothing,
      buildBatchARN = Lude.Nothing,
      logs = Lude.Nothing,
      resolvedSourceVersion = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      endTime = Lude.Nothing,
      projectName = Lude.Nothing,
      buildStatus = Lude.Nothing,
      source = Lude.Nothing,
      id = Lude.Nothing,
      fileSystemLocations = Lude.Nothing,
      reportARNs = Lude.Nothing,
      encryptionKey = Lude.Nothing,
      serviceRole = Lude.Nothing,
      timeoutInMinutes = Lude.Nothing
    }

-- | Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
--
-- /Note:/ Consider using 'phases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPhases :: Lens.Lens' Build (Lude.Maybe [BuildPhase])
bPhases = Lens.lens (phases :: Build -> Lude.Maybe [BuildPhase]) (\s a -> s {phases = a} :: Build)
{-# DEPRECATED bPhases "Use generic-lens or generic-optics with 'phases' instead." #-}

-- | Whether the build is complete. True if complete; otherwise, false.
--
-- /Note:/ Consider using 'buildComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildComplete :: Lens.Lens' Build (Lude.Maybe Lude.Bool)
bBuildComplete = Lens.lens (buildComplete :: Build -> Lude.Maybe Lude.Bool) (\s a -> s {buildComplete = a} :: Build)
{-# DEPRECATED bBuildComplete "Use generic-lens or generic-optics with 'buildComplete' instead." #-}

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecondaryArtifacts :: Lens.Lens' Build (Lude.Maybe [BuildArtifacts])
bSecondaryArtifacts = Lens.lens (secondaryArtifacts :: Build -> Lude.Maybe [BuildArtifacts]) (\s a -> s {secondaryArtifacts = a} :: Build)
{-# DEPRECATED bSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | The Amazon Resource Name (ARN) of the build.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bArn :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bArn = Lens.lens (arn :: Build -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Build)
{-# DEPRECATED bArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A list of exported environment variables for this build.
--
-- /Note:/ Consider using 'exportedEnvironmentVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bExportedEnvironmentVariables :: Lens.Lens' Build (Lude.Maybe [ExportedEnvironmentVariable])
bExportedEnvironmentVariables = Lens.lens (exportedEnvironmentVariables :: Build -> Lude.Maybe [ExportedEnvironmentVariable]) (\s a -> s {exportedEnvironmentVariables = a} :: Build)
{-# DEPRECATED bExportedEnvironmentVariables "Use generic-lens or generic-optics with 'exportedEnvironmentVariables' instead." #-}

-- | The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
--
-- /Note:/ Consider using 'buildNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildNumber :: Lens.Lens' Build (Lude.Maybe Lude.Integer)
bBuildNumber = Lens.lens (buildNumber :: Build -> Lude.Maybe Lude.Integer) (\s a -> s {buildNumber = a} :: Build)
{-# DEPRECATED bBuildNumber "Use generic-lens or generic-optics with 'buildNumber' instead." #-}

-- | When the build process started, expressed in Unix time format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStartTime :: Lens.Lens' Build (Lude.Maybe Lude.Timestamp)
bStartTime = Lens.lens (startTime :: Build -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: Build)
{-# DEPRECATED bStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Information about the output artifacts for the build.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bArtifacts :: Lens.Lens' Build (Lude.Maybe BuildArtifacts)
bArtifacts = Lens.lens (artifacts :: Build -> Lude.Maybe BuildArtifacts) (\s a -> s {artifacts = a} :: Build)
{-# DEPRECATED bArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Information about the build environment for this build.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEnvironment :: Lens.Lens' Build (Lude.Maybe ProjectEnvironment)
bEnvironment = Lens.lens (environment :: Build -> Lude.Maybe ProjectEnvironment) (\s a -> s {environment = a} :: Build)
{-# DEPRECATED bEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The entity that started the build. Valid values include:
--
--
--     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).
--
--
--     * If an AWS Identity and Access Management (IAM) user started the build, the user's name (for example, @MyUserName@ ).
--
--
--     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
--
--
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bInitiator :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bInitiator = Lens.lens (initiator :: Build -> Lude.Maybe Lude.Text) (\s a -> s {initiator = a} :: Build)
{-# DEPRECATED bInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

-- | Describes a network interface.
--
-- /Note:/ Consider using 'networkInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNetworkInterface :: Lens.Lens' Build (Lude.Maybe NetworkInterface)
bNetworkInterface = Lens.lens (networkInterface :: Build -> Lude.Maybe NetworkInterface) (\s a -> s {networkInterface = a} :: Build)
{-# DEPRECATED bNetworkInterface "Use generic-lens or generic-optics with 'networkInterface' instead." #-}

-- | An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@ must be one of:
--
--
--     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.
--
--
--     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example, @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
--
--
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecondarySourceVersions :: Lens.Lens' Build (Lude.Maybe [ProjectSourceVersion])
bSecondarySourceVersions = Lens.lens (secondarySourceVersions :: Build -> Lude.Maybe [ProjectSourceVersion]) (\s a -> s {secondarySourceVersions = a} :: Build)
{-# DEPRECATED bSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | The current build phase.
--
-- /Note:/ Consider using 'currentPhase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCurrentPhase :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bCurrentPhase = Lens.lens (currentPhase :: Build -> Lude.Maybe Lude.Text) (\s a -> s {currentPhase = a} :: Build)
{-# DEPRECATED bCurrentPhase "Use generic-lens or generic-optics with 'currentPhase' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bQueuedTimeoutInMinutes :: Lens.Lens' Build (Lude.Maybe Lude.Int)
bQueuedTimeoutInMinutes = Lens.lens (queuedTimeoutInMinutes :: Build -> Lude.Maybe Lude.Int) (\s a -> s {queuedTimeoutInMinutes = a} :: Build)
{-# DEPRECATED bQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | Information about the cache for the build.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCache :: Lens.Lens' Build (Lude.Maybe ProjectCache)
bCache = Lens.lens (cache :: Build -> Lude.Maybe ProjectCache) (\s a -> s {cache = a} :: Build)
{-# DEPRECATED bCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecondarySources :: Lens.Lens' Build (Lude.Maybe [ProjectSource])
bSecondarySources = Lens.lens (secondarySources :: Build -> Lude.Maybe [ProjectSource]) (\s a -> s {secondarySources = a} :: Build)
{-# DEPRECATED bSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

-- | Contains information about the debug session for this build.
--
-- /Note:/ Consider using 'debugSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDebugSession :: Lens.Lens' Build (Lude.Maybe DebugSession)
bDebugSession = Lens.lens (debugSession :: Build -> Lude.Maybe DebugSession) (\s a -> s {debugSession = a} :: Build)
{-# DEPRECATED bDebugSession "Use generic-lens or generic-optics with 'debugSession' instead." #-}

-- | Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceVersion :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bSourceVersion = Lens.lens (sourceVersion :: Build -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: Build)
{-# DEPRECATED bSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | The ARN of the batch build that this build is a member of, if applicable.
--
-- /Note:/ Consider using 'buildBatchARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildBatchARN :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bBuildBatchARN = Lens.lens (buildBatchARN :: Build -> Lude.Maybe Lude.Text) (\s a -> s {buildBatchARN = a} :: Build)
{-# DEPRECATED bBuildBatchARN "Use generic-lens or generic-optics with 'buildBatchARN' instead." #-}

-- | Information about the build's logs in Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLogs :: Lens.Lens' Build (Lude.Maybe LogsLocation)
bLogs = Lens.lens (logs :: Build -> Lude.Maybe LogsLocation) (\s a -> s {logs = a} :: Build)
{-# DEPRECATED bLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | An identifier for the version of this build's source code.
--
--
--     * For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit ID.
--
--
--     * For AWS CodePipeline, the source revision provided by AWS CodePipeline.
--
--
--     * For Amazon Simple Storage Service (Amazon S3), this does not apply.
--
--
--
-- /Note:/ Consider using 'resolvedSourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bResolvedSourceVersion :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bResolvedSourceVersion = Lens.lens (resolvedSourceVersion :: Build -> Lude.Maybe Lude.Text) (\s a -> s {resolvedSourceVersion = a} :: Build)
{-# DEPRECATED bResolvedSourceVersion "Use generic-lens or generic-optics with 'resolvedSourceVersion' instead." #-}

-- | If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVpcConfig :: Lens.Lens' Build (Lude.Maybe VPCConfig)
bVpcConfig = Lens.lens (vpcConfig :: Build -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: Build)
{-# DEPRECATED bVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | When the build process ended, expressed in Unix time format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEndTime :: Lens.Lens' Build (Lude.Maybe Lude.Timestamp)
bEndTime = Lens.lens (endTime :: Build -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: Build)
{-# DEPRECATED bEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bProjectName :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bProjectName = Lens.lens (projectName :: Build -> Lude.Maybe Lude.Text) (\s a -> s {projectName = a} :: Build)
{-# DEPRECATED bProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | The current status of the build. Valid values include:
--
--
--     * @FAILED@ : The build failed.
--
--
--     * @FAULT@ : The build faulted.
--
--
--     * @IN_PROGRESS@ : The build is still in progress.
--
--
--     * @STOPPED@ : The build stopped.
--
--
--     * @SUCCEEDED@ : The build succeeded.
--
--
--     * @TIMED_OUT@ : The build timed out.
--
--
--
-- /Note:/ Consider using 'buildStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildStatus :: Lens.Lens' Build (Lude.Maybe StatusType)
bBuildStatus = Lens.lens (buildStatus :: Build -> Lude.Maybe StatusType) (\s a -> s {buildStatus = a} :: Build)
{-# DEPRECATED bBuildStatus "Use generic-lens or generic-optics with 'buildStatus' instead." #-}

-- | Information about the source code to be built.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSource :: Lens.Lens' Build (Lude.Maybe ProjectSource)
bSource = Lens.lens (source :: Build -> Lude.Maybe ProjectSource) (\s a -> s {source = a} :: Build)
{-# DEPRECATED bSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The unique ID for the build.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bId :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bId = Lens.lens (id :: Build -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Build)
{-# DEPRECATED bId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bFileSystemLocations :: Lens.Lens' Build (Lude.Maybe [ProjectFileSystemLocation])
bFileSystemLocations = Lens.lens (fileSystemLocations :: Build -> Lude.Maybe [ProjectFileSystemLocation]) (\s a -> s {fileSystemLocations = a} :: Build)
{-# DEPRECATED bFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | An array of the ARNs associated with this build's reports.
--
-- /Note:/ Consider using 'reportARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bReportARNs :: Lens.Lens' Build (Lude.Maybe [Lude.Text])
bReportARNs = Lens.lens (reportARNs :: Build -> Lude.Maybe [Lude.Text]) (\s a -> s {reportARNs = a} :: Build)
{-# DEPRECATED bReportARNs "Use generic-lens or generic-optics with 'reportARNs' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEncryptionKey :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bEncryptionKey = Lens.lens (encryptionKey :: Build -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKey = a} :: Build)
{-# DEPRECATED bEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | The name of a service role used for this build.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServiceRole :: Lens.Lens' Build (Lude.Maybe Lude.Text)
bServiceRole = Lens.lens (serviceRole :: Build -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: Build)
{-# DEPRECATED bServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTimeoutInMinutes :: Lens.Lens' Build (Lude.Maybe Lude.Int)
bTimeoutInMinutes = Lens.lens (timeoutInMinutes :: Build -> Lude.Maybe Lude.Int) (\s a -> s {timeoutInMinutes = a} :: Build)
{-# DEPRECATED bTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

instance Lude.FromJSON Build where
  parseJSON =
    Lude.withObject
      "Build"
      ( \x ->
          Build'
            Lude.<$> (x Lude..:? "phases" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "buildComplete")
            Lude.<*> (x Lude..:? "secondaryArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "exportedEnvironmentVariables" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "buildNumber")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "artifacts")
            Lude.<*> (x Lude..:? "environment")
            Lude.<*> (x Lude..:? "initiator")
            Lude.<*> (x Lude..:? "networkInterface")
            Lude.<*> (x Lude..:? "secondarySourceVersions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "currentPhase")
            Lude.<*> (x Lude..:? "queuedTimeoutInMinutes")
            Lude.<*> (x Lude..:? "cache")
            Lude.<*> (x Lude..:? "secondarySources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "debugSession")
            Lude.<*> (x Lude..:? "sourceVersion")
            Lude.<*> (x Lude..:? "buildBatchArn")
            Lude.<*> (x Lude..:? "logs")
            Lude.<*> (x Lude..:? "resolvedSourceVersion")
            Lude.<*> (x Lude..:? "vpcConfig")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "projectName")
            Lude.<*> (x Lude..:? "buildStatus")
            Lude.<*> (x Lude..:? "source")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "fileSystemLocations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "reportArns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "encryptionKey")
            Lude.<*> (x Lude..:? "serviceRole")
            Lude.<*> (x Lude..:? "timeoutInMinutes")
      )
