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
    bArn,
    bArtifacts,
    bBuildBatchArn,
    bBuildComplete,
    bBuildNumber,
    bBuildStatus,
    bCache,
    bCurrentPhase,
    bDebugSession,
    bEncryptionKey,
    bEndTime,
    bEnvironment,
    bExportedEnvironmentVariables,
    bFileSystemLocations,
    bId,
    bInitiator,
    bLogs,
    bNetworkInterface,
    bPhases,
    bProjectName,
    bQueuedTimeoutInMinutes,
    bReportArns,
    bResolvedSourceVersion,
    bSecondaryArtifacts,
    bSecondarySourceVersions,
    bSecondarySources,
    bServiceRole,
    bSource,
    bSourceVersion,
    bStartTime,
    bTimeoutInMinutes,
    bVpcConfig,
  )
where

import qualified Network.AWS.CodeBuild.Types.Arn as Types
import qualified Network.AWS.CodeBuild.Types.BuildArtifacts as Types
import qualified Network.AWS.CodeBuild.Types.BuildPhase as Types
import qualified Network.AWS.CodeBuild.Types.DebugSession as Types
import qualified Network.AWS.CodeBuild.Types.EncryptionKey as Types
import qualified Network.AWS.CodeBuild.Types.ExportedEnvironmentVariable as Types
import qualified Network.AWS.CodeBuild.Types.Id as Types
import qualified Network.AWS.CodeBuild.Types.LogsLocation as Types
import qualified Network.AWS.CodeBuild.Types.NetworkInterface as Types
import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.CodeBuild.Types.ProjectCache as Types
import qualified Network.AWS.CodeBuild.Types.ProjectEnvironment as Types
import qualified Network.AWS.CodeBuild.Types.ProjectFileSystemLocation as Types
import qualified Network.AWS.CodeBuild.Types.ProjectSource as Types
import qualified Network.AWS.CodeBuild.Types.ProjectSourceVersion as Types
import qualified Network.AWS.CodeBuild.Types.ResolvedSourceVersion as Types
import qualified Network.AWS.CodeBuild.Types.ServiceRole as Types
import qualified Network.AWS.CodeBuild.Types.SourceVersion as Types
import qualified Network.AWS.CodeBuild.Types.StatusType as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.CodeBuild.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a build.
--
-- /See:/ 'mkBuild' smart constructor.
data Build = Build'
  { -- | The Amazon Resource Name (ARN) of the build.
    arn :: Core.Maybe Types.Arn,
    -- | Information about the output artifacts for the build.
    artifacts :: Core.Maybe Types.BuildArtifacts,
    -- | The ARN of the batch build that this build is a member of, if applicable.
    buildBatchArn :: Core.Maybe Types.String,
    -- | Whether the build is complete. True if complete; otherwise, false.
    buildComplete :: Core.Maybe Core.Bool,
    -- | The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
    buildNumber :: Core.Maybe Core.Integer,
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
    buildStatus :: Core.Maybe Types.StatusType,
    -- | Information about the cache for the build.
    cache :: Core.Maybe Types.ProjectCache,
    -- | The current build phase.
    currentPhase :: Core.Maybe Types.String,
    -- | Contains information about the debug session for this build.
    debugSession :: Core.Maybe Types.DebugSession,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKey :: Core.Maybe Types.EncryptionKey,
    -- | When the build process ended, expressed in Unix time format.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Information about the build environment for this build.
    environment :: Core.Maybe Types.ProjectEnvironment,
    -- | A list of exported environment variables for this build.
    exportedEnvironmentVariables :: Core.Maybe [Types.ExportedEnvironmentVariable],
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
    fileSystemLocations :: Core.Maybe [Types.ProjectFileSystemLocation],
    -- | The unique ID for the build.
    id :: Core.Maybe Types.Id,
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
    initiator :: Core.Maybe Types.String,
    -- | Information about the build's logs in Amazon CloudWatch Logs.
    logs :: Core.Maybe Types.LogsLocation,
    -- | Describes a network interface.
    networkInterface :: Core.Maybe Types.NetworkInterface,
    -- | Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
    phases :: Core.Maybe [Types.BuildPhase],
    -- | The name of the AWS CodeBuild project.
    projectName :: Core.Maybe Types.NonEmptyString,
    -- | The number of minutes a build is allowed to be queued before it times out.
    queuedTimeoutInMinutes :: Core.Maybe Core.Int,
    -- | An array of the ARNs associated with this build's reports.
    reportArns :: Core.Maybe [Types.String],
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
    resolvedSourceVersion :: Core.Maybe Types.ResolvedSourceVersion,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Core.Maybe [Types.BuildArtifacts],
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
    secondarySourceVersions :: Core.Maybe [Types.ProjectSourceVersion],
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Core.Maybe [Types.ProjectSource],
    -- | The name of a service role used for this build.
    serviceRole :: Core.Maybe Types.ServiceRole,
    -- | Information about the source code to be built.
    source :: Core.Maybe Types.ProjectSource,
    -- | Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.
    --
    -- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
    sourceVersion :: Core.Maybe Types.SourceVersion,
    -- | When the build process started, expressed in Unix time format.
    startTime :: Core.Maybe Core.NominalDiffTime,
    -- | How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
    timeoutInMinutes :: Core.Maybe Core.Int,
    -- | If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Build' value with any optional fields omitted.
mkBuild ::
  Build
mkBuild =
  Build'
    { arn = Core.Nothing,
      artifacts = Core.Nothing,
      buildBatchArn = Core.Nothing,
      buildComplete = Core.Nothing,
      buildNumber = Core.Nothing,
      buildStatus = Core.Nothing,
      cache = Core.Nothing,
      currentPhase = Core.Nothing,
      debugSession = Core.Nothing,
      encryptionKey = Core.Nothing,
      endTime = Core.Nothing,
      environment = Core.Nothing,
      exportedEnvironmentVariables = Core.Nothing,
      fileSystemLocations = Core.Nothing,
      id = Core.Nothing,
      initiator = Core.Nothing,
      logs = Core.Nothing,
      networkInterface = Core.Nothing,
      phases = Core.Nothing,
      projectName = Core.Nothing,
      queuedTimeoutInMinutes = Core.Nothing,
      reportArns = Core.Nothing,
      resolvedSourceVersion = Core.Nothing,
      secondaryArtifacts = Core.Nothing,
      secondarySourceVersions = Core.Nothing,
      secondarySources = Core.Nothing,
      serviceRole = Core.Nothing,
      source = Core.Nothing,
      sourceVersion = Core.Nothing,
      startTime = Core.Nothing,
      timeoutInMinutes = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the build.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bArn :: Lens.Lens' Build (Core.Maybe Types.Arn)
bArn = Lens.field @"arn"
{-# DEPRECATED bArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the output artifacts for the build.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bArtifacts :: Lens.Lens' Build (Core.Maybe Types.BuildArtifacts)
bArtifacts = Lens.field @"artifacts"
{-# DEPRECATED bArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | The ARN of the batch build that this build is a member of, if applicable.
--
-- /Note:/ Consider using 'buildBatchArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildBatchArn :: Lens.Lens' Build (Core.Maybe Types.String)
bBuildBatchArn = Lens.field @"buildBatchArn"
{-# DEPRECATED bBuildBatchArn "Use generic-lens or generic-optics with 'buildBatchArn' instead." #-}

-- | Whether the build is complete. True if complete; otherwise, false.
--
-- /Note:/ Consider using 'buildComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildComplete :: Lens.Lens' Build (Core.Maybe Core.Bool)
bBuildComplete = Lens.field @"buildComplete"
{-# DEPRECATED bBuildComplete "Use generic-lens or generic-optics with 'buildComplete' instead." #-}

-- | The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
--
-- /Note:/ Consider using 'buildNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildNumber :: Lens.Lens' Build (Core.Maybe Core.Integer)
bBuildNumber = Lens.field @"buildNumber"
{-# DEPRECATED bBuildNumber "Use generic-lens or generic-optics with 'buildNumber' instead." #-}

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
bBuildStatus :: Lens.Lens' Build (Core.Maybe Types.StatusType)
bBuildStatus = Lens.field @"buildStatus"
{-# DEPRECATED bBuildStatus "Use generic-lens or generic-optics with 'buildStatus' instead." #-}

-- | Information about the cache for the build.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCache :: Lens.Lens' Build (Core.Maybe Types.ProjectCache)
bCache = Lens.field @"cache"
{-# DEPRECATED bCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | The current build phase.
--
-- /Note:/ Consider using 'currentPhase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCurrentPhase :: Lens.Lens' Build (Core.Maybe Types.String)
bCurrentPhase = Lens.field @"currentPhase"
{-# DEPRECATED bCurrentPhase "Use generic-lens or generic-optics with 'currentPhase' instead." #-}

-- | Contains information about the debug session for this build.
--
-- /Note:/ Consider using 'debugSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDebugSession :: Lens.Lens' Build (Core.Maybe Types.DebugSession)
bDebugSession = Lens.field @"debugSession"
{-# DEPRECATED bDebugSession "Use generic-lens or generic-optics with 'debugSession' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEncryptionKey :: Lens.Lens' Build (Core.Maybe Types.EncryptionKey)
bEncryptionKey = Lens.field @"encryptionKey"
{-# DEPRECATED bEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | When the build process ended, expressed in Unix time format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEndTime :: Lens.Lens' Build (Core.Maybe Core.NominalDiffTime)
bEndTime = Lens.field @"endTime"
{-# DEPRECATED bEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Information about the build environment for this build.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEnvironment :: Lens.Lens' Build (Core.Maybe Types.ProjectEnvironment)
bEnvironment = Lens.field @"environment"
{-# DEPRECATED bEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | A list of exported environment variables for this build.
--
-- /Note:/ Consider using 'exportedEnvironmentVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bExportedEnvironmentVariables :: Lens.Lens' Build (Core.Maybe [Types.ExportedEnvironmentVariable])
bExportedEnvironmentVariables = Lens.field @"exportedEnvironmentVariables"
{-# DEPRECATED bExportedEnvironmentVariables "Use generic-lens or generic-optics with 'exportedEnvironmentVariables' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bFileSystemLocations :: Lens.Lens' Build (Core.Maybe [Types.ProjectFileSystemLocation])
bFileSystemLocations = Lens.field @"fileSystemLocations"
{-# DEPRECATED bFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | The unique ID for the build.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bId :: Lens.Lens' Build (Core.Maybe Types.Id)
bId = Lens.field @"id"
{-# DEPRECATED bId "Use generic-lens or generic-optics with 'id' instead." #-}

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
bInitiator :: Lens.Lens' Build (Core.Maybe Types.String)
bInitiator = Lens.field @"initiator"
{-# DEPRECATED bInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

-- | Information about the build's logs in Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLogs :: Lens.Lens' Build (Core.Maybe Types.LogsLocation)
bLogs = Lens.field @"logs"
{-# DEPRECATED bLogs "Use generic-lens or generic-optics with 'logs' instead." #-}

-- | Describes a network interface.
--
-- /Note:/ Consider using 'networkInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNetworkInterface :: Lens.Lens' Build (Core.Maybe Types.NetworkInterface)
bNetworkInterface = Lens.field @"networkInterface"
{-# DEPRECATED bNetworkInterface "Use generic-lens or generic-optics with 'networkInterface' instead." #-}

-- | Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
--
-- /Note:/ Consider using 'phases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPhases :: Lens.Lens' Build (Core.Maybe [Types.BuildPhase])
bPhases = Lens.field @"phases"
{-# DEPRECATED bPhases "Use generic-lens or generic-optics with 'phases' instead." #-}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bProjectName :: Lens.Lens' Build (Core.Maybe Types.NonEmptyString)
bProjectName = Lens.field @"projectName"
{-# DEPRECATED bProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bQueuedTimeoutInMinutes :: Lens.Lens' Build (Core.Maybe Core.Int)
bQueuedTimeoutInMinutes = Lens.field @"queuedTimeoutInMinutes"
{-# DEPRECATED bQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | An array of the ARNs associated with this build's reports.
--
-- /Note:/ Consider using 'reportArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bReportArns :: Lens.Lens' Build (Core.Maybe [Types.String])
bReportArns = Lens.field @"reportArns"
{-# DEPRECATED bReportArns "Use generic-lens or generic-optics with 'reportArns' instead." #-}

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
bResolvedSourceVersion :: Lens.Lens' Build (Core.Maybe Types.ResolvedSourceVersion)
bResolvedSourceVersion = Lens.field @"resolvedSourceVersion"
{-# DEPRECATED bResolvedSourceVersion "Use generic-lens or generic-optics with 'resolvedSourceVersion' instead." #-}

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecondaryArtifacts :: Lens.Lens' Build (Core.Maybe [Types.BuildArtifacts])
bSecondaryArtifacts = Lens.field @"secondaryArtifacts"
{-# DEPRECATED bSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

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
bSecondarySourceVersions :: Lens.Lens' Build (Core.Maybe [Types.ProjectSourceVersion])
bSecondarySourceVersions = Lens.field @"secondarySourceVersions"
{-# DEPRECATED bSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecondarySources :: Lens.Lens' Build (Core.Maybe [Types.ProjectSource])
bSecondarySources = Lens.field @"secondarySources"
{-# DEPRECATED bSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

-- | The name of a service role used for this build.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServiceRole :: Lens.Lens' Build (Core.Maybe Types.ServiceRole)
bServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED bServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Information about the source code to be built.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSource :: Lens.Lens' Build (Core.Maybe Types.ProjectSource)
bSource = Lens.field @"source"
{-# DEPRECATED bSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceVersion :: Lens.Lens' Build (Core.Maybe Types.SourceVersion)
bSourceVersion = Lens.field @"sourceVersion"
{-# DEPRECATED bSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | When the build process started, expressed in Unix time format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStartTime :: Lens.Lens' Build (Core.Maybe Core.NominalDiffTime)
bStartTime = Lens.field @"startTime"
{-# DEPRECATED bStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTimeoutInMinutes :: Lens.Lens' Build (Core.Maybe Core.Int)
bTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# DEPRECATED bTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVpcConfig :: Lens.Lens' Build (Core.Maybe Types.VpcConfig)
bVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED bVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON Build where
  parseJSON =
    Core.withObject "Build" Core.$
      \x ->
        Build'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "artifacts")
          Core.<*> (x Core..:? "buildBatchArn")
          Core.<*> (x Core..:? "buildComplete")
          Core.<*> (x Core..:? "buildNumber")
          Core.<*> (x Core..:? "buildStatus")
          Core.<*> (x Core..:? "cache")
          Core.<*> (x Core..:? "currentPhase")
          Core.<*> (x Core..:? "debugSession")
          Core.<*> (x Core..:? "encryptionKey")
          Core.<*> (x Core..:? "endTime")
          Core.<*> (x Core..:? "environment")
          Core.<*> (x Core..:? "exportedEnvironmentVariables")
          Core.<*> (x Core..:? "fileSystemLocations")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "initiator")
          Core.<*> (x Core..:? "logs")
          Core.<*> (x Core..:? "networkInterface")
          Core.<*> (x Core..:? "phases")
          Core.<*> (x Core..:? "projectName")
          Core.<*> (x Core..:? "queuedTimeoutInMinutes")
          Core.<*> (x Core..:? "reportArns")
          Core.<*> (x Core..:? "resolvedSourceVersion")
          Core.<*> (x Core..:? "secondaryArtifacts")
          Core.<*> (x Core..:? "secondarySourceVersions")
          Core.<*> (x Core..:? "secondarySources")
          Core.<*> (x Core..:? "serviceRole")
          Core.<*> (x Core..:? "source")
          Core.<*> (x Core..:? "sourceVersion")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "timeoutInMinutes")
          Core.<*> (x Core..:? "vpcConfig")
