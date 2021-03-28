{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Build
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.Build
  ( Build (..)
  -- * Smart constructor
  , mkBuild
  -- * Lenses
  , bArn
  , bArtifacts
  , bBuildBatchArn
  , bBuildComplete
  , bBuildNumber
  , bBuildStatus
  , bCache
  , bCurrentPhase
  , bDebugSession
  , bEncryptionKey
  , bEndTime
  , bEnvironment
  , bExportedEnvironmentVariables
  , bFileSystemLocations
  , bId
  , bInitiator
  , bLogs
  , bNetworkInterface
  , bPhases
  , bProjectName
  , bQueuedTimeoutInMinutes
  , bReportArns
  , bResolvedSourceVersion
  , bSecondaryArtifacts
  , bSecondarySourceVersions
  , bSecondarySources
  , bServiceRole
  , bSource
  , bSourceVersion
  , bStartTime
  , bTimeoutInMinutes
  , bVpcConfig
  ) where

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
import qualified Network.AWS.CodeBuild.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a build.
--
-- /See:/ 'mkBuild' smart constructor.
data Build = Build'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the build.
  , artifacts :: Core.Maybe Types.BuildArtifacts
    -- ^ Information about the output artifacts for the build.
  , buildBatchArn :: Core.Maybe Core.Text
    -- ^ The ARN of the batch build that this build is a member of, if applicable.
  , buildComplete :: Core.Maybe Core.Bool
    -- ^ Whether the build is complete. True if complete; otherwise, false.
  , buildNumber :: Core.Maybe Core.Integer
    -- ^ The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
  , buildStatus :: Core.Maybe Types.StatusType
    -- ^ The current status of the build. Valid values include:
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
  , cache :: Core.Maybe Types.ProjectCache
    -- ^ Information about the cache for the build.
  , currentPhase :: Core.Maybe Core.Text
    -- ^ The current build phase.
  , debugSession :: Core.Maybe Types.DebugSession
    -- ^ Contains information about the debug session for this build.
  , encryptionKey :: Core.Maybe Types.EncryptionKey
    -- ^ The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the build process ended, expressed in Unix time format.
  , environment :: Core.Maybe Types.ProjectEnvironment
    -- ^ Information about the build environment for this build.
  , exportedEnvironmentVariables :: Core.Maybe [Types.ExportedEnvironmentVariable]
    -- ^ A list of exported environment variables for this build. 
  , fileSystemLocations :: Core.Maybe [Types.ProjectFileSystemLocation]
    -- ^ An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System. 
  , id :: Core.Maybe Types.Id
    -- ^ The unique ID for the build.
  , initiator :: Core.Maybe Core.Text
    -- ^ The entity that started the build. Valid values include:
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
  , logs :: Core.Maybe Types.LogsLocation
    -- ^ Information about the build's logs in Amazon CloudWatch Logs.
  , networkInterface :: Core.Maybe Types.NetworkInterface
    -- ^ Describes a network interface.
  , phases :: Core.Maybe [Types.BuildPhase]
    -- ^ Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
  , projectName :: Core.Maybe Types.NonEmptyString
    -- ^ The name of the AWS CodeBuild project.
  , queuedTimeoutInMinutes :: Core.Maybe Core.Int
    -- ^ The number of minutes a build is allowed to be queued before it times out. 
  , reportArns :: Core.Maybe [Core.Text]
    -- ^ An array of the ARNs associated with this build's reports. 
  , resolvedSourceVersion :: Core.Maybe Types.ResolvedSourceVersion
    -- ^ An identifier for the version of this build's source code. 
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
  , secondaryArtifacts :: Core.Maybe [Types.BuildArtifacts]
    -- ^ An array of @ProjectArtifacts@ objects. 
  , secondarySourceVersions :: Core.Maybe [Types.ProjectSourceVersion]
    -- ^ An array of @ProjectSourceVersion@ objects. Each @ProjectSourceVersion@ must be one of: 
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
  , secondarySources :: Core.Maybe [Types.ProjectSource]
    -- ^ An array of @ProjectSource@ objects. 
  , serviceRole :: Core.Maybe Types.ServiceRole
    -- ^ The name of a service role used for this build.
  , source :: Core.Maybe Types.ProjectSource
    -- ^ Information about the source code to be built.
  , sourceVersion :: Core.Maybe Types.SourceVersion
    -- ^ Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence. 
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ . 
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the build process started, expressed in Unix time format.
  , timeoutInMinutes :: Core.Maybe Core.Int
    -- ^ How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Build' value with any optional fields omitted.
mkBuild
    :: Build
mkBuild
  = Build'{arn = Core.Nothing, artifacts = Core.Nothing,
           buildBatchArn = Core.Nothing, buildComplete = Core.Nothing,
           buildNumber = Core.Nothing, buildStatus = Core.Nothing,
           cache = Core.Nothing, currentPhase = Core.Nothing,
           debugSession = Core.Nothing, encryptionKey = Core.Nothing,
           endTime = Core.Nothing, environment = Core.Nothing,
           exportedEnvironmentVariables = Core.Nothing,
           fileSystemLocations = Core.Nothing, id = Core.Nothing,
           initiator = Core.Nothing, logs = Core.Nothing,
           networkInterface = Core.Nothing, phases = Core.Nothing,
           projectName = Core.Nothing, queuedTimeoutInMinutes = Core.Nothing,
           reportArns = Core.Nothing, resolvedSourceVersion = Core.Nothing,
           secondaryArtifacts = Core.Nothing,
           secondarySourceVersions = Core.Nothing,
           secondarySources = Core.Nothing, serviceRole = Core.Nothing,
           source = Core.Nothing, sourceVersion = Core.Nothing,
           startTime = Core.Nothing, timeoutInMinutes = Core.Nothing,
           vpcConfig = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the build.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bArn :: Lens.Lens' Build (Core.Maybe Types.Arn)
bArn = Lens.field @"arn"
{-# INLINEABLE bArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Information about the output artifacts for the build.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bArtifacts :: Lens.Lens' Build (Core.Maybe Types.BuildArtifacts)
bArtifacts = Lens.field @"artifacts"
{-# INLINEABLE bArtifacts #-}
{-# DEPRECATED artifacts "Use generic-lens or generic-optics with 'artifacts' instead"  #-}

-- | The ARN of the batch build that this build is a member of, if applicable.
--
-- /Note:/ Consider using 'buildBatchArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildBatchArn :: Lens.Lens' Build (Core.Maybe Core.Text)
bBuildBatchArn = Lens.field @"buildBatchArn"
{-# INLINEABLE bBuildBatchArn #-}
{-# DEPRECATED buildBatchArn "Use generic-lens or generic-optics with 'buildBatchArn' instead"  #-}

-- | Whether the build is complete. True if complete; otherwise, false.
--
-- /Note:/ Consider using 'buildComplete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildComplete :: Lens.Lens' Build (Core.Maybe Core.Bool)
bBuildComplete = Lens.field @"buildComplete"
{-# INLINEABLE bBuildComplete #-}
{-# DEPRECATED buildComplete "Use generic-lens or generic-optics with 'buildComplete' instead"  #-}

-- | The number of the build. For each project, the @buildNumber@ of its first build is @1@ . The @buildNumber@ of each subsequent build is incremented by @1@ . If a build is deleted, the @buildNumber@ of other builds does not change.
--
-- /Note:/ Consider using 'buildNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBuildNumber :: Lens.Lens' Build (Core.Maybe Core.Integer)
bBuildNumber = Lens.field @"buildNumber"
{-# INLINEABLE bBuildNumber #-}
{-# DEPRECATED buildNumber "Use generic-lens or generic-optics with 'buildNumber' instead"  #-}

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
{-# INLINEABLE bBuildStatus #-}
{-# DEPRECATED buildStatus "Use generic-lens or generic-optics with 'buildStatus' instead"  #-}

-- | Information about the cache for the build.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCache :: Lens.Lens' Build (Core.Maybe Types.ProjectCache)
bCache = Lens.field @"cache"
{-# INLINEABLE bCache #-}
{-# DEPRECATED cache "Use generic-lens or generic-optics with 'cache' instead"  #-}

-- | The current build phase.
--
-- /Note:/ Consider using 'currentPhase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bCurrentPhase :: Lens.Lens' Build (Core.Maybe Core.Text)
bCurrentPhase = Lens.field @"currentPhase"
{-# INLINEABLE bCurrentPhase #-}
{-# DEPRECATED currentPhase "Use generic-lens or generic-optics with 'currentPhase' instead"  #-}

-- | Contains information about the debug session for this build.
--
-- /Note:/ Consider using 'debugSession' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDebugSession :: Lens.Lens' Build (Core.Maybe Types.DebugSession)
bDebugSession = Lens.field @"debugSession"
{-# INLINEABLE bDebugSession #-}
{-# DEPRECATED debugSession "Use generic-lens or generic-optics with 'debugSession' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEncryptionKey :: Lens.Lens' Build (Core.Maybe Types.EncryptionKey)
bEncryptionKey = Lens.field @"encryptionKey"
{-# INLINEABLE bEncryptionKey #-}
{-# DEPRECATED encryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead"  #-}

-- | When the build process ended, expressed in Unix time format.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEndTime :: Lens.Lens' Build (Core.Maybe Core.NominalDiffTime)
bEndTime = Lens.field @"endTime"
{-# INLINEABLE bEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Information about the build environment for this build.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bEnvironment :: Lens.Lens' Build (Core.Maybe Types.ProjectEnvironment)
bEnvironment = Lens.field @"environment"
{-# INLINEABLE bEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | A list of exported environment variables for this build. 
--
-- /Note:/ Consider using 'exportedEnvironmentVariables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bExportedEnvironmentVariables :: Lens.Lens' Build (Core.Maybe [Types.ExportedEnvironmentVariable])
bExportedEnvironmentVariables = Lens.field @"exportedEnvironmentVariables"
{-# INLINEABLE bExportedEnvironmentVariables #-}
{-# DEPRECATED exportedEnvironmentVariables "Use generic-lens or generic-optics with 'exportedEnvironmentVariables' instead"  #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System. 
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bFileSystemLocations :: Lens.Lens' Build (Core.Maybe [Types.ProjectFileSystemLocation])
bFileSystemLocations = Lens.field @"fileSystemLocations"
{-# INLINEABLE bFileSystemLocations #-}
{-# DEPRECATED fileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead"  #-}

-- | The unique ID for the build.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bId :: Lens.Lens' Build (Core.Maybe Types.Id)
bId = Lens.field @"id"
{-# INLINEABLE bId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

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
bInitiator :: Lens.Lens' Build (Core.Maybe Core.Text)
bInitiator = Lens.field @"initiator"
{-# INLINEABLE bInitiator #-}
{-# DEPRECATED initiator "Use generic-lens or generic-optics with 'initiator' instead"  #-}

-- | Information about the build's logs in Amazon CloudWatch Logs.
--
-- /Note:/ Consider using 'logs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLogs :: Lens.Lens' Build (Core.Maybe Types.LogsLocation)
bLogs = Lens.field @"logs"
{-# INLINEABLE bLogs #-}
{-# DEPRECATED logs "Use generic-lens or generic-optics with 'logs' instead"  #-}

-- | Describes a network interface.
--
-- /Note:/ Consider using 'networkInterface' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNetworkInterface :: Lens.Lens' Build (Core.Maybe Types.NetworkInterface)
bNetworkInterface = Lens.field @"networkInterface"
{-# INLINEABLE bNetworkInterface #-}
{-# DEPRECATED networkInterface "Use generic-lens or generic-optics with 'networkInterface' instead"  #-}

-- | Information about all previous build phases that are complete and information about any current build phase that is not yet complete.
--
-- /Note:/ Consider using 'phases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPhases :: Lens.Lens' Build (Core.Maybe [Types.BuildPhase])
bPhases = Lens.field @"phases"
{-# INLINEABLE bPhases #-}
{-# DEPRECATED phases "Use generic-lens or generic-optics with 'phases' instead"  #-}

-- | The name of the AWS CodeBuild project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bProjectName :: Lens.Lens' Build (Core.Maybe Types.NonEmptyString)
bProjectName = Lens.field @"projectName"
{-# INLINEABLE bProjectName #-}
{-# DEPRECATED projectName "Use generic-lens or generic-optics with 'projectName' instead"  #-}

-- | The number of minutes a build is allowed to be queued before it times out. 
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bQueuedTimeoutInMinutes :: Lens.Lens' Build (Core.Maybe Core.Int)
bQueuedTimeoutInMinutes = Lens.field @"queuedTimeoutInMinutes"
{-# INLINEABLE bQueuedTimeoutInMinutes #-}
{-# DEPRECATED queuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead"  #-}

-- | An array of the ARNs associated with this build's reports. 
--
-- /Note:/ Consider using 'reportArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bReportArns :: Lens.Lens' Build (Core.Maybe [Core.Text])
bReportArns = Lens.field @"reportArns"
{-# INLINEABLE bReportArns #-}
{-# DEPRECATED reportArns "Use generic-lens or generic-optics with 'reportArns' instead"  #-}

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
{-# INLINEABLE bResolvedSourceVersion #-}
{-# DEPRECATED resolvedSourceVersion "Use generic-lens or generic-optics with 'resolvedSourceVersion' instead"  #-}

-- | An array of @ProjectArtifacts@ objects. 
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecondaryArtifacts :: Lens.Lens' Build (Core.Maybe [Types.BuildArtifacts])
bSecondaryArtifacts = Lens.field @"secondaryArtifacts"
{-# INLINEABLE bSecondaryArtifacts #-}
{-# DEPRECATED secondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead"  #-}

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
{-# INLINEABLE bSecondarySourceVersions #-}
{-# DEPRECATED secondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead"  #-}

-- | An array of @ProjectSource@ objects. 
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSecondarySources :: Lens.Lens' Build (Core.Maybe [Types.ProjectSource])
bSecondarySources = Lens.field @"secondarySources"
{-# INLINEABLE bSecondarySources #-}
{-# DEPRECATED secondarySources "Use generic-lens or generic-optics with 'secondarySources' instead"  #-}

-- | The name of a service role used for this build.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bServiceRole :: Lens.Lens' Build (Core.Maybe Types.ServiceRole)
bServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE bServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | Information about the source code to be built.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSource :: Lens.Lens' Build (Core.Maybe Types.ProjectSource)
bSource = Lens.field @"source"
{-# INLINEABLE bSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

-- | Any version identifier for the version of the source code to be built. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence. 
--
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ . 
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSourceVersion :: Lens.Lens' Build (Core.Maybe Types.SourceVersion)
bSourceVersion = Lens.field @"sourceVersion"
{-# INLINEABLE bSourceVersion #-}
{-# DEPRECATED sourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead"  #-}

-- | When the build process started, expressed in Unix time format.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bStartTime :: Lens.Lens' Build (Core.Maybe Core.NominalDiffTime)
bStartTime = Lens.field @"startTime"
{-# INLINEABLE bStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | How long, in minutes, for AWS CodeBuild to wait before timing out this build if it does not get marked as completed.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bTimeoutInMinutes :: Lens.Lens' Build (Core.Maybe Core.Int)
bTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# INLINEABLE bTimeoutInMinutes #-}
{-# DEPRECATED timeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead"  #-}

-- | If your AWS CodeBuild project accesses resources in an Amazon VPC, you provide this parameter that identifies the VPC ID and the list of security group IDs and subnet IDs. The security groups and subnets must belong to the same VPC. You must provide at least one security group and one subnet ID.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVpcConfig :: Lens.Lens' Build (Core.Maybe Types.VpcConfig)
bVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE bVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.FromJSON Build where
        parseJSON
          = Core.withObject "Build" Core.$
              \ x ->
                Build' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "artifacts" Core.<*>
                    x Core..:? "buildBatchArn"
                    Core.<*> x Core..:? "buildComplete"
                    Core.<*> x Core..:? "buildNumber"
                    Core.<*> x Core..:? "buildStatus"
                    Core.<*> x Core..:? "cache"
                    Core.<*> x Core..:? "currentPhase"
                    Core.<*> x Core..:? "debugSession"
                    Core.<*> x Core..:? "encryptionKey"
                    Core.<*> x Core..:? "endTime"
                    Core.<*> x Core..:? "environment"
                    Core.<*> x Core..:? "exportedEnvironmentVariables"
                    Core.<*> x Core..:? "fileSystemLocations"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "initiator"
                    Core.<*> x Core..:? "logs"
                    Core.<*> x Core..:? "networkInterface"
                    Core.<*> x Core..:? "phases"
                    Core.<*> x Core..:? "projectName"
                    Core.<*> x Core..:? "queuedTimeoutInMinutes"
                    Core.<*> x Core..:? "reportArns"
                    Core.<*> x Core..:? "resolvedSourceVersion"
                    Core.<*> x Core..:? "secondaryArtifacts"
                    Core.<*> x Core..:? "secondarySourceVersions"
                    Core.<*> x Core..:? "secondarySources"
                    Core.<*> x Core..:? "serviceRole"
                    Core.<*> x Core..:? "source"
                    Core.<*> x Core..:? "sourceVersion"
                    Core.<*> x Core..:? "startTime"
                    Core.<*> x Core..:? "timeoutInMinutes"
                    Core.<*> x Core..:? "vpcConfig"
