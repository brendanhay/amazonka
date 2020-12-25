{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatch
  ( BuildBatch (..),

    -- * Smart constructor
    mkBuildBatch,

    -- * Lenses
    bbArn,
    bbArtifacts,
    bbBuildBatchConfig,
    bbBuildBatchNumber,
    bbBuildBatchStatus,
    bbBuildGroups,
    bbBuildTimeoutInMinutes,
    bbCache,
    bbComplete,
    bbCurrentPhase,
    bbEncryptionKey,
    bbEndTime,
    bbEnvironment,
    bbFileSystemLocations,
    bbId,
    bbInitiator,
    bbLogConfig,
    bbPhases,
    bbProjectName,
    bbQueuedTimeoutInMinutes,
    bbResolvedSourceVersion,
    bbSecondaryArtifacts,
    bbSecondarySourceVersions,
    bbSecondarySources,
    bbServiceRole,
    bbSource,
    bbSourceVersion,
    bbStartTime,
    bbVpcConfig,
  )
where

import qualified Network.AWS.CodeBuild.Types.BuildArtifacts as Types
import qualified Network.AWS.CodeBuild.Types.BuildBatchPhase as Types
import qualified Network.AWS.CodeBuild.Types.BuildGroup as Types
import qualified Network.AWS.CodeBuild.Types.LogsConfig as Types
import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig as Types
import qualified Network.AWS.CodeBuild.Types.ProjectCache as Types
import qualified Network.AWS.CodeBuild.Types.ProjectEnvironment as Types
import qualified Network.AWS.CodeBuild.Types.ProjectFileSystemLocation as Types
import qualified Network.AWS.CodeBuild.Types.ProjectSource as Types
import qualified Network.AWS.CodeBuild.Types.ProjectSourceVersion as Types
import qualified Network.AWS.CodeBuild.Types.StatusType as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.CodeBuild.Types.VpcConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a batch build.
--
-- /See:/ 'mkBuildBatch' smart constructor.
data BuildBatch = BuildBatch'
  { -- | The ARN of the batch build.
    arn :: Core.Maybe Types.NonEmptyString,
    -- | A @BuildArtifacts@ object the defines the build artifacts for this batch build.
    artifacts :: Core.Maybe Types.BuildArtifacts,
    buildBatchConfig :: Core.Maybe Types.ProjectBuildBatchConfig,
    -- | The number of the batch build. For each project, the @buildBatchNumber@ of its first batch build is @1@ . The @buildBatchNumber@ of each subsequent batch build is incremented by @1@ . If a batch build is deleted, the @buildBatchNumber@ of other batch builds does not change.
    buildBatchNumber :: Core.Maybe Core.Integer,
    -- | The status of the batch build.
    buildBatchStatus :: Core.Maybe Types.StatusType,
    -- | An array of @BuildGroup@ objects that define the build groups for the batch build.
    buildGroups :: Core.Maybe [Types.BuildGroup],
    -- | Specifies the maximum amount of time, in minutes, that the build in a batch must be completed in.
    buildTimeoutInMinutes :: Core.Maybe Core.Int,
    cache :: Core.Maybe Types.ProjectCache,
    -- | Indicates if the batch build is complete.
    complete :: Core.Maybe Core.Bool,
    -- | The current phase of the batch build.
    currentPhase :: Core.Maybe Types.String,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the batch build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKey :: Core.Maybe Types.NonEmptyString,
    -- | The date and time that the batch build ended.
    endTime :: Core.Maybe Core.NominalDiffTime,
    environment :: Core.Maybe Types.ProjectEnvironment,
    -- | An array of @ProjectFileSystemLocation@ objects for the batch build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
    fileSystemLocations :: Core.Maybe [Types.ProjectFileSystemLocation],
    -- | The identifier of the batch build.
    id :: Core.Maybe Types.NonEmptyString,
    -- | The entity that started the batch build. Valid values include:
    --
    --
    --     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).
    --
    --
    --     * If an AWS Identity and Access Management (IAM) user started the build, the user's name.
    --
    --
    --     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
    initiator :: Core.Maybe Types.String,
    logConfig :: Core.Maybe Types.LogsConfig,
    -- | An array of @BuildBatchPhase@ objects the specify the phases of the batch build.
    phases :: Core.Maybe [Types.BuildBatchPhase],
    -- | The name of the batch build project.
    projectName :: Core.Maybe Types.NonEmptyString,
    -- | Specifies the amount of time, in minutes, that the batch build is allowed to be queued before it times out.
    queuedTimeoutInMinutes :: Core.Maybe Core.Int,
    -- | The identifier of the resolved version of this batch build's source code.
    --
    --
    --     * For AWS CodeCommit, GitHub, GitHub Enterprise, and BitBucket, the commit ID.
    --
    --
    --     * For AWS CodePipeline, the source revision provided by AWS CodePipeline.
    --
    --
    --     * For Amazon Simple Storage Service (Amazon S3), this does not apply.
    resolvedSourceVersion :: Core.Maybe Types.NonEmptyString,
    -- | An array of @BuildArtifacts@ objects the define the build artifacts for this batch build.
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
    -- | An array of @ProjectSource@ objects that define the sources for the batch build.
    secondarySources :: Core.Maybe [Types.ProjectSource],
    -- | The name of a service role used for builds in the batch.
    serviceRole :: Core.Maybe Types.NonEmptyString,
    source :: Core.Maybe Types.ProjectSource,
    -- | The identifier of the version of the source code to be built.
    sourceVersion :: Core.Maybe Types.NonEmptyString,
    -- | The date and time that the batch build started.
    startTime :: Core.Maybe Core.NominalDiffTime,
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BuildBatch' value with any optional fields omitted.
mkBuildBatch ::
  BuildBatch
mkBuildBatch =
  BuildBatch'
    { arn = Core.Nothing,
      artifacts = Core.Nothing,
      buildBatchConfig = Core.Nothing,
      buildBatchNumber = Core.Nothing,
      buildBatchStatus = Core.Nothing,
      buildGroups = Core.Nothing,
      buildTimeoutInMinutes = Core.Nothing,
      cache = Core.Nothing,
      complete = Core.Nothing,
      currentPhase = Core.Nothing,
      encryptionKey = Core.Nothing,
      endTime = Core.Nothing,
      environment = Core.Nothing,
      fileSystemLocations = Core.Nothing,
      id = Core.Nothing,
      initiator = Core.Nothing,
      logConfig = Core.Nothing,
      phases = Core.Nothing,
      projectName = Core.Nothing,
      queuedTimeoutInMinutes = Core.Nothing,
      resolvedSourceVersion = Core.Nothing,
      secondaryArtifacts = Core.Nothing,
      secondarySourceVersions = Core.Nothing,
      secondarySources = Core.Nothing,
      serviceRole = Core.Nothing,
      source = Core.Nothing,
      sourceVersion = Core.Nothing,
      startTime = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The ARN of the batch build.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbArn :: Lens.Lens' BuildBatch (Core.Maybe Types.NonEmptyString)
bbArn = Lens.field @"arn"
{-# DEPRECATED bbArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A @BuildArtifacts@ object the defines the build artifacts for this batch build.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbArtifacts :: Lens.Lens' BuildBatch (Core.Maybe Types.BuildArtifacts)
bbArtifacts = Lens.field @"artifacts"
{-# DEPRECATED bbArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildBatchConfig :: Lens.Lens' BuildBatch (Core.Maybe Types.ProjectBuildBatchConfig)
bbBuildBatchConfig = Lens.field @"buildBatchConfig"
{-# DEPRECATED bbBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | The number of the batch build. For each project, the @buildBatchNumber@ of its first batch build is @1@ . The @buildBatchNumber@ of each subsequent batch build is incremented by @1@ . If a batch build is deleted, the @buildBatchNumber@ of other batch builds does not change.
--
-- /Note:/ Consider using 'buildBatchNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildBatchNumber :: Lens.Lens' BuildBatch (Core.Maybe Core.Integer)
bbBuildBatchNumber = Lens.field @"buildBatchNumber"
{-# DEPRECATED bbBuildBatchNumber "Use generic-lens or generic-optics with 'buildBatchNumber' instead." #-}

-- | The status of the batch build.
--
-- /Note:/ Consider using 'buildBatchStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildBatchStatus :: Lens.Lens' BuildBatch (Core.Maybe Types.StatusType)
bbBuildBatchStatus = Lens.field @"buildBatchStatus"
{-# DEPRECATED bbBuildBatchStatus "Use generic-lens or generic-optics with 'buildBatchStatus' instead." #-}

-- | An array of @BuildGroup@ objects that define the build groups for the batch build.
--
-- /Note:/ Consider using 'buildGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildGroups :: Lens.Lens' BuildBatch (Core.Maybe [Types.BuildGroup])
bbBuildGroups = Lens.field @"buildGroups"
{-# DEPRECATED bbBuildGroups "Use generic-lens or generic-optics with 'buildGroups' instead." #-}

-- | Specifies the maximum amount of time, in minutes, that the build in a batch must be completed in.
--
-- /Note:/ Consider using 'buildTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildTimeoutInMinutes :: Lens.Lens' BuildBatch (Core.Maybe Core.Int)
bbBuildTimeoutInMinutes = Lens.field @"buildTimeoutInMinutes"
{-# DEPRECATED bbBuildTimeoutInMinutes "Use generic-lens or generic-optics with 'buildTimeoutInMinutes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbCache :: Lens.Lens' BuildBatch (Core.Maybe Types.ProjectCache)
bbCache = Lens.field @"cache"
{-# DEPRECATED bbCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | Indicates if the batch build is complete.
--
-- /Note:/ Consider using 'complete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbComplete :: Lens.Lens' BuildBatch (Core.Maybe Core.Bool)
bbComplete = Lens.field @"complete"
{-# DEPRECATED bbComplete "Use generic-lens or generic-optics with 'complete' instead." #-}

-- | The current phase of the batch build.
--
-- /Note:/ Consider using 'currentPhase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbCurrentPhase :: Lens.Lens' BuildBatch (Core.Maybe Types.String)
bbCurrentPhase = Lens.field @"currentPhase"
{-# DEPRECATED bbCurrentPhase "Use generic-lens or generic-optics with 'currentPhase' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the batch build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbEncryptionKey :: Lens.Lens' BuildBatch (Core.Maybe Types.NonEmptyString)
bbEncryptionKey = Lens.field @"encryptionKey"
{-# DEPRECATED bbEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | The date and time that the batch build ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbEndTime :: Lens.Lens' BuildBatch (Core.Maybe Core.NominalDiffTime)
bbEndTime = Lens.field @"endTime"
{-# DEPRECATED bbEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbEnvironment :: Lens.Lens' BuildBatch (Core.Maybe Types.ProjectEnvironment)
bbEnvironment = Lens.field @"environment"
{-# DEPRECATED bbEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for the batch build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbFileSystemLocations :: Lens.Lens' BuildBatch (Core.Maybe [Types.ProjectFileSystemLocation])
bbFileSystemLocations = Lens.field @"fileSystemLocations"
{-# DEPRECATED bbFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | The identifier of the batch build.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbId :: Lens.Lens' BuildBatch (Core.Maybe Types.NonEmptyString)
bbId = Lens.field @"id"
{-# DEPRECATED bbId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The entity that started the batch build. Valid values include:
--
--
--     * If AWS CodePipeline started the build, the pipeline's name (for example, @codepipeline/my-demo-pipeline@ ).
--
--
--     * If an AWS Identity and Access Management (IAM) user started the build, the user's name.
--
--
--     * If the Jenkins plugin for AWS CodeBuild started the build, the string @CodeBuild-Jenkins-Plugin@ .
--
--
--
-- /Note:/ Consider using 'initiator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbInitiator :: Lens.Lens' BuildBatch (Core.Maybe Types.String)
bbInitiator = Lens.field @"initiator"
{-# DEPRECATED bbInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbLogConfig :: Lens.Lens' BuildBatch (Core.Maybe Types.LogsConfig)
bbLogConfig = Lens.field @"logConfig"
{-# DEPRECATED bbLogConfig "Use generic-lens or generic-optics with 'logConfig' instead." #-}

-- | An array of @BuildBatchPhase@ objects the specify the phases of the batch build.
--
-- /Note:/ Consider using 'phases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbPhases :: Lens.Lens' BuildBatch (Core.Maybe [Types.BuildBatchPhase])
bbPhases = Lens.field @"phases"
{-# DEPRECATED bbPhases "Use generic-lens or generic-optics with 'phases' instead." #-}

-- | The name of the batch build project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbProjectName :: Lens.Lens' BuildBatch (Core.Maybe Types.NonEmptyString)
bbProjectName = Lens.field @"projectName"
{-# DEPRECATED bbProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | Specifies the amount of time, in minutes, that the batch build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbQueuedTimeoutInMinutes :: Lens.Lens' BuildBatch (Core.Maybe Core.Int)
bbQueuedTimeoutInMinutes = Lens.field @"queuedTimeoutInMinutes"
{-# DEPRECATED bbQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | The identifier of the resolved version of this batch build's source code.
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
bbResolvedSourceVersion :: Lens.Lens' BuildBatch (Core.Maybe Types.NonEmptyString)
bbResolvedSourceVersion = Lens.field @"resolvedSourceVersion"
{-# DEPRECATED bbResolvedSourceVersion "Use generic-lens or generic-optics with 'resolvedSourceVersion' instead." #-}

-- | An array of @BuildArtifacts@ objects the define the build artifacts for this batch build.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSecondaryArtifacts :: Lens.Lens' BuildBatch (Core.Maybe [Types.BuildArtifacts])
bbSecondaryArtifacts = Lens.field @"secondaryArtifacts"
{-# DEPRECATED bbSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

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
bbSecondarySourceVersions :: Lens.Lens' BuildBatch (Core.Maybe [Types.ProjectSourceVersion])
bbSecondarySourceVersions = Lens.field @"secondarySourceVersions"
{-# DEPRECATED bbSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | An array of @ProjectSource@ objects that define the sources for the batch build.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSecondarySources :: Lens.Lens' BuildBatch (Core.Maybe [Types.ProjectSource])
bbSecondarySources = Lens.field @"secondarySources"
{-# DEPRECATED bbSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

-- | The name of a service role used for builds in the batch.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbServiceRole :: Lens.Lens' BuildBatch (Core.Maybe Types.NonEmptyString)
bbServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED bbServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSource :: Lens.Lens' BuildBatch (Core.Maybe Types.ProjectSource)
bbSource = Lens.field @"source"
{-# DEPRECATED bbSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The identifier of the version of the source code to be built.
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSourceVersion :: Lens.Lens' BuildBatch (Core.Maybe Types.NonEmptyString)
bbSourceVersion = Lens.field @"sourceVersion"
{-# DEPRECATED bbSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | The date and time that the batch build started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbStartTime :: Lens.Lens' BuildBatch (Core.Maybe Core.NominalDiffTime)
bbStartTime = Lens.field @"startTime"
{-# DEPRECATED bbStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbVpcConfig :: Lens.Lens' BuildBatch (Core.Maybe Types.VpcConfig)
bbVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED bbVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON BuildBatch where
  parseJSON =
    Core.withObject "BuildBatch" Core.$
      \x ->
        BuildBatch'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "artifacts")
          Core.<*> (x Core..:? "buildBatchConfig")
          Core.<*> (x Core..:? "buildBatchNumber")
          Core.<*> (x Core..:? "buildBatchStatus")
          Core.<*> (x Core..:? "buildGroups")
          Core.<*> (x Core..:? "buildTimeoutInMinutes")
          Core.<*> (x Core..:? "cache")
          Core.<*> (x Core..:? "complete")
          Core.<*> (x Core..:? "currentPhase")
          Core.<*> (x Core..:? "encryptionKey")
          Core.<*> (x Core..:? "endTime")
          Core.<*> (x Core..:? "environment")
          Core.<*> (x Core..:? "fileSystemLocations")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "initiator")
          Core.<*> (x Core..:? "logConfig")
          Core.<*> (x Core..:? "phases")
          Core.<*> (x Core..:? "projectName")
          Core.<*> (x Core..:? "queuedTimeoutInMinutes")
          Core.<*> (x Core..:? "resolvedSourceVersion")
          Core.<*> (x Core..:? "secondaryArtifacts")
          Core.<*> (x Core..:? "secondarySourceVersions")
          Core.<*> (x Core..:? "secondarySources")
          Core.<*> (x Core..:? "serviceRole")
          Core.<*> (x Core..:? "source")
          Core.<*> (x Core..:? "sourceVersion")
          Core.<*> (x Core..:? "startTime")
          Core.<*> (x Core..:? "vpcConfig")
