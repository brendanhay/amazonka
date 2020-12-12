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
    bbPhases,
    bbSecondaryArtifacts,
    bbBuildTimeoutInMinutes,
    bbArn,
    bbStartTime,
    bbArtifacts,
    bbEnvironment,
    bbInitiator,
    bbSecondarySourceVersions,
    bbBuildBatchStatus,
    bbCurrentPhase,
    bbBuildBatchNumber,
    bbQueuedTimeoutInMinutes,
    bbCache,
    bbSecondarySources,
    bbSourceVersion,
    bbResolvedSourceVersion,
    bbVpcConfig,
    bbEndTime,
    bbProjectName,
    bbBuildGroups,
    bbSource,
    bbId,
    bbFileSystemLocations,
    bbBuildBatchConfig,
    bbEncryptionKey,
    bbLogConfig,
    bbServiceRole,
    bbComplete,
  )
where

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
import Network.AWS.CodeBuild.Types.VPCConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a batch build.
--
-- /See:/ 'mkBuildBatch' smart constructor.
data BuildBatch = BuildBatch'
  { phases ::
      Lude.Maybe [BuildBatchPhase],
    secondaryArtifacts :: Lude.Maybe [BuildArtifacts],
    buildTimeoutInMinutes :: Lude.Maybe Lude.Int,
    arn :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    artifacts :: Lude.Maybe BuildArtifacts,
    environment :: Lude.Maybe ProjectEnvironment,
    initiator :: Lude.Maybe Lude.Text,
    secondarySourceVersions :: Lude.Maybe [ProjectSourceVersion],
    buildBatchStatus :: Lude.Maybe StatusType,
    currentPhase :: Lude.Maybe Lude.Text,
    buildBatchNumber :: Lude.Maybe Lude.Integer,
    queuedTimeoutInMinutes :: Lude.Maybe Lude.Int,
    cache :: Lude.Maybe ProjectCache,
    secondarySources :: Lude.Maybe [ProjectSource],
    sourceVersion :: Lude.Maybe Lude.Text,
    resolvedSourceVersion :: Lude.Maybe Lude.Text,
    vpcConfig :: Lude.Maybe VPCConfig,
    endTime :: Lude.Maybe Lude.Timestamp,
    projectName :: Lude.Maybe Lude.Text,
    buildGroups :: Lude.Maybe [BuildGroup],
    source :: Lude.Maybe ProjectSource,
    id :: Lude.Maybe Lude.Text,
    fileSystemLocations :: Lude.Maybe [ProjectFileSystemLocation],
    buildBatchConfig :: Lude.Maybe ProjectBuildBatchConfig,
    encryptionKey :: Lude.Maybe Lude.Text,
    logConfig :: Lude.Maybe LogsConfig,
    serviceRole :: Lude.Maybe Lude.Text,
    complete :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildBatch' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the batch build.
-- * 'artifacts' - A @BuildArtifacts@ object the defines the build artifacts for this batch build.
-- * 'buildBatchConfig' - Undocumented field.
-- * 'buildBatchNumber' - The number of the batch build. For each project, the @buildBatchNumber@ of its first batch build is @1@ . The @buildBatchNumber@ of each subsequent batch build is incremented by @1@ . If a batch build is deleted, the @buildBatchNumber@ of other batch builds does not change.
-- * 'buildBatchStatus' - The status of the batch build.
-- * 'buildGroups' - An array of @BuildGroup@ objects that define the build groups for the batch build.
-- * 'buildTimeoutInMinutes' - Specifies the maximum amount of time, in minutes, that the build in a batch must be completed in.
-- * 'cache' - Undocumented field.
-- * 'complete' - Indicates if the batch build is complete.
-- * 'currentPhase' - The current phase of the batch build.
-- * 'encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the batch build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
-- * 'endTime' - The date and time that the batch build ended.
-- * 'environment' - Undocumented field.
-- * 'fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for the batch build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
-- * 'id' - The identifier of the batch build.
-- * 'initiator' - The entity that started the batch build. Valid values include:
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
-- * 'logConfig' - Undocumented field.
-- * 'phases' - An array of @BuildBatchPhase@ objects the specify the phases of the batch build.
-- * 'projectName' - The name of the batch build project.
-- * 'queuedTimeoutInMinutes' - Specifies the amount of time, in minutes, that the batch build is allowed to be queued before it times out.
-- * 'resolvedSourceVersion' - The identifier of the resolved version of this batch build's source code.
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
-- * 'secondaryArtifacts' - An array of @BuildArtifacts@ objects the define the build artifacts for this batch build.
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
-- * 'secondarySources' - An array of @ProjectSource@ objects that define the sources for the batch build.
-- * 'serviceRole' - The name of a service role used for builds in the batch.
-- * 'source' - Undocumented field.
-- * 'sourceVersion' - The identifier of the version of the source code to be built.
-- * 'startTime' - The date and time that the batch build started.
-- * 'vpcConfig' - Undocumented field.
mkBuildBatch ::
  BuildBatch
mkBuildBatch =
  BuildBatch'
    { phases = Lude.Nothing,
      secondaryArtifacts = Lude.Nothing,
      buildTimeoutInMinutes = Lude.Nothing,
      arn = Lude.Nothing,
      startTime = Lude.Nothing,
      artifacts = Lude.Nothing,
      environment = Lude.Nothing,
      initiator = Lude.Nothing,
      secondarySourceVersions = Lude.Nothing,
      buildBatchStatus = Lude.Nothing,
      currentPhase = Lude.Nothing,
      buildBatchNumber = Lude.Nothing,
      queuedTimeoutInMinutes = Lude.Nothing,
      cache = Lude.Nothing,
      secondarySources = Lude.Nothing,
      sourceVersion = Lude.Nothing,
      resolvedSourceVersion = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      endTime = Lude.Nothing,
      projectName = Lude.Nothing,
      buildGroups = Lude.Nothing,
      source = Lude.Nothing,
      id = Lude.Nothing,
      fileSystemLocations = Lude.Nothing,
      buildBatchConfig = Lude.Nothing,
      encryptionKey = Lude.Nothing,
      logConfig = Lude.Nothing,
      serviceRole = Lude.Nothing,
      complete = Lude.Nothing
    }

-- | An array of @BuildBatchPhase@ objects the specify the phases of the batch build.
--
-- /Note:/ Consider using 'phases' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbPhases :: Lens.Lens' BuildBatch (Lude.Maybe [BuildBatchPhase])
bbPhases = Lens.lens (phases :: BuildBatch -> Lude.Maybe [BuildBatchPhase]) (\s a -> s {phases = a} :: BuildBatch)
{-# DEPRECATED bbPhases "Use generic-lens or generic-optics with 'phases' instead." #-}

-- | An array of @BuildArtifacts@ objects the define the build artifacts for this batch build.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSecondaryArtifacts :: Lens.Lens' BuildBatch (Lude.Maybe [BuildArtifacts])
bbSecondaryArtifacts = Lens.lens (secondaryArtifacts :: BuildBatch -> Lude.Maybe [BuildArtifacts]) (\s a -> s {secondaryArtifacts = a} :: BuildBatch)
{-# DEPRECATED bbSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | Specifies the maximum amount of time, in minutes, that the build in a batch must be completed in.
--
-- /Note:/ Consider using 'buildTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildTimeoutInMinutes :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Int)
bbBuildTimeoutInMinutes = Lens.lens (buildTimeoutInMinutes :: BuildBatch -> Lude.Maybe Lude.Int) (\s a -> s {buildTimeoutInMinutes = a} :: BuildBatch)
{-# DEPRECATED bbBuildTimeoutInMinutes "Use generic-lens or generic-optics with 'buildTimeoutInMinutes' instead." #-}

-- | The ARN of the batch build.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbArn :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbArn = Lens.lens (arn :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: BuildBatch)
{-# DEPRECATED bbArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time that the batch build started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbStartTime :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Timestamp)
bbStartTime = Lens.lens (startTime :: BuildBatch -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: BuildBatch)
{-# DEPRECATED bbStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | A @BuildArtifacts@ object the defines the build artifacts for this batch build.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbArtifacts :: Lens.Lens' BuildBatch (Lude.Maybe BuildArtifacts)
bbArtifacts = Lens.lens (artifacts :: BuildBatch -> Lude.Maybe BuildArtifacts) (\s a -> s {artifacts = a} :: BuildBatch)
{-# DEPRECATED bbArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbEnvironment :: Lens.Lens' BuildBatch (Lude.Maybe ProjectEnvironment)
bbEnvironment = Lens.lens (environment :: BuildBatch -> Lude.Maybe ProjectEnvironment) (\s a -> s {environment = a} :: BuildBatch)
{-# DEPRECATED bbEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

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
bbInitiator :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbInitiator = Lens.lens (initiator :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {initiator = a} :: BuildBatch)
{-# DEPRECATED bbInitiator "Use generic-lens or generic-optics with 'initiator' instead." #-}

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
bbSecondarySourceVersions :: Lens.Lens' BuildBatch (Lude.Maybe [ProjectSourceVersion])
bbSecondarySourceVersions = Lens.lens (secondarySourceVersions :: BuildBatch -> Lude.Maybe [ProjectSourceVersion]) (\s a -> s {secondarySourceVersions = a} :: BuildBatch)
{-# DEPRECATED bbSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | The status of the batch build.
--
-- /Note:/ Consider using 'buildBatchStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildBatchStatus :: Lens.Lens' BuildBatch (Lude.Maybe StatusType)
bbBuildBatchStatus = Lens.lens (buildBatchStatus :: BuildBatch -> Lude.Maybe StatusType) (\s a -> s {buildBatchStatus = a} :: BuildBatch)
{-# DEPRECATED bbBuildBatchStatus "Use generic-lens or generic-optics with 'buildBatchStatus' instead." #-}

-- | The current phase of the batch build.
--
-- /Note:/ Consider using 'currentPhase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbCurrentPhase :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbCurrentPhase = Lens.lens (currentPhase :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {currentPhase = a} :: BuildBatch)
{-# DEPRECATED bbCurrentPhase "Use generic-lens or generic-optics with 'currentPhase' instead." #-}

-- | The number of the batch build. For each project, the @buildBatchNumber@ of its first batch build is @1@ . The @buildBatchNumber@ of each subsequent batch build is incremented by @1@ . If a batch build is deleted, the @buildBatchNumber@ of other batch builds does not change.
--
-- /Note:/ Consider using 'buildBatchNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildBatchNumber :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Integer)
bbBuildBatchNumber = Lens.lens (buildBatchNumber :: BuildBatch -> Lude.Maybe Lude.Integer) (\s a -> s {buildBatchNumber = a} :: BuildBatch)
{-# DEPRECATED bbBuildBatchNumber "Use generic-lens or generic-optics with 'buildBatchNumber' instead." #-}

-- | Specifies the amount of time, in minutes, that the batch build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbQueuedTimeoutInMinutes :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Int)
bbQueuedTimeoutInMinutes = Lens.lens (queuedTimeoutInMinutes :: BuildBatch -> Lude.Maybe Lude.Int) (\s a -> s {queuedTimeoutInMinutes = a} :: BuildBatch)
{-# DEPRECATED bbQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbCache :: Lens.Lens' BuildBatch (Lude.Maybe ProjectCache)
bbCache = Lens.lens (cache :: BuildBatch -> Lude.Maybe ProjectCache) (\s a -> s {cache = a} :: BuildBatch)
{-# DEPRECATED bbCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | An array of @ProjectSource@ objects that define the sources for the batch build.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSecondarySources :: Lens.Lens' BuildBatch (Lude.Maybe [ProjectSource])
bbSecondarySources = Lens.lens (secondarySources :: BuildBatch -> Lude.Maybe [ProjectSource]) (\s a -> s {secondarySources = a} :: BuildBatch)
{-# DEPRECATED bbSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

-- | The identifier of the version of the source code to be built.
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSourceVersion :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbSourceVersion = Lens.lens (sourceVersion :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: BuildBatch)
{-# DEPRECATED bbSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

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
bbResolvedSourceVersion :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbResolvedSourceVersion = Lens.lens (resolvedSourceVersion :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {resolvedSourceVersion = a} :: BuildBatch)
{-# DEPRECATED bbResolvedSourceVersion "Use generic-lens or generic-optics with 'resolvedSourceVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbVpcConfig :: Lens.Lens' BuildBatch (Lude.Maybe VPCConfig)
bbVpcConfig = Lens.lens (vpcConfig :: BuildBatch -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: BuildBatch)
{-# DEPRECATED bbVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | The date and time that the batch build ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbEndTime :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Timestamp)
bbEndTime = Lens.lens (endTime :: BuildBatch -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: BuildBatch)
{-# DEPRECATED bbEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the batch build project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbProjectName :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbProjectName = Lens.lens (projectName :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {projectName = a} :: BuildBatch)
{-# DEPRECATED bbProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | An array of @BuildGroup@ objects that define the build groups for the batch build.
--
-- /Note:/ Consider using 'buildGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildGroups :: Lens.Lens' BuildBatch (Lude.Maybe [BuildGroup])
bbBuildGroups = Lens.lens (buildGroups :: BuildBatch -> Lude.Maybe [BuildGroup]) (\s a -> s {buildGroups = a} :: BuildBatch)
{-# DEPRECATED bbBuildGroups "Use generic-lens or generic-optics with 'buildGroups' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbSource :: Lens.Lens' BuildBatch (Lude.Maybe ProjectSource)
bbSource = Lens.lens (source :: BuildBatch -> Lude.Maybe ProjectSource) (\s a -> s {source = a} :: BuildBatch)
{-# DEPRECATED bbSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The identifier of the batch build.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbId :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbId = Lens.lens (id :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: BuildBatch)
{-# DEPRECATED bbId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for the batch build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbFileSystemLocations :: Lens.Lens' BuildBatch (Lude.Maybe [ProjectFileSystemLocation])
bbFileSystemLocations = Lens.lens (fileSystemLocations :: BuildBatch -> Lude.Maybe [ProjectFileSystemLocation]) (\s a -> s {fileSystemLocations = a} :: BuildBatch)
{-# DEPRECATED bbFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbBuildBatchConfig :: Lens.Lens' BuildBatch (Lude.Maybe ProjectBuildBatchConfig)
bbBuildBatchConfig = Lens.lens (buildBatchConfig :: BuildBatch -> Lude.Maybe ProjectBuildBatchConfig) (\s a -> s {buildBatchConfig = a} :: BuildBatch)
{-# DEPRECATED bbBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the batch build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbEncryptionKey :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbEncryptionKey = Lens.lens (encryptionKey :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKey = a} :: BuildBatch)
{-# DEPRECATED bbEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'logConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbLogConfig :: Lens.Lens' BuildBatch (Lude.Maybe LogsConfig)
bbLogConfig = Lens.lens (logConfig :: BuildBatch -> Lude.Maybe LogsConfig) (\s a -> s {logConfig = a} :: BuildBatch)
{-# DEPRECATED bbLogConfig "Use generic-lens or generic-optics with 'logConfig' instead." #-}

-- | The name of a service role used for builds in the batch.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbServiceRole :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Text)
bbServiceRole = Lens.lens (serviceRole :: BuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: BuildBatch)
{-# DEPRECATED bbServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Indicates if the batch build is complete.
--
-- /Note:/ Consider using 'complete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbComplete :: Lens.Lens' BuildBatch (Lude.Maybe Lude.Bool)
bbComplete = Lens.lens (complete :: BuildBatch -> Lude.Maybe Lude.Bool) (\s a -> s {complete = a} :: BuildBatch)
{-# DEPRECATED bbComplete "Use generic-lens or generic-optics with 'complete' instead." #-}

instance Lude.FromJSON BuildBatch where
  parseJSON =
    Lude.withObject
      "BuildBatch"
      ( \x ->
          BuildBatch'
            Lude.<$> (x Lude..:? "phases" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "secondaryArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "buildTimeoutInMinutes")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "artifacts")
            Lude.<*> (x Lude..:? "environment")
            Lude.<*> (x Lude..:? "initiator")
            Lude.<*> (x Lude..:? "secondarySourceVersions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "buildBatchStatus")
            Lude.<*> (x Lude..:? "currentPhase")
            Lude.<*> (x Lude..:? "buildBatchNumber")
            Lude.<*> (x Lude..:? "queuedTimeoutInMinutes")
            Lude.<*> (x Lude..:? "cache")
            Lude.<*> (x Lude..:? "secondarySources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "sourceVersion")
            Lude.<*> (x Lude..:? "resolvedSourceVersion")
            Lude.<*> (x Lude..:? "vpcConfig")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "projectName")
            Lude.<*> (x Lude..:? "buildGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "source")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "fileSystemLocations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "buildBatchConfig")
            Lude.<*> (x Lude..:? "encryptionKey")
            Lude.<*> (x Lude..:? "logConfig")
            Lude.<*> (x Lude..:? "serviceRole")
            Lude.<*> (x Lude..:? "complete")
      )
