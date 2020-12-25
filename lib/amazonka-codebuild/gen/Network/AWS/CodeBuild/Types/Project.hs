{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Project
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Project
  ( Project (..),

    -- * Smart constructor
    mkProject,

    -- * Lenses
    pArn,
    pArtifacts,
    pBadge,
    pBuildBatchConfig,
    pCache,
    pCreated,
    pDescription,
    pEncryptionKey,
    pEnvironment,
    pFileSystemLocations,
    pLastModified,
    pLogsConfig,
    pName,
    pQueuedTimeoutInMinutes,
    pSecondaryArtifacts,
    pSecondarySourceVersions,
    pSecondarySources,
    pServiceRole,
    pSource,
    pSourceVersion,
    pTags,
    pTimeoutInMinutes,
    pVpcConfig,
    pWebhook,
  )
where

import qualified Network.AWS.CodeBuild.Types.Description as Types
import qualified Network.AWS.CodeBuild.Types.EncryptionKey as Types
import qualified Network.AWS.CodeBuild.Types.LogsConfig as Types
import qualified Network.AWS.CodeBuild.Types.Name as Types
import qualified Network.AWS.CodeBuild.Types.ProjectArtifacts as Types
import qualified Network.AWS.CodeBuild.Types.ProjectBadge as Types
import qualified Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig as Types
import qualified Network.AWS.CodeBuild.Types.ProjectCache as Types
import qualified Network.AWS.CodeBuild.Types.ProjectEnvironment as Types
import qualified Network.AWS.CodeBuild.Types.ProjectFileSystemLocation as Types
import qualified Network.AWS.CodeBuild.Types.ProjectSource as Types
import qualified Network.AWS.CodeBuild.Types.ProjectSourceVersion as Types
import qualified Network.AWS.CodeBuild.Types.ServiceRole as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.CodeBuild.Types.Tag as Types
import qualified Network.AWS.CodeBuild.Types.VpcConfig as Types
import qualified Network.AWS.CodeBuild.Types.Webhook as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a build project.
--
-- /See:/ 'mkProject' smart constructor.
data Project = Project'
  { -- | The Amazon Resource Name (ARN) of the build project.
    arn :: Core.Maybe Types.String,
    -- | Information about the build output artifacts for the build project.
    artifacts :: Core.Maybe Types.ProjectArtifacts,
    -- | Information about the build badge for the build project.
    badge :: Core.Maybe Types.ProjectBadge,
    -- | A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
    buildBatchConfig :: Core.Maybe Types.ProjectBuildBatchConfig,
    -- | Information about the cache for the build project.
    cache :: Core.Maybe Types.ProjectCache,
    -- | When the build project was created, expressed in Unix time format.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | A description that makes the build project easy to identify.
    description :: Core.Maybe Types.Description,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKey :: Core.Maybe Types.EncryptionKey,
    -- | Information about the build environment for this build project.
    environment :: Core.Maybe Types.ProjectEnvironment,
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
    fileSystemLocations :: Core.Maybe [Types.ProjectFileSystemLocation],
    -- | When the build project's settings were last modified, expressed in Unix time format.
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, an S3 bucket, or both.
    logsConfig :: Core.Maybe Types.LogsConfig,
    -- | The name of the build project.
    name :: Core.Maybe Types.Name,
    -- | The number of minutes a build is allowed to be queued before it times out.
    queuedTimeoutInMinutes :: Core.Maybe Core.Natural,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Core.Maybe [Types.ProjectArtifacts],
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Core.Maybe [Types.ProjectSourceVersion],
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Core.Maybe [Types.ProjectSource],
    -- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
    serviceRole :: Core.Maybe Types.ServiceRole,
    -- | Information about the build input source code for this build project.
    source :: Core.Maybe Types.ProjectSource,
    -- | A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of:
    --
    --
    --     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.
    --
    --
    --     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
    --
    --
    --     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
    --
    --
    --     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
    --
    --
    -- If @sourceVersion@ is specified at the build level, then that version takes precedence over this @sourceVersion@ (at the project level).
    -- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
    sourceVersion :: Core.Maybe Types.String,
    -- | A list of tag key and value pairs associated with this build project.
    --
    -- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
    tags :: Core.Maybe [Types.Tag],
    -- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
    timeoutInMinutes :: Core.Maybe Core.Natural,
    -- | Information about the VPC configuration that AWS CodeBuild accesses.
    vpcConfig :: Core.Maybe Types.VpcConfig,
    -- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
    webhook :: Core.Maybe Types.Webhook
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Project' value with any optional fields omitted.
mkProject ::
  Project
mkProject =
  Project'
    { arn = Core.Nothing,
      artifacts = Core.Nothing,
      badge = Core.Nothing,
      buildBatchConfig = Core.Nothing,
      cache = Core.Nothing,
      created = Core.Nothing,
      description = Core.Nothing,
      encryptionKey = Core.Nothing,
      environment = Core.Nothing,
      fileSystemLocations = Core.Nothing,
      lastModified = Core.Nothing,
      logsConfig = Core.Nothing,
      name = Core.Nothing,
      queuedTimeoutInMinutes = Core.Nothing,
      secondaryArtifacts = Core.Nothing,
      secondarySourceVersions = Core.Nothing,
      secondarySources = Core.Nothing,
      serviceRole = Core.Nothing,
      source = Core.Nothing,
      sourceVersion = Core.Nothing,
      tags = Core.Nothing,
      timeoutInMinutes = Core.Nothing,
      vpcConfig = Core.Nothing,
      webhook = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the build project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArn :: Lens.Lens' Project (Core.Maybe Types.String)
pArn = Lens.field @"arn"
{-# DEPRECATED pArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the build output artifacts for the build project.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArtifacts :: Lens.Lens' Project (Core.Maybe Types.ProjectArtifacts)
pArtifacts = Lens.field @"artifacts"
{-# DEPRECATED pArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Information about the build badge for the build project.
--
-- /Note:/ Consider using 'badge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBadge :: Lens.Lens' Project (Core.Maybe Types.ProjectBadge)
pBadge = Lens.field @"badge"
{-# DEPRECATED pBadge "Use generic-lens or generic-optics with 'badge' instead." #-}

-- | A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBuildBatchConfig :: Lens.Lens' Project (Core.Maybe Types.ProjectBuildBatchConfig)
pBuildBatchConfig = Lens.field @"buildBatchConfig"
{-# DEPRECATED pBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | Information about the cache for the build project.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCache :: Lens.Lens' Project (Core.Maybe Types.ProjectCache)
pCache = Lens.field @"cache"
{-# DEPRECATED pCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | When the build project was created, expressed in Unix time format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreated :: Lens.Lens' Project (Core.Maybe Core.NominalDiffTime)
pCreated = Lens.field @"created"
{-# DEPRECATED pCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | A description that makes the build project easy to identify.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Project (Core.Maybe Types.Description)
pDescription = Lens.field @"description"
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pEncryptionKey :: Lens.Lens' Project (Core.Maybe Types.EncryptionKey)
pEncryptionKey = Lens.field @"encryptionKey"
{-# DEPRECATED pEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | Information about the build environment for this build project.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pEnvironment :: Lens.Lens' Project (Core.Maybe Types.ProjectEnvironment)
pEnvironment = Lens.field @"environment"
{-# DEPRECATED pEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pFileSystemLocations :: Lens.Lens' Project (Core.Maybe [Types.ProjectFileSystemLocation])
pFileSystemLocations = Lens.field @"fileSystemLocations"
{-# DEPRECATED pFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | When the build project's settings were last modified, expressed in Unix time format.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastModified :: Lens.Lens' Project (Core.Maybe Core.NominalDiffTime)
pLastModified = Lens.field @"lastModified"
{-# DEPRECATED pLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, an S3 bucket, or both.
--
-- /Note:/ Consider using 'logsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLogsConfig :: Lens.Lens' Project (Core.Maybe Types.LogsConfig)
pLogsConfig = Lens.field @"logsConfig"
{-# DEPRECATED pLogsConfig "Use generic-lens or generic-optics with 'logsConfig' instead." #-}

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Project (Core.Maybe Types.Name)
pName = Lens.field @"name"
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pQueuedTimeoutInMinutes :: Lens.Lens' Project (Core.Maybe Core.Natural)
pQueuedTimeoutInMinutes = Lens.field @"queuedTimeoutInMinutes"
{-# DEPRECATED pQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecondaryArtifacts :: Lens.Lens' Project (Core.Maybe [Types.ProjectArtifacts])
pSecondaryArtifacts = Lens.field @"secondaryArtifacts"
{-# DEPRECATED pSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecondarySourceVersions :: Lens.Lens' Project (Core.Maybe [Types.ProjectSourceVersion])
pSecondarySourceVersions = Lens.field @"secondarySourceVersions"
{-# DEPRECATED pSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecondarySources :: Lens.Lens' Project (Core.Maybe [Types.ProjectSource])
pSecondarySources = Lens.field @"secondarySources"
{-# DEPRECATED pSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pServiceRole :: Lens.Lens' Project (Core.Maybe Types.ServiceRole)
pServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED pServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Information about the build input source code for this build project.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Project (Core.Maybe Types.ProjectSource)
pSource = Lens.field @"source"
{-# DEPRECATED pSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of:
--
--
--     * For AWS CodeCommit: the commit ID, branch, or Git tag to use.
--
--
--     * For GitHub: the commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Bitbucket: the commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * For Amazon Simple Storage Service (Amazon S3): the version ID of the object that represents the build input ZIP file to use.
--
--
-- If @sourceVersion@ is specified at the build level, then that version takes precedence over this @sourceVersion@ (at the project level).
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSourceVersion :: Lens.Lens' Project (Core.Maybe Types.String)
pSourceVersion = Lens.field @"sourceVersion"
{-# DEPRECATED pSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTags :: Lens.Lens' Project (Core.Maybe [Types.Tag])
pTags = Lens.field @"tags"
{-# DEPRECATED pTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTimeoutInMinutes :: Lens.Lens' Project (Core.Maybe Core.Natural)
pTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# DEPRECATED pTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | Information about the VPC configuration that AWS CodeBuild accesses.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pVpcConfig :: Lens.Lens' Project (Core.Maybe Types.VpcConfig)
pVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED pVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
--
-- /Note:/ Consider using 'webhook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pWebhook :: Lens.Lens' Project (Core.Maybe Types.Webhook)
pWebhook = Lens.field @"webhook"
{-# DEPRECATED pWebhook "Use generic-lens or generic-optics with 'webhook' instead." #-}

instance Core.FromJSON Project where
  parseJSON =
    Core.withObject "Project" Core.$
      \x ->
        Project'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "artifacts")
          Core.<*> (x Core..:? "badge")
          Core.<*> (x Core..:? "buildBatchConfig")
          Core.<*> (x Core..:? "cache")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "encryptionKey")
          Core.<*> (x Core..:? "environment")
          Core.<*> (x Core..:? "fileSystemLocations")
          Core.<*> (x Core..:? "lastModified")
          Core.<*> (x Core..:? "logsConfig")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "queuedTimeoutInMinutes")
          Core.<*> (x Core..:? "secondaryArtifacts")
          Core.<*> (x Core..:? "secondarySourceVersions")
          Core.<*> (x Core..:? "secondarySources")
          Core.<*> (x Core..:? "serviceRole")
          Core.<*> (x Core..:? "source")
          Core.<*> (x Core..:? "sourceVersion")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "timeoutInMinutes")
          Core.<*> (x Core..:? "vpcConfig")
          Core.<*> (x Core..:? "webhook")
