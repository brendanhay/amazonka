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
    pSecondaryArtifacts,
    pArn,
    pArtifacts,
    pEnvironment,
    pCreated,
    pSecondarySourceVersions,
    pQueuedTimeoutInMinutes,
    pCache,
    pSecondarySources,
    pSourceVersion,
    pName,
    pVpcConfig,
    pSource,
    pBadge,
    pLogsConfig,
    pFileSystemLocations,
    pBuildBatchConfig,
    pEncryptionKey,
    pLastModified,
    pWebhook,
    pDescription,
    pServiceRole,
    pTags,
    pTimeoutInMinutes,
  )
where

import Network.AWS.CodeBuild.Types.LogsConfig
import Network.AWS.CodeBuild.Types.ProjectArtifacts
import Network.AWS.CodeBuild.Types.ProjectBadge
import Network.AWS.CodeBuild.Types.ProjectBuildBatchConfig
import Network.AWS.CodeBuild.Types.ProjectCache
import Network.AWS.CodeBuild.Types.ProjectEnvironment
import Network.AWS.CodeBuild.Types.ProjectFileSystemLocation
import Network.AWS.CodeBuild.Types.ProjectSource
import Network.AWS.CodeBuild.Types.ProjectSourceVersion
import Network.AWS.CodeBuild.Types.Tag
import Network.AWS.CodeBuild.Types.VPCConfig
import Network.AWS.CodeBuild.Types.Webhook
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a build project.
--
-- /See:/ 'mkProject' smart constructor.
data Project = Project'
  { secondaryArtifacts ::
      Lude.Maybe [ProjectArtifacts],
    arn :: Lude.Maybe Lude.Text,
    artifacts :: Lude.Maybe ProjectArtifacts,
    environment :: Lude.Maybe ProjectEnvironment,
    created :: Lude.Maybe Lude.Timestamp,
    secondarySourceVersions :: Lude.Maybe [ProjectSourceVersion],
    queuedTimeoutInMinutes :: Lude.Maybe Lude.Natural,
    cache :: Lude.Maybe ProjectCache,
    secondarySources :: Lude.Maybe [ProjectSource],
    sourceVersion :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    vpcConfig :: Lude.Maybe VPCConfig,
    source :: Lude.Maybe ProjectSource,
    badge :: Lude.Maybe ProjectBadge,
    logsConfig :: Lude.Maybe LogsConfig,
    fileSystemLocations :: Lude.Maybe [ProjectFileSystemLocation],
    buildBatchConfig :: Lude.Maybe ProjectBuildBatchConfig,
    encryptionKey :: Lude.Maybe Lude.Text,
    lastModified :: Lude.Maybe Lude.Timestamp,
    webhook :: Lude.Maybe Webhook,
    description :: Lude.Maybe Lude.Text,
    serviceRole :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    timeoutInMinutes :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Project' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the build project.
-- * 'artifacts' - Information about the build output artifacts for the build project.
-- * 'badge' - Information about the build badge for the build project.
-- * 'buildBatchConfig' - A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
-- * 'cache' - Information about the cache for the build project.
-- * 'created' - When the build project was created, expressed in Unix time format.
-- * 'description' - A description that makes the build project easy to identify.
-- * 'encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
-- * 'environment' - Information about the build environment for this build project.
-- * 'fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
-- * 'lastModified' - When the build project's settings were last modified, expressed in Unix time format.
-- * 'logsConfig' - Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, an S3 bucket, or both.
-- * 'name' - The name of the build project.
-- * 'queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times out.
-- * 'secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
-- * 'secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
-- * 'secondarySources' - An array of @ProjectSource@ objects.
-- * 'serviceRole' - The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
-- * 'source' - Information about the build input source code for this build project.
-- * 'sourceVersion' - A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of:
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
-- * 'tags' - A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
-- * 'timeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
-- * 'vpcConfig' - Information about the VPC configuration that AWS CodeBuild accesses.
-- * 'webhook' - Information about a webhook that connects repository events to a build project in AWS CodeBuild.
mkProject ::
  Project
mkProject =
  Project'
    { secondaryArtifacts = Lude.Nothing,
      arn = Lude.Nothing,
      artifacts = Lude.Nothing,
      environment = Lude.Nothing,
      created = Lude.Nothing,
      secondarySourceVersions = Lude.Nothing,
      queuedTimeoutInMinutes = Lude.Nothing,
      cache = Lude.Nothing,
      secondarySources = Lude.Nothing,
      sourceVersion = Lude.Nothing,
      name = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      source = Lude.Nothing,
      badge = Lude.Nothing,
      logsConfig = Lude.Nothing,
      fileSystemLocations = Lude.Nothing,
      buildBatchConfig = Lude.Nothing,
      encryptionKey = Lude.Nothing,
      lastModified = Lude.Nothing,
      webhook = Lude.Nothing,
      description = Lude.Nothing,
      serviceRole = Lude.Nothing,
      tags = Lude.Nothing,
      timeoutInMinutes = Lude.Nothing
    }

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecondaryArtifacts :: Lens.Lens' Project (Lude.Maybe [ProjectArtifacts])
pSecondaryArtifacts = Lens.lens (secondaryArtifacts :: Project -> Lude.Maybe [ProjectArtifacts]) (\s a -> s {secondaryArtifacts = a} :: Project)
{-# DEPRECATED pSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | The Amazon Resource Name (ARN) of the build project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArn :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pArn = Lens.lens (arn :: Project -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Project)
{-# DEPRECATED pArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the build output artifacts for the build project.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArtifacts :: Lens.Lens' Project (Lude.Maybe ProjectArtifacts)
pArtifacts = Lens.lens (artifacts :: Project -> Lude.Maybe ProjectArtifacts) (\s a -> s {artifacts = a} :: Project)
{-# DEPRECATED pArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Information about the build environment for this build project.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pEnvironment :: Lens.Lens' Project (Lude.Maybe ProjectEnvironment)
pEnvironment = Lens.lens (environment :: Project -> Lude.Maybe ProjectEnvironment) (\s a -> s {environment = a} :: Project)
{-# DEPRECATED pEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | When the build project was created, expressed in Unix time format.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreated :: Lens.Lens' Project (Lude.Maybe Lude.Timestamp)
pCreated = Lens.lens (created :: Project -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: Project)
{-# DEPRECATED pCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecondarySourceVersions :: Lens.Lens' Project (Lude.Maybe [ProjectSourceVersion])
pSecondarySourceVersions = Lens.lens (secondarySourceVersions :: Project -> Lude.Maybe [ProjectSourceVersion]) (\s a -> s {secondarySourceVersions = a} :: Project)
{-# DEPRECATED pSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pQueuedTimeoutInMinutes :: Lens.Lens' Project (Lude.Maybe Lude.Natural)
pQueuedTimeoutInMinutes = Lens.lens (queuedTimeoutInMinutes :: Project -> Lude.Maybe Lude.Natural) (\s a -> s {queuedTimeoutInMinutes = a} :: Project)
{-# DEPRECATED pQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | Information about the cache for the build project.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCache :: Lens.Lens' Project (Lude.Maybe ProjectCache)
pCache = Lens.lens (cache :: Project -> Lude.Maybe ProjectCache) (\s a -> s {cache = a} :: Project)
{-# DEPRECATED pCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSecondarySources :: Lens.Lens' Project (Lude.Maybe [ProjectSource])
pSecondarySources = Lens.lens (secondarySources :: Project -> Lude.Maybe [ProjectSource]) (\s a -> s {secondarySources = a} :: Project)
{-# DEPRECATED pSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

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
pSourceVersion :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pSourceVersion = Lens.lens (sourceVersion :: Project -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: Project)
{-# DEPRECATED pSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pName = Lens.lens (name :: Project -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Project)
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information about the VPC configuration that AWS CodeBuild accesses.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pVpcConfig :: Lens.Lens' Project (Lude.Maybe VPCConfig)
pVpcConfig = Lens.lens (vpcConfig :: Project -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: Project)
{-# DEPRECATED pVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | Information about the build input source code for this build project.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pSource :: Lens.Lens' Project (Lude.Maybe ProjectSource)
pSource = Lens.lens (source :: Project -> Lude.Maybe ProjectSource) (\s a -> s {source = a} :: Project)
{-# DEPRECATED pSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Information about the build badge for the build project.
--
-- /Note:/ Consider using 'badge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBadge :: Lens.Lens' Project (Lude.Maybe ProjectBadge)
pBadge = Lens.lens (badge :: Project -> Lude.Maybe ProjectBadge) (\s a -> s {badge = a} :: Project)
{-# DEPRECATED pBadge "Use generic-lens or generic-optics with 'badge' instead." #-}

-- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, an S3 bucket, or both.
--
-- /Note:/ Consider using 'logsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLogsConfig :: Lens.Lens' Project (Lude.Maybe LogsConfig)
pLogsConfig = Lens.lens (logsConfig :: Project -> Lude.Maybe LogsConfig) (\s a -> s {logsConfig = a} :: Project)
{-# DEPRECATED pLogsConfig "Use generic-lens or generic-optics with 'logsConfig' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pFileSystemLocations :: Lens.Lens' Project (Lude.Maybe [ProjectFileSystemLocation])
pFileSystemLocations = Lens.lens (fileSystemLocations :: Project -> Lude.Maybe [ProjectFileSystemLocation]) (\s a -> s {fileSystemLocations = a} :: Project)
{-# DEPRECATED pFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pBuildBatchConfig :: Lens.Lens' Project (Lude.Maybe ProjectBuildBatchConfig)
pBuildBatchConfig = Lens.lens (buildBatchConfig :: Project -> Lude.Maybe ProjectBuildBatchConfig) (\s a -> s {buildBatchConfig = a} :: Project)
{-# DEPRECATED pBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pEncryptionKey :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pEncryptionKey = Lens.lens (encryptionKey :: Project -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKey = a} :: Project)
{-# DEPRECATED pEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | When the build project's settings were last modified, expressed in Unix time format.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastModified :: Lens.Lens' Project (Lude.Maybe Lude.Timestamp)
pLastModified = Lens.lens (lastModified :: Project -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: Project)
{-# DEPRECATED pLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
--
-- /Note:/ Consider using 'webhook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pWebhook :: Lens.Lens' Project (Lude.Maybe Webhook)
pWebhook = Lens.lens (webhook :: Project -> Lude.Maybe Webhook) (\s a -> s {webhook = a} :: Project)
{-# DEPRECATED pWebhook "Use generic-lens or generic-optics with 'webhook' instead." #-}

-- | A description that makes the build project easy to identify.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pDescription :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pDescription = Lens.lens (description :: Project -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Project)
{-# DEPRECATED pDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pServiceRole :: Lens.Lens' Project (Lude.Maybe Lude.Text)
pServiceRole = Lens.lens (serviceRole :: Project -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: Project)
{-# DEPRECATED pServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTags :: Lens.Lens' Project (Lude.Maybe [Tag])
pTags = Lens.lens (tags :: Project -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Project)
{-# DEPRECATED pTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed. The default is 60 minutes.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pTimeoutInMinutes :: Lens.Lens' Project (Lude.Maybe Lude.Natural)
pTimeoutInMinutes = Lens.lens (timeoutInMinutes :: Project -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInMinutes = a} :: Project)
{-# DEPRECATED pTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

instance Lude.FromJSON Project where
  parseJSON =
    Lude.withObject
      "Project"
      ( \x ->
          Project'
            Lude.<$> (x Lude..:? "secondaryArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "artifacts")
            Lude.<*> (x Lude..:? "environment")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "secondarySourceVersions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "queuedTimeoutInMinutes")
            Lude.<*> (x Lude..:? "cache")
            Lude.<*> (x Lude..:? "secondarySources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "sourceVersion")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "vpcConfig")
            Lude.<*> (x Lude..:? "source")
            Lude.<*> (x Lude..:? "badge")
            Lude.<*> (x Lude..:? "logsConfig")
            Lude.<*> (x Lude..:? "fileSystemLocations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "buildBatchConfig")
            Lude.<*> (x Lude..:? "encryptionKey")
            Lude.<*> (x Lude..:? "lastModified")
            Lude.<*> (x Lude..:? "webhook")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "serviceRole")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "timeoutInMinutes")
      )
