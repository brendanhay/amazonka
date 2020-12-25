{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a build project.
module Network.AWS.CodeBuild.CreateProject
  ( -- * Creating a request
    CreateProject (..),
    mkCreateProject,

    -- ** Request lenses
    cpName,
    cpSource,
    cpArtifacts,
    cpEnvironment,
    cpServiceRole,
    cpBadgeEnabled,
    cpBuildBatchConfig,
    cpCache,
    cpDescription,
    cpEncryptionKey,
    cpFileSystemLocations,
    cpLogsConfig,
    cpQueuedTimeoutInMinutes,
    cpSecondaryArtifacts,
    cpSecondarySourceVersions,
    cpSecondarySources,
    cpSourceVersion,
    cpTags,
    cpTimeoutInMinutes,
    cpVpcConfig,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprrsProject,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | The name of the build project.
    name :: Types.Name,
    -- | Information about the build input source code for the build project.
    source :: Types.ProjectSource,
    -- | Information about the build output artifacts for the build project.
    artifacts :: Types.ProjectArtifacts,
    -- | Information about the build environment for the build project.
    environment :: Types.ProjectEnvironment,
    -- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
    serviceRole :: Types.NonEmptyString,
    -- | Set this to true to generate a publicly accessible URL for your project's build badge.
    badgeEnabled :: Core.Maybe Core.Bool,
    -- | A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
    buildBatchConfig :: Core.Maybe Types.ProjectBuildBatchConfig,
    -- | Stores recently used information so that it can be quickly accessed at a later time.
    cache :: Core.Maybe Types.ProjectCache,
    -- | A description that makes the build project easy to identify.
    description :: Core.Maybe Types.Description,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKey :: Core.Maybe Types.NonEmptyString,
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
    fileSystemLocations :: Core.Maybe [Types.ProjectFileSystemLocation],
    -- | Information about logs for the build project. These can be logs in Amazon CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
    logsConfig :: Core.Maybe Types.LogsConfig,
    -- | The number of minutes a build is allowed to be queued before it times out.
    queuedTimeoutInMinutes :: Core.Maybe Core.Natural,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifacts :: Core.Maybe [Types.ProjectArtifacts],
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take precedence over these @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Core.Maybe [Types.ProjectSourceVersion],
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Core.Maybe [Types.ProjectSource],
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
    -- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before it times out any build that has not been marked as completed. The default is 60 minutes.
    timeoutInMinutes :: Core.Maybe Core.Natural,
    -- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateProject' value with any optional fields omitted.
mkCreateProject ::
  -- | 'name'
  Types.Name ->
  -- | 'source'
  Types.ProjectSource ->
  -- | 'artifacts'
  Types.ProjectArtifacts ->
  -- | 'environment'
  Types.ProjectEnvironment ->
  -- | 'serviceRole'
  Types.NonEmptyString ->
  CreateProject
mkCreateProject name source artifacts environment serviceRole =
  CreateProject'
    { name,
      source,
      artifacts,
      environment,
      serviceRole,
      badgeEnabled = Core.Nothing,
      buildBatchConfig = Core.Nothing,
      cache = Core.Nothing,
      description = Core.Nothing,
      encryptionKey = Core.Nothing,
      fileSystemLocations = Core.Nothing,
      logsConfig = Core.Nothing,
      queuedTimeoutInMinutes = Core.Nothing,
      secondaryArtifacts = Core.Nothing,
      secondarySourceVersions = Core.Nothing,
      secondarySources = Core.Nothing,
      sourceVersion = Core.Nothing,
      tags = Core.Nothing,
      timeoutInMinutes = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject Types.Name
cpName = Lens.field @"name"
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information about the build input source code for the build project.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSource :: Lens.Lens' CreateProject Types.ProjectSource
cpSource = Lens.field @"source"
{-# DEPRECATED cpSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Information about the build output artifacts for the build project.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpArtifacts :: Lens.Lens' CreateProject Types.ProjectArtifacts
cpArtifacts = Lens.field @"artifacts"
{-# DEPRECATED cpArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Information about the build environment for the build project.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEnvironment :: Lens.Lens' CreateProject Types.ProjectEnvironment
cpEnvironment = Lens.field @"environment"
{-# DEPRECATED cpEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpServiceRole :: Lens.Lens' CreateProject Types.NonEmptyString
cpServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED cpServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- /Note:/ Consider using 'badgeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpBadgeEnabled :: Lens.Lens' CreateProject (Core.Maybe Core.Bool)
cpBadgeEnabled = Lens.field @"badgeEnabled"
{-# DEPRECATED cpBadgeEnabled "Use generic-lens or generic-optics with 'badgeEnabled' instead." #-}

-- | A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpBuildBatchConfig :: Lens.Lens' CreateProject (Core.Maybe Types.ProjectBuildBatchConfig)
cpBuildBatchConfig = Lens.field @"buildBatchConfig"
{-# DEPRECATED cpBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | Stores recently used information so that it can be quickly accessed at a later time.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCache :: Lens.Lens' CreateProject (Core.Maybe Types.ProjectCache)
cpCache = Lens.field @"cache"
{-# DEPRECATED cpCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | A description that makes the build project easy to identify.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreateProject (Core.Maybe Types.Description)
cpDescription = Lens.field @"description"
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEncryptionKey :: Lens.Lens' CreateProject (Core.Maybe Types.NonEmptyString)
cpEncryptionKey = Lens.field @"encryptionKey"
{-# DEPRECATED cpEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpFileSystemLocations :: Lens.Lens' CreateProject (Core.Maybe [Types.ProjectFileSystemLocation])
cpFileSystemLocations = Lens.field @"fileSystemLocations"
{-# DEPRECATED cpFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | Information about logs for the build project. These can be logs in Amazon CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
--
-- /Note:/ Consider using 'logsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLogsConfig :: Lens.Lens' CreateProject (Core.Maybe Types.LogsConfig)
cpLogsConfig = Lens.field @"logsConfig"
{-# DEPRECATED cpLogsConfig "Use generic-lens or generic-optics with 'logsConfig' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpQueuedTimeoutInMinutes :: Lens.Lens' CreateProject (Core.Maybe Core.Natural)
cpQueuedTimeoutInMinutes = Lens.field @"queuedTimeoutInMinutes"
{-# DEPRECATED cpQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecondaryArtifacts :: Lens.Lens' CreateProject (Core.Maybe [Types.ProjectArtifacts])
cpSecondaryArtifacts = Lens.field @"secondaryArtifacts"
{-# DEPRECATED cpSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take precedence over these @secondarySourceVersions@ (at the project level).
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecondarySourceVersions :: Lens.Lens' CreateProject (Core.Maybe [Types.ProjectSourceVersion])
cpSecondarySourceVersions = Lens.field @"secondarySourceVersions"
{-# DEPRECATED cpSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecondarySources :: Lens.Lens' CreateProject (Core.Maybe [Types.ProjectSource])
cpSecondarySources = Lens.field @"secondarySources"
{-# DEPRECATED cpSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

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
cpSourceVersion :: Lens.Lens' CreateProject (Core.Maybe Types.String)
cpSourceVersion = Lens.field @"sourceVersion"
{-# DEPRECATED cpSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProject (Core.Maybe [Types.Tag])
cpTags = Lens.field @"tags"
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before it times out any build that has not been marked as completed. The default is 60 minutes.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTimeoutInMinutes :: Lens.Lens' CreateProject (Core.Maybe Core.Natural)
cpTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# DEPRECATED cpTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVpcConfig :: Lens.Lens' CreateProject (Core.Maybe Types.VpcConfig)
cpVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED cpVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON CreateProject where
  toJSON CreateProject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("source" Core..= source),
            Core.Just ("artifacts" Core..= artifacts),
            Core.Just ("environment" Core..= environment),
            Core.Just ("serviceRole" Core..= serviceRole),
            ("badgeEnabled" Core..=) Core.<$> badgeEnabled,
            ("buildBatchConfig" Core..=) Core.<$> buildBatchConfig,
            ("cache" Core..=) Core.<$> cache,
            ("description" Core..=) Core.<$> description,
            ("encryptionKey" Core..=) Core.<$> encryptionKey,
            ("fileSystemLocations" Core..=) Core.<$> fileSystemLocations,
            ("logsConfig" Core..=) Core.<$> logsConfig,
            ("queuedTimeoutInMinutes" Core..=) Core.<$> queuedTimeoutInMinutes,
            ("secondaryArtifacts" Core..=) Core.<$> secondaryArtifacts,
            ("secondarySourceVersions" Core..=)
              Core.<$> secondarySourceVersions,
            ("secondarySources" Core..=) Core.<$> secondarySources,
            ("sourceVersion" Core..=) Core.<$> sourceVersion,
            ("tags" Core..=) Core.<$> tags,
            ("timeoutInMinutes" Core..=) Core.<$> timeoutInMinutes,
            ("vpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.CreateProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Core.<$> (x Core..:? "project") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Information about the build project that was created.
    project :: Core.Maybe Types.Project,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateProjectResponse' value with any optional fields omitted.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateProjectResponse
mkCreateProjectResponse responseStatus =
  CreateProjectResponse' {project = Core.Nothing, responseStatus}

-- | Information about the build project that was created.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsProject :: Lens.Lens' CreateProjectResponse (Core.Maybe Types.Project)
cprrsProject = Lens.field @"project"
{-# DEPRECATED cprrsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreateProjectResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
