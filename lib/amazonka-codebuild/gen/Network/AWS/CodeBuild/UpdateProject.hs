{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.UpdateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of a build project.
module Network.AWS.CodeBuild.UpdateProject
  ( -- * Creating a request
    UpdateProject (..),
    mkUpdateProject,

    -- ** Request lenses
    upName,
    upArtifacts,
    upBadgeEnabled,
    upBuildBatchConfig,
    upCache,
    upDescription,
    upEncryptionKey,
    upEnvironment,
    upFileSystemLocations,
    upLogsConfig,
    upQueuedTimeoutInMinutes,
    upSecondaryArtifacts,
    upSecondarySourceVersions,
    upSecondarySources,
    upServiceRole,
    upSource,
    upSourceVersion,
    upTags,
    upTimeoutInMinutes,
    upVpcConfig,

    -- * Destructuring the response
    UpdateProjectResponse (..),
    mkUpdateProjectResponse,

    -- ** Response lenses
    uprrsProject,
    uprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | The name of the build project.
    name :: Types.NonEmptyString,
    -- | Information to be changed about the build output artifacts for the build project.
    artifacts :: Core.Maybe Types.ProjectArtifacts,
    -- | Set this to true to generate a publicly accessible URL for your project's build badge.
    badgeEnabled :: Core.Maybe Core.Bool,
    buildBatchConfig :: Core.Maybe Types.ProjectBuildBatchConfig,
    -- | Stores recently used information so that it can be quickly accessed at a later time.
    cache :: Core.Maybe Types.ProjectCache,
    -- | A new or replacement description of the build project.
    description :: Core.Maybe Types.Description,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKey :: Core.Maybe Types.NonEmptyString,
    -- | Information to be changed about the build environment for the build project.
    environment :: Core.Maybe Types.ProjectEnvironment,
    -- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
    fileSystemLocations :: Core.Maybe [Types.ProjectFileSystemLocation],
    -- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both.
    logsConfig :: Core.Maybe Types.LogsConfig,
    -- | The number of minutes a build is allowed to be queued before it times out.
    queuedTimeoutInMinutes :: Core.Maybe Core.Natural,
    -- | An array of @ProjectSource@ objects.
    secondaryArtifacts :: Core.Maybe [Types.ProjectArtifacts],
    -- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
    secondarySourceVersions :: Core.Maybe [Types.ProjectSourceVersion],
    -- | An array of @ProjectSource@ objects.
    secondarySources :: Core.Maybe [Types.ProjectSource],
    -- | The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
    serviceRole :: Core.Maybe Types.NonEmptyString,
    -- | Information to be changed about the build input source code for the build project.
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
    -- | An updated list of tag key and value pairs associated with this build project.
    --
    -- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
    tags :: Core.Maybe [Types.Tag],
    -- | The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
    timeoutInMinutes :: Core.Maybe Core.Natural,
    -- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
    vpcConfig :: Core.Maybe Types.VpcConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProject' value with any optional fields omitted.
mkUpdateProject ::
  -- | 'name'
  Types.NonEmptyString ->
  UpdateProject
mkUpdateProject name =
  UpdateProject'
    { name,
      artifacts = Core.Nothing,
      badgeEnabled = Core.Nothing,
      buildBatchConfig = Core.Nothing,
      cache = Core.Nothing,
      description = Core.Nothing,
      encryptionKey = Core.Nothing,
      environment = Core.Nothing,
      fileSystemLocations = Core.Nothing,
      logsConfig = Core.Nothing,
      queuedTimeoutInMinutes = Core.Nothing,
      secondaryArtifacts = Core.Nothing,
      secondarySourceVersions = Core.Nothing,
      secondarySources = Core.Nothing,
      serviceRole = Core.Nothing,
      source = Core.Nothing,
      sourceVersion = Core.Nothing,
      tags = Core.Nothing,
      timeoutInMinutes = Core.Nothing,
      vpcConfig = Core.Nothing
    }

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject Types.NonEmptyString
upName = Lens.field @"name"
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information to be changed about the build output artifacts for the build project.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upArtifacts :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectArtifacts)
upArtifacts = Lens.field @"artifacts"
{-# DEPRECATED upArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- /Note:/ Consider using 'badgeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBadgeEnabled :: Lens.Lens' UpdateProject (Core.Maybe Core.Bool)
upBadgeEnabled = Lens.field @"badgeEnabled"
{-# DEPRECATED upBadgeEnabled "Use generic-lens or generic-optics with 'badgeEnabled' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBuildBatchConfig :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectBuildBatchConfig)
upBuildBatchConfig = Lens.field @"buildBatchConfig"
{-# DEPRECATED upBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | Stores recently used information so that it can be quickly accessed at a later time.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCache :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectCache)
upCache = Lens.field @"cache"
{-# DEPRECATED upCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | A new or replacement description of the build project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProject (Core.Maybe Types.Description)
upDescription = Lens.field @"description"
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upEncryptionKey :: Lens.Lens' UpdateProject (Core.Maybe Types.NonEmptyString)
upEncryptionKey = Lens.field @"encryptionKey"
{-# DEPRECATED upEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | Information to be changed about the build environment for the build project.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upEnvironment :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectEnvironment)
upEnvironment = Lens.field @"environment"
{-# DEPRECATED upEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upFileSystemLocations :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectFileSystemLocation])
upFileSystemLocations = Lens.field @"fileSystemLocations"
{-# DEPRECATED upFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both.
--
-- /Note:/ Consider using 'logsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upLogsConfig :: Lens.Lens' UpdateProject (Core.Maybe Types.LogsConfig)
upLogsConfig = Lens.field @"logsConfig"
{-# DEPRECATED upLogsConfig "Use generic-lens or generic-optics with 'logsConfig' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upQueuedTimeoutInMinutes :: Lens.Lens' UpdateProject (Core.Maybe Core.Natural)
upQueuedTimeoutInMinutes = Lens.field @"queuedTimeoutInMinutes"
{-# DEPRECATED upQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondaryArtifacts :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectArtifacts])
upSecondaryArtifacts = Lens.field @"secondaryArtifacts"
{-# DEPRECATED upSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondarySourceVersions :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectSourceVersion])
upSecondarySourceVersions = Lens.field @"secondarySourceVersions"
{-# DEPRECATED upSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondarySources :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectSource])
upSecondarySources = Lens.field @"secondarySources"
{-# DEPRECATED upSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

-- | The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upServiceRole :: Lens.Lens' UpdateProject (Core.Maybe Types.NonEmptyString)
upServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED upServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | Information to be changed about the build input source code for the build project.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSource :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectSource)
upSource = Lens.field @"source"
{-# DEPRECATED upSource "Use generic-lens or generic-optics with 'source' instead." #-}

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
upSourceVersion :: Lens.Lens' UpdateProject (Core.Maybe Types.String)
upSourceVersion = Lens.field @"sourceVersion"
{-# DEPRECATED upSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | An updated list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTags :: Lens.Lens' UpdateProject (Core.Maybe [Types.Tag])
upTags = Lens.field @"tags"
{-# DEPRECATED upTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTimeoutInMinutes :: Lens.Lens' UpdateProject (Core.Maybe Core.Natural)
upTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# DEPRECATED upTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upVpcConfig :: Lens.Lens' UpdateProject (Core.Maybe Types.VpcConfig)
upVpcConfig = Lens.field @"vpcConfig"
{-# DEPRECATED upVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

instance Core.FromJSON UpdateProject where
  toJSON UpdateProject {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            ("artifacts" Core..=) Core.<$> artifacts,
            ("badgeEnabled" Core..=) Core.<$> badgeEnabled,
            ("buildBatchConfig" Core..=) Core.<$> buildBatchConfig,
            ("cache" Core..=) Core.<$> cache,
            ("description" Core..=) Core.<$> description,
            ("encryptionKey" Core..=) Core.<$> encryptionKey,
            ("environment" Core..=) Core.<$> environment,
            ("fileSystemLocations" Core..=) Core.<$> fileSystemLocations,
            ("logsConfig" Core..=) Core.<$> logsConfig,
            ("queuedTimeoutInMinutes" Core..=) Core.<$> queuedTimeoutInMinutes,
            ("secondaryArtifacts" Core..=) Core.<$> secondaryArtifacts,
            ("secondarySourceVersions" Core..=)
              Core.<$> secondarySourceVersions,
            ("secondarySources" Core..=) Core.<$> secondarySources,
            ("serviceRole" Core..=) Core.<$> serviceRole,
            ("source" Core..=) Core.<$> source,
            ("sourceVersion" Core..=) Core.<$> sourceVersion,
            ("tags" Core..=) Core.<$> tags,
            ("timeoutInMinutes" Core..=) Core.<$> timeoutInMinutes,
            ("vpcConfig" Core..=) Core.<$> vpcConfig
          ]
      )

instance Core.AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.UpdateProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Core.<$> (x Core..:? "project") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | Information about the build project that was changed.
    project :: Core.Maybe Types.Project,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateProjectResponse' value with any optional fields omitted.
mkUpdateProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateProjectResponse
mkUpdateProjectResponse responseStatus =
  UpdateProjectResponse' {project = Core.Nothing, responseStatus}

-- | Information about the build project that was changed.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsProject :: Lens.Lens' UpdateProjectResponse (Core.Maybe Types.Project)
uprrsProject = Lens.field @"project"
{-# DEPRECATED uprrsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProjectResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
