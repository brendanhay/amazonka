{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateProject (..)
    , mkUpdateProject
    -- ** Request lenses
    , upName
    , upArtifacts
    , upBadgeEnabled
    , upBuildBatchConfig
    , upCache
    , upDescription
    , upEncryptionKey
    , upEnvironment
    , upFileSystemLocations
    , upLogsConfig
    , upQueuedTimeoutInMinutes
    , upSecondaryArtifacts
    , upSecondarySourceVersions
    , upSecondarySources
    , upServiceRole
    , upSource
    , upSourceVersion
    , upTags
    , upTimeoutInMinutes
    , upVpcConfig

    -- * Destructuring the response
    , UpdateProjectResponse (..)
    , mkUpdateProjectResponse
    -- ** Response lenses
    , uprrsProject
    , uprrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { name :: Types.NonEmptyString
    -- ^ The name of the build project.
  , artifacts :: Core.Maybe Types.ProjectArtifacts
    -- ^ Information to be changed about the build output artifacts for the build project.
  , badgeEnabled :: Core.Maybe Core.Bool
    -- ^ Set this to true to generate a publicly accessible URL for your project's build badge.
  , buildBatchConfig :: Core.Maybe Types.ProjectBuildBatchConfig
  , cache :: Core.Maybe Types.ProjectCache
    -- ^ Stores recently used information so that it can be quickly accessed at a later time.
  , description :: Core.Maybe Types.Description
    -- ^ A new or replacement description of the build project.
  , encryptionKey :: Core.Maybe Types.NonEmptyString
    -- ^ The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
  , environment :: Core.Maybe Types.ProjectEnvironment
    -- ^ Information to be changed about the build environment for the build project.
  , fileSystemLocations :: Core.Maybe [Types.ProjectFileSystemLocation]
    -- ^ An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System. 
  , logsConfig :: Core.Maybe Types.LogsConfig
    -- ^ Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both. 
  , queuedTimeoutInMinutes :: Core.Maybe Core.Natural
    -- ^ The number of minutes a build is allowed to be queued before it times out. 
  , secondaryArtifacts :: Core.Maybe [Types.ProjectArtifacts]
    -- ^ An array of @ProjectSource@ objects. 
  , secondarySourceVersions :: Core.Maybe [Types.ProjectSourceVersion]
    -- ^ An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level). 
  , secondarySources :: Core.Maybe [Types.ProjectSource]
    -- ^ An array of @ProjectSource@ objects. 
  , serviceRole :: Core.Maybe Types.NonEmptyString
    -- ^ The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
  , source :: Core.Maybe Types.ProjectSource
    -- ^ Information to be changed about the build input source code for the build project.
  , sourceVersion :: Core.Maybe Core.Text
    -- ^ A version of the build input to be built for this project. If not specified, the latest version is used. If specified, it must be one of: 
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
  , tags :: Core.Maybe [Types.Tag]
    -- ^ An updated list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
  , timeoutInMinutes :: Core.Maybe Core.Natural
    -- ^ The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
  , vpcConfig :: Core.Maybe Types.VpcConfig
    -- ^ VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateProject' value with any optional fields omitted.
mkUpdateProject
    :: Types.NonEmptyString -- ^ 'name'
    -> UpdateProject
mkUpdateProject name
  = UpdateProject'{name, artifacts = Core.Nothing,
                   badgeEnabled = Core.Nothing, buildBatchConfig = Core.Nothing,
                   cache = Core.Nothing, description = Core.Nothing,
                   encryptionKey = Core.Nothing, environment = Core.Nothing,
                   fileSystemLocations = Core.Nothing, logsConfig = Core.Nothing,
                   queuedTimeoutInMinutes = Core.Nothing,
                   secondaryArtifacts = Core.Nothing,
                   secondarySourceVersions = Core.Nothing,
                   secondarySources = Core.Nothing, serviceRole = Core.Nothing,
                   source = Core.Nothing, sourceVersion = Core.Nothing,
                   tags = Core.Nothing, timeoutInMinutes = Core.Nothing,
                   vpcConfig = Core.Nothing}

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject Types.NonEmptyString
upName = Lens.field @"name"
{-# INLINEABLE upName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Information to be changed about the build output artifacts for the build project.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upArtifacts :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectArtifacts)
upArtifacts = Lens.field @"artifacts"
{-# INLINEABLE upArtifacts #-}
{-# DEPRECATED artifacts "Use generic-lens or generic-optics with 'artifacts' instead"  #-}

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- /Note:/ Consider using 'badgeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBadgeEnabled :: Lens.Lens' UpdateProject (Core.Maybe Core.Bool)
upBadgeEnabled = Lens.field @"badgeEnabled"
{-# INLINEABLE upBadgeEnabled #-}
{-# DEPRECATED badgeEnabled "Use generic-lens or generic-optics with 'badgeEnabled' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBuildBatchConfig :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectBuildBatchConfig)
upBuildBatchConfig = Lens.field @"buildBatchConfig"
{-# INLINEABLE upBuildBatchConfig #-}
{-# DEPRECATED buildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead"  #-}

-- | Stores recently used information so that it can be quickly accessed at a later time.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCache :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectCache)
upCache = Lens.field @"cache"
{-# INLINEABLE upCache #-}
{-# DEPRECATED cache "Use generic-lens or generic-optics with 'cache' instead"  #-}

-- | A new or replacement description of the build project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProject (Core.Maybe Types.Description)
upDescription = Lens.field @"description"
{-# INLINEABLE upDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upEncryptionKey :: Lens.Lens' UpdateProject (Core.Maybe Types.NonEmptyString)
upEncryptionKey = Lens.field @"encryptionKey"
{-# INLINEABLE upEncryptionKey #-}
{-# DEPRECATED encryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead"  #-}

-- | Information to be changed about the build environment for the build project.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upEnvironment :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectEnvironment)
upEnvironment = Lens.field @"environment"
{-# INLINEABLE upEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System. 
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upFileSystemLocations :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectFileSystemLocation])
upFileSystemLocations = Lens.field @"fileSystemLocations"
{-# INLINEABLE upFileSystemLocations #-}
{-# DEPRECATED fileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead"  #-}

-- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both. 
--
-- /Note:/ Consider using 'logsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upLogsConfig :: Lens.Lens' UpdateProject (Core.Maybe Types.LogsConfig)
upLogsConfig = Lens.field @"logsConfig"
{-# INLINEABLE upLogsConfig #-}
{-# DEPRECATED logsConfig "Use generic-lens or generic-optics with 'logsConfig' instead"  #-}

-- | The number of minutes a build is allowed to be queued before it times out. 
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upQueuedTimeoutInMinutes :: Lens.Lens' UpdateProject (Core.Maybe Core.Natural)
upQueuedTimeoutInMinutes = Lens.field @"queuedTimeoutInMinutes"
{-# INLINEABLE upQueuedTimeoutInMinutes #-}
{-# DEPRECATED queuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead"  #-}

-- | An array of @ProjectSource@ objects. 
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondaryArtifacts :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectArtifacts])
upSecondaryArtifacts = Lens.field @"secondaryArtifacts"
{-# INLINEABLE upSecondaryArtifacts #-}
{-# DEPRECATED secondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead"  #-}

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level). 
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondarySourceVersions :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectSourceVersion])
upSecondarySourceVersions = Lens.field @"secondarySourceVersions"
{-# INLINEABLE upSecondarySourceVersions #-}
{-# DEPRECATED secondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead"  #-}

-- | An array of @ProjectSource@ objects. 
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondarySources :: Lens.Lens' UpdateProject (Core.Maybe [Types.ProjectSource])
upSecondarySources = Lens.field @"secondarySources"
{-# INLINEABLE upSecondarySources #-}
{-# DEPRECATED secondarySources "Use generic-lens or generic-optics with 'secondarySources' instead"  #-}

-- | The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upServiceRole :: Lens.Lens' UpdateProject (Core.Maybe Types.NonEmptyString)
upServiceRole = Lens.field @"serviceRole"
{-# INLINEABLE upServiceRole #-}
{-# DEPRECATED serviceRole "Use generic-lens or generic-optics with 'serviceRole' instead"  #-}

-- | Information to be changed about the build input source code for the build project.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSource :: Lens.Lens' UpdateProject (Core.Maybe Types.ProjectSource)
upSource = Lens.field @"source"
{-# INLINEABLE upSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

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
upSourceVersion :: Lens.Lens' UpdateProject (Core.Maybe Core.Text)
upSourceVersion = Lens.field @"sourceVersion"
{-# INLINEABLE upSourceVersion #-}
{-# DEPRECATED sourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead"  #-}

-- | An updated list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTags :: Lens.Lens' UpdateProject (Core.Maybe [Types.Tag])
upTags = Lens.field @"tags"
{-# INLINEABLE upTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTimeoutInMinutes :: Lens.Lens' UpdateProject (Core.Maybe Core.Natural)
upTimeoutInMinutes = Lens.field @"timeoutInMinutes"
{-# INLINEABLE upTimeoutInMinutes #-}
{-# DEPRECATED timeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead"  #-}

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upVpcConfig :: Lens.Lens' UpdateProject (Core.Maybe Types.VpcConfig)
upVpcConfig = Lens.field @"vpcConfig"
{-# INLINEABLE upVpcConfig #-}
{-# DEPRECATED vpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead"  #-}

instance Core.ToQuery UpdateProject where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateProject where
        toHeaders UpdateProject{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.UpdateProject")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateProject where
        toJSON UpdateProject{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
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
                  ("secondarySourceVersions" Core..=) Core.<$>
                    secondarySourceVersions,
                  ("secondarySources" Core..=) Core.<$> secondarySources,
                  ("serviceRole" Core..=) Core.<$> serviceRole,
                  ("source" Core..=) Core.<$> source,
                  ("sourceVersion" Core..=) Core.<$> sourceVersion,
                  ("tags" Core..=) Core.<$> tags,
                  ("timeoutInMinutes" Core..=) Core.<$> timeoutInMinutes,
                  ("vpcConfig" Core..=) Core.<$> vpcConfig])

instance Core.AWSRequest UpdateProject where
        type Rs UpdateProject = UpdateProjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateProjectResponse' Core.<$>
                   (x Core..:? "project") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { project :: Core.Maybe Types.Project
    -- ^ Information about the build project that was changed.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateProjectResponse' value with any optional fields omitted.
mkUpdateProjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateProjectResponse
mkUpdateProjectResponse responseStatus
  = UpdateProjectResponse'{project = Core.Nothing, responseStatus}

-- | Information about the build project that was changed.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsProject :: Lens.Lens' UpdateProjectResponse (Core.Maybe Types.Project)
uprrsProject = Lens.field @"project"
{-# INLINEABLE uprrsProject #-}
{-# DEPRECATED project "Use generic-lens or generic-optics with 'project' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprrsResponseStatus :: Lens.Lens' UpdateProjectResponse Core.Int
uprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
