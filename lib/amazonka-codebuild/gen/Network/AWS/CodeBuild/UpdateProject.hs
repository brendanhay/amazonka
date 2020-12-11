{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    upSecondaryArtifacts,
    upArtifacts,
    upEnvironment,
    upBadgeEnabled,
    upSecondarySourceVersions,
    upQueuedTimeoutInMinutes,
    upCache,
    upSecondarySources,
    upSourceVersion,
    upVpcConfig,
    upSource,
    upLogsConfig,
    upFileSystemLocations,
    upBuildBatchConfig,
    upEncryptionKey,
    upDescription,
    upServiceRole,
    upTags,
    upTimeoutInMinutes,
    upName,

    -- * Destructuring the response
    UpdateProjectResponse (..),
    mkUpdateProjectResponse,

    -- ** Response lenses
    uprsProject,
    uprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { secondaryArtifacts ::
      Lude.Maybe [ProjectArtifacts],
    artifacts :: Lude.Maybe ProjectArtifacts,
    environment :: Lude.Maybe ProjectEnvironment,
    badgeEnabled :: Lude.Maybe Lude.Bool,
    secondarySourceVersions :: Lude.Maybe [ProjectSourceVersion],
    queuedTimeoutInMinutes :: Lude.Maybe Lude.Natural,
    cache :: Lude.Maybe ProjectCache,
    secondarySources :: Lude.Maybe [ProjectSource],
    sourceVersion :: Lude.Maybe Lude.Text,
    vpcConfig :: Lude.Maybe VPCConfig,
    source :: Lude.Maybe ProjectSource,
    logsConfig :: Lude.Maybe LogsConfig,
    fileSystemLocations :: Lude.Maybe [ProjectFileSystemLocation],
    buildBatchConfig :: Lude.Maybe ProjectBuildBatchConfig,
    encryptionKey :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    serviceRole :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    timeoutInMinutes :: Lude.Maybe Lude.Natural,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- * 'artifacts' - Information to be changed about the build output artifacts for the build project.
-- * 'badgeEnabled' - Set this to true to generate a publicly accessible URL for your project's build badge.
-- * 'buildBatchConfig' - Undocumented field.
-- * 'cache' - Stores recently used information so that it can be quickly accessed at a later time.
-- * 'description' - A new or replacement description of the build project.
-- * 'encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
-- * 'environment' - Information to be changed about the build environment for the build project.
-- * 'fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
-- * 'logsConfig' - Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both.
-- * 'name' - The name of the build project.
-- * 'queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times out.
-- * 'secondaryArtifacts' - An array of @ProjectSource@ objects.
-- * 'secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
-- * 'secondarySources' - An array of @ProjectSource@ objects.
-- * 'serviceRole' - The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
-- * 'source' - Information to be changed about the build input source code for the build project.
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
-- * 'tags' - An updated list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
-- * 'timeoutInMinutes' - The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
-- * 'vpcConfig' - VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
mkUpdateProject ::
  -- | 'name'
  Lude.Text ->
  UpdateProject
mkUpdateProject pName_ =
  UpdateProject'
    { secondaryArtifacts = Lude.Nothing,
      artifacts = Lude.Nothing,
      environment = Lude.Nothing,
      badgeEnabled = Lude.Nothing,
      secondarySourceVersions = Lude.Nothing,
      queuedTimeoutInMinutes = Lude.Nothing,
      cache = Lude.Nothing,
      secondarySources = Lude.Nothing,
      sourceVersion = Lude.Nothing,
      vpcConfig = Lude.Nothing,
      source = Lude.Nothing,
      logsConfig = Lude.Nothing,
      fileSystemLocations = Lude.Nothing,
      buildBatchConfig = Lude.Nothing,
      encryptionKey = Lude.Nothing,
      description = Lude.Nothing,
      serviceRole = Lude.Nothing,
      tags = Lude.Nothing,
      timeoutInMinutes = Lude.Nothing,
      name = pName_
    }

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondaryArtifacts :: Lens.Lens' UpdateProject (Lude.Maybe [ProjectArtifacts])
upSecondaryArtifacts = Lens.lens (secondaryArtifacts :: UpdateProject -> Lude.Maybe [ProjectArtifacts]) (\s a -> s {secondaryArtifacts = a} :: UpdateProject)
{-# DEPRECATED upSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | Information to be changed about the build output artifacts for the build project.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upArtifacts :: Lens.Lens' UpdateProject (Lude.Maybe ProjectArtifacts)
upArtifacts = Lens.lens (artifacts :: UpdateProject -> Lude.Maybe ProjectArtifacts) (\s a -> s {artifacts = a} :: UpdateProject)
{-# DEPRECATED upArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Information to be changed about the build environment for the build project.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upEnvironment :: Lens.Lens' UpdateProject (Lude.Maybe ProjectEnvironment)
upEnvironment = Lens.lens (environment :: UpdateProject -> Lude.Maybe ProjectEnvironment) (\s a -> s {environment = a} :: UpdateProject)
{-# DEPRECATED upEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- /Note:/ Consider using 'badgeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBadgeEnabled :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Bool)
upBadgeEnabled = Lens.lens (badgeEnabled :: UpdateProject -> Lude.Maybe Lude.Bool) (\s a -> s {badgeEnabled = a} :: UpdateProject)
{-# DEPRECATED upBadgeEnabled "Use generic-lens or generic-optics with 'badgeEnabled' instead." #-}

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take over these @secondarySourceVersions@ (at the project level).
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondarySourceVersions :: Lens.Lens' UpdateProject (Lude.Maybe [ProjectSourceVersion])
upSecondarySourceVersions = Lens.lens (secondarySourceVersions :: UpdateProject -> Lude.Maybe [ProjectSourceVersion]) (\s a -> s {secondarySourceVersions = a} :: UpdateProject)
{-# DEPRECATED upSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upQueuedTimeoutInMinutes :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Natural)
upQueuedTimeoutInMinutes = Lens.lens (queuedTimeoutInMinutes :: UpdateProject -> Lude.Maybe Lude.Natural) (\s a -> s {queuedTimeoutInMinutes = a} :: UpdateProject)
{-# DEPRECATED upQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | Stores recently used information so that it can be quickly accessed at a later time.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upCache :: Lens.Lens' UpdateProject (Lude.Maybe ProjectCache)
upCache = Lens.lens (cache :: UpdateProject -> Lude.Maybe ProjectCache) (\s a -> s {cache = a} :: UpdateProject)
{-# DEPRECATED upCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSecondarySources :: Lens.Lens' UpdateProject (Lude.Maybe [ProjectSource])
upSecondarySources = Lens.lens (secondarySources :: UpdateProject -> Lude.Maybe [ProjectSource]) (\s a -> s {secondarySources = a} :: UpdateProject)
{-# DEPRECATED upSecondarySources "Use generic-lens or generic-optics with 'secondarySources' instead." #-}

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
upSourceVersion :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Text)
upSourceVersion = Lens.lens (sourceVersion :: UpdateProject -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: UpdateProject)
{-# DEPRECATED upSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upVpcConfig :: Lens.Lens' UpdateProject (Lude.Maybe VPCConfig)
upVpcConfig = Lens.lens (vpcConfig :: UpdateProject -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: UpdateProject)
{-# DEPRECATED upVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | Information to be changed about the build input source code for the build project.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upSource :: Lens.Lens' UpdateProject (Lude.Maybe ProjectSource)
upSource = Lens.lens (source :: UpdateProject -> Lude.Maybe ProjectSource) (\s a -> s {source = a} :: UpdateProject)
{-# DEPRECATED upSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Information about logs for the build project. A project can create logs in Amazon CloudWatch Logs, logs in an S3 bucket, or both.
--
-- /Note:/ Consider using 'logsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upLogsConfig :: Lens.Lens' UpdateProject (Lude.Maybe LogsConfig)
upLogsConfig = Lens.lens (logsConfig :: UpdateProject -> Lude.Maybe LogsConfig) (\s a -> s {logsConfig = a} :: UpdateProject)
{-# DEPRECATED upLogsConfig "Use generic-lens or generic-optics with 'logsConfig' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upFileSystemLocations :: Lens.Lens' UpdateProject (Lude.Maybe [ProjectFileSystemLocation])
upFileSystemLocations = Lens.lens (fileSystemLocations :: UpdateProject -> Lude.Maybe [ProjectFileSystemLocation]) (\s a -> s {fileSystemLocations = a} :: UpdateProject)
{-# DEPRECATED upFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upBuildBatchConfig :: Lens.Lens' UpdateProject (Lude.Maybe ProjectBuildBatchConfig)
upBuildBatchConfig = Lens.lens (buildBatchConfig :: UpdateProject -> Lude.Maybe ProjectBuildBatchConfig) (\s a -> s {buildBatchConfig = a} :: UpdateProject)
{-# DEPRECATED upBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upEncryptionKey :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Text)
upEncryptionKey = Lens.lens (encryptionKey :: UpdateProject -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKey = a} :: UpdateProject)
{-# DEPRECATED upEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | A new or replacement description of the build project.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Text)
upDescription = Lens.lens (description :: UpdateProject -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateProject)
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The replacement ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upServiceRole :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Text)
upServiceRole = Lens.lens (serviceRole :: UpdateProject -> Lude.Maybe Lude.Text) (\s a -> s {serviceRole = a} :: UpdateProject)
{-# DEPRECATED upServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | An updated list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTags :: Lens.Lens' UpdateProject (Lude.Maybe [Tag])
upTags = Lens.lens (tags :: UpdateProject -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateProject)
{-# DEPRECATED upTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The replacement value in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before timing out any related build that did not get marked as completed.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upTimeoutInMinutes :: Lens.Lens' UpdateProject (Lude.Maybe Lude.Natural)
upTimeoutInMinutes = Lens.lens (timeoutInMinutes :: UpdateProject -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInMinutes = a} :: UpdateProject)
{-# DEPRECATED upTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdateProject Lude.Text
upName = Lens.lens (name :: UpdateProject -> Lude.Text) (\s a -> s {name = a} :: UpdateProject)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateProject where
  type Rs UpdateProject = UpdateProjectResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Lude.<$> (x Lude..?> "project") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.UpdateProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("secondaryArtifacts" Lude..=) Lude.<$> secondaryArtifacts,
            ("artifacts" Lude..=) Lude.<$> artifacts,
            ("environment" Lude..=) Lude.<$> environment,
            ("badgeEnabled" Lude..=) Lude.<$> badgeEnabled,
            ("secondarySourceVersions" Lude..=)
              Lude.<$> secondarySourceVersions,
            ("queuedTimeoutInMinutes" Lude..=) Lude.<$> queuedTimeoutInMinutes,
            ("cache" Lude..=) Lude.<$> cache,
            ("secondarySources" Lude..=) Lude.<$> secondarySources,
            ("sourceVersion" Lude..=) Lude.<$> sourceVersion,
            ("vpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("source" Lude..=) Lude.<$> source,
            ("logsConfig" Lude..=) Lude.<$> logsConfig,
            ("fileSystemLocations" Lude..=) Lude.<$> fileSystemLocations,
            ("buildBatchConfig" Lude..=) Lude.<$> buildBatchConfig,
            ("encryptionKey" Lude..=) Lude.<$> encryptionKey,
            ("description" Lude..=) Lude.<$> description,
            ("serviceRole" Lude..=) Lude.<$> serviceRole,
            ("tags" Lude..=) Lude.<$> tags,
            ("timeoutInMinutes" Lude..=) Lude.<$> timeoutInMinutes,
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath UpdateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { project ::
      Lude.Maybe Project,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- * 'project' - Information about the build project that was changed.
-- * 'responseStatus' - The response status code.
mkUpdateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProjectResponse
mkUpdateProjectResponse pResponseStatus_ =
  UpdateProjectResponse'
    { project = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the build project that was changed.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsProject :: Lens.Lens' UpdateProjectResponse (Lude.Maybe Project)
uprsProject = Lens.lens (project :: UpdateProjectResponse -> Lude.Maybe Project) (\s a -> s {project = a} :: UpdateProjectResponse)
{-# DEPRECATED uprsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdateProjectResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProjectResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
