{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cpSecondaryArtifacts,
    cpBadgeEnabled,
    cpSecondarySourceVersions,
    cpQueuedTimeoutInMinutes,
    cpCache,
    cpSecondarySources,
    cpSourceVersion,
    cpVpcConfig,
    cpLogsConfig,
    cpFileSystemLocations,
    cpBuildBatchConfig,
    cpEncryptionKey,
    cpDescription,
    cpTags,
    cpTimeoutInMinutes,
    cpName,
    cpSource,
    cpArtifacts,
    cpEnvironment,
    cpServiceRole,

    -- * Destructuring the response
    CreateProjectResponse (..),
    mkCreateProjectResponse,

    -- ** Response lenses
    cprsProject,
    cprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProject' smart constructor.
data CreateProject = CreateProject'
  { secondaryArtifacts ::
      Lude.Maybe [ProjectArtifacts],
    badgeEnabled :: Lude.Maybe Lude.Bool,
    secondarySourceVersions :: Lude.Maybe [ProjectSourceVersion],
    queuedTimeoutInMinutes :: Lude.Maybe Lude.Natural,
    cache :: Lude.Maybe ProjectCache,
    secondarySources :: Lude.Maybe [ProjectSource],
    sourceVersion :: Lude.Maybe Lude.Text,
    vpcConfig :: Lude.Maybe VPCConfig,
    logsConfig :: Lude.Maybe LogsConfig,
    fileSystemLocations :: Lude.Maybe [ProjectFileSystemLocation],
    buildBatchConfig :: Lude.Maybe ProjectBuildBatchConfig,
    encryptionKey :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    timeoutInMinutes :: Lude.Maybe Lude.Natural,
    name :: Lude.Text,
    source :: ProjectSource,
    artifacts :: ProjectArtifacts,
    environment :: ProjectEnvironment,
    serviceRole :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- * 'artifacts' - Information about the build output artifacts for the build project.
-- * 'badgeEnabled' - Set this to true to generate a publicly accessible URL for your project's build badge.
-- * 'buildBatchConfig' - A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
-- * 'cache' - Stores recently used information so that it can be quickly accessed at a later time.
-- * 'description' - A description that makes the build project easy to identify.
-- * 'encryptionKey' - The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
-- * 'environment' - Information about the build environment for the build project.
-- * 'fileSystemLocations' - An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
-- * 'logsConfig' - Information about logs for the build project. These can be logs in Amazon CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
-- * 'name' - The name of the build project.
-- * 'queuedTimeoutInMinutes' - The number of minutes a build is allowed to be queued before it times out.
-- * 'secondaryArtifacts' - An array of @ProjectArtifacts@ objects.
-- * 'secondarySourceVersions' - An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take precedence over these @secondarySourceVersions@ (at the project level).
-- * 'secondarySources' - An array of @ProjectSource@ objects.
-- * 'serviceRole' - The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
-- * 'source' - Information about the build input source code for the build project.
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
-- * 'timeoutInMinutes' - How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before it times out any build that has not been marked as completed. The default is 60 minutes.
-- * 'vpcConfig' - VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
mkCreateProject ::
  -- | 'name'
  Lude.Text ->
  -- | 'source'
  ProjectSource ->
  -- | 'artifacts'
  ProjectArtifacts ->
  -- | 'environment'
  ProjectEnvironment ->
  -- | 'serviceRole'
  Lude.Text ->
  CreateProject
mkCreateProject
  pName_
  pSource_
  pArtifacts_
  pEnvironment_
  pServiceRole_ =
    CreateProject'
      { secondaryArtifacts = Lude.Nothing,
        badgeEnabled = Lude.Nothing,
        secondarySourceVersions = Lude.Nothing,
        queuedTimeoutInMinutes = Lude.Nothing,
        cache = Lude.Nothing,
        secondarySources = Lude.Nothing,
        sourceVersion = Lude.Nothing,
        vpcConfig = Lude.Nothing,
        logsConfig = Lude.Nothing,
        fileSystemLocations = Lude.Nothing,
        buildBatchConfig = Lude.Nothing,
        encryptionKey = Lude.Nothing,
        description = Lude.Nothing,
        tags = Lude.Nothing,
        timeoutInMinutes = Lude.Nothing,
        name = pName_,
        source = pSource_,
        artifacts = pArtifacts_,
        environment = pEnvironment_,
        serviceRole = pServiceRole_
      }

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecondaryArtifacts :: Lens.Lens' CreateProject (Lude.Maybe [ProjectArtifacts])
cpSecondaryArtifacts = Lens.lens (secondaryArtifacts :: CreateProject -> Lude.Maybe [ProjectArtifacts]) (\s a -> s {secondaryArtifacts = a} :: CreateProject)
{-# DEPRECATED cpSecondaryArtifacts "Use generic-lens or generic-optics with 'secondaryArtifacts' instead." #-}

-- | Set this to true to generate a publicly accessible URL for your project's build badge.
--
-- /Note:/ Consider using 'badgeEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpBadgeEnabled :: Lens.Lens' CreateProject (Lude.Maybe Lude.Bool)
cpBadgeEnabled = Lens.lens (badgeEnabled :: CreateProject -> Lude.Maybe Lude.Bool) (\s a -> s {badgeEnabled = a} :: CreateProject)
{-# DEPRECATED cpBadgeEnabled "Use generic-lens or generic-optics with 'badgeEnabled' instead." #-}

-- | An array of @ProjectSourceVersion@ objects. If @secondarySourceVersions@ is specified at the build level, then they take precedence over these @secondarySourceVersions@ (at the project level).
--
-- /Note:/ Consider using 'secondarySourceVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecondarySourceVersions :: Lens.Lens' CreateProject (Lude.Maybe [ProjectSourceVersion])
cpSecondarySourceVersions = Lens.lens (secondarySourceVersions :: CreateProject -> Lude.Maybe [ProjectSourceVersion]) (\s a -> s {secondarySourceVersions = a} :: CreateProject)
{-# DEPRECATED cpSecondarySourceVersions "Use generic-lens or generic-optics with 'secondarySourceVersions' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpQueuedTimeoutInMinutes :: Lens.Lens' CreateProject (Lude.Maybe Lude.Natural)
cpQueuedTimeoutInMinutes = Lens.lens (queuedTimeoutInMinutes :: CreateProject -> Lude.Maybe Lude.Natural) (\s a -> s {queuedTimeoutInMinutes = a} :: CreateProject)
{-# DEPRECATED cpQueuedTimeoutInMinutes "Use generic-lens or generic-optics with 'queuedTimeoutInMinutes' instead." #-}

-- | Stores recently used information so that it can be quickly accessed at a later time.
--
-- /Note:/ Consider using 'cache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpCache :: Lens.Lens' CreateProject (Lude.Maybe ProjectCache)
cpCache = Lens.lens (cache :: CreateProject -> Lude.Maybe ProjectCache) (\s a -> s {cache = a} :: CreateProject)
{-# DEPRECATED cpCache "Use generic-lens or generic-optics with 'cache' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSecondarySources :: Lens.Lens' CreateProject (Lude.Maybe [ProjectSource])
cpSecondarySources = Lens.lens (secondarySources :: CreateProject -> Lude.Maybe [ProjectSource]) (\s a -> s {secondarySources = a} :: CreateProject)
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
cpSourceVersion :: Lens.Lens' CreateProject (Lude.Maybe Lude.Text)
cpSourceVersion = Lens.lens (sourceVersion :: CreateProject -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: CreateProject)
{-# DEPRECATED cpSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | VpcConfig enables AWS CodeBuild to access resources in an Amazon VPC.
--
-- /Note:/ Consider using 'vpcConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVpcConfig :: Lens.Lens' CreateProject (Lude.Maybe VPCConfig)
cpVpcConfig = Lens.lens (vpcConfig :: CreateProject -> Lude.Maybe VPCConfig) (\s a -> s {vpcConfig = a} :: CreateProject)
{-# DEPRECATED cpVpcConfig "Use generic-lens or generic-optics with 'vpcConfig' instead." #-}

-- | Information about logs for the build project. These can be logs in Amazon CloudWatch Logs, logs uploaded to a specified S3 bucket, or both.
--
-- /Note:/ Consider using 'logsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLogsConfig :: Lens.Lens' CreateProject (Lude.Maybe LogsConfig)
cpLogsConfig = Lens.lens (logsConfig :: CreateProject -> Lude.Maybe LogsConfig) (\s a -> s {logsConfig = a} :: CreateProject)
{-# DEPRECATED cpLogsConfig "Use generic-lens or generic-optics with 'logsConfig' instead." #-}

-- | An array of @ProjectFileSystemLocation@ objects for a CodeBuild build project. A @ProjectFileSystemLocation@ object specifies the @identifier@ , @location@ , @mountOptions@ , @mountPoint@ , and @type@ of a file system created using Amazon Elastic File System.
--
-- /Note:/ Consider using 'fileSystemLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpFileSystemLocations :: Lens.Lens' CreateProject (Lude.Maybe [ProjectFileSystemLocation])
cpFileSystemLocations = Lens.lens (fileSystemLocations :: CreateProject -> Lude.Maybe [ProjectFileSystemLocation]) (\s a -> s {fileSystemLocations = a} :: CreateProject)
{-# DEPRECATED cpFileSystemLocations "Use generic-lens or generic-optics with 'fileSystemLocations' instead." #-}

-- | A 'ProjectBuildBatchConfig' object that defines the batch build options for the project.
--
-- /Note:/ Consider using 'buildBatchConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpBuildBatchConfig :: Lens.Lens' CreateProject (Lude.Maybe ProjectBuildBatchConfig)
cpBuildBatchConfig = Lens.lens (buildBatchConfig :: CreateProject -> Lude.Maybe ProjectBuildBatchConfig) (\s a -> s {buildBatchConfig = a} :: CreateProject)
{-# DEPRECATED cpBuildBatchConfig "Use generic-lens or generic-optics with 'buildBatchConfig' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) to be used for encrypting the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEncryptionKey :: Lens.Lens' CreateProject (Lude.Maybe Lude.Text)
cpEncryptionKey = Lens.lens (encryptionKey :: CreateProject -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKey = a} :: CreateProject)
{-# DEPRECATED cpEncryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead." #-}

-- | A description that makes the build project easy to identify.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreateProject (Lude.Maybe Lude.Text)
cpDescription = Lens.lens (description :: CreateProject -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateProject)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of tag key and value pairs associated with this build project.
--
-- These tags are available for use by AWS services that support AWS CodeBuild build project tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CreateProject (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CreateProject -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateProject)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | How long, in minutes, from 5 to 480 (8 hours), for AWS CodeBuild to wait before it times out any build that has not been marked as completed. The default is 60 minutes.
--
-- /Note:/ Consider using 'timeoutInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTimeoutInMinutes :: Lens.Lens' CreateProject (Lude.Maybe Lude.Natural)
cpTimeoutInMinutes = Lens.lens (timeoutInMinutes :: CreateProject -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInMinutes = a} :: CreateProject)
{-# DEPRECATED cpTimeoutInMinutes "Use generic-lens or generic-optics with 'timeoutInMinutes' instead." #-}

-- | The name of the build project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreateProject Lude.Text
cpName = Lens.lens (name :: CreateProject -> Lude.Text) (\s a -> s {name = a} :: CreateProject)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Information about the build input source code for the build project.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpSource :: Lens.Lens' CreateProject ProjectSource
cpSource = Lens.lens (source :: CreateProject -> ProjectSource) (\s a -> s {source = a} :: CreateProject)
{-# DEPRECATED cpSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | Information about the build output artifacts for the build project.
--
-- /Note:/ Consider using 'artifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpArtifacts :: Lens.Lens' CreateProject ProjectArtifacts
cpArtifacts = Lens.lens (artifacts :: CreateProject -> ProjectArtifacts) (\s a -> s {artifacts = a} :: CreateProject)
{-# DEPRECATED cpArtifacts "Use generic-lens or generic-optics with 'artifacts' instead." #-}

-- | Information about the build environment for the build project.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpEnvironment :: Lens.Lens' CreateProject ProjectEnvironment
cpEnvironment = Lens.lens (environment :: CreateProject -> ProjectEnvironment) (\s a -> s {environment = a} :: CreateProject)
{-# DEPRECATED cpEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that enables AWS CodeBuild to interact with dependent AWS services on behalf of the AWS account.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpServiceRole :: Lens.Lens' CreateProject Lude.Text
cpServiceRole = Lens.lens (serviceRole :: CreateProject -> Lude.Text) (\s a -> s {serviceRole = a} :: CreateProject)
{-# DEPRECATED cpServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Lude.<$> (x Lude..?> "project") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProject where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.CreateProject" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateProject where
  toJSON CreateProject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("secondaryArtifacts" Lude..=) Lude.<$> secondaryArtifacts,
            ("badgeEnabled" Lude..=) Lude.<$> badgeEnabled,
            ("secondarySourceVersions" Lude..=)
              Lude.<$> secondarySourceVersions,
            ("queuedTimeoutInMinutes" Lude..=) Lude.<$> queuedTimeoutInMinutes,
            ("cache" Lude..=) Lude.<$> cache,
            ("secondarySources" Lude..=) Lude.<$> secondarySources,
            ("sourceVersion" Lude..=) Lude.<$> sourceVersion,
            ("vpcConfig" Lude..=) Lude.<$> vpcConfig,
            ("logsConfig" Lude..=) Lude.<$> logsConfig,
            ("fileSystemLocations" Lude..=) Lude.<$> fileSystemLocations,
            ("buildBatchConfig" Lude..=) Lude.<$> buildBatchConfig,
            ("encryptionKey" Lude..=) Lude.<$> encryptionKey,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags,
            ("timeoutInMinutes" Lude..=) Lude.<$> timeoutInMinutes,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("source" Lude..= source),
            Lude.Just ("artifacts" Lude..= artifacts),
            Lude.Just ("environment" Lude..= environment),
            Lude.Just ("serviceRole" Lude..= serviceRole)
          ]
      )

instance Lude.ToPath CreateProject where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateProject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
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

-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- * 'project' - Information about the build project that was created.
-- * 'responseStatus' - The response status code.
mkCreateProjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProjectResponse
mkCreateProjectResponse pResponseStatus_ =
  CreateProjectResponse'
    { project = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the build project that was created.
--
-- /Note:/ Consider using 'project' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsProject :: Lens.Lens' CreateProjectResponse (Lude.Maybe Project)
cprsProject = Lens.lens (project :: CreateProjectResponse -> Lude.Maybe Project) (\s a -> s {project = a} :: CreateProjectResponse)
{-# DEPRECATED cprsProject "Use generic-lens or generic-optics with 'project' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreateProjectResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreateProjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProjectResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
