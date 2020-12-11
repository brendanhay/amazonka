{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StartBuild
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts running a build.
module Network.AWS.CodeBuild.StartBuild
  ( -- * Creating a request
    StartBuild (..),
    mkStartBuild,

    -- ** Request lenses
    sbEncryptionKeyOverride,
    sbSourceLocationOverride,
    sbEnvironmentVariablesOverride,
    sbBuildStatusConfigOverride,
    sbIdempotencyToken,
    sbDebugSessionEnabled,
    sbRegistryCredentialOverride,
    sbTimeoutInMinutesOverride,
    sbServiceRoleOverride,
    sbCacheOverride,
    sbQueuedTimeoutInMinutesOverride,
    sbSecondarySourcesOverride,
    sbGitCloneDepthOverride,
    sbImagePullCredentialsTypeOverride,
    sbLogsConfigOverride,
    sbSourceAuthOverride,
    sbGitSubmodulesConfigOverride,
    sbEnvironmentTypeOverride,
    sbCertificateOverride,
    sbComputeTypeOverride,
    sbPrivilegedModeOverride,
    sbSourceVersion,
    sbBuildspecOverride,
    sbSecondarySourcesVersionOverride,
    sbReportBuildStatusOverride,
    sbInsecureSSLOverride,
    sbImageOverride,
    sbSecondaryArtifactsOverride,
    sbArtifactsOverride,
    sbSourceTypeOverride,
    sbProjectName,

    -- * Destructuring the response
    StartBuildResponse (..),
    mkStartBuildResponse,

    -- ** Response lenses
    srsBuild,
    srsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartBuild' smart constructor.
data StartBuild = StartBuild'
  { encryptionKeyOverride ::
      Lude.Maybe Lude.Text,
    sourceLocationOverride :: Lude.Maybe Lude.Text,
    environmentVariablesOverride :: Lude.Maybe [EnvironmentVariable],
    buildStatusConfigOverride :: Lude.Maybe BuildStatusConfig,
    idempotencyToken :: Lude.Maybe Lude.Text,
    debugSessionEnabled :: Lude.Maybe Lude.Bool,
    registryCredentialOverride :: Lude.Maybe RegistryCredential,
    timeoutInMinutesOverride :: Lude.Maybe Lude.Natural,
    serviceRoleOverride :: Lude.Maybe Lude.Text,
    cacheOverride :: Lude.Maybe ProjectCache,
    queuedTimeoutInMinutesOverride :: Lude.Maybe Lude.Natural,
    secondarySourcesOverride :: Lude.Maybe [ProjectSource],
    gitCloneDepthOverride :: Lude.Maybe Lude.Natural,
    imagePullCredentialsTypeOverride ::
      Lude.Maybe ImagePullCredentialsType,
    logsConfigOverride :: Lude.Maybe LogsConfig,
    sourceAuthOverride :: Lude.Maybe SourceAuth,
    gitSubmodulesConfigOverride :: Lude.Maybe GitSubmodulesConfig,
    environmentTypeOverride :: Lude.Maybe EnvironmentType,
    certificateOverride :: Lude.Maybe Lude.Text,
    computeTypeOverride :: Lude.Maybe ComputeType,
    privilegedModeOverride :: Lude.Maybe Lude.Bool,
    sourceVersion :: Lude.Maybe Lude.Text,
    buildspecOverride :: Lude.Maybe Lude.Text,
    secondarySourcesVersionOverride ::
      Lude.Maybe [ProjectSourceVersion],
    reportBuildStatusOverride :: Lude.Maybe Lude.Bool,
    insecureSSLOverride :: Lude.Maybe Lude.Bool,
    imageOverride :: Lude.Maybe Lude.Text,
    secondaryArtifactsOverride :: Lude.Maybe [ProjectArtifacts],
    artifactsOverride :: Lude.Maybe ProjectArtifacts,
    sourceTypeOverride :: Lude.Maybe SourceType,
    projectName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartBuild' with the minimum fields required to make a request.
--
-- * 'artifactsOverride' - Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
-- * 'buildStatusConfigOverride' - Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
-- * 'buildspecOverride' - A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
-- * 'cacheOverride' - A ProjectCache object specified for this build that overrides the one defined in the build project.
-- * 'certificateOverride' - The name of a certificate for this build that overrides the one specified in the build project.
-- * 'computeTypeOverride' - The name of a compute type for this build that overrides the one specified in the build project.
-- * 'debugSessionEnabled' - Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
-- * 'encryptionKeyOverride' - The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
-- * 'environmentTypeOverride' - A container type for this build that overrides the one specified in the build project.
-- * 'environmentVariablesOverride' - A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
-- * 'gitCloneDepthOverride' - The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
-- * 'gitSubmodulesConfigOverride' - Information about the Git submodules configuration for this build of an AWS CodeBuild build project.
-- * 'idempotencyToken' - A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
-- * 'imageOverride' - The name of an image for this build that overrides the one specified in the build project.
-- * 'imagePullCredentialsTypeOverride' - The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:
--
--
--     * CODEBUILD
--
--     * Specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.
--
--
--     * SERVICE_ROLE
--
--     * Specifies that AWS CodeBuild uses your build project's service role.
--
--
-- When using a cross-account or private registry image, you must use @SERVICE_ROLE@ credentials. When using an AWS CodeBuild curated image, you must use @CODEBUILD@ credentials.
-- * 'insecureSSLOverride' - Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
-- * 'logsConfigOverride' - Log settings for this build that override the log settings defined in the build project.
-- * 'privilegedModeOverride' - Enable this flag to override privileged mode in the build project.
-- * 'projectName' - The name of the AWS CodeBuild build project to start running a build.
-- * 'queuedTimeoutInMinutesOverride' - The number of minutes a build is allowed to be queued before it times out.
-- * 'registryCredentialOverride' - The credentials for access to a private registry.
-- * 'reportBuildStatusOverride' - Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown.
-- * 'secondaryArtifactsOverride' - An array of @ProjectArtifacts@ objects.
-- * 'secondarySourcesOverride' - An array of @ProjectSource@ objects.
-- * 'secondarySourcesVersionOverride' - An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only.
-- * 'serviceRoleOverride' - The name of a service role for this build that overrides the one specified in the build project.
-- * 'sourceAuthOverride' - An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
-- * 'sourceLocationOverride' - A location that overrides, for this build, the source location for the one defined in the build project.
-- * 'sourceTypeOverride' - A source input type, for this build, that overrides the source input defined in the build project.
-- * 'sourceVersion' - The version of the build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:
--
--
--     * AWS CodeCommit
--
--     * The commit ID, branch, or Git tag to use.
--
--
--     * GitHub
--
--     * The commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * Bitbucket
--
--     * The commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * Amazon Simple Storage Service (Amazon S3)
--
--     * The version ID of the object that represents the build input ZIP file to use.
--
--
-- If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
-- * 'timeoutInMinutesOverride' - The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
mkStartBuild ::
  -- | 'projectName'
  Lude.Text ->
  StartBuild
mkStartBuild pProjectName_ =
  StartBuild'
    { encryptionKeyOverride = Lude.Nothing,
      sourceLocationOverride = Lude.Nothing,
      environmentVariablesOverride = Lude.Nothing,
      buildStatusConfigOverride = Lude.Nothing,
      idempotencyToken = Lude.Nothing,
      debugSessionEnabled = Lude.Nothing,
      registryCredentialOverride = Lude.Nothing,
      timeoutInMinutesOverride = Lude.Nothing,
      serviceRoleOverride = Lude.Nothing,
      cacheOverride = Lude.Nothing,
      queuedTimeoutInMinutesOverride = Lude.Nothing,
      secondarySourcesOverride = Lude.Nothing,
      gitCloneDepthOverride = Lude.Nothing,
      imagePullCredentialsTypeOverride = Lude.Nothing,
      logsConfigOverride = Lude.Nothing,
      sourceAuthOverride = Lude.Nothing,
      gitSubmodulesConfigOverride = Lude.Nothing,
      environmentTypeOverride = Lude.Nothing,
      certificateOverride = Lude.Nothing,
      computeTypeOverride = Lude.Nothing,
      privilegedModeOverride = Lude.Nothing,
      sourceVersion = Lude.Nothing,
      buildspecOverride = Lude.Nothing,
      secondarySourcesVersionOverride = Lude.Nothing,
      reportBuildStatusOverride = Lude.Nothing,
      insecureSSLOverride = Lude.Nothing,
      imageOverride = Lude.Nothing,
      secondaryArtifactsOverride = Lude.Nothing,
      artifactsOverride = Lude.Nothing,
      sourceTypeOverride = Lude.Nothing,
      projectName = pProjectName_
    }

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKeyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEncryptionKeyOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbEncryptionKeyOverride = Lens.lens (encryptionKeyOverride :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKeyOverride = a} :: StartBuild)
{-# DEPRECATED sbEncryptionKeyOverride "Use generic-lens or generic-optics with 'encryptionKeyOverride' instead." #-}

-- | A location that overrides, for this build, the source location for the one defined in the build project.
--
-- /Note:/ Consider using 'sourceLocationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceLocationOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbSourceLocationOverride = Lens.lens (sourceLocationOverride :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {sourceLocationOverride = a} :: StartBuild)
{-# DEPRECATED sbSourceLocationOverride "Use generic-lens or generic-optics with 'sourceLocationOverride' instead." #-}

-- | A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
--
-- /Note:/ Consider using 'environmentVariablesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEnvironmentVariablesOverride :: Lens.Lens' StartBuild (Lude.Maybe [EnvironmentVariable])
sbEnvironmentVariablesOverride = Lens.lens (environmentVariablesOverride :: StartBuild -> Lude.Maybe [EnvironmentVariable]) (\s a -> s {environmentVariablesOverride = a} :: StartBuild)
{-# DEPRECATED sbEnvironmentVariablesOverride "Use generic-lens or generic-optics with 'environmentVariablesOverride' instead." #-}

-- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
--
-- /Note:/ Consider using 'buildStatusConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBuildStatusConfigOverride :: Lens.Lens' StartBuild (Lude.Maybe BuildStatusConfig)
sbBuildStatusConfigOverride = Lens.lens (buildStatusConfigOverride :: StartBuild -> Lude.Maybe BuildStatusConfig) (\s a -> s {buildStatusConfigOverride = a} :: StartBuild)
{-# DEPRECATED sbBuildStatusConfigOverride "Use generic-lens or generic-optics with 'buildStatusConfigOverride' instead." #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbIdempotencyToken :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbIdempotencyToken = Lens.lens (idempotencyToken :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: StartBuild)
{-# DEPRECATED sbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
--
-- /Note:/ Consider using 'debugSessionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbDebugSessionEnabled :: Lens.Lens' StartBuild (Lude.Maybe Lude.Bool)
sbDebugSessionEnabled = Lens.lens (debugSessionEnabled :: StartBuild -> Lude.Maybe Lude.Bool) (\s a -> s {debugSessionEnabled = a} :: StartBuild)
{-# DEPRECATED sbDebugSessionEnabled "Use generic-lens or generic-optics with 'debugSessionEnabled' instead." #-}

-- | The credentials for access to a private registry.
--
-- /Note:/ Consider using 'registryCredentialOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbRegistryCredentialOverride :: Lens.Lens' StartBuild (Lude.Maybe RegistryCredential)
sbRegistryCredentialOverride = Lens.lens (registryCredentialOverride :: StartBuild -> Lude.Maybe RegistryCredential) (\s a -> s {registryCredentialOverride = a} :: StartBuild)
{-# DEPRECATED sbRegistryCredentialOverride "Use generic-lens or generic-optics with 'registryCredentialOverride' instead." #-}

-- | The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
--
-- /Note:/ Consider using 'timeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Natural)
sbTimeoutInMinutesOverride = Lens.lens (timeoutInMinutesOverride :: StartBuild -> Lude.Maybe Lude.Natural) (\s a -> s {timeoutInMinutesOverride = a} :: StartBuild)
{-# DEPRECATED sbTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'timeoutInMinutesOverride' instead." #-}

-- | The name of a service role for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'serviceRoleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbServiceRoleOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbServiceRoleOverride = Lens.lens (serviceRoleOverride :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleOverride = a} :: StartBuild)
{-# DEPRECATED sbServiceRoleOverride "Use generic-lens or generic-optics with 'serviceRoleOverride' instead." #-}

-- | A ProjectCache object specified for this build that overrides the one defined in the build project.
--
-- /Note:/ Consider using 'cacheOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbCacheOverride :: Lens.Lens' StartBuild (Lude.Maybe ProjectCache)
sbCacheOverride = Lens.lens (cacheOverride :: StartBuild -> Lude.Maybe ProjectCache) (\s a -> s {cacheOverride = a} :: StartBuild)
{-# DEPRECATED sbCacheOverride "Use generic-lens or generic-optics with 'cacheOverride' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbQueuedTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Natural)
sbQueuedTimeoutInMinutesOverride = Lens.lens (queuedTimeoutInMinutesOverride :: StartBuild -> Lude.Maybe Lude.Natural) (\s a -> s {queuedTimeoutInMinutesOverride = a} :: StartBuild)
{-# DEPRECATED sbQueuedTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'queuedTimeoutInMinutesOverride' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySourcesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondarySourcesOverride :: Lens.Lens' StartBuild (Lude.Maybe [ProjectSource])
sbSecondarySourcesOverride = Lens.lens (secondarySourcesOverride :: StartBuild -> Lude.Maybe [ProjectSource]) (\s a -> s {secondarySourcesOverride = a} :: StartBuild)
{-# DEPRECATED sbSecondarySourcesOverride "Use generic-lens or generic-optics with 'secondarySourcesOverride' instead." #-}

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
--
-- /Note:/ Consider using 'gitCloneDepthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbGitCloneDepthOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Natural)
sbGitCloneDepthOverride = Lens.lens (gitCloneDepthOverride :: StartBuild -> Lude.Maybe Lude.Natural) (\s a -> s {gitCloneDepthOverride = a} :: StartBuild)
{-# DEPRECATED sbGitCloneDepthOverride "Use generic-lens or generic-optics with 'gitCloneDepthOverride' instead." #-}

-- | The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:
--
--
--     * CODEBUILD
--
--     * Specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.
--
--
--     * SERVICE_ROLE
--
--     * Specifies that AWS CodeBuild uses your build project's service role.
--
--
-- When using a cross-account or private registry image, you must use @SERVICE_ROLE@ credentials. When using an AWS CodeBuild curated image, you must use @CODEBUILD@ credentials.
--
-- /Note:/ Consider using 'imagePullCredentialsTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbImagePullCredentialsTypeOverride :: Lens.Lens' StartBuild (Lude.Maybe ImagePullCredentialsType)
sbImagePullCredentialsTypeOverride = Lens.lens (imagePullCredentialsTypeOverride :: StartBuild -> Lude.Maybe ImagePullCredentialsType) (\s a -> s {imagePullCredentialsTypeOverride = a} :: StartBuild)
{-# DEPRECATED sbImagePullCredentialsTypeOverride "Use generic-lens or generic-optics with 'imagePullCredentialsTypeOverride' instead." #-}

-- | Log settings for this build that override the log settings defined in the build project.
--
-- /Note:/ Consider using 'logsConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbLogsConfigOverride :: Lens.Lens' StartBuild (Lude.Maybe LogsConfig)
sbLogsConfigOverride = Lens.lens (logsConfigOverride :: StartBuild -> Lude.Maybe LogsConfig) (\s a -> s {logsConfigOverride = a} :: StartBuild)
{-# DEPRECATED sbLogsConfigOverride "Use generic-lens or generic-optics with 'logsConfigOverride' instead." #-}

-- | An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
--
-- /Note:/ Consider using 'sourceAuthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceAuthOverride :: Lens.Lens' StartBuild (Lude.Maybe SourceAuth)
sbSourceAuthOverride = Lens.lens (sourceAuthOverride :: StartBuild -> Lude.Maybe SourceAuth) (\s a -> s {sourceAuthOverride = a} :: StartBuild)
{-# DEPRECATED sbSourceAuthOverride "Use generic-lens or generic-optics with 'sourceAuthOverride' instead." #-}

-- | Information about the Git submodules configuration for this build of an AWS CodeBuild build project.
--
-- /Note:/ Consider using 'gitSubmodulesConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbGitSubmodulesConfigOverride :: Lens.Lens' StartBuild (Lude.Maybe GitSubmodulesConfig)
sbGitSubmodulesConfigOverride = Lens.lens (gitSubmodulesConfigOverride :: StartBuild -> Lude.Maybe GitSubmodulesConfig) (\s a -> s {gitSubmodulesConfigOverride = a} :: StartBuild)
{-# DEPRECATED sbGitSubmodulesConfigOverride "Use generic-lens or generic-optics with 'gitSubmodulesConfigOverride' instead." #-}

-- | A container type for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'environmentTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEnvironmentTypeOverride :: Lens.Lens' StartBuild (Lude.Maybe EnvironmentType)
sbEnvironmentTypeOverride = Lens.lens (environmentTypeOverride :: StartBuild -> Lude.Maybe EnvironmentType) (\s a -> s {environmentTypeOverride = a} :: StartBuild)
{-# DEPRECATED sbEnvironmentTypeOverride "Use generic-lens or generic-optics with 'environmentTypeOverride' instead." #-}

-- | The name of a certificate for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'certificateOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbCertificateOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbCertificateOverride = Lens.lens (certificateOverride :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {certificateOverride = a} :: StartBuild)
{-# DEPRECATED sbCertificateOverride "Use generic-lens or generic-optics with 'certificateOverride' instead." #-}

-- | The name of a compute type for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'computeTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbComputeTypeOverride :: Lens.Lens' StartBuild (Lude.Maybe ComputeType)
sbComputeTypeOverride = Lens.lens (computeTypeOverride :: StartBuild -> Lude.Maybe ComputeType) (\s a -> s {computeTypeOverride = a} :: StartBuild)
{-# DEPRECATED sbComputeTypeOverride "Use generic-lens or generic-optics with 'computeTypeOverride' instead." #-}

-- | Enable this flag to override privileged mode in the build project.
--
-- /Note:/ Consider using 'privilegedModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbPrivilegedModeOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Bool)
sbPrivilegedModeOverride = Lens.lens (privilegedModeOverride :: StartBuild -> Lude.Maybe Lude.Bool) (\s a -> s {privilegedModeOverride = a} :: StartBuild)
{-# DEPRECATED sbPrivilegedModeOverride "Use generic-lens or generic-optics with 'privilegedModeOverride' instead." #-}

-- | The version of the build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:
--
--
--     * AWS CodeCommit
--
--     * The commit ID, branch, or Git tag to use.
--
--
--     * GitHub
--
--     * The commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * Bitbucket
--
--     * The commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.
--
--
--     * Amazon Simple Storage Service (Amazon S3)
--
--     * The version ID of the object that represents the build input ZIP file to use.
--
--
-- If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.
-- For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- /Note:/ Consider using 'sourceVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceVersion :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbSourceVersion = Lens.lens (sourceVersion :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: StartBuild)
{-# DEPRECATED sbSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- /Note:/ Consider using 'buildspecOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBuildspecOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbBuildspecOverride = Lens.lens (buildspecOverride :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {buildspecOverride = a} :: StartBuild)
{-# DEPRECATED sbBuildspecOverride "Use generic-lens or generic-optics with 'buildspecOverride' instead." #-}

-- | An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only.
--
-- /Note:/ Consider using 'secondarySourcesVersionOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondarySourcesVersionOverride :: Lens.Lens' StartBuild (Lude.Maybe [ProjectSourceVersion])
sbSecondarySourcesVersionOverride = Lens.lens (secondarySourcesVersionOverride :: StartBuild -> Lude.Maybe [ProjectSourceVersion]) (\s a -> s {secondarySourcesVersionOverride = a} :: StartBuild)
{-# DEPRECATED sbSecondarySourcesVersionOverride "Use generic-lens or generic-optics with 'secondarySourcesVersionOverride' instead." #-}

-- | Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown.
--
-- /Note:/ Consider using 'reportBuildStatusOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbReportBuildStatusOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Bool)
sbReportBuildStatusOverride = Lens.lens (reportBuildStatusOverride :: StartBuild -> Lude.Maybe Lude.Bool) (\s a -> s {reportBuildStatusOverride = a} :: StartBuild)
{-# DEPRECATED sbReportBuildStatusOverride "Use generic-lens or generic-optics with 'reportBuildStatusOverride' instead." #-}

-- | Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
--
-- /Note:/ Consider using 'insecureSSLOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbInsecureSSLOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Bool)
sbInsecureSSLOverride = Lens.lens (insecureSSLOverride :: StartBuild -> Lude.Maybe Lude.Bool) (\s a -> s {insecureSSLOverride = a} :: StartBuild)
{-# DEPRECATED sbInsecureSSLOverride "Use generic-lens or generic-optics with 'insecureSSLOverride' instead." #-}

-- | The name of an image for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'imageOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbImageOverride :: Lens.Lens' StartBuild (Lude.Maybe Lude.Text)
sbImageOverride = Lens.lens (imageOverride :: StartBuild -> Lude.Maybe Lude.Text) (\s a -> s {imageOverride = a} :: StartBuild)
{-# DEPRECATED sbImageOverride "Use generic-lens or generic-optics with 'imageOverride' instead." #-}

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondaryArtifactsOverride :: Lens.Lens' StartBuild (Lude.Maybe [ProjectArtifacts])
sbSecondaryArtifactsOverride = Lens.lens (secondaryArtifactsOverride :: StartBuild -> Lude.Maybe [ProjectArtifacts]) (\s a -> s {secondaryArtifactsOverride = a} :: StartBuild)
{-# DEPRECATED sbSecondaryArtifactsOverride "Use generic-lens or generic-optics with 'secondaryArtifactsOverride' instead." #-}

-- | Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
--
-- /Note:/ Consider using 'artifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbArtifactsOverride :: Lens.Lens' StartBuild (Lude.Maybe ProjectArtifacts)
sbArtifactsOverride = Lens.lens (artifactsOverride :: StartBuild -> Lude.Maybe ProjectArtifacts) (\s a -> s {artifactsOverride = a} :: StartBuild)
{-# DEPRECATED sbArtifactsOverride "Use generic-lens or generic-optics with 'artifactsOverride' instead." #-}

-- | A source input type, for this build, that overrides the source input defined in the build project.
--
-- /Note:/ Consider using 'sourceTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceTypeOverride :: Lens.Lens' StartBuild (Lude.Maybe SourceType)
sbSourceTypeOverride = Lens.lens (sourceTypeOverride :: StartBuild -> Lude.Maybe SourceType) (\s a -> s {sourceTypeOverride = a} :: StartBuild)
{-# DEPRECATED sbSourceTypeOverride "Use generic-lens or generic-optics with 'sourceTypeOverride' instead." #-}

-- | The name of the AWS CodeBuild build project to start running a build.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbProjectName :: Lens.Lens' StartBuild Lude.Text
sbProjectName = Lens.lens (projectName :: StartBuild -> Lude.Text) (\s a -> s {projectName = a} :: StartBuild)
{-# DEPRECATED sbProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

instance Lude.AWSRequest StartBuild where
  type Rs StartBuild = StartBuildResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartBuildResponse'
            Lude.<$> (x Lude..?> "build") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartBuild where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.StartBuild" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartBuild where
  toJSON StartBuild' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("encryptionKeyOverride" Lude..=) Lude.<$> encryptionKeyOverride,
            ("sourceLocationOverride" Lude..=) Lude.<$> sourceLocationOverride,
            ("environmentVariablesOverride" Lude..=)
              Lude.<$> environmentVariablesOverride,
            ("buildStatusConfigOverride" Lude..=)
              Lude.<$> buildStatusConfigOverride,
            ("idempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            ("debugSessionEnabled" Lude..=) Lude.<$> debugSessionEnabled,
            ("registryCredentialOverride" Lude..=)
              Lude.<$> registryCredentialOverride,
            ("timeoutInMinutesOverride" Lude..=)
              Lude.<$> timeoutInMinutesOverride,
            ("serviceRoleOverride" Lude..=) Lude.<$> serviceRoleOverride,
            ("cacheOverride" Lude..=) Lude.<$> cacheOverride,
            ("queuedTimeoutInMinutesOverride" Lude..=)
              Lude.<$> queuedTimeoutInMinutesOverride,
            ("secondarySourcesOverride" Lude..=)
              Lude.<$> secondarySourcesOverride,
            ("gitCloneDepthOverride" Lude..=) Lude.<$> gitCloneDepthOverride,
            ("imagePullCredentialsTypeOverride" Lude..=)
              Lude.<$> imagePullCredentialsTypeOverride,
            ("logsConfigOverride" Lude..=) Lude.<$> logsConfigOverride,
            ("sourceAuthOverride" Lude..=) Lude.<$> sourceAuthOverride,
            ("gitSubmodulesConfigOverride" Lude..=)
              Lude.<$> gitSubmodulesConfigOverride,
            ("environmentTypeOverride" Lude..=)
              Lude.<$> environmentTypeOverride,
            ("certificateOverride" Lude..=) Lude.<$> certificateOverride,
            ("computeTypeOverride" Lude..=) Lude.<$> computeTypeOverride,
            ("privilegedModeOverride" Lude..=) Lude.<$> privilegedModeOverride,
            ("sourceVersion" Lude..=) Lude.<$> sourceVersion,
            ("buildspecOverride" Lude..=) Lude.<$> buildspecOverride,
            ("secondarySourcesVersionOverride" Lude..=)
              Lude.<$> secondarySourcesVersionOverride,
            ("reportBuildStatusOverride" Lude..=)
              Lude.<$> reportBuildStatusOverride,
            ("insecureSslOverride" Lude..=) Lude.<$> insecureSSLOverride,
            ("imageOverride" Lude..=) Lude.<$> imageOverride,
            ("secondaryArtifactsOverride" Lude..=)
              Lude.<$> secondaryArtifactsOverride,
            ("artifactsOverride" Lude..=) Lude.<$> artifactsOverride,
            ("sourceTypeOverride" Lude..=) Lude.<$> sourceTypeOverride,
            Lude.Just ("projectName" Lude..= projectName)
          ]
      )

instance Lude.ToPath StartBuild where
  toPath = Lude.const "/"

instance Lude.ToQuery StartBuild where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartBuildResponse' smart constructor.
data StartBuildResponse = StartBuildResponse'
  { build ::
      Lude.Maybe Build,
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

-- | Creates a value of 'StartBuildResponse' with the minimum fields required to make a request.
--
-- * 'build' - Information about the build to be run.
-- * 'responseStatus' - The response status code.
mkStartBuildResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartBuildResponse
mkStartBuildResponse pResponseStatus_ =
  StartBuildResponse'
    { build = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the build to be run.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsBuild :: Lens.Lens' StartBuildResponse (Lude.Maybe Build)
srsBuild = Lens.lens (build :: StartBuildResponse -> Lude.Maybe Build) (\s a -> s {build = a} :: StartBuildResponse)
{-# DEPRECATED srsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartBuildResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartBuildResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartBuildResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
