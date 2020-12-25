{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    sbProjectName,
    sbArtifactsOverride,
    sbBuildStatusConfigOverride,
    sbBuildspecOverride,
    sbCacheOverride,
    sbCertificateOverride,
    sbComputeTypeOverride,
    sbDebugSessionEnabled,
    sbEncryptionKeyOverride,
    sbEnvironmentTypeOverride,
    sbEnvironmentVariablesOverride,
    sbGitCloneDepthOverride,
    sbGitSubmodulesConfigOverride,
    sbIdempotencyToken,
    sbImageOverride,
    sbImagePullCredentialsTypeOverride,
    sbInsecureSslOverride,
    sbLogsConfigOverride,
    sbPrivilegedModeOverride,
    sbQueuedTimeoutInMinutesOverride,
    sbRegistryCredentialOverride,
    sbReportBuildStatusOverride,
    sbSecondaryArtifactsOverride,
    sbSecondarySourcesOverride,
    sbSecondarySourcesVersionOverride,
    sbServiceRoleOverride,
    sbSourceAuthOverride,
    sbSourceLocationOverride,
    sbSourceTypeOverride,
    sbSourceVersion,
    sbTimeoutInMinutesOverride,

    -- * Destructuring the response
    StartBuildResponse (..),
    mkStartBuildResponse,

    -- ** Response lenses
    sbrrsBuild,
    sbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartBuild' smart constructor.
data StartBuild = StartBuild'
  { -- | The name of the AWS CodeBuild build project to start running a build.
    projectName :: Types.NonEmptyString,
    -- | Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
    artifactsOverride :: Core.Maybe Types.ProjectArtifacts,
    -- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
    buildStatusConfigOverride :: Core.Maybe Types.BuildStatusConfig,
    -- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
    --
    -- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
    buildspecOverride :: Core.Maybe Types.String,
    -- | A ProjectCache object specified for this build that overrides the one defined in the build project.
    cacheOverride :: Core.Maybe Types.ProjectCache,
    -- | The name of a certificate for this build that overrides the one specified in the build project.
    certificateOverride :: Core.Maybe Types.String,
    -- | The name of a compute type for this build that overrides the one specified in the build project.
    computeTypeOverride :: Core.Maybe Types.ComputeType,
    -- | Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
    debugSessionEnabled :: Core.Maybe Core.Bool,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKeyOverride :: Core.Maybe Types.EncryptionKeyOverride,
    -- | A container type for this build that overrides the one specified in the build project.
    environmentTypeOverride :: Core.Maybe Types.EnvironmentType,
    -- | A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
    environmentVariablesOverride :: Core.Maybe [Types.EnvironmentVariable],
    -- | The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
    gitCloneDepthOverride :: Core.Maybe Core.Natural,
    -- | Information about the Git submodules configuration for this build of an AWS CodeBuild build project.
    gitSubmodulesConfigOverride :: Core.Maybe Types.GitSubmodulesConfig,
    -- | A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Core.Maybe Types.String,
    -- | The name of an image for this build that overrides the one specified in the build project.
    imageOverride :: Core.Maybe Types.ImageOverride,
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
    imagePullCredentialsTypeOverride :: Core.Maybe Types.ImagePullCredentialsType,
    -- | Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
    insecureSslOverride :: Core.Maybe Core.Bool,
    -- | Log settings for this build that override the log settings defined in the build project.
    logsConfigOverride :: Core.Maybe Types.LogsConfig,
    -- | Enable this flag to override privileged mode in the build project.
    privilegedModeOverride :: Core.Maybe Core.Bool,
    -- | The number of minutes a build is allowed to be queued before it times out.
    queuedTimeoutInMinutesOverride :: Core.Maybe Core.Natural,
    -- | The credentials for access to a private registry.
    registryCredentialOverride :: Core.Maybe Types.RegistryCredential,
    -- | Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown.
    reportBuildStatusOverride :: Core.Maybe Core.Bool,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifactsOverride :: Core.Maybe [Types.ProjectArtifacts],
    -- | An array of @ProjectSource@ objects.
    secondarySourcesOverride :: Core.Maybe [Types.ProjectSource],
    -- | An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only.
    secondarySourcesVersionOverride :: Core.Maybe [Types.ProjectSourceVersion],
    -- | The name of a service role for this build that overrides the one specified in the build project.
    serviceRoleOverride :: Core.Maybe Types.ServiceRoleOverride,
    -- | An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
    sourceAuthOverride :: Core.Maybe Types.SourceAuth,
    -- | A location that overrides, for this build, the source location for the one defined in the build project.
    sourceLocationOverride :: Core.Maybe Types.String,
    -- | A source input type, for this build, that overrides the source input defined in the build project.
    sourceTypeOverride :: Core.Maybe Types.SourceType,
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
    sourceVersion :: Core.Maybe Types.String,
    -- | The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
    timeoutInMinutesOverride :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartBuild' value with any optional fields omitted.
mkStartBuild ::
  -- | 'projectName'
  Types.NonEmptyString ->
  StartBuild
mkStartBuild projectName =
  StartBuild'
    { projectName,
      artifactsOverride = Core.Nothing,
      buildStatusConfigOverride = Core.Nothing,
      buildspecOverride = Core.Nothing,
      cacheOverride = Core.Nothing,
      certificateOverride = Core.Nothing,
      computeTypeOverride = Core.Nothing,
      debugSessionEnabled = Core.Nothing,
      encryptionKeyOverride = Core.Nothing,
      environmentTypeOverride = Core.Nothing,
      environmentVariablesOverride = Core.Nothing,
      gitCloneDepthOverride = Core.Nothing,
      gitSubmodulesConfigOverride = Core.Nothing,
      idempotencyToken = Core.Nothing,
      imageOverride = Core.Nothing,
      imagePullCredentialsTypeOverride = Core.Nothing,
      insecureSslOverride = Core.Nothing,
      logsConfigOverride = Core.Nothing,
      privilegedModeOverride = Core.Nothing,
      queuedTimeoutInMinutesOverride = Core.Nothing,
      registryCredentialOverride = Core.Nothing,
      reportBuildStatusOverride = Core.Nothing,
      secondaryArtifactsOverride = Core.Nothing,
      secondarySourcesOverride = Core.Nothing,
      secondarySourcesVersionOverride = Core.Nothing,
      serviceRoleOverride = Core.Nothing,
      sourceAuthOverride = Core.Nothing,
      sourceLocationOverride = Core.Nothing,
      sourceTypeOverride = Core.Nothing,
      sourceVersion = Core.Nothing,
      timeoutInMinutesOverride = Core.Nothing
    }

-- | The name of the AWS CodeBuild build project to start running a build.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbProjectName :: Lens.Lens' StartBuild Types.NonEmptyString
sbProjectName = Lens.field @"projectName"
{-# DEPRECATED sbProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
--
-- /Note:/ Consider using 'artifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbArtifactsOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ProjectArtifacts)
sbArtifactsOverride = Lens.field @"artifactsOverride"
{-# DEPRECATED sbArtifactsOverride "Use generic-lens or generic-optics with 'artifactsOverride' instead." #-}

-- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
--
-- /Note:/ Consider using 'buildStatusConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBuildStatusConfigOverride :: Lens.Lens' StartBuild (Core.Maybe Types.BuildStatusConfig)
sbBuildStatusConfigOverride = Lens.field @"buildStatusConfigOverride"
{-# DEPRECATED sbBuildStatusConfigOverride "Use generic-lens or generic-optics with 'buildStatusConfigOverride' instead." #-}

-- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- /Note:/ Consider using 'buildspecOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBuildspecOverride :: Lens.Lens' StartBuild (Core.Maybe Types.String)
sbBuildspecOverride = Lens.field @"buildspecOverride"
{-# DEPRECATED sbBuildspecOverride "Use generic-lens or generic-optics with 'buildspecOverride' instead." #-}

-- | A ProjectCache object specified for this build that overrides the one defined in the build project.
--
-- /Note:/ Consider using 'cacheOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbCacheOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ProjectCache)
sbCacheOverride = Lens.field @"cacheOverride"
{-# DEPRECATED sbCacheOverride "Use generic-lens or generic-optics with 'cacheOverride' instead." #-}

-- | The name of a certificate for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'certificateOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbCertificateOverride :: Lens.Lens' StartBuild (Core.Maybe Types.String)
sbCertificateOverride = Lens.field @"certificateOverride"
{-# DEPRECATED sbCertificateOverride "Use generic-lens or generic-optics with 'certificateOverride' instead." #-}

-- | The name of a compute type for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'computeTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbComputeTypeOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ComputeType)
sbComputeTypeOverride = Lens.field @"computeTypeOverride"
{-# DEPRECATED sbComputeTypeOverride "Use generic-lens or generic-optics with 'computeTypeOverride' instead." #-}

-- | Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
--
-- /Note:/ Consider using 'debugSessionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbDebugSessionEnabled :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbDebugSessionEnabled = Lens.field @"debugSessionEnabled"
{-# DEPRECATED sbDebugSessionEnabled "Use generic-lens or generic-optics with 'debugSessionEnabled' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKeyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEncryptionKeyOverride :: Lens.Lens' StartBuild (Core.Maybe Types.EncryptionKeyOverride)
sbEncryptionKeyOverride = Lens.field @"encryptionKeyOverride"
{-# DEPRECATED sbEncryptionKeyOverride "Use generic-lens or generic-optics with 'encryptionKeyOverride' instead." #-}

-- | A container type for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'environmentTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEnvironmentTypeOverride :: Lens.Lens' StartBuild (Core.Maybe Types.EnvironmentType)
sbEnvironmentTypeOverride = Lens.field @"environmentTypeOverride"
{-# DEPRECATED sbEnvironmentTypeOverride "Use generic-lens or generic-optics with 'environmentTypeOverride' instead." #-}

-- | A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
--
-- /Note:/ Consider using 'environmentVariablesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEnvironmentVariablesOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.EnvironmentVariable])
sbEnvironmentVariablesOverride = Lens.field @"environmentVariablesOverride"
{-# DEPRECATED sbEnvironmentVariablesOverride "Use generic-lens or generic-optics with 'environmentVariablesOverride' instead." #-}

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
--
-- /Note:/ Consider using 'gitCloneDepthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbGitCloneDepthOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
sbGitCloneDepthOverride = Lens.field @"gitCloneDepthOverride"
{-# DEPRECATED sbGitCloneDepthOverride "Use generic-lens or generic-optics with 'gitCloneDepthOverride' instead." #-}

-- | Information about the Git submodules configuration for this build of an AWS CodeBuild build project.
--
-- /Note:/ Consider using 'gitSubmodulesConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbGitSubmodulesConfigOverride :: Lens.Lens' StartBuild (Core.Maybe Types.GitSubmodulesConfig)
sbGitSubmodulesConfigOverride = Lens.field @"gitSubmodulesConfigOverride"
{-# DEPRECATED sbGitSubmodulesConfigOverride "Use generic-lens or generic-optics with 'gitSubmodulesConfigOverride' instead." #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbIdempotencyToken :: Lens.Lens' StartBuild (Core.Maybe Types.String)
sbIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED sbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The name of an image for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'imageOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbImageOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ImageOverride)
sbImageOverride = Lens.field @"imageOverride"
{-# DEPRECATED sbImageOverride "Use generic-lens or generic-optics with 'imageOverride' instead." #-}

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
sbImagePullCredentialsTypeOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ImagePullCredentialsType)
sbImagePullCredentialsTypeOverride = Lens.field @"imagePullCredentialsTypeOverride"
{-# DEPRECATED sbImagePullCredentialsTypeOverride "Use generic-lens or generic-optics with 'imagePullCredentialsTypeOverride' instead." #-}

-- | Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
--
-- /Note:/ Consider using 'insecureSslOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbInsecureSslOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbInsecureSslOverride = Lens.field @"insecureSslOverride"
{-# DEPRECATED sbInsecureSslOverride "Use generic-lens or generic-optics with 'insecureSslOverride' instead." #-}

-- | Log settings for this build that override the log settings defined in the build project.
--
-- /Note:/ Consider using 'logsConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbLogsConfigOverride :: Lens.Lens' StartBuild (Core.Maybe Types.LogsConfig)
sbLogsConfigOverride = Lens.field @"logsConfigOverride"
{-# DEPRECATED sbLogsConfigOverride "Use generic-lens or generic-optics with 'logsConfigOverride' instead." #-}

-- | Enable this flag to override privileged mode in the build project.
--
-- /Note:/ Consider using 'privilegedModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbPrivilegedModeOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbPrivilegedModeOverride = Lens.field @"privilegedModeOverride"
{-# DEPRECATED sbPrivilegedModeOverride "Use generic-lens or generic-optics with 'privilegedModeOverride' instead." #-}

-- | The number of minutes a build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbQueuedTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
sbQueuedTimeoutInMinutesOverride = Lens.field @"queuedTimeoutInMinutesOverride"
{-# DEPRECATED sbQueuedTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'queuedTimeoutInMinutesOverride' instead." #-}

-- | The credentials for access to a private registry.
--
-- /Note:/ Consider using 'registryCredentialOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbRegistryCredentialOverride :: Lens.Lens' StartBuild (Core.Maybe Types.RegistryCredential)
sbRegistryCredentialOverride = Lens.field @"registryCredentialOverride"
{-# DEPRECATED sbRegistryCredentialOverride "Use generic-lens or generic-optics with 'registryCredentialOverride' instead." #-}

-- | Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown.
--
-- /Note:/ Consider using 'reportBuildStatusOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbReportBuildStatusOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbReportBuildStatusOverride = Lens.field @"reportBuildStatusOverride"
{-# DEPRECATED sbReportBuildStatusOverride "Use generic-lens or generic-optics with 'reportBuildStatusOverride' instead." #-}

-- | An array of @ProjectArtifacts@ objects.
--
-- /Note:/ Consider using 'secondaryArtifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondaryArtifactsOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.ProjectArtifacts])
sbSecondaryArtifactsOverride = Lens.field @"secondaryArtifactsOverride"
{-# DEPRECATED sbSecondaryArtifactsOverride "Use generic-lens or generic-optics with 'secondaryArtifactsOverride' instead." #-}

-- | An array of @ProjectSource@ objects.
--
-- /Note:/ Consider using 'secondarySourcesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondarySourcesOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.ProjectSource])
sbSecondarySourcesOverride = Lens.field @"secondarySourcesOverride"
{-# DEPRECATED sbSecondarySourcesOverride "Use generic-lens or generic-optics with 'secondarySourcesOverride' instead." #-}

-- | An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only.
--
-- /Note:/ Consider using 'secondarySourcesVersionOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondarySourcesVersionOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.ProjectSourceVersion])
sbSecondarySourcesVersionOverride = Lens.field @"secondarySourcesVersionOverride"
{-# DEPRECATED sbSecondarySourcesVersionOverride "Use generic-lens or generic-optics with 'secondarySourcesVersionOverride' instead." #-}

-- | The name of a service role for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'serviceRoleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbServiceRoleOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ServiceRoleOverride)
sbServiceRoleOverride = Lens.field @"serviceRoleOverride"
{-# DEPRECATED sbServiceRoleOverride "Use generic-lens or generic-optics with 'serviceRoleOverride' instead." #-}

-- | An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
--
-- /Note:/ Consider using 'sourceAuthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceAuthOverride :: Lens.Lens' StartBuild (Core.Maybe Types.SourceAuth)
sbSourceAuthOverride = Lens.field @"sourceAuthOverride"
{-# DEPRECATED sbSourceAuthOverride "Use generic-lens or generic-optics with 'sourceAuthOverride' instead." #-}

-- | A location that overrides, for this build, the source location for the one defined in the build project.
--
-- /Note:/ Consider using 'sourceLocationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceLocationOverride :: Lens.Lens' StartBuild (Core.Maybe Types.String)
sbSourceLocationOverride = Lens.field @"sourceLocationOverride"
{-# DEPRECATED sbSourceLocationOverride "Use generic-lens or generic-optics with 'sourceLocationOverride' instead." #-}

-- | A source input type, for this build, that overrides the source input defined in the build project.
--
-- /Note:/ Consider using 'sourceTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceTypeOverride :: Lens.Lens' StartBuild (Core.Maybe Types.SourceType)
sbSourceTypeOverride = Lens.field @"sourceTypeOverride"
{-# DEPRECATED sbSourceTypeOverride "Use generic-lens or generic-optics with 'sourceTypeOverride' instead." #-}

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
sbSourceVersion :: Lens.Lens' StartBuild (Core.Maybe Types.String)
sbSourceVersion = Lens.field @"sourceVersion"
{-# DEPRECATED sbSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
--
-- /Note:/ Consider using 'timeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
sbTimeoutInMinutesOverride = Lens.field @"timeoutInMinutesOverride"
{-# DEPRECATED sbTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'timeoutInMinutesOverride' instead." #-}

instance Core.FromJSON StartBuild where
  toJSON StartBuild {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectName" Core..= projectName),
            ("artifactsOverride" Core..=) Core.<$> artifactsOverride,
            ("buildStatusConfigOverride" Core..=)
              Core.<$> buildStatusConfigOverride,
            ("buildspecOverride" Core..=) Core.<$> buildspecOverride,
            ("cacheOverride" Core..=) Core.<$> cacheOverride,
            ("certificateOverride" Core..=) Core.<$> certificateOverride,
            ("computeTypeOverride" Core..=) Core.<$> computeTypeOverride,
            ("debugSessionEnabled" Core..=) Core.<$> debugSessionEnabled,
            ("encryptionKeyOverride" Core..=) Core.<$> encryptionKeyOverride,
            ("environmentTypeOverride" Core..=)
              Core.<$> environmentTypeOverride,
            ("environmentVariablesOverride" Core..=)
              Core.<$> environmentVariablesOverride,
            ("gitCloneDepthOverride" Core..=) Core.<$> gitCloneDepthOverride,
            ("gitSubmodulesConfigOverride" Core..=)
              Core.<$> gitSubmodulesConfigOverride,
            ("idempotencyToken" Core..=) Core.<$> idempotencyToken,
            ("imageOverride" Core..=) Core.<$> imageOverride,
            ("imagePullCredentialsTypeOverride" Core..=)
              Core.<$> imagePullCredentialsTypeOverride,
            ("insecureSslOverride" Core..=) Core.<$> insecureSslOverride,
            ("logsConfigOverride" Core..=) Core.<$> logsConfigOverride,
            ("privilegedModeOverride" Core..=) Core.<$> privilegedModeOverride,
            ("queuedTimeoutInMinutesOverride" Core..=)
              Core.<$> queuedTimeoutInMinutesOverride,
            ("registryCredentialOverride" Core..=)
              Core.<$> registryCredentialOverride,
            ("reportBuildStatusOverride" Core..=)
              Core.<$> reportBuildStatusOverride,
            ("secondaryArtifactsOverride" Core..=)
              Core.<$> secondaryArtifactsOverride,
            ("secondarySourcesOverride" Core..=)
              Core.<$> secondarySourcesOverride,
            ("secondarySourcesVersionOverride" Core..=)
              Core.<$> secondarySourcesVersionOverride,
            ("serviceRoleOverride" Core..=) Core.<$> serviceRoleOverride,
            ("sourceAuthOverride" Core..=) Core.<$> sourceAuthOverride,
            ("sourceLocationOverride" Core..=) Core.<$> sourceLocationOverride,
            ("sourceTypeOverride" Core..=) Core.<$> sourceTypeOverride,
            ("sourceVersion" Core..=) Core.<$> sourceVersion,
            ("timeoutInMinutesOverride" Core..=)
              Core.<$> timeoutInMinutesOverride
          ]
      )

instance Core.AWSRequest StartBuild where
  type Rs StartBuild = StartBuildResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.StartBuild")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBuildResponse'
            Core.<$> (x Core..:? "build") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartBuildResponse' smart constructor.
data StartBuildResponse = StartBuildResponse'
  { -- | Information about the build to be run.
    build :: Core.Maybe Types.Build,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartBuildResponse' value with any optional fields omitted.
mkStartBuildResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartBuildResponse
mkStartBuildResponse responseStatus =
  StartBuildResponse' {build = Core.Nothing, responseStatus}

-- | Information about the build to be run.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrrsBuild :: Lens.Lens' StartBuildResponse (Core.Maybe Types.Build)
sbrrsBuild = Lens.field @"build"
{-# DEPRECATED sbrrsBuild "Use generic-lens or generic-optics with 'build' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrrsResponseStatus :: Lens.Lens' StartBuildResponse Core.Int
sbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
