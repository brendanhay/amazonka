{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StartBuildBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a batch build for a project.
module Network.AWS.CodeBuild.StartBuildBatch
  ( -- * Creating a request
    StartBuildBatch (..),
    mkStartBuildBatch,

    -- ** Request lenses
    sbbProjectName,
    sbbArtifactsOverride,
    sbbBuildBatchConfigOverride,
    sbbBuildTimeoutInMinutesOverride,
    sbbBuildspecOverride,
    sbbCacheOverride,
    sbbCertificateOverride,
    sbbComputeTypeOverride,
    sbbEncryptionKeyOverride,
    sbbEnvironmentTypeOverride,
    sbbEnvironmentVariablesOverride,
    sbbGitCloneDepthOverride,
    sbbGitSubmodulesConfigOverride,
    sbbIdempotencyToken,
    sbbImageOverride,
    sbbImagePullCredentialsTypeOverride,
    sbbInsecureSslOverride,
    sbbLogsConfigOverride,
    sbbPrivilegedModeOverride,
    sbbQueuedTimeoutInMinutesOverride,
    sbbRegistryCredentialOverride,
    sbbReportBuildBatchStatusOverride,
    sbbSecondaryArtifactsOverride,
    sbbSecondarySourcesOverride,
    sbbSecondarySourcesVersionOverride,
    sbbServiceRoleOverride,
    sbbSourceAuthOverride,
    sbbSourceLocationOverride,
    sbbSourceTypeOverride,
    sbbSourceVersion,

    -- * Destructuring the response
    StartBuildBatchResponse (..),
    mkStartBuildBatchResponse,

    -- ** Response lenses
    srsBuildBatch,
    srsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartBuildBatch' smart constructor.
data StartBuildBatch = StartBuildBatch'
  { -- | The name of the project.
    projectName :: Types.NonEmptyString,
    -- | An array of @ProjectArtifacts@ objects that contains information about the build output artifact overrides for the build project.
    artifactsOverride :: Core.Maybe Types.ProjectArtifacts,
    -- | A @BuildBatchConfigOverride@ object that contains batch build configuration overrides.
    buildBatchConfigOverride :: Core.Maybe Types.ProjectBuildBatchConfig,
    -- | Overrides the build timeout specified in the batch build project.
    buildTimeoutInMinutesOverride :: Core.Maybe Core.Natural,
    -- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
    --
    -- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
    buildspecOverride :: Core.Maybe Types.BuildspecOverride,
    -- | A @ProjectCache@ object that specifies cache overrides.
    cacheOverride :: Core.Maybe Types.ProjectCache,
    -- | The name of a certificate for this batch build that overrides the one specified in the batch build project.
    certificateOverride :: Core.Maybe Types.CertificateOverride,
    -- | The name of a compute type for this batch build that overrides the one specified in the batch build project.
    computeTypeOverride :: Core.Maybe Types.ComputeType,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the batch build project. The CMK key encrypts the build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKeyOverride :: Core.Maybe Types.EncryptionKeyOverride,
    -- | A container type for this batch build that overrides the one specified in the batch build project.
    environmentTypeOverride :: Core.Maybe Types.EnvironmentType,
    -- | An array of @EnvironmentVariable@ objects that override, or add to, the environment variables defined in the batch build project.
    environmentVariablesOverride :: Core.Maybe [Types.EnvironmentVariable],
    -- | The user-defined depth of history, with a minimum value of 0, that overrides, for this batch build only, any previous depth of history defined in the batch build project.
    gitCloneDepthOverride :: Core.Maybe Core.Natural,
    -- | A @GitSubmodulesConfig@ object that overrides the Git submodules configuration for this batch build.
    gitSubmodulesConfigOverride :: Core.Maybe Types.GitSubmodulesConfig,
    -- | A unique, case sensitive identifier you provide to ensure the idempotency of the @StartBuildBatch@ request. The token is included in the @StartBuildBatch@ request and is valid for five minutes. If you repeat the @StartBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Core.Maybe Types.IdempotencyToken,
    -- | The name of an image for this batch build that overrides the one specified in the batch build project.
    imageOverride :: Core.Maybe Types.ImageOverride,
    -- | The type of credentials AWS CodeBuild uses to pull images in your batch build. There are two valid values:
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
    -- | Enable this flag to override the insecure SSL setting that is specified in the batch build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
    insecureSslOverride :: Core.Maybe Core.Bool,
    -- | A @LogsConfig@ object that override the log settings defined in the batch build project.
    logsConfigOverride :: Core.Maybe Types.LogsConfig,
    -- | Enable this flag to override privileged mode in the batch build project.
    privilegedModeOverride :: Core.Maybe Core.Bool,
    -- | The number of minutes a batch build is allowed to be queued before it times out.
    queuedTimeoutInMinutesOverride :: Core.Maybe Core.Natural,
    -- | A @RegistryCredential@ object that overrides credentials for access to a private registry.
    registryCredentialOverride :: Core.Maybe Types.RegistryCredential,
    -- | Set to @true@ to report to your source provider the status of a batch build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an @invalidInputException@ is thrown.
    reportBuildBatchStatusOverride :: Core.Maybe Core.Bool,
    -- | An array of @ProjectArtifacts@ objects that override the secondary artifacts defined in the batch build project.
    secondaryArtifactsOverride :: Core.Maybe [Types.ProjectArtifacts],
    -- | An array of @ProjectSource@ objects that override the secondary sources defined in the batch build project.
    secondarySourcesOverride :: Core.Maybe [Types.ProjectSource],
    -- | An array of @ProjectSourceVersion@ objects that override the secondary source versions in the batch build project.
    secondarySourcesVersionOverride :: Core.Maybe [Types.ProjectSourceVersion],
    -- | The name of a service role for this batch build that overrides the one specified in the batch build project.
    serviceRoleOverride :: Core.Maybe Types.ServiceRoleOverride,
    -- | A @SourceAuth@ object that overrides the one defined in the batch build project. This override applies only if the build project's source is BitBucket or GitHub.
    sourceAuthOverride :: Core.Maybe Types.SourceAuth,
    -- | A location that overrides, for this batch build, the source location defined in the batch build project.
    sourceLocationOverride :: Core.Maybe Types.SourceLocationOverride,
    -- | The source input type that overrides the source input defined in the batch build project.
    sourceTypeOverride :: Core.Maybe Types.SourceType,
    -- | The version of the batch build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:
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
    sourceVersion :: Core.Maybe Types.SourceVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartBuildBatch' value with any optional fields omitted.
mkStartBuildBatch ::
  -- | 'projectName'
  Types.NonEmptyString ->
  StartBuildBatch
mkStartBuildBatch projectName =
  StartBuildBatch'
    { projectName,
      artifactsOverride = Core.Nothing,
      buildBatchConfigOverride = Core.Nothing,
      buildTimeoutInMinutesOverride = Core.Nothing,
      buildspecOverride = Core.Nothing,
      cacheOverride = Core.Nothing,
      certificateOverride = Core.Nothing,
      computeTypeOverride = Core.Nothing,
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
      reportBuildBatchStatusOverride = Core.Nothing,
      secondaryArtifactsOverride = Core.Nothing,
      secondarySourcesOverride = Core.Nothing,
      secondarySourcesVersionOverride = Core.Nothing,
      serviceRoleOverride = Core.Nothing,
      sourceAuthOverride = Core.Nothing,
      sourceLocationOverride = Core.Nothing,
      sourceTypeOverride = Core.Nothing,
      sourceVersion = Core.Nothing
    }

-- | The name of the project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbProjectName :: Lens.Lens' StartBuildBatch Types.NonEmptyString
sbbProjectName = Lens.field @"projectName"
{-# DEPRECATED sbbProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | An array of @ProjectArtifacts@ objects that contains information about the build output artifact overrides for the build project.
--
-- /Note:/ Consider using 'artifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbArtifactsOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ProjectArtifacts)
sbbArtifactsOverride = Lens.field @"artifactsOverride"
{-# DEPRECATED sbbArtifactsOverride "Use generic-lens or generic-optics with 'artifactsOverride' instead." #-}

-- | A @BuildBatchConfigOverride@ object that contains batch build configuration overrides.
--
-- /Note:/ Consider using 'buildBatchConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildBatchConfigOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ProjectBuildBatchConfig)
sbbBuildBatchConfigOverride = Lens.field @"buildBatchConfigOverride"
{-# DEPRECATED sbbBuildBatchConfigOverride "Use generic-lens or generic-optics with 'buildBatchConfigOverride' instead." #-}

-- | Overrides the build timeout specified in the batch build project.
--
-- /Note:/ Consider using 'buildTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Natural)
sbbBuildTimeoutInMinutesOverride = Lens.field @"buildTimeoutInMinutesOverride"
{-# DEPRECATED sbbBuildTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'buildTimeoutInMinutesOverride' instead." #-}

-- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- /Note:/ Consider using 'buildspecOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildspecOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.BuildspecOverride)
sbbBuildspecOverride = Lens.field @"buildspecOverride"
{-# DEPRECATED sbbBuildspecOverride "Use generic-lens or generic-optics with 'buildspecOverride' instead." #-}

-- | A @ProjectCache@ object that specifies cache overrides.
--
-- /Note:/ Consider using 'cacheOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbCacheOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ProjectCache)
sbbCacheOverride = Lens.field @"cacheOverride"
{-# DEPRECATED sbbCacheOverride "Use generic-lens or generic-optics with 'cacheOverride' instead." #-}

-- | The name of a certificate for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'certificateOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbCertificateOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.CertificateOverride)
sbbCertificateOverride = Lens.field @"certificateOverride"
{-# DEPRECATED sbbCertificateOverride "Use generic-lens or generic-optics with 'certificateOverride' instead." #-}

-- | The name of a compute type for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'computeTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbComputeTypeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ComputeType)
sbbComputeTypeOverride = Lens.field @"computeTypeOverride"
{-# DEPRECATED sbbComputeTypeOverride "Use generic-lens or generic-optics with 'computeTypeOverride' instead." #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the batch build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKeyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEncryptionKeyOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.EncryptionKeyOverride)
sbbEncryptionKeyOverride = Lens.field @"encryptionKeyOverride"
{-# DEPRECATED sbbEncryptionKeyOverride "Use generic-lens or generic-optics with 'encryptionKeyOverride' instead." #-}

-- | A container type for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'environmentTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEnvironmentTypeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.EnvironmentType)
sbbEnvironmentTypeOverride = Lens.field @"environmentTypeOverride"
{-# DEPRECATED sbbEnvironmentTypeOverride "Use generic-lens or generic-optics with 'environmentTypeOverride' instead." #-}

-- | An array of @EnvironmentVariable@ objects that override, or add to, the environment variables defined in the batch build project.
--
-- /Note:/ Consider using 'environmentVariablesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEnvironmentVariablesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.EnvironmentVariable])
sbbEnvironmentVariablesOverride = Lens.field @"environmentVariablesOverride"
{-# DEPRECATED sbbEnvironmentVariablesOverride "Use generic-lens or generic-optics with 'environmentVariablesOverride' instead." #-}

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this batch build only, any previous depth of history defined in the batch build project.
--
-- /Note:/ Consider using 'gitCloneDepthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbGitCloneDepthOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Natural)
sbbGitCloneDepthOverride = Lens.field @"gitCloneDepthOverride"
{-# DEPRECATED sbbGitCloneDepthOverride "Use generic-lens or generic-optics with 'gitCloneDepthOverride' instead." #-}

-- | A @GitSubmodulesConfig@ object that overrides the Git submodules configuration for this batch build.
--
-- /Note:/ Consider using 'gitSubmodulesConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbGitSubmodulesConfigOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.GitSubmodulesConfig)
sbbGitSubmodulesConfigOverride = Lens.field @"gitSubmodulesConfigOverride"
{-# DEPRECATED sbbGitSubmodulesConfigOverride "Use generic-lens or generic-optics with 'gitSubmodulesConfigOverride' instead." #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @StartBuildBatch@ request. The token is included in the @StartBuildBatch@ request and is valid for five minutes. If you repeat the @StartBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbIdempotencyToken :: Lens.Lens' StartBuildBatch (Core.Maybe Types.IdempotencyToken)
sbbIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED sbbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The name of an image for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'imageOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbImageOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ImageOverride)
sbbImageOverride = Lens.field @"imageOverride"
{-# DEPRECATED sbbImageOverride "Use generic-lens or generic-optics with 'imageOverride' instead." #-}

-- | The type of credentials AWS CodeBuild uses to pull images in your batch build. There are two valid values:
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
sbbImagePullCredentialsTypeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ImagePullCredentialsType)
sbbImagePullCredentialsTypeOverride = Lens.field @"imagePullCredentialsTypeOverride"
{-# DEPRECATED sbbImagePullCredentialsTypeOverride "Use generic-lens or generic-optics with 'imagePullCredentialsTypeOverride' instead." #-}

-- | Enable this flag to override the insecure SSL setting that is specified in the batch build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
--
-- /Note:/ Consider using 'insecureSslOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbInsecureSslOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Bool)
sbbInsecureSslOverride = Lens.field @"insecureSslOverride"
{-# DEPRECATED sbbInsecureSslOverride "Use generic-lens or generic-optics with 'insecureSslOverride' instead." #-}

-- | A @LogsConfig@ object that override the log settings defined in the batch build project.
--
-- /Note:/ Consider using 'logsConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbLogsConfigOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.LogsConfig)
sbbLogsConfigOverride = Lens.field @"logsConfigOverride"
{-# DEPRECATED sbbLogsConfigOverride "Use generic-lens or generic-optics with 'logsConfigOverride' instead." #-}

-- | Enable this flag to override privileged mode in the batch build project.
--
-- /Note:/ Consider using 'privilegedModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbPrivilegedModeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Bool)
sbbPrivilegedModeOverride = Lens.field @"privilegedModeOverride"
{-# DEPRECATED sbbPrivilegedModeOverride "Use generic-lens or generic-optics with 'privilegedModeOverride' instead." #-}

-- | The number of minutes a batch build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbQueuedTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Natural)
sbbQueuedTimeoutInMinutesOverride = Lens.field @"queuedTimeoutInMinutesOverride"
{-# DEPRECATED sbbQueuedTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'queuedTimeoutInMinutesOverride' instead." #-}

-- | A @RegistryCredential@ object that overrides credentials for access to a private registry.
--
-- /Note:/ Consider using 'registryCredentialOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbRegistryCredentialOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.RegistryCredential)
sbbRegistryCredentialOverride = Lens.field @"registryCredentialOverride"
{-# DEPRECATED sbbRegistryCredentialOverride "Use generic-lens or generic-optics with 'registryCredentialOverride' instead." #-}

-- | Set to @true@ to report to your source provider the status of a batch build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an @invalidInputException@ is thrown.
--
-- /Note:/ Consider using 'reportBuildBatchStatusOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbReportBuildBatchStatusOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Bool)
sbbReportBuildBatchStatusOverride = Lens.field @"reportBuildBatchStatusOverride"
{-# DEPRECATED sbbReportBuildBatchStatusOverride "Use generic-lens or generic-optics with 'reportBuildBatchStatusOverride' instead." #-}

-- | An array of @ProjectArtifacts@ objects that override the secondary artifacts defined in the batch build project.
--
-- /Note:/ Consider using 'secondaryArtifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondaryArtifactsOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.ProjectArtifacts])
sbbSecondaryArtifactsOverride = Lens.field @"secondaryArtifactsOverride"
{-# DEPRECATED sbbSecondaryArtifactsOverride "Use generic-lens or generic-optics with 'secondaryArtifactsOverride' instead." #-}

-- | An array of @ProjectSource@ objects that override the secondary sources defined in the batch build project.
--
-- /Note:/ Consider using 'secondarySourcesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondarySourcesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.ProjectSource])
sbbSecondarySourcesOverride = Lens.field @"secondarySourcesOverride"
{-# DEPRECATED sbbSecondarySourcesOverride "Use generic-lens or generic-optics with 'secondarySourcesOverride' instead." #-}

-- | An array of @ProjectSourceVersion@ objects that override the secondary source versions in the batch build project.
--
-- /Note:/ Consider using 'secondarySourcesVersionOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondarySourcesVersionOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.ProjectSourceVersion])
sbbSecondarySourcesVersionOverride = Lens.field @"secondarySourcesVersionOverride"
{-# DEPRECATED sbbSecondarySourcesVersionOverride "Use generic-lens or generic-optics with 'secondarySourcesVersionOverride' instead." #-}

-- | The name of a service role for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'serviceRoleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbServiceRoleOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ServiceRoleOverride)
sbbServiceRoleOverride = Lens.field @"serviceRoleOverride"
{-# DEPRECATED sbbServiceRoleOverride "Use generic-lens or generic-optics with 'serviceRoleOverride' instead." #-}

-- | A @SourceAuth@ object that overrides the one defined in the batch build project. This override applies only if the build project's source is BitBucket or GitHub.
--
-- /Note:/ Consider using 'sourceAuthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceAuthOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.SourceAuth)
sbbSourceAuthOverride = Lens.field @"sourceAuthOverride"
{-# DEPRECATED sbbSourceAuthOverride "Use generic-lens or generic-optics with 'sourceAuthOverride' instead." #-}

-- | A location that overrides, for this batch build, the source location defined in the batch build project.
--
-- /Note:/ Consider using 'sourceLocationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceLocationOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.SourceLocationOverride)
sbbSourceLocationOverride = Lens.field @"sourceLocationOverride"
{-# DEPRECATED sbbSourceLocationOverride "Use generic-lens or generic-optics with 'sourceLocationOverride' instead." #-}

-- | The source input type that overrides the source input defined in the batch build project.
--
-- /Note:/ Consider using 'sourceTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceTypeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.SourceType)
sbbSourceTypeOverride = Lens.field @"sourceTypeOverride"
{-# DEPRECATED sbbSourceTypeOverride "Use generic-lens or generic-optics with 'sourceTypeOverride' instead." #-}

-- | The version of the batch build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:
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
sbbSourceVersion :: Lens.Lens' StartBuildBatch (Core.Maybe Types.SourceVersion)
sbbSourceVersion = Lens.field @"sourceVersion"
{-# DEPRECATED sbbSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

instance Core.FromJSON StartBuildBatch where
  toJSON StartBuildBatch {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("projectName" Core..= projectName),
            ("artifactsOverride" Core..=) Core.<$> artifactsOverride,
            ("buildBatchConfigOverride" Core..=)
              Core.<$> buildBatchConfigOverride,
            ("buildTimeoutInMinutesOverride" Core..=)
              Core.<$> buildTimeoutInMinutesOverride,
            ("buildspecOverride" Core..=) Core.<$> buildspecOverride,
            ("cacheOverride" Core..=) Core.<$> cacheOverride,
            ("certificateOverride" Core..=) Core.<$> certificateOverride,
            ("computeTypeOverride" Core..=) Core.<$> computeTypeOverride,
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
            ("reportBuildBatchStatusOverride" Core..=)
              Core.<$> reportBuildBatchStatusOverride,
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
            ("sourceVersion" Core..=) Core.<$> sourceVersion
          ]
      )

instance Core.AWSRequest StartBuildBatch where
  type Rs StartBuildBatch = StartBuildBatchResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.StartBuildBatch")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBuildBatchResponse'
            Core.<$> (x Core..:? "buildBatch") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartBuildBatchResponse' smart constructor.
data StartBuildBatchResponse = StartBuildBatchResponse'
  { -- | A @BuildBatch@ object that contains information about the batch build.
    buildBatch :: Core.Maybe Types.BuildBatch,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartBuildBatchResponse' value with any optional fields omitted.
mkStartBuildBatchResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartBuildBatchResponse
mkStartBuildBatchResponse responseStatus =
  StartBuildBatchResponse'
    { buildBatch = Core.Nothing,
      responseStatus
    }

-- | A @BuildBatch@ object that contains information about the batch build.
--
-- /Note:/ Consider using 'buildBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsBuildBatch :: Lens.Lens' StartBuildBatchResponse (Core.Maybe Types.BuildBatch)
srsBuildBatch = Lens.field @"buildBatch"
{-# DEPRECATED srsBuildBatch "Use generic-lens or generic-optics with 'buildBatch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartBuildBatchResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
