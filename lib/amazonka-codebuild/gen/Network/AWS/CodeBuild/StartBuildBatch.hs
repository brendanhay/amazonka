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
    sbbEncryptionKeyOverride,
    sbbSourceLocationOverride,
    sbbBuildBatchConfigOverride,
    sbbEnvironmentVariablesOverride,
    sbbIdempotencyToken,
    sbbRegistryCredentialOverride,
    sbbServiceRoleOverride,
    sbbCacheOverride,
    sbbQueuedTimeoutInMinutesOverride,
    sbbSecondarySourcesOverride,
    sbbGitCloneDepthOverride,
    sbbImagePullCredentialsTypeOverride,
    sbbLogsConfigOverride,
    sbbSourceAuthOverride,
    sbbGitSubmodulesConfigOverride,
    sbbEnvironmentTypeOverride,
    sbbCertificateOverride,
    sbbComputeTypeOverride,
    sbbReportBuildBatchStatusOverride,
    sbbPrivilegedModeOverride,
    sbbSourceVersion,
    sbbBuildspecOverride,
    sbbProjectName,
    sbbSecondarySourcesVersionOverride,
    sbbInsecureSSLOverride,
    sbbImageOverride,
    sbbSecondaryArtifactsOverride,
    sbbBuildTimeoutInMinutesOverride,
    sbbArtifactsOverride,
    sbbSourceTypeOverride,

    -- * Destructuring the response
    StartBuildBatchResponse (..),
    mkStartBuildBatchResponse,

    -- ** Response lenses
    sbbrsBuildBatch,
    sbbrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartBuildBatch' smart constructor.
data StartBuildBatch = StartBuildBatch'
  { -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the batch build project. The CMK key encrypts the build output artifacts.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
    encryptionKeyOverride :: Lude.Maybe Lude.Text,
    -- | A location that overrides, for this batch build, the source location defined in the batch build project.
    sourceLocationOverride :: Lude.Maybe Lude.Text,
    -- | A @BuildBatchConfigOverride@ object that contains batch build configuration overrides.
    buildBatchConfigOverride :: Lude.Maybe ProjectBuildBatchConfig,
    -- | An array of @EnvironmentVariable@ objects that override, or add to, the environment variables defined in the batch build project.
    environmentVariablesOverride :: Lude.Maybe [EnvironmentVariable],
    -- | A unique, case sensitive identifier you provide to ensure the idempotency of the @StartBuildBatch@ request. The token is included in the @StartBuildBatch@ request and is valid for five minutes. If you repeat the @StartBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Lude.Maybe Lude.Text,
    -- | A @RegistryCredential@ object that overrides credentials for access to a private registry.
    registryCredentialOverride :: Lude.Maybe RegistryCredential,
    -- | The name of a service role for this batch build that overrides the one specified in the batch build project.
    serviceRoleOverride :: Lude.Maybe Lude.Text,
    -- | A @ProjectCache@ object that specifies cache overrides.
    cacheOverride :: Lude.Maybe ProjectCache,
    -- | The number of minutes a batch build is allowed to be queued before it times out.
    queuedTimeoutInMinutesOverride :: Lude.Maybe Lude.Natural,
    -- | An array of @ProjectSource@ objects that override the secondary sources defined in the batch build project.
    secondarySourcesOverride :: Lude.Maybe [ProjectSource],
    -- | The user-defined depth of history, with a minimum value of 0, that overrides, for this batch build only, any previous depth of history defined in the batch build project.
    gitCloneDepthOverride :: Lude.Maybe Lude.Natural,
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
    imagePullCredentialsTypeOverride :: Lude.Maybe ImagePullCredentialsType,
    -- | A @LogsConfig@ object that override the log settings defined in the batch build project.
    logsConfigOverride :: Lude.Maybe LogsConfig,
    -- | A @SourceAuth@ object that overrides the one defined in the batch build project. This override applies only if the build project's source is BitBucket or GitHub.
    sourceAuthOverride :: Lude.Maybe SourceAuth,
    -- | A @GitSubmodulesConfig@ object that overrides the Git submodules configuration for this batch build.
    gitSubmodulesConfigOverride :: Lude.Maybe GitSubmodulesConfig,
    -- | A container type for this batch build that overrides the one specified in the batch build project.
    environmentTypeOverride :: Lude.Maybe EnvironmentType,
    -- | The name of a certificate for this batch build that overrides the one specified in the batch build project.
    certificateOverride :: Lude.Maybe Lude.Text,
    -- | The name of a compute type for this batch build that overrides the one specified in the batch build project.
    computeTypeOverride :: Lude.Maybe ComputeType,
    -- | Set to @true@ to report to your source provider the status of a batch build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an @invalidInputException@ is thrown.
    reportBuildBatchStatusOverride :: Lude.Maybe Lude.Bool,
    -- | Enable this flag to override privileged mode in the batch build project.
    privilegedModeOverride :: Lude.Maybe Lude.Bool,
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
    sourceVersion :: Lude.Maybe Lude.Text,
    -- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
    --
    -- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
    buildspecOverride :: Lude.Maybe Lude.Text,
    -- | The name of the project.
    projectName :: Lude.Text,
    -- | An array of @ProjectSourceVersion@ objects that override the secondary source versions in the batch build project.
    secondarySourcesVersionOverride :: Lude.Maybe [ProjectSourceVersion],
    -- | Enable this flag to override the insecure SSL setting that is specified in the batch build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
    insecureSSLOverride :: Lude.Maybe Lude.Bool,
    -- | The name of an image for this batch build that overrides the one specified in the batch build project.
    imageOverride :: Lude.Maybe Lude.Text,
    -- | An array of @ProjectArtifacts@ objects that override the secondary artifacts defined in the batch build project.
    secondaryArtifactsOverride :: Lude.Maybe [ProjectArtifacts],
    -- | Overrides the build timeout specified in the batch build project.
    buildTimeoutInMinutesOverride :: Lude.Maybe Lude.Natural,
    -- | An array of @ProjectArtifacts@ objects that contains information about the build output artifact overrides for the build project.
    artifactsOverride :: Lude.Maybe ProjectArtifacts,
    -- | The source input type that overrides the source input defined in the batch build project.
    sourceTypeOverride :: Lude.Maybe SourceType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartBuildBatch' with the minimum fields required to make a request.
--
-- * 'encryptionKeyOverride' - The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the batch build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
-- * 'sourceLocationOverride' - A location that overrides, for this batch build, the source location defined in the batch build project.
-- * 'buildBatchConfigOverride' - A @BuildBatchConfigOverride@ object that contains batch build configuration overrides.
-- * 'environmentVariablesOverride' - An array of @EnvironmentVariable@ objects that override, or add to, the environment variables defined in the batch build project.
-- * 'idempotencyToken' - A unique, case sensitive identifier you provide to ensure the idempotency of the @StartBuildBatch@ request. The token is included in the @StartBuildBatch@ request and is valid for five minutes. If you repeat the @StartBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
-- * 'registryCredentialOverride' - A @RegistryCredential@ object that overrides credentials for access to a private registry.
-- * 'serviceRoleOverride' - The name of a service role for this batch build that overrides the one specified in the batch build project.
-- * 'cacheOverride' - A @ProjectCache@ object that specifies cache overrides.
-- * 'queuedTimeoutInMinutesOverride' - The number of minutes a batch build is allowed to be queued before it times out.
-- * 'secondarySourcesOverride' - An array of @ProjectSource@ objects that override the secondary sources defined in the batch build project.
-- * 'gitCloneDepthOverride' - The user-defined depth of history, with a minimum value of 0, that overrides, for this batch build only, any previous depth of history defined in the batch build project.
-- * 'imagePullCredentialsTypeOverride' - The type of credentials AWS CodeBuild uses to pull images in your batch build. There are two valid values:
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
-- * 'logsConfigOverride' - A @LogsConfig@ object that override the log settings defined in the batch build project.
-- * 'sourceAuthOverride' - A @SourceAuth@ object that overrides the one defined in the batch build project. This override applies only if the build project's source is BitBucket or GitHub.
-- * 'gitSubmodulesConfigOverride' - A @GitSubmodulesConfig@ object that overrides the Git submodules configuration for this batch build.
-- * 'environmentTypeOverride' - A container type for this batch build that overrides the one specified in the batch build project.
-- * 'certificateOverride' - The name of a certificate for this batch build that overrides the one specified in the batch build project.
-- * 'computeTypeOverride' - The name of a compute type for this batch build that overrides the one specified in the batch build project.
-- * 'reportBuildBatchStatusOverride' - Set to @true@ to report to your source provider the status of a batch build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an @invalidInputException@ is thrown.
-- * 'privilegedModeOverride' - Enable this flag to override privileged mode in the batch build project.
-- * 'sourceVersion' - The version of the batch build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:
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
-- * 'buildspecOverride' - A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
-- * 'projectName' - The name of the project.
-- * 'secondarySourcesVersionOverride' - An array of @ProjectSourceVersion@ objects that override the secondary source versions in the batch build project.
-- * 'insecureSSLOverride' - Enable this flag to override the insecure SSL setting that is specified in the batch build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
-- * 'imageOverride' - The name of an image for this batch build that overrides the one specified in the batch build project.
-- * 'secondaryArtifactsOverride' - An array of @ProjectArtifacts@ objects that override the secondary artifacts defined in the batch build project.
-- * 'buildTimeoutInMinutesOverride' - Overrides the build timeout specified in the batch build project.
-- * 'artifactsOverride' - An array of @ProjectArtifacts@ objects that contains information about the build output artifact overrides for the build project.
-- * 'sourceTypeOverride' - The source input type that overrides the source input defined in the batch build project.
mkStartBuildBatch ::
  -- | 'projectName'
  Lude.Text ->
  StartBuildBatch
mkStartBuildBatch pProjectName_ =
  StartBuildBatch'
    { encryptionKeyOverride = Lude.Nothing,
      sourceLocationOverride = Lude.Nothing,
      buildBatchConfigOverride = Lude.Nothing,
      environmentVariablesOverride = Lude.Nothing,
      idempotencyToken = Lude.Nothing,
      registryCredentialOverride = Lude.Nothing,
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
      reportBuildBatchStatusOverride = Lude.Nothing,
      privilegedModeOverride = Lude.Nothing,
      sourceVersion = Lude.Nothing,
      buildspecOverride = Lude.Nothing,
      projectName = pProjectName_,
      secondarySourcesVersionOverride = Lude.Nothing,
      insecureSSLOverride = Lude.Nothing,
      imageOverride = Lude.Nothing,
      secondaryArtifactsOverride = Lude.Nothing,
      buildTimeoutInMinutesOverride = Lude.Nothing,
      artifactsOverride = Lude.Nothing,
      sourceTypeOverride = Lude.Nothing
    }

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the batch build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKeyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEncryptionKeyOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbEncryptionKeyOverride = Lens.lens (encryptionKeyOverride :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {encryptionKeyOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbEncryptionKeyOverride "Use generic-lens or generic-optics with 'encryptionKeyOverride' instead." #-}

-- | A location that overrides, for this batch build, the source location defined in the batch build project.
--
-- /Note:/ Consider using 'sourceLocationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceLocationOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbSourceLocationOverride = Lens.lens (sourceLocationOverride :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {sourceLocationOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbSourceLocationOverride "Use generic-lens or generic-optics with 'sourceLocationOverride' instead." #-}

-- | A @BuildBatchConfigOverride@ object that contains batch build configuration overrides.
--
-- /Note:/ Consider using 'buildBatchConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildBatchConfigOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe ProjectBuildBatchConfig)
sbbBuildBatchConfigOverride = Lens.lens (buildBatchConfigOverride :: StartBuildBatch -> Lude.Maybe ProjectBuildBatchConfig) (\s a -> s {buildBatchConfigOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbBuildBatchConfigOverride "Use generic-lens or generic-optics with 'buildBatchConfigOverride' instead." #-}

-- | An array of @EnvironmentVariable@ objects that override, or add to, the environment variables defined in the batch build project.
--
-- /Note:/ Consider using 'environmentVariablesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEnvironmentVariablesOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe [EnvironmentVariable])
sbbEnvironmentVariablesOverride = Lens.lens (environmentVariablesOverride :: StartBuildBatch -> Lude.Maybe [EnvironmentVariable]) (\s a -> s {environmentVariablesOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbEnvironmentVariablesOverride "Use generic-lens or generic-optics with 'environmentVariablesOverride' instead." #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @StartBuildBatch@ request. The token is included in the @StartBuildBatch@ request and is valid for five minutes. If you repeat the @StartBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbIdempotencyToken :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbIdempotencyToken = Lens.lens (idempotencyToken :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {idempotencyToken = a} :: StartBuildBatch)
{-# DEPRECATED sbbIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | A @RegistryCredential@ object that overrides credentials for access to a private registry.
--
-- /Note:/ Consider using 'registryCredentialOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbRegistryCredentialOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe RegistryCredential)
sbbRegistryCredentialOverride = Lens.lens (registryCredentialOverride :: StartBuildBatch -> Lude.Maybe RegistryCredential) (\s a -> s {registryCredentialOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbRegistryCredentialOverride "Use generic-lens or generic-optics with 'registryCredentialOverride' instead." #-}

-- | The name of a service role for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'serviceRoleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbServiceRoleOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbServiceRoleOverride = Lens.lens (serviceRoleOverride :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {serviceRoleOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbServiceRoleOverride "Use generic-lens or generic-optics with 'serviceRoleOverride' instead." #-}

-- | A @ProjectCache@ object that specifies cache overrides.
--
-- /Note:/ Consider using 'cacheOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbCacheOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe ProjectCache)
sbbCacheOverride = Lens.lens (cacheOverride :: StartBuildBatch -> Lude.Maybe ProjectCache) (\s a -> s {cacheOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbCacheOverride "Use generic-lens or generic-optics with 'cacheOverride' instead." #-}

-- | The number of minutes a batch build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbQueuedTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Natural)
sbbQueuedTimeoutInMinutesOverride = Lens.lens (queuedTimeoutInMinutesOverride :: StartBuildBatch -> Lude.Maybe Lude.Natural) (\s a -> s {queuedTimeoutInMinutesOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbQueuedTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'queuedTimeoutInMinutesOverride' instead." #-}

-- | An array of @ProjectSource@ objects that override the secondary sources defined in the batch build project.
--
-- /Note:/ Consider using 'secondarySourcesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondarySourcesOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe [ProjectSource])
sbbSecondarySourcesOverride = Lens.lens (secondarySourcesOverride :: StartBuildBatch -> Lude.Maybe [ProjectSource]) (\s a -> s {secondarySourcesOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbSecondarySourcesOverride "Use generic-lens or generic-optics with 'secondarySourcesOverride' instead." #-}

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this batch build only, any previous depth of history defined in the batch build project.
--
-- /Note:/ Consider using 'gitCloneDepthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbGitCloneDepthOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Natural)
sbbGitCloneDepthOverride = Lens.lens (gitCloneDepthOverride :: StartBuildBatch -> Lude.Maybe Lude.Natural) (\s a -> s {gitCloneDepthOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbGitCloneDepthOverride "Use generic-lens or generic-optics with 'gitCloneDepthOverride' instead." #-}

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
sbbImagePullCredentialsTypeOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe ImagePullCredentialsType)
sbbImagePullCredentialsTypeOverride = Lens.lens (imagePullCredentialsTypeOverride :: StartBuildBatch -> Lude.Maybe ImagePullCredentialsType) (\s a -> s {imagePullCredentialsTypeOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbImagePullCredentialsTypeOverride "Use generic-lens or generic-optics with 'imagePullCredentialsTypeOverride' instead." #-}

-- | A @LogsConfig@ object that override the log settings defined in the batch build project.
--
-- /Note:/ Consider using 'logsConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbLogsConfigOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe LogsConfig)
sbbLogsConfigOverride = Lens.lens (logsConfigOverride :: StartBuildBatch -> Lude.Maybe LogsConfig) (\s a -> s {logsConfigOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbLogsConfigOverride "Use generic-lens or generic-optics with 'logsConfigOverride' instead." #-}

-- | A @SourceAuth@ object that overrides the one defined in the batch build project. This override applies only if the build project's source is BitBucket or GitHub.
--
-- /Note:/ Consider using 'sourceAuthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceAuthOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe SourceAuth)
sbbSourceAuthOverride = Lens.lens (sourceAuthOverride :: StartBuildBatch -> Lude.Maybe SourceAuth) (\s a -> s {sourceAuthOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbSourceAuthOverride "Use generic-lens or generic-optics with 'sourceAuthOverride' instead." #-}

-- | A @GitSubmodulesConfig@ object that overrides the Git submodules configuration for this batch build.
--
-- /Note:/ Consider using 'gitSubmodulesConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbGitSubmodulesConfigOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe GitSubmodulesConfig)
sbbGitSubmodulesConfigOverride = Lens.lens (gitSubmodulesConfigOverride :: StartBuildBatch -> Lude.Maybe GitSubmodulesConfig) (\s a -> s {gitSubmodulesConfigOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbGitSubmodulesConfigOverride "Use generic-lens or generic-optics with 'gitSubmodulesConfigOverride' instead." #-}

-- | A container type for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'environmentTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEnvironmentTypeOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe EnvironmentType)
sbbEnvironmentTypeOverride = Lens.lens (environmentTypeOverride :: StartBuildBatch -> Lude.Maybe EnvironmentType) (\s a -> s {environmentTypeOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbEnvironmentTypeOverride "Use generic-lens or generic-optics with 'environmentTypeOverride' instead." #-}

-- | The name of a certificate for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'certificateOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbCertificateOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbCertificateOverride = Lens.lens (certificateOverride :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {certificateOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbCertificateOverride "Use generic-lens or generic-optics with 'certificateOverride' instead." #-}

-- | The name of a compute type for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'computeTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbComputeTypeOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe ComputeType)
sbbComputeTypeOverride = Lens.lens (computeTypeOverride :: StartBuildBatch -> Lude.Maybe ComputeType) (\s a -> s {computeTypeOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbComputeTypeOverride "Use generic-lens or generic-optics with 'computeTypeOverride' instead." #-}

-- | Set to @true@ to report to your source provider the status of a batch build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an @invalidInputException@ is thrown.
--
-- /Note:/ Consider using 'reportBuildBatchStatusOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbReportBuildBatchStatusOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Bool)
sbbReportBuildBatchStatusOverride = Lens.lens (reportBuildBatchStatusOverride :: StartBuildBatch -> Lude.Maybe Lude.Bool) (\s a -> s {reportBuildBatchStatusOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbReportBuildBatchStatusOverride "Use generic-lens or generic-optics with 'reportBuildBatchStatusOverride' instead." #-}

-- | Enable this flag to override privileged mode in the batch build project.
--
-- /Note:/ Consider using 'privilegedModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbPrivilegedModeOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Bool)
sbbPrivilegedModeOverride = Lens.lens (privilegedModeOverride :: StartBuildBatch -> Lude.Maybe Lude.Bool) (\s a -> s {privilegedModeOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbPrivilegedModeOverride "Use generic-lens or generic-optics with 'privilegedModeOverride' instead." #-}

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
sbbSourceVersion :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbSourceVersion = Lens.lens (sourceVersion :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {sourceVersion = a} :: StartBuildBatch)
{-# DEPRECATED sbbSourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead." #-}

-- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- /Note:/ Consider using 'buildspecOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildspecOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbBuildspecOverride = Lens.lens (buildspecOverride :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {buildspecOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbBuildspecOverride "Use generic-lens or generic-optics with 'buildspecOverride' instead." #-}

-- | The name of the project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbProjectName :: Lens.Lens' StartBuildBatch Lude.Text
sbbProjectName = Lens.lens (projectName :: StartBuildBatch -> Lude.Text) (\s a -> s {projectName = a} :: StartBuildBatch)
{-# DEPRECATED sbbProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

-- | An array of @ProjectSourceVersion@ objects that override the secondary source versions in the batch build project.
--
-- /Note:/ Consider using 'secondarySourcesVersionOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondarySourcesVersionOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe [ProjectSourceVersion])
sbbSecondarySourcesVersionOverride = Lens.lens (secondarySourcesVersionOverride :: StartBuildBatch -> Lude.Maybe [ProjectSourceVersion]) (\s a -> s {secondarySourcesVersionOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbSecondarySourcesVersionOverride "Use generic-lens or generic-optics with 'secondarySourcesVersionOverride' instead." #-}

-- | Enable this flag to override the insecure SSL setting that is specified in the batch build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
--
-- /Note:/ Consider using 'insecureSSLOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbInsecureSSLOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Bool)
sbbInsecureSSLOverride = Lens.lens (insecureSSLOverride :: StartBuildBatch -> Lude.Maybe Lude.Bool) (\s a -> s {insecureSSLOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbInsecureSSLOverride "Use generic-lens or generic-optics with 'insecureSSLOverride' instead." #-}

-- | The name of an image for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'imageOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbImageOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Text)
sbbImageOverride = Lens.lens (imageOverride :: StartBuildBatch -> Lude.Maybe Lude.Text) (\s a -> s {imageOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbImageOverride "Use generic-lens or generic-optics with 'imageOverride' instead." #-}

-- | An array of @ProjectArtifacts@ objects that override the secondary artifacts defined in the batch build project.
--
-- /Note:/ Consider using 'secondaryArtifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondaryArtifactsOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe [ProjectArtifacts])
sbbSecondaryArtifactsOverride = Lens.lens (secondaryArtifactsOverride :: StartBuildBatch -> Lude.Maybe [ProjectArtifacts]) (\s a -> s {secondaryArtifactsOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbSecondaryArtifactsOverride "Use generic-lens or generic-optics with 'secondaryArtifactsOverride' instead." #-}

-- | Overrides the build timeout specified in the batch build project.
--
-- /Note:/ Consider using 'buildTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe Lude.Natural)
sbbBuildTimeoutInMinutesOverride = Lens.lens (buildTimeoutInMinutesOverride :: StartBuildBatch -> Lude.Maybe Lude.Natural) (\s a -> s {buildTimeoutInMinutesOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbBuildTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'buildTimeoutInMinutesOverride' instead." #-}

-- | An array of @ProjectArtifacts@ objects that contains information about the build output artifact overrides for the build project.
--
-- /Note:/ Consider using 'artifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbArtifactsOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe ProjectArtifacts)
sbbArtifactsOverride = Lens.lens (artifactsOverride :: StartBuildBatch -> Lude.Maybe ProjectArtifacts) (\s a -> s {artifactsOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbArtifactsOverride "Use generic-lens or generic-optics with 'artifactsOverride' instead." #-}

-- | The source input type that overrides the source input defined in the batch build project.
--
-- /Note:/ Consider using 'sourceTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceTypeOverride :: Lens.Lens' StartBuildBatch (Lude.Maybe SourceType)
sbbSourceTypeOverride = Lens.lens (sourceTypeOverride :: StartBuildBatch -> Lude.Maybe SourceType) (\s a -> s {sourceTypeOverride = a} :: StartBuildBatch)
{-# DEPRECATED sbbSourceTypeOverride "Use generic-lens or generic-optics with 'sourceTypeOverride' instead." #-}

instance Lude.AWSRequest StartBuildBatch where
  type Rs StartBuildBatch = StartBuildBatchResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartBuildBatchResponse'
            Lude.<$> (x Lude..?> "buildBatch") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartBuildBatch where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.StartBuildBatch" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartBuildBatch where
  toJSON StartBuildBatch' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("encryptionKeyOverride" Lude..=) Lude.<$> encryptionKeyOverride,
            ("sourceLocationOverride" Lude..=) Lude.<$> sourceLocationOverride,
            ("buildBatchConfigOverride" Lude..=)
              Lude.<$> buildBatchConfigOverride,
            ("environmentVariablesOverride" Lude..=)
              Lude.<$> environmentVariablesOverride,
            ("idempotencyToken" Lude..=) Lude.<$> idempotencyToken,
            ("registryCredentialOverride" Lude..=)
              Lude.<$> registryCredentialOverride,
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
            ("reportBuildBatchStatusOverride" Lude..=)
              Lude.<$> reportBuildBatchStatusOverride,
            ("privilegedModeOverride" Lude..=) Lude.<$> privilegedModeOverride,
            ("sourceVersion" Lude..=) Lude.<$> sourceVersion,
            ("buildspecOverride" Lude..=) Lude.<$> buildspecOverride,
            Lude.Just ("projectName" Lude..= projectName),
            ("secondarySourcesVersionOverride" Lude..=)
              Lude.<$> secondarySourcesVersionOverride,
            ("insecureSslOverride" Lude..=) Lude.<$> insecureSSLOverride,
            ("imageOverride" Lude..=) Lude.<$> imageOverride,
            ("secondaryArtifactsOverride" Lude..=)
              Lude.<$> secondaryArtifactsOverride,
            ("buildTimeoutInMinutesOverride" Lude..=)
              Lude.<$> buildTimeoutInMinutesOverride,
            ("artifactsOverride" Lude..=) Lude.<$> artifactsOverride,
            ("sourceTypeOverride" Lude..=) Lude.<$> sourceTypeOverride
          ]
      )

instance Lude.ToPath StartBuildBatch where
  toPath = Lude.const "/"

instance Lude.ToQuery StartBuildBatch where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartBuildBatchResponse' smart constructor.
data StartBuildBatchResponse = StartBuildBatchResponse'
  { -- | A @BuildBatch@ object that contains information about the batch build.
    buildBatch :: Lude.Maybe BuildBatch,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartBuildBatchResponse' with the minimum fields required to make a request.
--
-- * 'buildBatch' - A @BuildBatch@ object that contains information about the batch build.
-- * 'responseStatus' - The response status code.
mkStartBuildBatchResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartBuildBatchResponse
mkStartBuildBatchResponse pResponseStatus_ =
  StartBuildBatchResponse'
    { buildBatch = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A @BuildBatch@ object that contains information about the batch build.
--
-- /Note:/ Consider using 'buildBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbrsBuildBatch :: Lens.Lens' StartBuildBatchResponse (Lude.Maybe BuildBatch)
sbbrsBuildBatch = Lens.lens (buildBatch :: StartBuildBatchResponse -> Lude.Maybe BuildBatch) (\s a -> s {buildBatch = a} :: StartBuildBatchResponse)
{-# DEPRECATED sbbrsBuildBatch "Use generic-lens or generic-optics with 'buildBatch' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbrsResponseStatus :: Lens.Lens' StartBuildBatchResponse Lude.Int
sbbrsResponseStatus = Lens.lens (responseStatus :: StartBuildBatchResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartBuildBatchResponse)
{-# DEPRECATED sbbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
