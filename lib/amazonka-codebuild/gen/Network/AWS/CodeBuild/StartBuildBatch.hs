{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartBuildBatch (..)
    , mkStartBuildBatch
    -- ** Request lenses
    , sbbProjectName
    , sbbArtifactsOverride
    , sbbBuildBatchConfigOverride
    , sbbBuildTimeoutInMinutesOverride
    , sbbBuildspecOverride
    , sbbCacheOverride
    , sbbCertificateOverride
    , sbbComputeTypeOverride
    , sbbEncryptionKeyOverride
    , sbbEnvironmentTypeOverride
    , sbbEnvironmentVariablesOverride
    , sbbGitCloneDepthOverride
    , sbbGitSubmodulesConfigOverride
    , sbbIdempotencyToken
    , sbbImageOverride
    , sbbImagePullCredentialsTypeOverride
    , sbbInsecureSslOverride
    , sbbLogsConfigOverride
    , sbbPrivilegedModeOverride
    , sbbQueuedTimeoutInMinutesOverride
    , sbbRegistryCredentialOverride
    , sbbReportBuildBatchStatusOverride
    , sbbSecondaryArtifactsOverride
    , sbbSecondarySourcesOverride
    , sbbSecondarySourcesVersionOverride
    , sbbServiceRoleOverride
    , sbbSourceAuthOverride
    , sbbSourceLocationOverride
    , sbbSourceTypeOverride
    , sbbSourceVersion

    -- * Destructuring the response
    , StartBuildBatchResponse (..)
    , mkStartBuildBatchResponse
    -- ** Response lenses
    , srsBuildBatch
    , srsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartBuildBatch' smart constructor.
data StartBuildBatch = StartBuildBatch'
  { projectName :: Types.NonEmptyString
    -- ^ The name of the project.
  , artifactsOverride :: Core.Maybe Types.ProjectArtifacts
    -- ^ An array of @ProjectArtifacts@ objects that contains information about the build output artifact overrides for the build project.
  , buildBatchConfigOverride :: Core.Maybe Types.ProjectBuildBatchConfig
    -- ^ A @BuildBatchConfigOverride@ object that contains batch build configuration overrides.
  , buildTimeoutInMinutesOverride :: Core.Maybe Core.Natural
    -- ^ Overrides the build timeout specified in the batch build project.
  , buildspecOverride :: Core.Maybe Core.Text
    -- ^ A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> . 
  , cacheOverride :: Core.Maybe Types.ProjectCache
    -- ^ A @ProjectCache@ object that specifies cache overrides.
  , certificateOverride :: Core.Maybe Core.Text
    -- ^ The name of a certificate for this batch build that overrides the one specified in the batch build project.
  , computeTypeOverride :: Core.Maybe Types.ComputeType
    -- ^ The name of a compute type for this batch build that overrides the one specified in the batch build project.
  , encryptionKeyOverride :: Core.Maybe Types.EncryptionKeyOverride
    -- ^ The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the batch build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
  , environmentTypeOverride :: Core.Maybe Types.EnvironmentType
    -- ^ A container type for this batch build that overrides the one specified in the batch build project.
  , environmentVariablesOverride :: Core.Maybe [Types.EnvironmentVariable]
    -- ^ An array of @EnvironmentVariable@ objects that override, or add to, the environment variables defined in the batch build project.
  , gitCloneDepthOverride :: Core.Maybe Core.Natural
    -- ^ The user-defined depth of history, with a minimum value of 0, that overrides, for this batch build only, any previous depth of history defined in the batch build project.
  , gitSubmodulesConfigOverride :: Core.Maybe Types.GitSubmodulesConfig
    -- ^ A @GitSubmodulesConfig@ object that overrides the Git submodules configuration for this batch build.
  , idempotencyToken :: Core.Maybe Core.Text
    -- ^ A unique, case sensitive identifier you provide to ensure the idempotency of the @StartBuildBatch@ request. The token is included in the @StartBuildBatch@ request and is valid for five minutes. If you repeat the @StartBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
  , imageOverride :: Core.Maybe Types.ImageOverride
    -- ^ The name of an image for this batch build that overrides the one specified in the batch build project.
  , imagePullCredentialsTypeOverride :: Core.Maybe Types.ImagePullCredentialsType
    -- ^ The type of credentials AWS CodeBuild uses to pull images in your batch build. There are two valid values: 
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
  , insecureSslOverride :: Core.Maybe Core.Bool
    -- ^ Enable this flag to override the insecure SSL setting that is specified in the batch build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
  , logsConfigOverride :: Core.Maybe Types.LogsConfig
    -- ^ A @LogsConfig@ object that override the log settings defined in the batch build project.
  , privilegedModeOverride :: Core.Maybe Core.Bool
    -- ^ Enable this flag to override privileged mode in the batch build project.
  , queuedTimeoutInMinutesOverride :: Core.Maybe Core.Natural
    -- ^ The number of minutes a batch build is allowed to be queued before it times out.
  , registryCredentialOverride :: Core.Maybe Types.RegistryCredential
    -- ^ A @RegistryCredential@ object that overrides credentials for access to a private registry.
  , reportBuildBatchStatusOverride :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to report to your source provider the status of a batch build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an @invalidInputException@ is thrown. 
  , secondaryArtifactsOverride :: Core.Maybe [Types.ProjectArtifacts]
    -- ^ An array of @ProjectArtifacts@ objects that override the secondary artifacts defined in the batch build project.
  , secondarySourcesOverride :: Core.Maybe [Types.ProjectSource]
    -- ^ An array of @ProjectSource@ objects that override the secondary sources defined in the batch build project.
  , secondarySourcesVersionOverride :: Core.Maybe [Types.ProjectSourceVersion]
    -- ^ An array of @ProjectSourceVersion@ objects that override the secondary source versions in the batch build project.
  , serviceRoleOverride :: Core.Maybe Types.ServiceRoleOverride
    -- ^ The name of a service role for this batch build that overrides the one specified in the batch build project.
  , sourceAuthOverride :: Core.Maybe Types.SourceAuth
    -- ^ A @SourceAuth@ object that overrides the one defined in the batch build project. This override applies only if the build project's source is BitBucket or GitHub.
  , sourceLocationOverride :: Core.Maybe Core.Text
    -- ^ A location that overrides, for this batch build, the source location defined in the batch build project.
  , sourceTypeOverride :: Core.Maybe Types.SourceType
    -- ^ The source input type that overrides the source input defined in the batch build project.
  , sourceVersion :: Core.Maybe Core.Text
    -- ^ The version of the batch build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartBuildBatch' value with any optional fields omitted.
mkStartBuildBatch
    :: Types.NonEmptyString -- ^ 'projectName'
    -> StartBuildBatch
mkStartBuildBatch projectName
  = StartBuildBatch'{projectName, artifactsOverride = Core.Nothing,
                     buildBatchConfigOverride = Core.Nothing,
                     buildTimeoutInMinutesOverride = Core.Nothing,
                     buildspecOverride = Core.Nothing, cacheOverride = Core.Nothing,
                     certificateOverride = Core.Nothing,
                     computeTypeOverride = Core.Nothing,
                     encryptionKeyOverride = Core.Nothing,
                     environmentTypeOverride = Core.Nothing,
                     environmentVariablesOverride = Core.Nothing,
                     gitCloneDepthOverride = Core.Nothing,
                     gitSubmodulesConfigOverride = Core.Nothing,
                     idempotencyToken = Core.Nothing, imageOverride = Core.Nothing,
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
                     sourceTypeOverride = Core.Nothing, sourceVersion = Core.Nothing}

-- | The name of the project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbProjectName :: Lens.Lens' StartBuildBatch Types.NonEmptyString
sbbProjectName = Lens.field @"projectName"
{-# INLINEABLE sbbProjectName #-}
{-# DEPRECATED projectName "Use generic-lens or generic-optics with 'projectName' instead"  #-}

-- | An array of @ProjectArtifacts@ objects that contains information about the build output artifact overrides for the build project.
--
-- /Note:/ Consider using 'artifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbArtifactsOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ProjectArtifacts)
sbbArtifactsOverride = Lens.field @"artifactsOverride"
{-# INLINEABLE sbbArtifactsOverride #-}
{-# DEPRECATED artifactsOverride "Use generic-lens or generic-optics with 'artifactsOverride' instead"  #-}

-- | A @BuildBatchConfigOverride@ object that contains batch build configuration overrides.
--
-- /Note:/ Consider using 'buildBatchConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildBatchConfigOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ProjectBuildBatchConfig)
sbbBuildBatchConfigOverride = Lens.field @"buildBatchConfigOverride"
{-# INLINEABLE sbbBuildBatchConfigOverride #-}
{-# DEPRECATED buildBatchConfigOverride "Use generic-lens or generic-optics with 'buildBatchConfigOverride' instead"  #-}

-- | Overrides the build timeout specified in the batch build project.
--
-- /Note:/ Consider using 'buildTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Natural)
sbbBuildTimeoutInMinutesOverride = Lens.field @"buildTimeoutInMinutesOverride"
{-# INLINEABLE sbbBuildTimeoutInMinutesOverride #-}
{-# DEPRECATED buildTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'buildTimeoutInMinutesOverride' instead"  #-}

-- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> . 
--
-- /Note:/ Consider using 'buildspecOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbBuildspecOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Text)
sbbBuildspecOverride = Lens.field @"buildspecOverride"
{-# INLINEABLE sbbBuildspecOverride #-}
{-# DEPRECATED buildspecOverride "Use generic-lens or generic-optics with 'buildspecOverride' instead"  #-}

-- | A @ProjectCache@ object that specifies cache overrides.
--
-- /Note:/ Consider using 'cacheOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbCacheOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ProjectCache)
sbbCacheOverride = Lens.field @"cacheOverride"
{-# INLINEABLE sbbCacheOverride #-}
{-# DEPRECATED cacheOverride "Use generic-lens or generic-optics with 'cacheOverride' instead"  #-}

-- | The name of a certificate for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'certificateOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbCertificateOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Text)
sbbCertificateOverride = Lens.field @"certificateOverride"
{-# INLINEABLE sbbCertificateOverride #-}
{-# DEPRECATED certificateOverride "Use generic-lens or generic-optics with 'certificateOverride' instead"  #-}

-- | The name of a compute type for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'computeTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbComputeTypeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ComputeType)
sbbComputeTypeOverride = Lens.field @"computeTypeOverride"
{-# INLINEABLE sbbComputeTypeOverride #-}
{-# DEPRECATED computeTypeOverride "Use generic-lens or generic-optics with 'computeTypeOverride' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the batch build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKeyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEncryptionKeyOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.EncryptionKeyOverride)
sbbEncryptionKeyOverride = Lens.field @"encryptionKeyOverride"
{-# INLINEABLE sbbEncryptionKeyOverride #-}
{-# DEPRECATED encryptionKeyOverride "Use generic-lens or generic-optics with 'encryptionKeyOverride' instead"  #-}

-- | A container type for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'environmentTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEnvironmentTypeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.EnvironmentType)
sbbEnvironmentTypeOverride = Lens.field @"environmentTypeOverride"
{-# INLINEABLE sbbEnvironmentTypeOverride #-}
{-# DEPRECATED environmentTypeOverride "Use generic-lens or generic-optics with 'environmentTypeOverride' instead"  #-}

-- | An array of @EnvironmentVariable@ objects that override, or add to, the environment variables defined in the batch build project.
--
-- /Note:/ Consider using 'environmentVariablesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbEnvironmentVariablesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.EnvironmentVariable])
sbbEnvironmentVariablesOverride = Lens.field @"environmentVariablesOverride"
{-# INLINEABLE sbbEnvironmentVariablesOverride #-}
{-# DEPRECATED environmentVariablesOverride "Use generic-lens or generic-optics with 'environmentVariablesOverride' instead"  #-}

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this batch build only, any previous depth of history defined in the batch build project.
--
-- /Note:/ Consider using 'gitCloneDepthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbGitCloneDepthOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Natural)
sbbGitCloneDepthOverride = Lens.field @"gitCloneDepthOverride"
{-# INLINEABLE sbbGitCloneDepthOverride #-}
{-# DEPRECATED gitCloneDepthOverride "Use generic-lens or generic-optics with 'gitCloneDepthOverride' instead"  #-}

-- | A @GitSubmodulesConfig@ object that overrides the Git submodules configuration for this batch build.
--
-- /Note:/ Consider using 'gitSubmodulesConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbGitSubmodulesConfigOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.GitSubmodulesConfig)
sbbGitSubmodulesConfigOverride = Lens.field @"gitSubmodulesConfigOverride"
{-# INLINEABLE sbbGitSubmodulesConfigOverride #-}
{-# DEPRECATED gitSubmodulesConfigOverride "Use generic-lens or generic-optics with 'gitSubmodulesConfigOverride' instead"  #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the @StartBuildBatch@ request. The token is included in the @StartBuildBatch@ request and is valid for five minutes. If you repeat the @StartBuildBatch@ request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbIdempotencyToken :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Text)
sbbIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE sbbIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

-- | The name of an image for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'imageOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbImageOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ImageOverride)
sbbImageOverride = Lens.field @"imageOverride"
{-# INLINEABLE sbbImageOverride #-}
{-# DEPRECATED imageOverride "Use generic-lens or generic-optics with 'imageOverride' instead"  #-}

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
{-# INLINEABLE sbbImagePullCredentialsTypeOverride #-}
{-# DEPRECATED imagePullCredentialsTypeOverride "Use generic-lens or generic-optics with 'imagePullCredentialsTypeOverride' instead"  #-}

-- | Enable this flag to override the insecure SSL setting that is specified in the batch build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
--
-- /Note:/ Consider using 'insecureSslOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbInsecureSslOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Bool)
sbbInsecureSslOverride = Lens.field @"insecureSslOverride"
{-# INLINEABLE sbbInsecureSslOverride #-}
{-# DEPRECATED insecureSslOverride "Use generic-lens or generic-optics with 'insecureSslOverride' instead"  #-}

-- | A @LogsConfig@ object that override the log settings defined in the batch build project.
--
-- /Note:/ Consider using 'logsConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbLogsConfigOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.LogsConfig)
sbbLogsConfigOverride = Lens.field @"logsConfigOverride"
{-# INLINEABLE sbbLogsConfigOverride #-}
{-# DEPRECATED logsConfigOverride "Use generic-lens or generic-optics with 'logsConfigOverride' instead"  #-}

-- | Enable this flag to override privileged mode in the batch build project.
--
-- /Note:/ Consider using 'privilegedModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbPrivilegedModeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Bool)
sbbPrivilegedModeOverride = Lens.field @"privilegedModeOverride"
{-# INLINEABLE sbbPrivilegedModeOverride #-}
{-# DEPRECATED privilegedModeOverride "Use generic-lens or generic-optics with 'privilegedModeOverride' instead"  #-}

-- | The number of minutes a batch build is allowed to be queued before it times out.
--
-- /Note:/ Consider using 'queuedTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbQueuedTimeoutInMinutesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Natural)
sbbQueuedTimeoutInMinutesOverride = Lens.field @"queuedTimeoutInMinutesOverride"
{-# INLINEABLE sbbQueuedTimeoutInMinutesOverride #-}
{-# DEPRECATED queuedTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'queuedTimeoutInMinutesOverride' instead"  #-}

-- | A @RegistryCredential@ object that overrides credentials for access to a private registry.
--
-- /Note:/ Consider using 'registryCredentialOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbRegistryCredentialOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.RegistryCredential)
sbbRegistryCredentialOverride = Lens.field @"registryCredentialOverride"
{-# INLINEABLE sbbRegistryCredentialOverride #-}
{-# DEPRECATED registryCredentialOverride "Use generic-lens or generic-optics with 'registryCredentialOverride' instead"  #-}

-- | Set to @true@ to report to your source provider the status of a batch build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an @invalidInputException@ is thrown. 
--
-- /Note:/ Consider using 'reportBuildBatchStatusOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbReportBuildBatchStatusOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Bool)
sbbReportBuildBatchStatusOverride = Lens.field @"reportBuildBatchStatusOverride"
{-# INLINEABLE sbbReportBuildBatchStatusOverride #-}
{-# DEPRECATED reportBuildBatchStatusOverride "Use generic-lens or generic-optics with 'reportBuildBatchStatusOverride' instead"  #-}

-- | An array of @ProjectArtifacts@ objects that override the secondary artifacts defined in the batch build project.
--
-- /Note:/ Consider using 'secondaryArtifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondaryArtifactsOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.ProjectArtifacts])
sbbSecondaryArtifactsOverride = Lens.field @"secondaryArtifactsOverride"
{-# INLINEABLE sbbSecondaryArtifactsOverride #-}
{-# DEPRECATED secondaryArtifactsOverride "Use generic-lens or generic-optics with 'secondaryArtifactsOverride' instead"  #-}

-- | An array of @ProjectSource@ objects that override the secondary sources defined in the batch build project.
--
-- /Note:/ Consider using 'secondarySourcesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondarySourcesOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.ProjectSource])
sbbSecondarySourcesOverride = Lens.field @"secondarySourcesOverride"
{-# INLINEABLE sbbSecondarySourcesOverride #-}
{-# DEPRECATED secondarySourcesOverride "Use generic-lens or generic-optics with 'secondarySourcesOverride' instead"  #-}

-- | An array of @ProjectSourceVersion@ objects that override the secondary source versions in the batch build project.
--
-- /Note:/ Consider using 'secondarySourcesVersionOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSecondarySourcesVersionOverride :: Lens.Lens' StartBuildBatch (Core.Maybe [Types.ProjectSourceVersion])
sbbSecondarySourcesVersionOverride = Lens.field @"secondarySourcesVersionOverride"
{-# INLINEABLE sbbSecondarySourcesVersionOverride #-}
{-# DEPRECATED secondarySourcesVersionOverride "Use generic-lens or generic-optics with 'secondarySourcesVersionOverride' instead"  #-}

-- | The name of a service role for this batch build that overrides the one specified in the batch build project.
--
-- /Note:/ Consider using 'serviceRoleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbServiceRoleOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.ServiceRoleOverride)
sbbServiceRoleOverride = Lens.field @"serviceRoleOverride"
{-# INLINEABLE sbbServiceRoleOverride #-}
{-# DEPRECATED serviceRoleOverride "Use generic-lens or generic-optics with 'serviceRoleOverride' instead"  #-}

-- | A @SourceAuth@ object that overrides the one defined in the batch build project. This override applies only if the build project's source is BitBucket or GitHub.
--
-- /Note:/ Consider using 'sourceAuthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceAuthOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.SourceAuth)
sbbSourceAuthOverride = Lens.field @"sourceAuthOverride"
{-# INLINEABLE sbbSourceAuthOverride #-}
{-# DEPRECATED sourceAuthOverride "Use generic-lens or generic-optics with 'sourceAuthOverride' instead"  #-}

-- | A location that overrides, for this batch build, the source location defined in the batch build project.
--
-- /Note:/ Consider using 'sourceLocationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceLocationOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Text)
sbbSourceLocationOverride = Lens.field @"sourceLocationOverride"
{-# INLINEABLE sbbSourceLocationOverride #-}
{-# DEPRECATED sourceLocationOverride "Use generic-lens or generic-optics with 'sourceLocationOverride' instead"  #-}

-- | The source input type that overrides the source input defined in the batch build project.
--
-- /Note:/ Consider using 'sourceTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbbSourceTypeOverride :: Lens.Lens' StartBuildBatch (Core.Maybe Types.SourceType)
sbbSourceTypeOverride = Lens.field @"sourceTypeOverride"
{-# INLINEABLE sbbSourceTypeOverride #-}
{-# DEPRECATED sourceTypeOverride "Use generic-lens or generic-optics with 'sourceTypeOverride' instead"  #-}

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
sbbSourceVersion :: Lens.Lens' StartBuildBatch (Core.Maybe Core.Text)
sbbSourceVersion = Lens.field @"sourceVersion"
{-# INLINEABLE sbbSourceVersion #-}
{-# DEPRECATED sourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead"  #-}

instance Core.ToQuery StartBuildBatch where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartBuildBatch where
        toHeaders StartBuildBatch{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.StartBuildBatch")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartBuildBatch where
        toJSON StartBuildBatch{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectName" Core..= projectName),
                  ("artifactsOverride" Core..=) Core.<$> artifactsOverride,
                  ("buildBatchConfigOverride" Core..=) Core.<$>
                    buildBatchConfigOverride,
                  ("buildTimeoutInMinutesOverride" Core..=) Core.<$>
                    buildTimeoutInMinutesOverride,
                  ("buildspecOverride" Core..=) Core.<$> buildspecOverride,
                  ("cacheOverride" Core..=) Core.<$> cacheOverride,
                  ("certificateOverride" Core..=) Core.<$> certificateOverride,
                  ("computeTypeOverride" Core..=) Core.<$> computeTypeOverride,
                  ("encryptionKeyOverride" Core..=) Core.<$> encryptionKeyOverride,
                  ("environmentTypeOverride" Core..=) Core.<$>
                    environmentTypeOverride,
                  ("environmentVariablesOverride" Core..=) Core.<$>
                    environmentVariablesOverride,
                  ("gitCloneDepthOverride" Core..=) Core.<$> gitCloneDepthOverride,
                  ("gitSubmodulesConfigOverride" Core..=) Core.<$>
                    gitSubmodulesConfigOverride,
                  ("idempotencyToken" Core..=) Core.<$> idempotencyToken,
                  ("imageOverride" Core..=) Core.<$> imageOverride,
                  ("imagePullCredentialsTypeOverride" Core..=) Core.<$>
                    imagePullCredentialsTypeOverride,
                  ("insecureSslOverride" Core..=) Core.<$> insecureSslOverride,
                  ("logsConfigOverride" Core..=) Core.<$> logsConfigOverride,
                  ("privilegedModeOverride" Core..=) Core.<$> privilegedModeOverride,
                  ("queuedTimeoutInMinutesOverride" Core..=) Core.<$>
                    queuedTimeoutInMinutesOverride,
                  ("registryCredentialOverride" Core..=) Core.<$>
                    registryCredentialOverride,
                  ("reportBuildBatchStatusOverride" Core..=) Core.<$>
                    reportBuildBatchStatusOverride,
                  ("secondaryArtifactsOverride" Core..=) Core.<$>
                    secondaryArtifactsOverride,
                  ("secondarySourcesOverride" Core..=) Core.<$>
                    secondarySourcesOverride,
                  ("secondarySourcesVersionOverride" Core..=) Core.<$>
                    secondarySourcesVersionOverride,
                  ("serviceRoleOverride" Core..=) Core.<$> serviceRoleOverride,
                  ("sourceAuthOverride" Core..=) Core.<$> sourceAuthOverride,
                  ("sourceLocationOverride" Core..=) Core.<$> sourceLocationOverride,
                  ("sourceTypeOverride" Core..=) Core.<$> sourceTypeOverride,
                  ("sourceVersion" Core..=) Core.<$> sourceVersion])

instance Core.AWSRequest StartBuildBatch where
        type Rs StartBuildBatch = StartBuildBatchResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartBuildBatchResponse' Core.<$>
                   (x Core..:? "buildBatch") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartBuildBatchResponse' smart constructor.
data StartBuildBatchResponse = StartBuildBatchResponse'
  { buildBatch :: Core.Maybe Types.BuildBatch
    -- ^ A @BuildBatch@ object that contains information about the batch build.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartBuildBatchResponse' value with any optional fields omitted.
mkStartBuildBatchResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartBuildBatchResponse
mkStartBuildBatchResponse responseStatus
  = StartBuildBatchResponse'{buildBatch = Core.Nothing,
                             responseStatus}

-- | A @BuildBatch@ object that contains information about the batch build.
--
-- /Note:/ Consider using 'buildBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsBuildBatch :: Lens.Lens' StartBuildBatchResponse (Core.Maybe Types.BuildBatch)
srsBuildBatch = Lens.field @"buildBatch"
{-# INLINEABLE srsBuildBatch #-}
{-# DEPRECATED buildBatch "Use generic-lens or generic-optics with 'buildBatch' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartBuildBatchResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
