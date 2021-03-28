{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      StartBuild (..)
    , mkStartBuild
    -- ** Request lenses
    , sbProjectName
    , sbArtifactsOverride
    , sbBuildStatusConfigOverride
    , sbBuildspecOverride
    , sbCacheOverride
    , sbCertificateOverride
    , sbComputeTypeOverride
    , sbDebugSessionEnabled
    , sbEncryptionKeyOverride
    , sbEnvironmentTypeOverride
    , sbEnvironmentVariablesOverride
    , sbGitCloneDepthOverride
    , sbGitSubmodulesConfigOverride
    , sbIdempotencyToken
    , sbImageOverride
    , sbImagePullCredentialsTypeOverride
    , sbInsecureSslOverride
    , sbLogsConfigOverride
    , sbPrivilegedModeOverride
    , sbQueuedTimeoutInMinutesOverride
    , sbRegistryCredentialOverride
    , sbReportBuildStatusOverride
    , sbSecondaryArtifactsOverride
    , sbSecondarySourcesOverride
    , sbSecondarySourcesVersionOverride
    , sbServiceRoleOverride
    , sbSourceAuthOverride
    , sbSourceLocationOverride
    , sbSourceTypeOverride
    , sbSourceVersion
    , sbTimeoutInMinutesOverride

    -- * Destructuring the response
    , StartBuildResponse (..)
    , mkStartBuildResponse
    -- ** Response lenses
    , sbrrsBuild
    , sbrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartBuild' smart constructor.
data StartBuild = StartBuild'
  { projectName :: Types.NonEmptyString
    -- ^ The name of the AWS CodeBuild build project to start running a build.
  , artifactsOverride :: Core.Maybe Types.ProjectArtifacts
    -- ^ Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
  , buildStatusConfigOverride :: Core.Maybe Types.BuildStatusConfig
    -- ^ Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
  , buildspecOverride :: Core.Maybe Core.Text
    -- ^ A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> . 
  , cacheOverride :: Core.Maybe Types.ProjectCache
    -- ^ A ProjectCache object specified for this build that overrides the one defined in the build project.
  , certificateOverride :: Core.Maybe Core.Text
    -- ^ The name of a certificate for this build that overrides the one specified in the build project.
  , computeTypeOverride :: Core.Maybe Types.ComputeType
    -- ^ The name of a compute type for this build that overrides the one specified in the build project.
  , debugSessionEnabled :: Core.Maybe Core.Bool
    -- ^ Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
  , encryptionKeyOverride :: Core.Maybe Types.EncryptionKeyOverride
    -- ^ The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
  , environmentTypeOverride :: Core.Maybe Types.EnvironmentType
    -- ^ A container type for this build that overrides the one specified in the build project.
  , environmentVariablesOverride :: Core.Maybe [Types.EnvironmentVariable]
    -- ^ A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
  , gitCloneDepthOverride :: Core.Maybe Core.Natural
    -- ^ The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
  , gitSubmodulesConfigOverride :: Core.Maybe Types.GitSubmodulesConfig
    -- ^ Information about the Git submodules configuration for this build of an AWS CodeBuild build project. 
  , idempotencyToken :: Core.Maybe Core.Text
    -- ^ A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error. 
  , imageOverride :: Core.Maybe Types.ImageOverride
    -- ^ The name of an image for this build that overrides the one specified in the build project.
  , imagePullCredentialsTypeOverride :: Core.Maybe Types.ImagePullCredentialsType
    -- ^ The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values: 
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
    -- ^ Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
  , logsConfigOverride :: Core.Maybe Types.LogsConfig
    -- ^ Log settings for this build that override the log settings defined in the build project. 
  , privilegedModeOverride :: Core.Maybe Core.Bool
    -- ^ Enable this flag to override privileged mode in the build project.
  , queuedTimeoutInMinutesOverride :: Core.Maybe Core.Natural
    -- ^ The number of minutes a build is allowed to be queued before it times out. 
  , registryCredentialOverride :: Core.Maybe Types.RegistryCredential
    -- ^ The credentials for access to a private registry. 
  , reportBuildStatusOverride :: Core.Maybe Core.Bool
    -- ^ Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown. 
  , secondaryArtifactsOverride :: Core.Maybe [Types.ProjectArtifacts]
    -- ^ An array of @ProjectArtifacts@ objects. 
  , secondarySourcesOverride :: Core.Maybe [Types.ProjectSource]
    -- ^ An array of @ProjectSource@ objects. 
  , secondarySourcesVersionOverride :: Core.Maybe [Types.ProjectSourceVersion]
    -- ^ An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only. 
  , serviceRoleOverride :: Core.Maybe Types.ServiceRoleOverride
    -- ^ The name of a service role for this build that overrides the one specified in the build project.
  , sourceAuthOverride :: Core.Maybe Types.SourceAuth
    -- ^ An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
  , sourceLocationOverride :: Core.Maybe Core.Text
    -- ^ A location that overrides, for this build, the source location for the one defined in the build project.
  , sourceTypeOverride :: Core.Maybe Types.SourceType
    -- ^ A source input type, for this build, that overrides the source input defined in the build project.
  , sourceVersion :: Core.Maybe Core.Text
    -- ^ The version of the build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:
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
  , timeoutInMinutesOverride :: Core.Maybe Core.Natural
    -- ^ The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartBuild' value with any optional fields omitted.
mkStartBuild
    :: Types.NonEmptyString -- ^ 'projectName'
    -> StartBuild
mkStartBuild projectName
  = StartBuild'{projectName, artifactsOverride = Core.Nothing,
                buildStatusConfigOverride = Core.Nothing,
                buildspecOverride = Core.Nothing, cacheOverride = Core.Nothing,
                certificateOverride = Core.Nothing,
                computeTypeOverride = Core.Nothing,
                debugSessionEnabled = Core.Nothing,
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
                reportBuildStatusOverride = Core.Nothing,
                secondaryArtifactsOverride = Core.Nothing,
                secondarySourcesOverride = Core.Nothing,
                secondarySourcesVersionOverride = Core.Nothing,
                serviceRoleOverride = Core.Nothing,
                sourceAuthOverride = Core.Nothing,
                sourceLocationOverride = Core.Nothing,
                sourceTypeOverride = Core.Nothing, sourceVersion = Core.Nothing,
                timeoutInMinutesOverride = Core.Nothing}

-- | The name of the AWS CodeBuild build project to start running a build.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbProjectName :: Lens.Lens' StartBuild Types.NonEmptyString
sbProjectName = Lens.field @"projectName"
{-# INLINEABLE sbProjectName #-}
{-# DEPRECATED projectName "Use generic-lens or generic-optics with 'projectName' instead"  #-}

-- | Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
--
-- /Note:/ Consider using 'artifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbArtifactsOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ProjectArtifacts)
sbArtifactsOverride = Lens.field @"artifactsOverride"
{-# INLINEABLE sbArtifactsOverride #-}
{-# DEPRECATED artifactsOverride "Use generic-lens or generic-optics with 'artifactsOverride' instead"  #-}

-- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
--
-- /Note:/ Consider using 'buildStatusConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBuildStatusConfigOverride :: Lens.Lens' StartBuild (Core.Maybe Types.BuildStatusConfig)
sbBuildStatusConfigOverride = Lens.field @"buildStatusConfigOverride"
{-# INLINEABLE sbBuildStatusConfigOverride #-}
{-# DEPRECATED buildStatusConfigOverride "Use generic-lens or generic-optics with 'buildStatusConfigOverride' instead"  #-}

-- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> . 
--
-- /Note:/ Consider using 'buildspecOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbBuildspecOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
sbBuildspecOverride = Lens.field @"buildspecOverride"
{-# INLINEABLE sbBuildspecOverride #-}
{-# DEPRECATED buildspecOverride "Use generic-lens or generic-optics with 'buildspecOverride' instead"  #-}

-- | A ProjectCache object specified for this build that overrides the one defined in the build project.
--
-- /Note:/ Consider using 'cacheOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbCacheOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ProjectCache)
sbCacheOverride = Lens.field @"cacheOverride"
{-# INLINEABLE sbCacheOverride #-}
{-# DEPRECATED cacheOverride "Use generic-lens or generic-optics with 'cacheOverride' instead"  #-}

-- | The name of a certificate for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'certificateOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbCertificateOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
sbCertificateOverride = Lens.field @"certificateOverride"
{-# INLINEABLE sbCertificateOverride #-}
{-# DEPRECATED certificateOverride "Use generic-lens or generic-optics with 'certificateOverride' instead"  #-}

-- | The name of a compute type for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'computeTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbComputeTypeOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ComputeType)
sbComputeTypeOverride = Lens.field @"computeTypeOverride"
{-# INLINEABLE sbComputeTypeOverride #-}
{-# DEPRECATED computeTypeOverride "Use generic-lens or generic-optics with 'computeTypeOverride' instead"  #-}

-- | Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
--
-- /Note:/ Consider using 'debugSessionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbDebugSessionEnabled :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbDebugSessionEnabled = Lens.field @"debugSessionEnabled"
{-# INLINEABLE sbDebugSessionEnabled #-}
{-# DEPRECATED debugSessionEnabled "Use generic-lens or generic-optics with 'debugSessionEnabled' instead"  #-}

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- /Note:/ Consider using 'encryptionKeyOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEncryptionKeyOverride :: Lens.Lens' StartBuild (Core.Maybe Types.EncryptionKeyOverride)
sbEncryptionKeyOverride = Lens.field @"encryptionKeyOverride"
{-# INLINEABLE sbEncryptionKeyOverride #-}
{-# DEPRECATED encryptionKeyOverride "Use generic-lens or generic-optics with 'encryptionKeyOverride' instead"  #-}

-- | A container type for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'environmentTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEnvironmentTypeOverride :: Lens.Lens' StartBuild (Core.Maybe Types.EnvironmentType)
sbEnvironmentTypeOverride = Lens.field @"environmentTypeOverride"
{-# INLINEABLE sbEnvironmentTypeOverride #-}
{-# DEPRECATED environmentTypeOverride "Use generic-lens or generic-optics with 'environmentTypeOverride' instead"  #-}

-- | A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
--
-- /Note:/ Consider using 'environmentVariablesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbEnvironmentVariablesOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.EnvironmentVariable])
sbEnvironmentVariablesOverride = Lens.field @"environmentVariablesOverride"
{-# INLINEABLE sbEnvironmentVariablesOverride #-}
{-# DEPRECATED environmentVariablesOverride "Use generic-lens or generic-optics with 'environmentVariablesOverride' instead"  #-}

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
--
-- /Note:/ Consider using 'gitCloneDepthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbGitCloneDepthOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
sbGitCloneDepthOverride = Lens.field @"gitCloneDepthOverride"
{-# INLINEABLE sbGitCloneDepthOverride #-}
{-# DEPRECATED gitCloneDepthOverride "Use generic-lens or generic-optics with 'gitCloneDepthOverride' instead"  #-}

-- | Information about the Git submodules configuration for this build of an AWS CodeBuild build project. 
--
-- /Note:/ Consider using 'gitSubmodulesConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbGitSubmodulesConfigOverride :: Lens.Lens' StartBuild (Core.Maybe Types.GitSubmodulesConfig)
sbGitSubmodulesConfigOverride = Lens.field @"gitSubmodulesConfigOverride"
{-# INLINEABLE sbGitSubmodulesConfigOverride #-}
{-# DEPRECATED gitSubmodulesConfigOverride "Use generic-lens or generic-optics with 'gitSubmodulesConfigOverride' instead"  #-}

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error. 
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbIdempotencyToken :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
sbIdempotencyToken = Lens.field @"idempotencyToken"
{-# INLINEABLE sbIdempotencyToken #-}
{-# DEPRECATED idempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead"  #-}

-- | The name of an image for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'imageOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbImageOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ImageOverride)
sbImageOverride = Lens.field @"imageOverride"
{-# INLINEABLE sbImageOverride #-}
{-# DEPRECATED imageOverride "Use generic-lens or generic-optics with 'imageOverride' instead"  #-}

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
{-# INLINEABLE sbImagePullCredentialsTypeOverride #-}
{-# DEPRECATED imagePullCredentialsTypeOverride "Use generic-lens or generic-optics with 'imagePullCredentialsTypeOverride' instead"  #-}

-- | Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
--
-- /Note:/ Consider using 'insecureSslOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbInsecureSslOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbInsecureSslOverride = Lens.field @"insecureSslOverride"
{-# INLINEABLE sbInsecureSslOverride #-}
{-# DEPRECATED insecureSslOverride "Use generic-lens or generic-optics with 'insecureSslOverride' instead"  #-}

-- | Log settings for this build that override the log settings defined in the build project. 
--
-- /Note:/ Consider using 'logsConfigOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbLogsConfigOverride :: Lens.Lens' StartBuild (Core.Maybe Types.LogsConfig)
sbLogsConfigOverride = Lens.field @"logsConfigOverride"
{-# INLINEABLE sbLogsConfigOverride #-}
{-# DEPRECATED logsConfigOverride "Use generic-lens or generic-optics with 'logsConfigOverride' instead"  #-}

-- | Enable this flag to override privileged mode in the build project.
--
-- /Note:/ Consider using 'privilegedModeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbPrivilegedModeOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbPrivilegedModeOverride = Lens.field @"privilegedModeOverride"
{-# INLINEABLE sbPrivilegedModeOverride #-}
{-# DEPRECATED privilegedModeOverride "Use generic-lens or generic-optics with 'privilegedModeOverride' instead"  #-}

-- | The number of minutes a build is allowed to be queued before it times out. 
--
-- /Note:/ Consider using 'queuedTimeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbQueuedTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
sbQueuedTimeoutInMinutesOverride = Lens.field @"queuedTimeoutInMinutesOverride"
{-# INLINEABLE sbQueuedTimeoutInMinutesOverride #-}
{-# DEPRECATED queuedTimeoutInMinutesOverride "Use generic-lens or generic-optics with 'queuedTimeoutInMinutesOverride' instead"  #-}

-- | The credentials for access to a private registry. 
--
-- /Note:/ Consider using 'registryCredentialOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbRegistryCredentialOverride :: Lens.Lens' StartBuild (Core.Maybe Types.RegistryCredential)
sbRegistryCredentialOverride = Lens.field @"registryCredentialOverride"
{-# INLINEABLE sbRegistryCredentialOverride #-}
{-# DEPRECATED registryCredentialOverride "Use generic-lens or generic-optics with 'registryCredentialOverride' instead"  #-}

-- | Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown. 
--
-- /Note:/ Consider using 'reportBuildStatusOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbReportBuildStatusOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
sbReportBuildStatusOverride = Lens.field @"reportBuildStatusOverride"
{-# INLINEABLE sbReportBuildStatusOverride #-}
{-# DEPRECATED reportBuildStatusOverride "Use generic-lens or generic-optics with 'reportBuildStatusOverride' instead"  #-}

-- | An array of @ProjectArtifacts@ objects. 
--
-- /Note:/ Consider using 'secondaryArtifactsOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondaryArtifactsOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.ProjectArtifacts])
sbSecondaryArtifactsOverride = Lens.field @"secondaryArtifactsOverride"
{-# INLINEABLE sbSecondaryArtifactsOverride #-}
{-# DEPRECATED secondaryArtifactsOverride "Use generic-lens or generic-optics with 'secondaryArtifactsOverride' instead"  #-}

-- | An array of @ProjectSource@ objects. 
--
-- /Note:/ Consider using 'secondarySourcesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondarySourcesOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.ProjectSource])
sbSecondarySourcesOverride = Lens.field @"secondarySourcesOverride"
{-# INLINEABLE sbSecondarySourcesOverride #-}
{-# DEPRECATED secondarySourcesOverride "Use generic-lens or generic-optics with 'secondarySourcesOverride' instead"  #-}

-- | An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only. 
--
-- /Note:/ Consider using 'secondarySourcesVersionOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSecondarySourcesVersionOverride :: Lens.Lens' StartBuild (Core.Maybe [Types.ProjectSourceVersion])
sbSecondarySourcesVersionOverride = Lens.field @"secondarySourcesVersionOverride"
{-# INLINEABLE sbSecondarySourcesVersionOverride #-}
{-# DEPRECATED secondarySourcesVersionOverride "Use generic-lens or generic-optics with 'secondarySourcesVersionOverride' instead"  #-}

-- | The name of a service role for this build that overrides the one specified in the build project.
--
-- /Note:/ Consider using 'serviceRoleOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbServiceRoleOverride :: Lens.Lens' StartBuild (Core.Maybe Types.ServiceRoleOverride)
sbServiceRoleOverride = Lens.field @"serviceRoleOverride"
{-# INLINEABLE sbServiceRoleOverride #-}
{-# DEPRECATED serviceRoleOverride "Use generic-lens or generic-optics with 'serviceRoleOverride' instead"  #-}

-- | An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
--
-- /Note:/ Consider using 'sourceAuthOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceAuthOverride :: Lens.Lens' StartBuild (Core.Maybe Types.SourceAuth)
sbSourceAuthOverride = Lens.field @"sourceAuthOverride"
{-# INLINEABLE sbSourceAuthOverride #-}
{-# DEPRECATED sourceAuthOverride "Use generic-lens or generic-optics with 'sourceAuthOverride' instead"  #-}

-- | A location that overrides, for this build, the source location for the one defined in the build project.
--
-- /Note:/ Consider using 'sourceLocationOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceLocationOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
sbSourceLocationOverride = Lens.field @"sourceLocationOverride"
{-# INLINEABLE sbSourceLocationOverride #-}
{-# DEPRECATED sourceLocationOverride "Use generic-lens or generic-optics with 'sourceLocationOverride' instead"  #-}

-- | A source input type, for this build, that overrides the source input defined in the build project.
--
-- /Note:/ Consider using 'sourceTypeOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbSourceTypeOverride :: Lens.Lens' StartBuild (Core.Maybe Types.SourceType)
sbSourceTypeOverride = Lens.field @"sourceTypeOverride"
{-# INLINEABLE sbSourceTypeOverride #-}
{-# DEPRECATED sourceTypeOverride "Use generic-lens or generic-optics with 'sourceTypeOverride' instead"  #-}

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
sbSourceVersion :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
sbSourceVersion = Lens.field @"sourceVersion"
{-# INLINEABLE sbSourceVersion #-}
{-# DEPRECATED sourceVersion "Use generic-lens or generic-optics with 'sourceVersion' instead"  #-}

-- | The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
--
-- /Note:/ Consider using 'timeoutInMinutesOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
sbTimeoutInMinutesOverride = Lens.field @"timeoutInMinutesOverride"
{-# INLINEABLE sbTimeoutInMinutesOverride #-}
{-# DEPRECATED timeoutInMinutesOverride "Use generic-lens or generic-optics with 'timeoutInMinutesOverride' instead"  #-}

instance Core.ToQuery StartBuild where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartBuild where
        toHeaders StartBuild{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.StartBuild")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartBuild where
        toJSON StartBuild{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("projectName" Core..= projectName),
                  ("artifactsOverride" Core..=) Core.<$> artifactsOverride,
                  ("buildStatusConfigOverride" Core..=) Core.<$>
                    buildStatusConfigOverride,
                  ("buildspecOverride" Core..=) Core.<$> buildspecOverride,
                  ("cacheOverride" Core..=) Core.<$> cacheOverride,
                  ("certificateOverride" Core..=) Core.<$> certificateOverride,
                  ("computeTypeOverride" Core..=) Core.<$> computeTypeOverride,
                  ("debugSessionEnabled" Core..=) Core.<$> debugSessionEnabled,
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
                  ("reportBuildStatusOverride" Core..=) Core.<$>
                    reportBuildStatusOverride,
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
                  ("sourceVersion" Core..=) Core.<$> sourceVersion,
                  ("timeoutInMinutesOverride" Core..=) Core.<$>
                    timeoutInMinutesOverride])

instance Core.AWSRequest StartBuild where
        type Rs StartBuild = StartBuildResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 StartBuildResponse' Core.<$>
                   (x Core..:? "build") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartBuildResponse' smart constructor.
data StartBuildResponse = StartBuildResponse'
  { build :: Core.Maybe Types.Build
    -- ^ Information about the build to be run.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StartBuildResponse' value with any optional fields omitted.
mkStartBuildResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartBuildResponse
mkStartBuildResponse responseStatus
  = StartBuildResponse'{build = Core.Nothing, responseStatus}

-- | Information about the build to be run.
--
-- /Note:/ Consider using 'build' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrrsBuild :: Lens.Lens' StartBuildResponse (Core.Maybe Types.Build)
sbrrsBuild = Lens.field @"build"
{-# INLINEABLE sbrrsBuild #-}
{-# DEPRECATED build "Use generic-lens or generic-optics with 'build' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbrrsResponseStatus :: Lens.Lens' StartBuildResponse Core.Int
sbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
