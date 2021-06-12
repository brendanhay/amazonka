{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.StartBuild
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts running a build.
module Network.AWS.CodeBuild.StartBuild
  ( -- * Creating a Request
    StartBuild (..),
    newStartBuild,

    -- * Request Lenses
    startBuild_buildspecOverride,
    startBuild_sourceVersion,
    startBuild_environmentVariablesOverride,
    startBuild_idempotencyToken,
    startBuild_sourceLocationOverride,
    startBuild_logsConfigOverride,
    startBuild_artifactsOverride,
    startBuild_sourceAuthOverride,
    startBuild_imageOverride,
    startBuild_queuedTimeoutInMinutesOverride,
    startBuild_insecureSslOverride,
    startBuild_secondarySourcesOverride,
    startBuild_reportBuildStatusOverride,
    startBuild_serviceRoleOverride,
    startBuild_registryCredentialOverride,
    startBuild_secondarySourcesVersionOverride,
    startBuild_buildStatusConfigOverride,
    startBuild_encryptionKeyOverride,
    startBuild_privilegedModeOverride,
    startBuild_gitSubmodulesConfigOverride,
    startBuild_computeTypeOverride,
    startBuild_certificateOverride,
    startBuild_sourceTypeOverride,
    startBuild_environmentTypeOverride,
    startBuild_imagePullCredentialsTypeOverride,
    startBuild_secondaryArtifactsOverride,
    startBuild_gitCloneDepthOverride,
    startBuild_cacheOverride,
    startBuild_timeoutInMinutesOverride,
    startBuild_debugSessionEnabled,
    startBuild_projectName,

    -- * Destructuring the Response
    StartBuildResponse (..),
    newStartBuildResponse,

    -- * Response Lenses
    startBuildResponse_build,
    startBuildResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartBuild' smart constructor.
data StartBuild = StartBuild'
  { -- | A buildspec file declaration that overrides, for this build only, the
    -- latest one already defined in the build project.
    --
    -- If this value is set, it can be either an inline buildspec definition,
    -- the path to an alternate buildspec file relative to the value of the
    -- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
    -- bucket. The bucket must be in the same AWS Region as the build project.
    -- Specify the buildspec file using its ARN (for example,
    -- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
    -- not provided or is set to an empty string, the source code must contain
    -- a buildspec file in its root directory. For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
    buildspecOverride :: Core.Maybe Core.Text,
    -- | The version of the build input to be built, for this build only. If not
    -- specified, the latest version is used. If specified, the contents
    -- depends on the source provider:
    --
    -- [AWS CodeCommit]
    --     The commit ID, branch, or Git tag to use.
    --
    -- [GitHub]
    --     The commit ID, pull request ID, branch name, or tag name that
    --     corresponds to the version of the source code you want to build. If
    --     a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- [Bitbucket]
    --     The commit ID, branch name, or tag name that corresponds to the
    --     version of the source code you want to build. If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- [Amazon S3]
    --     The version ID of the object that represents the build input ZIP
    --     file to use.
    --
    -- If @sourceVersion@ is specified at the project level, then this
    -- @sourceVersion@ (at the build level) takes precedence.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /AWS CodeBuild User Guide/.
    sourceVersion :: Core.Maybe Core.Text,
    -- | A set of environment variables that overrides, for this build only, the
    -- latest ones already defined in the build project.
    environmentVariablesOverride :: Core.Maybe [EnvironmentVariable],
    -- | A unique, case sensitive identifier you provide to ensure the
    -- idempotency of the StartBuild request. The token is included in the
    -- StartBuild request and is valid for 5 minutes. If you repeat the
    -- StartBuild request with the same token, but change a parameter, AWS
    -- CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Core.Maybe Core.Text,
    -- | A location that overrides, for this build, the source location for the
    -- one defined in the build project.
    sourceLocationOverride :: Core.Maybe Core.Text,
    -- | Log settings for this build that override the log settings defined in
    -- the build project.
    logsConfigOverride :: Core.Maybe LogsConfig,
    -- | Build output artifact settings that override, for this build only, the
    -- latest ones already defined in the build project.
    artifactsOverride :: Core.Maybe ProjectArtifacts,
    -- | An authorization type for this build that overrides the one defined in
    -- the build project. This override applies only if the build project\'s
    -- source is BitBucket or GitHub.
    sourceAuthOverride :: Core.Maybe SourceAuth,
    -- | The name of an image for this build that overrides the one specified in
    -- the build project.
    imageOverride :: Core.Maybe Core.Text,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutesOverride :: Core.Maybe Core.Natural,
    -- | Enable this flag to override the insecure SSL setting that is specified
    -- in the build project. The insecure SSL setting determines whether to
    -- ignore SSL warnings while connecting to the project source code. This
    -- override applies only if the build\'s source is GitHub Enterprise.
    insecureSslOverride :: Core.Maybe Core.Bool,
    -- | An array of @ProjectSource@ objects.
    secondarySourcesOverride :: Core.Maybe [ProjectSource],
    -- | Set to true to report to your source provider the status of a build\'s
    -- start and completion. If you use this option with a source provider
    -- other than GitHub, GitHub Enterprise, or Bitbucket, an
    -- @invalidInputException@ is thrown.
    --
    -- To be able to report the build status to the source provider, the user
    -- associated with the source provider must have write access to the repo.
    -- If the user does not have write access, the build status cannot be
    -- updated. For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/access-tokens.html Source provider access>
    -- in the /AWS CodeBuild User Guide/.
    --
    -- The status of a build triggered by a webhook is always reported to your
    -- source provider.
    reportBuildStatusOverride :: Core.Maybe Core.Bool,
    -- | The name of a service role for this build that overrides the one
    -- specified in the build project.
    serviceRoleOverride :: Core.Maybe Core.Text,
    -- | The credentials for access to a private registry.
    registryCredentialOverride :: Core.Maybe RegistryCredential,
    -- | An array of @ProjectSourceVersion@ objects that specify one or more
    -- versions of the project\'s secondary sources to be used for this build
    -- only.
    secondarySourcesVersionOverride :: Core.Maybe [ProjectSourceVersion],
    -- | Contains information that defines how the build project reports the
    -- build status to the source provider. This option is only used when the
    -- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
    buildStatusConfigOverride :: Core.Maybe BuildStatusConfig,
    -- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that
    -- overrides the one specified in the build project. The CMK key encrypts
    -- the build output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKeyOverride :: Core.Maybe Core.Text,
    -- | Enable this flag to override privileged mode in the build project.
    privilegedModeOverride :: Core.Maybe Core.Bool,
    -- | Information about the Git submodules configuration for this build of an
    -- AWS CodeBuild build project.
    gitSubmodulesConfigOverride :: Core.Maybe GitSubmodulesConfig,
    -- | The name of a compute type for this build that overrides the one
    -- specified in the build project.
    computeTypeOverride :: Core.Maybe ComputeType,
    -- | The name of a certificate for this build that overrides the one
    -- specified in the build project.
    certificateOverride :: Core.Maybe Core.Text,
    -- | A source input type, for this build, that overrides the source input
    -- defined in the build project.
    sourceTypeOverride :: Core.Maybe SourceType,
    -- | A container type for this build that overrides the one specified in the
    -- build project.
    environmentTypeOverride :: Core.Maybe EnvironmentType,
    -- | The type of credentials AWS CodeBuild uses to pull images in your build.
    -- There are two valid values:
    --
    -- [CODEBUILD]
    --     Specifies that AWS CodeBuild uses its own credentials. This requires
    --     that you modify your ECR repository policy to trust AWS CodeBuild\'s
    --     service principal.
    --
    -- [SERVICE_ROLE]
    --     Specifies that AWS CodeBuild uses your build project\'s service
    --     role.
    --
    -- When using a cross-account or private registry image, you must use
    -- @SERVICE_ROLE@ credentials. When using an AWS CodeBuild curated image,
    -- you must use @CODEBUILD@ credentials.
    imagePullCredentialsTypeOverride :: Core.Maybe ImagePullCredentialsType,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifactsOverride :: Core.Maybe [ProjectArtifacts],
    -- | The user-defined depth of history, with a minimum value of 0, that
    -- overrides, for this build only, any previous depth of history defined in
    -- the build project.
    gitCloneDepthOverride :: Core.Maybe Core.Natural,
    -- | A ProjectCache object specified for this build that overrides the one
    -- defined in the build project.
    cacheOverride :: Core.Maybe ProjectCache,
    -- | The number of build timeout minutes, from 5 to 480 (8 hours), that
    -- overrides, for this build only, the latest setting already defined in
    -- the build project.
    timeoutInMinutesOverride :: Core.Maybe Core.Natural,
    -- | Specifies if session debugging is enabled for this build. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
    debugSessionEnabled :: Core.Maybe Core.Bool,
    -- | The name of the AWS CodeBuild build project to start running a build.
    projectName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buildspecOverride', 'startBuild_buildspecOverride' - A buildspec file declaration that overrides, for this build only, the
-- latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition,
-- the path to an alternate buildspec file relative to the value of the
-- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
-- bucket. The bucket must be in the same AWS Region as the build project.
-- Specify the buildspec file using its ARN (for example,
-- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
-- not provided or is set to an empty string, the source code must contain
-- a buildspec file in its root directory. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
--
-- 'sourceVersion', 'startBuild_sourceVersion' - The version of the build input to be built, for this build only. If not
-- specified, the latest version is used. If specified, the contents
-- depends on the source provider:
--
-- [AWS CodeCommit]
--     The commit ID, branch, or Git tag to use.
--
-- [GitHub]
--     The commit ID, pull request ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Bitbucket]
--     The commit ID, branch name, or tag name that corresponds to the
--     version of the source code you want to build. If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Amazon S3]
--     The version ID of the object that represents the build input ZIP
--     file to use.
--
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /AWS CodeBuild User Guide/.
--
-- 'environmentVariablesOverride', 'startBuild_environmentVariablesOverride' - A set of environment variables that overrides, for this build only, the
-- latest ones already defined in the build project.
--
-- 'idempotencyToken', 'startBuild_idempotencyToken' - A unique, case sensitive identifier you provide to ensure the
-- idempotency of the StartBuild request. The token is included in the
-- StartBuild request and is valid for 5 minutes. If you repeat the
-- StartBuild request with the same token, but change a parameter, AWS
-- CodeBuild returns a parameter mismatch error.
--
-- 'sourceLocationOverride', 'startBuild_sourceLocationOverride' - A location that overrides, for this build, the source location for the
-- one defined in the build project.
--
-- 'logsConfigOverride', 'startBuild_logsConfigOverride' - Log settings for this build that override the log settings defined in
-- the build project.
--
-- 'artifactsOverride', 'startBuild_artifactsOverride' - Build output artifact settings that override, for this build only, the
-- latest ones already defined in the build project.
--
-- 'sourceAuthOverride', 'startBuild_sourceAuthOverride' - An authorization type for this build that overrides the one defined in
-- the build project. This override applies only if the build project\'s
-- source is BitBucket or GitHub.
--
-- 'imageOverride', 'startBuild_imageOverride' - The name of an image for this build that overrides the one specified in
-- the build project.
--
-- 'queuedTimeoutInMinutesOverride', 'startBuild_queuedTimeoutInMinutesOverride' - The number of minutes a build is allowed to be queued before it times
-- out.
--
-- 'insecureSslOverride', 'startBuild_insecureSslOverride' - Enable this flag to override the insecure SSL setting that is specified
-- in the build project. The insecure SSL setting determines whether to
-- ignore SSL warnings while connecting to the project source code. This
-- override applies only if the build\'s source is GitHub Enterprise.
--
-- 'secondarySourcesOverride', 'startBuild_secondarySourcesOverride' - An array of @ProjectSource@ objects.
--
-- 'reportBuildStatusOverride', 'startBuild_reportBuildStatusOverride' - Set to true to report to your source provider the status of a build\'s
-- start and completion. If you use this option with a source provider
-- other than GitHub, GitHub Enterprise, or Bitbucket, an
-- @invalidInputException@ is thrown.
--
-- To be able to report the build status to the source provider, the user
-- associated with the source provider must have write access to the repo.
-- If the user does not have write access, the build status cannot be
-- updated. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/access-tokens.html Source provider access>
-- in the /AWS CodeBuild User Guide/.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
--
-- 'serviceRoleOverride', 'startBuild_serviceRoleOverride' - The name of a service role for this build that overrides the one
-- specified in the build project.
--
-- 'registryCredentialOverride', 'startBuild_registryCredentialOverride' - The credentials for access to a private registry.
--
-- 'secondarySourcesVersionOverride', 'startBuild_secondarySourcesVersionOverride' - An array of @ProjectSourceVersion@ objects that specify one or more
-- versions of the project\'s secondary sources to be used for this build
-- only.
--
-- 'buildStatusConfigOverride', 'startBuild_buildStatusConfigOverride' - Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
--
-- 'encryptionKeyOverride', 'startBuild_encryptionKeyOverride' - The AWS Key Management Service (AWS KMS) customer master key (CMK) that
-- overrides the one specified in the build project. The CMK key encrypts
-- the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
--
-- 'privilegedModeOverride', 'startBuild_privilegedModeOverride' - Enable this flag to override privileged mode in the build project.
--
-- 'gitSubmodulesConfigOverride', 'startBuild_gitSubmodulesConfigOverride' - Information about the Git submodules configuration for this build of an
-- AWS CodeBuild build project.
--
-- 'computeTypeOverride', 'startBuild_computeTypeOverride' - The name of a compute type for this build that overrides the one
-- specified in the build project.
--
-- 'certificateOverride', 'startBuild_certificateOverride' - The name of a certificate for this build that overrides the one
-- specified in the build project.
--
-- 'sourceTypeOverride', 'startBuild_sourceTypeOverride' - A source input type, for this build, that overrides the source input
-- defined in the build project.
--
-- 'environmentTypeOverride', 'startBuild_environmentTypeOverride' - A container type for this build that overrides the one specified in the
-- build project.
--
-- 'imagePullCredentialsTypeOverride', 'startBuild_imagePullCredentialsTypeOverride' - The type of credentials AWS CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- [CODEBUILD]
--     Specifies that AWS CodeBuild uses its own credentials. This requires
--     that you modify your ECR repository policy to trust AWS CodeBuild\'s
--     service principal.
--
-- [SERVICE_ROLE]
--     Specifies that AWS CodeBuild uses your build project\'s service
--     role.
--
-- When using a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When using an AWS CodeBuild curated image,
-- you must use @CODEBUILD@ credentials.
--
-- 'secondaryArtifactsOverride', 'startBuild_secondaryArtifactsOverride' - An array of @ProjectArtifacts@ objects.
--
-- 'gitCloneDepthOverride', 'startBuild_gitCloneDepthOverride' - The user-defined depth of history, with a minimum value of 0, that
-- overrides, for this build only, any previous depth of history defined in
-- the build project.
--
-- 'cacheOverride', 'startBuild_cacheOverride' - A ProjectCache object specified for this build that overrides the one
-- defined in the build project.
--
-- 'timeoutInMinutesOverride', 'startBuild_timeoutInMinutesOverride' - The number of build timeout minutes, from 5 to 480 (8 hours), that
-- overrides, for this build only, the latest setting already defined in
-- the build project.
--
-- 'debugSessionEnabled', 'startBuild_debugSessionEnabled' - Specifies if session debugging is enabled for this build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
--
-- 'projectName', 'startBuild_projectName' - The name of the AWS CodeBuild build project to start running a build.
newStartBuild ::
  -- | 'projectName'
  Core.Text ->
  StartBuild
newStartBuild pProjectName_ =
  StartBuild'
    { buildspecOverride = Core.Nothing,
      sourceVersion = Core.Nothing,
      environmentVariablesOverride = Core.Nothing,
      idempotencyToken = Core.Nothing,
      sourceLocationOverride = Core.Nothing,
      logsConfigOverride = Core.Nothing,
      artifactsOverride = Core.Nothing,
      sourceAuthOverride = Core.Nothing,
      imageOverride = Core.Nothing,
      queuedTimeoutInMinutesOverride = Core.Nothing,
      insecureSslOverride = Core.Nothing,
      secondarySourcesOverride = Core.Nothing,
      reportBuildStatusOverride = Core.Nothing,
      serviceRoleOverride = Core.Nothing,
      registryCredentialOverride = Core.Nothing,
      secondarySourcesVersionOverride = Core.Nothing,
      buildStatusConfigOverride = Core.Nothing,
      encryptionKeyOverride = Core.Nothing,
      privilegedModeOverride = Core.Nothing,
      gitSubmodulesConfigOverride = Core.Nothing,
      computeTypeOverride = Core.Nothing,
      certificateOverride = Core.Nothing,
      sourceTypeOverride = Core.Nothing,
      environmentTypeOverride = Core.Nothing,
      imagePullCredentialsTypeOverride = Core.Nothing,
      secondaryArtifactsOverride = Core.Nothing,
      gitCloneDepthOverride = Core.Nothing,
      cacheOverride = Core.Nothing,
      timeoutInMinutesOverride = Core.Nothing,
      debugSessionEnabled = Core.Nothing,
      projectName = pProjectName_
    }

-- | A buildspec file declaration that overrides, for this build only, the
-- latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition,
-- the path to an alternate buildspec file relative to the value of the
-- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
-- bucket. The bucket must be in the same AWS Region as the build project.
-- Specify the buildspec file using its ARN (for example,
-- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
-- not provided or is set to an empty string, the source code must contain
-- a buildspec file in its root directory. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
startBuild_buildspecOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_buildspecOverride = Lens.lens (\StartBuild' {buildspecOverride} -> buildspecOverride) (\s@StartBuild' {} a -> s {buildspecOverride = a} :: StartBuild)

-- | The version of the build input to be built, for this build only. If not
-- specified, the latest version is used. If specified, the contents
-- depends on the source provider:
--
-- [AWS CodeCommit]
--     The commit ID, branch, or Git tag to use.
--
-- [GitHub]
--     The commit ID, pull request ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Bitbucket]
--     The commit ID, branch name, or tag name that corresponds to the
--     version of the source code you want to build. If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- [Amazon S3]
--     The version ID of the object that represents the build input ZIP
--     file to use.
--
-- If @sourceVersion@ is specified at the project level, then this
-- @sourceVersion@ (at the build level) takes precedence.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /AWS CodeBuild User Guide/.
startBuild_sourceVersion :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_sourceVersion = Lens.lens (\StartBuild' {sourceVersion} -> sourceVersion) (\s@StartBuild' {} a -> s {sourceVersion = a} :: StartBuild)

-- | A set of environment variables that overrides, for this build only, the
-- latest ones already defined in the build project.
startBuild_environmentVariablesOverride :: Lens.Lens' StartBuild (Core.Maybe [EnvironmentVariable])
startBuild_environmentVariablesOverride = Lens.lens (\StartBuild' {environmentVariablesOverride} -> environmentVariablesOverride) (\s@StartBuild' {} a -> s {environmentVariablesOverride = a} :: StartBuild) Core.. Lens.mapping Lens._Coerce

-- | A unique, case sensitive identifier you provide to ensure the
-- idempotency of the StartBuild request. The token is included in the
-- StartBuild request and is valid for 5 minutes. If you repeat the
-- StartBuild request with the same token, but change a parameter, AWS
-- CodeBuild returns a parameter mismatch error.
startBuild_idempotencyToken :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_idempotencyToken = Lens.lens (\StartBuild' {idempotencyToken} -> idempotencyToken) (\s@StartBuild' {} a -> s {idempotencyToken = a} :: StartBuild)

-- | A location that overrides, for this build, the source location for the
-- one defined in the build project.
startBuild_sourceLocationOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_sourceLocationOverride = Lens.lens (\StartBuild' {sourceLocationOverride} -> sourceLocationOverride) (\s@StartBuild' {} a -> s {sourceLocationOverride = a} :: StartBuild)

-- | Log settings for this build that override the log settings defined in
-- the build project.
startBuild_logsConfigOverride :: Lens.Lens' StartBuild (Core.Maybe LogsConfig)
startBuild_logsConfigOverride = Lens.lens (\StartBuild' {logsConfigOverride} -> logsConfigOverride) (\s@StartBuild' {} a -> s {logsConfigOverride = a} :: StartBuild)

-- | Build output artifact settings that override, for this build only, the
-- latest ones already defined in the build project.
startBuild_artifactsOverride :: Lens.Lens' StartBuild (Core.Maybe ProjectArtifacts)
startBuild_artifactsOverride = Lens.lens (\StartBuild' {artifactsOverride} -> artifactsOverride) (\s@StartBuild' {} a -> s {artifactsOverride = a} :: StartBuild)

-- | An authorization type for this build that overrides the one defined in
-- the build project. This override applies only if the build project\'s
-- source is BitBucket or GitHub.
startBuild_sourceAuthOverride :: Lens.Lens' StartBuild (Core.Maybe SourceAuth)
startBuild_sourceAuthOverride = Lens.lens (\StartBuild' {sourceAuthOverride} -> sourceAuthOverride) (\s@StartBuild' {} a -> s {sourceAuthOverride = a} :: StartBuild)

-- | The name of an image for this build that overrides the one specified in
-- the build project.
startBuild_imageOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_imageOverride = Lens.lens (\StartBuild' {imageOverride} -> imageOverride) (\s@StartBuild' {} a -> s {imageOverride = a} :: StartBuild)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
startBuild_queuedTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
startBuild_queuedTimeoutInMinutesOverride = Lens.lens (\StartBuild' {queuedTimeoutInMinutesOverride} -> queuedTimeoutInMinutesOverride) (\s@StartBuild' {} a -> s {queuedTimeoutInMinutesOverride = a} :: StartBuild)

-- | Enable this flag to override the insecure SSL setting that is specified
-- in the build project. The insecure SSL setting determines whether to
-- ignore SSL warnings while connecting to the project source code. This
-- override applies only if the build\'s source is GitHub Enterprise.
startBuild_insecureSslOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
startBuild_insecureSslOverride = Lens.lens (\StartBuild' {insecureSslOverride} -> insecureSslOverride) (\s@StartBuild' {} a -> s {insecureSslOverride = a} :: StartBuild)

-- | An array of @ProjectSource@ objects.
startBuild_secondarySourcesOverride :: Lens.Lens' StartBuild (Core.Maybe [ProjectSource])
startBuild_secondarySourcesOverride = Lens.lens (\StartBuild' {secondarySourcesOverride} -> secondarySourcesOverride) (\s@StartBuild' {} a -> s {secondarySourcesOverride = a} :: StartBuild) Core.. Lens.mapping Lens._Coerce

-- | Set to true to report to your source provider the status of a build\'s
-- start and completion. If you use this option with a source provider
-- other than GitHub, GitHub Enterprise, or Bitbucket, an
-- @invalidInputException@ is thrown.
--
-- To be able to report the build status to the source provider, the user
-- associated with the source provider must have write access to the repo.
-- If the user does not have write access, the build status cannot be
-- updated. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/access-tokens.html Source provider access>
-- in the /AWS CodeBuild User Guide/.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
startBuild_reportBuildStatusOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
startBuild_reportBuildStatusOverride = Lens.lens (\StartBuild' {reportBuildStatusOverride} -> reportBuildStatusOverride) (\s@StartBuild' {} a -> s {reportBuildStatusOverride = a} :: StartBuild)

-- | The name of a service role for this build that overrides the one
-- specified in the build project.
startBuild_serviceRoleOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_serviceRoleOverride = Lens.lens (\StartBuild' {serviceRoleOverride} -> serviceRoleOverride) (\s@StartBuild' {} a -> s {serviceRoleOverride = a} :: StartBuild)

-- | The credentials for access to a private registry.
startBuild_registryCredentialOverride :: Lens.Lens' StartBuild (Core.Maybe RegistryCredential)
startBuild_registryCredentialOverride = Lens.lens (\StartBuild' {registryCredentialOverride} -> registryCredentialOverride) (\s@StartBuild' {} a -> s {registryCredentialOverride = a} :: StartBuild)

-- | An array of @ProjectSourceVersion@ objects that specify one or more
-- versions of the project\'s secondary sources to be used for this build
-- only.
startBuild_secondarySourcesVersionOverride :: Lens.Lens' StartBuild (Core.Maybe [ProjectSourceVersion])
startBuild_secondarySourcesVersionOverride = Lens.lens (\StartBuild' {secondarySourcesVersionOverride} -> secondarySourcesVersionOverride) (\s@StartBuild' {} a -> s {secondarySourcesVersionOverride = a} :: StartBuild) Core.. Lens.mapping Lens._Coerce

-- | Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
startBuild_buildStatusConfigOverride :: Lens.Lens' StartBuild (Core.Maybe BuildStatusConfig)
startBuild_buildStatusConfigOverride = Lens.lens (\StartBuild' {buildStatusConfigOverride} -> buildStatusConfigOverride) (\s@StartBuild' {} a -> s {buildStatusConfigOverride = a} :: StartBuild)

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that
-- overrides the one specified in the build project. The CMK key encrypts
-- the build output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
startBuild_encryptionKeyOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_encryptionKeyOverride = Lens.lens (\StartBuild' {encryptionKeyOverride} -> encryptionKeyOverride) (\s@StartBuild' {} a -> s {encryptionKeyOverride = a} :: StartBuild)

-- | Enable this flag to override privileged mode in the build project.
startBuild_privilegedModeOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
startBuild_privilegedModeOverride = Lens.lens (\StartBuild' {privilegedModeOverride} -> privilegedModeOverride) (\s@StartBuild' {} a -> s {privilegedModeOverride = a} :: StartBuild)

-- | Information about the Git submodules configuration for this build of an
-- AWS CodeBuild build project.
startBuild_gitSubmodulesConfigOverride :: Lens.Lens' StartBuild (Core.Maybe GitSubmodulesConfig)
startBuild_gitSubmodulesConfigOverride = Lens.lens (\StartBuild' {gitSubmodulesConfigOverride} -> gitSubmodulesConfigOverride) (\s@StartBuild' {} a -> s {gitSubmodulesConfigOverride = a} :: StartBuild)

-- | The name of a compute type for this build that overrides the one
-- specified in the build project.
startBuild_computeTypeOverride :: Lens.Lens' StartBuild (Core.Maybe ComputeType)
startBuild_computeTypeOverride = Lens.lens (\StartBuild' {computeTypeOverride} -> computeTypeOverride) (\s@StartBuild' {} a -> s {computeTypeOverride = a} :: StartBuild)

-- | The name of a certificate for this build that overrides the one
-- specified in the build project.
startBuild_certificateOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Text)
startBuild_certificateOverride = Lens.lens (\StartBuild' {certificateOverride} -> certificateOverride) (\s@StartBuild' {} a -> s {certificateOverride = a} :: StartBuild)

-- | A source input type, for this build, that overrides the source input
-- defined in the build project.
startBuild_sourceTypeOverride :: Lens.Lens' StartBuild (Core.Maybe SourceType)
startBuild_sourceTypeOverride = Lens.lens (\StartBuild' {sourceTypeOverride} -> sourceTypeOverride) (\s@StartBuild' {} a -> s {sourceTypeOverride = a} :: StartBuild)

-- | A container type for this build that overrides the one specified in the
-- build project.
startBuild_environmentTypeOverride :: Lens.Lens' StartBuild (Core.Maybe EnvironmentType)
startBuild_environmentTypeOverride = Lens.lens (\StartBuild' {environmentTypeOverride} -> environmentTypeOverride) (\s@StartBuild' {} a -> s {environmentTypeOverride = a} :: StartBuild)

-- | The type of credentials AWS CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- [CODEBUILD]
--     Specifies that AWS CodeBuild uses its own credentials. This requires
--     that you modify your ECR repository policy to trust AWS CodeBuild\'s
--     service principal.
--
-- [SERVICE_ROLE]
--     Specifies that AWS CodeBuild uses your build project\'s service
--     role.
--
-- When using a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When using an AWS CodeBuild curated image,
-- you must use @CODEBUILD@ credentials.
startBuild_imagePullCredentialsTypeOverride :: Lens.Lens' StartBuild (Core.Maybe ImagePullCredentialsType)
startBuild_imagePullCredentialsTypeOverride = Lens.lens (\StartBuild' {imagePullCredentialsTypeOverride} -> imagePullCredentialsTypeOverride) (\s@StartBuild' {} a -> s {imagePullCredentialsTypeOverride = a} :: StartBuild)

-- | An array of @ProjectArtifacts@ objects.
startBuild_secondaryArtifactsOverride :: Lens.Lens' StartBuild (Core.Maybe [ProjectArtifacts])
startBuild_secondaryArtifactsOverride = Lens.lens (\StartBuild' {secondaryArtifactsOverride} -> secondaryArtifactsOverride) (\s@StartBuild' {} a -> s {secondaryArtifactsOverride = a} :: StartBuild) Core.. Lens.mapping Lens._Coerce

-- | The user-defined depth of history, with a minimum value of 0, that
-- overrides, for this build only, any previous depth of history defined in
-- the build project.
startBuild_gitCloneDepthOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
startBuild_gitCloneDepthOverride = Lens.lens (\StartBuild' {gitCloneDepthOverride} -> gitCloneDepthOverride) (\s@StartBuild' {} a -> s {gitCloneDepthOverride = a} :: StartBuild)

-- | A ProjectCache object specified for this build that overrides the one
-- defined in the build project.
startBuild_cacheOverride :: Lens.Lens' StartBuild (Core.Maybe ProjectCache)
startBuild_cacheOverride = Lens.lens (\StartBuild' {cacheOverride} -> cacheOverride) (\s@StartBuild' {} a -> s {cacheOverride = a} :: StartBuild)

-- | The number of build timeout minutes, from 5 to 480 (8 hours), that
-- overrides, for this build only, the latest setting already defined in
-- the build project.
startBuild_timeoutInMinutesOverride :: Lens.Lens' StartBuild (Core.Maybe Core.Natural)
startBuild_timeoutInMinutesOverride = Lens.lens (\StartBuild' {timeoutInMinutesOverride} -> timeoutInMinutesOverride) (\s@StartBuild' {} a -> s {timeoutInMinutesOverride = a} :: StartBuild)

-- | Specifies if session debugging is enabled for this build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
startBuild_debugSessionEnabled :: Lens.Lens' StartBuild (Core.Maybe Core.Bool)
startBuild_debugSessionEnabled = Lens.lens (\StartBuild' {debugSessionEnabled} -> debugSessionEnabled) (\s@StartBuild' {} a -> s {debugSessionEnabled = a} :: StartBuild)

-- | The name of the AWS CodeBuild build project to start running a build.
startBuild_projectName :: Lens.Lens' StartBuild Core.Text
startBuild_projectName = Lens.lens (\StartBuild' {projectName} -> projectName) (\s@StartBuild' {} a -> s {projectName = a} :: StartBuild)

instance Core.AWSRequest StartBuild where
  type AWSResponse StartBuild = StartBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBuildResponse'
            Core.<$> (x Core..?> "build")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartBuild

instance Core.NFData StartBuild

instance Core.ToHeaders StartBuild where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("CodeBuild_20161006.StartBuild" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartBuild where
  toJSON StartBuild' {..} =
    Core.object
      ( Core.catMaybes
          [ ("buildspecOverride" Core..=)
              Core.<$> buildspecOverride,
            ("sourceVersion" Core..=) Core.<$> sourceVersion,
            ("environmentVariablesOverride" Core..=)
              Core.<$> environmentVariablesOverride,
            ("idempotencyToken" Core..=)
              Core.<$> idempotencyToken,
            ("sourceLocationOverride" Core..=)
              Core.<$> sourceLocationOverride,
            ("logsConfigOverride" Core..=)
              Core.<$> logsConfigOverride,
            ("artifactsOverride" Core..=)
              Core.<$> artifactsOverride,
            ("sourceAuthOverride" Core..=)
              Core.<$> sourceAuthOverride,
            ("imageOverride" Core..=) Core.<$> imageOverride,
            ("queuedTimeoutInMinutesOverride" Core..=)
              Core.<$> queuedTimeoutInMinutesOverride,
            ("insecureSslOverride" Core..=)
              Core.<$> insecureSslOverride,
            ("secondarySourcesOverride" Core..=)
              Core.<$> secondarySourcesOverride,
            ("reportBuildStatusOverride" Core..=)
              Core.<$> reportBuildStatusOverride,
            ("serviceRoleOverride" Core..=)
              Core.<$> serviceRoleOverride,
            ("registryCredentialOverride" Core..=)
              Core.<$> registryCredentialOverride,
            ("secondarySourcesVersionOverride" Core..=)
              Core.<$> secondarySourcesVersionOverride,
            ("buildStatusConfigOverride" Core..=)
              Core.<$> buildStatusConfigOverride,
            ("encryptionKeyOverride" Core..=)
              Core.<$> encryptionKeyOverride,
            ("privilegedModeOverride" Core..=)
              Core.<$> privilegedModeOverride,
            ("gitSubmodulesConfigOverride" Core..=)
              Core.<$> gitSubmodulesConfigOverride,
            ("computeTypeOverride" Core..=)
              Core.<$> computeTypeOverride,
            ("certificateOverride" Core..=)
              Core.<$> certificateOverride,
            ("sourceTypeOverride" Core..=)
              Core.<$> sourceTypeOverride,
            ("environmentTypeOverride" Core..=)
              Core.<$> environmentTypeOverride,
            ("imagePullCredentialsTypeOverride" Core..=)
              Core.<$> imagePullCredentialsTypeOverride,
            ("secondaryArtifactsOverride" Core..=)
              Core.<$> secondaryArtifactsOverride,
            ("gitCloneDepthOverride" Core..=)
              Core.<$> gitCloneDepthOverride,
            ("cacheOverride" Core..=) Core.<$> cacheOverride,
            ("timeoutInMinutesOverride" Core..=)
              Core.<$> timeoutInMinutesOverride,
            ("debugSessionEnabled" Core..=)
              Core.<$> debugSessionEnabled,
            Core.Just ("projectName" Core..= projectName)
          ]
      )

instance Core.ToPath StartBuild where
  toPath = Core.const "/"

instance Core.ToQuery StartBuild where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartBuildResponse' smart constructor.
data StartBuildResponse = StartBuildResponse'
  { -- | Information about the build to be run.
    build :: Core.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartBuildResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'build', 'startBuildResponse_build' - Information about the build to be run.
--
-- 'httpStatus', 'startBuildResponse_httpStatus' - The response's http status code.
newStartBuildResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartBuildResponse
newStartBuildResponse pHttpStatus_ =
  StartBuildResponse'
    { build = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the build to be run.
startBuildResponse_build :: Lens.Lens' StartBuildResponse (Core.Maybe Build)
startBuildResponse_build = Lens.lens (\StartBuildResponse' {build} -> build) (\s@StartBuildResponse' {} a -> s {build = a} :: StartBuildResponse)

-- | The response's http status code.
startBuildResponse_httpStatus :: Lens.Lens' StartBuildResponse Core.Int
startBuildResponse_httpStatus = Lens.lens (\StartBuildResponse' {httpStatus} -> httpStatus) (\s@StartBuildResponse' {} a -> s {httpStatus = a} :: StartBuildResponse)

instance Core.NFData StartBuildResponse
