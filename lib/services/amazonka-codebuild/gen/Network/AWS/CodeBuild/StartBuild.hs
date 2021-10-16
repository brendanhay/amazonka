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
    startBuild_sourceVersion,
    startBuild_buildspecOverride,
    startBuild_sourceLocationOverride,
    startBuild_idempotencyToken,
    startBuild_environmentVariablesOverride,
    startBuild_logsConfigOverride,
    startBuild_sourceAuthOverride,
    startBuild_artifactsOverride,
    startBuild_imageOverride,
    startBuild_insecureSslOverride,
    startBuild_queuedTimeoutInMinutesOverride,
    startBuild_reportBuildStatusOverride,
    startBuild_secondarySourcesOverride,
    startBuild_serviceRoleOverride,
    startBuild_secondarySourcesVersionOverride,
    startBuild_registryCredentialOverride,
    startBuild_buildStatusConfigOverride,
    startBuild_encryptionKeyOverride,
    startBuild_privilegedModeOverride,
    startBuild_gitSubmodulesConfigOverride,
    startBuild_sourceTypeOverride,
    startBuild_environmentTypeOverride,
    startBuild_certificateOverride,
    startBuild_computeTypeOverride,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartBuild' smart constructor.
data StartBuild = StartBuild'
  { -- | The version of the build input to be built, for this build only. If not
    -- specified, the latest version is used. If specified, the contents
    -- depends on the source provider:
    --
    -- [CodeCommit]
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
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | A buildspec file declaration that overrides, for this build only, the
    -- latest one already defined in the build project.
    --
    -- If this value is set, it can be either an inline buildspec definition,
    -- the path to an alternate buildspec file relative to the value of the
    -- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
    -- bucket. The bucket must be in the same Amazon Web Services Region as the
    -- build project. Specify the buildspec file using its ARN (for example,
    -- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
    -- not provided or is set to an empty string, the source code must contain
    -- a buildspec file in its root directory. For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
    buildspecOverride :: Prelude.Maybe Prelude.Text,
    -- | A location that overrides, for this build, the source location for the
    -- one defined in the build project.
    sourceLocationOverride :: Prelude.Maybe Prelude.Text,
    -- | A unique, case sensitive identifier you provide to ensure the
    -- idempotency of the StartBuild request. The token is included in the
    -- StartBuild request and is valid for 5 minutes. If you repeat the
    -- StartBuild request with the same token, but change a parameter,
    -- CodeBuild returns a parameter mismatch error.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | A set of environment variables that overrides, for this build only, the
    -- latest ones already defined in the build project.
    environmentVariablesOverride :: Prelude.Maybe [EnvironmentVariable],
    -- | Log settings for this build that override the log settings defined in
    -- the build project.
    logsConfigOverride :: Prelude.Maybe LogsConfig,
    -- | An authorization type for this build that overrides the one defined in
    -- the build project. This override applies only if the build project\'s
    -- source is BitBucket or GitHub.
    sourceAuthOverride :: Prelude.Maybe SourceAuth,
    -- | Build output artifact settings that override, for this build only, the
    -- latest ones already defined in the build project.
    artifactsOverride :: Prelude.Maybe ProjectArtifacts,
    -- | The name of an image for this build that overrides the one specified in
    -- the build project.
    imageOverride :: Prelude.Maybe Prelude.Text,
    -- | Enable this flag to override the insecure SSL setting that is specified
    -- in the build project. The insecure SSL setting determines whether to
    -- ignore SSL warnings while connecting to the project source code. This
    -- override applies only if the build\'s source is GitHub Enterprise.
    insecureSslOverride :: Prelude.Maybe Prelude.Bool,
    -- | The number of minutes a build is allowed to be queued before it times
    -- out.
    queuedTimeoutInMinutesOverride :: Prelude.Maybe Prelude.Natural,
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
    -- in the /CodeBuild User Guide/.
    --
    -- The status of a build triggered by a webhook is always reported to your
    -- source provider.
    reportBuildStatusOverride :: Prelude.Maybe Prelude.Bool,
    -- | An array of @ProjectSource@ objects.
    secondarySourcesOverride :: Prelude.Maybe [ProjectSource],
    -- | The name of a service role for this build that overrides the one
    -- specified in the build project.
    serviceRoleOverride :: Prelude.Maybe Prelude.Text,
    -- | An array of @ProjectSourceVersion@ objects that specify one or more
    -- versions of the project\'s secondary sources to be used for this build
    -- only.
    secondarySourcesVersionOverride :: Prelude.Maybe [ProjectSourceVersion],
    -- | The credentials for access to a private registry.
    registryCredentialOverride :: Prelude.Maybe RegistryCredential,
    -- | Contains information that defines how the build project reports the
    -- build status to the source provider. This option is only used when the
    -- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
    buildStatusConfigOverride :: Prelude.Maybe BuildStatusConfig,
    -- | The Key Management Service customer master key (CMK) that overrides the
    -- one specified in the build project. The CMK key encrypts the build
    -- output artifacts.
    --
    -- You can use a cross-account KMS key to encrypt the build output
    -- artifacts if your service role has permission to that key.
    --
    -- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
    -- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
    encryptionKeyOverride :: Prelude.Maybe Prelude.Text,
    -- | Enable this flag to override privileged mode in the build project.
    privilegedModeOverride :: Prelude.Maybe Prelude.Bool,
    -- | Information about the Git submodules configuration for this build of an
    -- CodeBuild build project.
    gitSubmodulesConfigOverride :: Prelude.Maybe GitSubmodulesConfig,
    -- | A source input type, for this build, that overrides the source input
    -- defined in the build project.
    sourceTypeOverride :: Prelude.Maybe SourceType,
    -- | A container type for this build that overrides the one specified in the
    -- build project.
    environmentTypeOverride :: Prelude.Maybe EnvironmentType,
    -- | The name of a certificate for this build that overrides the one
    -- specified in the build project.
    certificateOverride :: Prelude.Maybe Prelude.Text,
    -- | The name of a compute type for this build that overrides the one
    -- specified in the build project.
    computeTypeOverride :: Prelude.Maybe ComputeType,
    -- | The type of credentials CodeBuild uses to pull images in your build.
    -- There are two valid values:
    --
    -- [CODEBUILD]
    --     Specifies that CodeBuild uses its own credentials. This requires
    --     that you modify your ECR repository policy to trust CodeBuild\'s
    --     service principal.
    --
    -- [SERVICE_ROLE]
    --     Specifies that CodeBuild uses your build project\'s service role.
    --
    -- When using a cross-account or private registry image, you must use
    -- @SERVICE_ROLE@ credentials. When using an CodeBuild curated image, you
    -- must use @CODEBUILD@ credentials.
    imagePullCredentialsTypeOverride :: Prelude.Maybe ImagePullCredentialsType,
    -- | An array of @ProjectArtifacts@ objects.
    secondaryArtifactsOverride :: Prelude.Maybe [ProjectArtifacts],
    -- | The user-defined depth of history, with a minimum value of 0, that
    -- overrides, for this build only, any previous depth of history defined in
    -- the build project.
    gitCloneDepthOverride :: Prelude.Maybe Prelude.Natural,
    -- | A ProjectCache object specified for this build that overrides the one
    -- defined in the build project.
    cacheOverride :: Prelude.Maybe ProjectCache,
    -- | The number of build timeout minutes, from 5 to 480 (8 hours), that
    -- overrides, for this build only, the latest setting already defined in
    -- the build project.
    timeoutInMinutesOverride :: Prelude.Maybe Prelude.Natural,
    -- | Specifies if session debugging is enabled for this build. For more
    -- information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
    debugSessionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CodeBuild build project to start running a build.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartBuild' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceVersion', 'startBuild_sourceVersion' - The version of the build input to be built, for this build only. If not
-- specified, the latest version is used. If specified, the contents
-- depends on the source provider:
--
-- [CodeCommit]
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
-- in the /CodeBuild User Guide/.
--
-- 'buildspecOverride', 'startBuild_buildspecOverride' - A buildspec file declaration that overrides, for this build only, the
-- latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition,
-- the path to an alternate buildspec file relative to the value of the
-- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
-- bucket. The bucket must be in the same Amazon Web Services Region as the
-- build project. Specify the buildspec file using its ARN (for example,
-- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
-- not provided or is set to an empty string, the source code must contain
-- a buildspec file in its root directory. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
--
-- 'sourceLocationOverride', 'startBuild_sourceLocationOverride' - A location that overrides, for this build, the source location for the
-- one defined in the build project.
--
-- 'idempotencyToken', 'startBuild_idempotencyToken' - A unique, case sensitive identifier you provide to ensure the
-- idempotency of the StartBuild request. The token is included in the
-- StartBuild request and is valid for 5 minutes. If you repeat the
-- StartBuild request with the same token, but change a parameter,
-- CodeBuild returns a parameter mismatch error.
--
-- 'environmentVariablesOverride', 'startBuild_environmentVariablesOverride' - A set of environment variables that overrides, for this build only, the
-- latest ones already defined in the build project.
--
-- 'logsConfigOverride', 'startBuild_logsConfigOverride' - Log settings for this build that override the log settings defined in
-- the build project.
--
-- 'sourceAuthOverride', 'startBuild_sourceAuthOverride' - An authorization type for this build that overrides the one defined in
-- the build project. This override applies only if the build project\'s
-- source is BitBucket or GitHub.
--
-- 'artifactsOverride', 'startBuild_artifactsOverride' - Build output artifact settings that override, for this build only, the
-- latest ones already defined in the build project.
--
-- 'imageOverride', 'startBuild_imageOverride' - The name of an image for this build that overrides the one specified in
-- the build project.
--
-- 'insecureSslOverride', 'startBuild_insecureSslOverride' - Enable this flag to override the insecure SSL setting that is specified
-- in the build project. The insecure SSL setting determines whether to
-- ignore SSL warnings while connecting to the project source code. This
-- override applies only if the build\'s source is GitHub Enterprise.
--
-- 'queuedTimeoutInMinutesOverride', 'startBuild_queuedTimeoutInMinutesOverride' - The number of minutes a build is allowed to be queued before it times
-- out.
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
-- in the /CodeBuild User Guide/.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
--
-- 'secondarySourcesOverride', 'startBuild_secondarySourcesOverride' - An array of @ProjectSource@ objects.
--
-- 'serviceRoleOverride', 'startBuild_serviceRoleOverride' - The name of a service role for this build that overrides the one
-- specified in the build project.
--
-- 'secondarySourcesVersionOverride', 'startBuild_secondarySourcesVersionOverride' - An array of @ProjectSourceVersion@ objects that specify one or more
-- versions of the project\'s secondary sources to be used for this build
-- only.
--
-- 'registryCredentialOverride', 'startBuild_registryCredentialOverride' - The credentials for access to a private registry.
--
-- 'buildStatusConfigOverride', 'startBuild_buildStatusConfigOverride' - Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
--
-- 'encryptionKeyOverride', 'startBuild_encryptionKeyOverride' - The Key Management Service customer master key (CMK) that overrides the
-- one specified in the build project. The CMK key encrypts the build
-- output artifacts.
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
-- CodeBuild build project.
--
-- 'sourceTypeOverride', 'startBuild_sourceTypeOverride' - A source input type, for this build, that overrides the source input
-- defined in the build project.
--
-- 'environmentTypeOverride', 'startBuild_environmentTypeOverride' - A container type for this build that overrides the one specified in the
-- build project.
--
-- 'certificateOverride', 'startBuild_certificateOverride' - The name of a certificate for this build that overrides the one
-- specified in the build project.
--
-- 'computeTypeOverride', 'startBuild_computeTypeOverride' - The name of a compute type for this build that overrides the one
-- specified in the build project.
--
-- 'imagePullCredentialsTypeOverride', 'startBuild_imagePullCredentialsTypeOverride' - The type of credentials CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- [CODEBUILD]
--     Specifies that CodeBuild uses its own credentials. This requires
--     that you modify your ECR repository policy to trust CodeBuild\'s
--     service principal.
--
-- [SERVICE_ROLE]
--     Specifies that CodeBuild uses your build project\'s service role.
--
-- When using a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When using an CodeBuild curated image, you
-- must use @CODEBUILD@ credentials.
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
-- 'projectName', 'startBuild_projectName' - The name of the CodeBuild build project to start running a build.
newStartBuild ::
  -- | 'projectName'
  Prelude.Text ->
  StartBuild
newStartBuild pProjectName_ =
  StartBuild'
    { sourceVersion = Prelude.Nothing,
      buildspecOverride = Prelude.Nothing,
      sourceLocationOverride = Prelude.Nothing,
      idempotencyToken = Prelude.Nothing,
      environmentVariablesOverride = Prelude.Nothing,
      logsConfigOverride = Prelude.Nothing,
      sourceAuthOverride = Prelude.Nothing,
      artifactsOverride = Prelude.Nothing,
      imageOverride = Prelude.Nothing,
      insecureSslOverride = Prelude.Nothing,
      queuedTimeoutInMinutesOverride = Prelude.Nothing,
      reportBuildStatusOverride = Prelude.Nothing,
      secondarySourcesOverride = Prelude.Nothing,
      serviceRoleOverride = Prelude.Nothing,
      secondarySourcesVersionOverride = Prelude.Nothing,
      registryCredentialOverride = Prelude.Nothing,
      buildStatusConfigOverride = Prelude.Nothing,
      encryptionKeyOverride = Prelude.Nothing,
      privilegedModeOverride = Prelude.Nothing,
      gitSubmodulesConfigOverride = Prelude.Nothing,
      sourceTypeOverride = Prelude.Nothing,
      environmentTypeOverride = Prelude.Nothing,
      certificateOverride = Prelude.Nothing,
      computeTypeOverride = Prelude.Nothing,
      imagePullCredentialsTypeOverride = Prelude.Nothing,
      secondaryArtifactsOverride = Prelude.Nothing,
      gitCloneDepthOverride = Prelude.Nothing,
      cacheOverride = Prelude.Nothing,
      timeoutInMinutesOverride = Prelude.Nothing,
      debugSessionEnabled = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | The version of the build input to be built, for this build only. If not
-- specified, the latest version is used. If specified, the contents
-- depends on the source provider:
--
-- [CodeCommit]
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
-- in the /CodeBuild User Guide/.
startBuild_sourceVersion :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_sourceVersion = Lens.lens (\StartBuild' {sourceVersion} -> sourceVersion) (\s@StartBuild' {} a -> s {sourceVersion = a} :: StartBuild)

-- | A buildspec file declaration that overrides, for this build only, the
-- latest one already defined in the build project.
--
-- If this value is set, it can be either an inline buildspec definition,
-- the path to an alternate buildspec file relative to the value of the
-- built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3
-- bucket. The bucket must be in the same Amazon Web Services Region as the
-- build project. Specify the buildspec file using its ARN (for example,
-- @arn:aws:s3:::my-codebuild-sample2\/buildspec.yml@). If this value is
-- not provided or is set to an empty string, the source code must contain
-- a buildspec file in its root directory. For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location>.
startBuild_buildspecOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_buildspecOverride = Lens.lens (\StartBuild' {buildspecOverride} -> buildspecOverride) (\s@StartBuild' {} a -> s {buildspecOverride = a} :: StartBuild)

-- | A location that overrides, for this build, the source location for the
-- one defined in the build project.
startBuild_sourceLocationOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_sourceLocationOverride = Lens.lens (\StartBuild' {sourceLocationOverride} -> sourceLocationOverride) (\s@StartBuild' {} a -> s {sourceLocationOverride = a} :: StartBuild)

-- | A unique, case sensitive identifier you provide to ensure the
-- idempotency of the StartBuild request. The token is included in the
-- StartBuild request and is valid for 5 minutes. If you repeat the
-- StartBuild request with the same token, but change a parameter,
-- CodeBuild returns a parameter mismatch error.
startBuild_idempotencyToken :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_idempotencyToken = Lens.lens (\StartBuild' {idempotencyToken} -> idempotencyToken) (\s@StartBuild' {} a -> s {idempotencyToken = a} :: StartBuild)

-- | A set of environment variables that overrides, for this build only, the
-- latest ones already defined in the build project.
startBuild_environmentVariablesOverride :: Lens.Lens' StartBuild (Prelude.Maybe [EnvironmentVariable])
startBuild_environmentVariablesOverride = Lens.lens (\StartBuild' {environmentVariablesOverride} -> environmentVariablesOverride) (\s@StartBuild' {} a -> s {environmentVariablesOverride = a} :: StartBuild) Prelude.. Lens.mapping Lens._Coerce

-- | Log settings for this build that override the log settings defined in
-- the build project.
startBuild_logsConfigOverride :: Lens.Lens' StartBuild (Prelude.Maybe LogsConfig)
startBuild_logsConfigOverride = Lens.lens (\StartBuild' {logsConfigOverride} -> logsConfigOverride) (\s@StartBuild' {} a -> s {logsConfigOverride = a} :: StartBuild)

-- | An authorization type for this build that overrides the one defined in
-- the build project. This override applies only if the build project\'s
-- source is BitBucket or GitHub.
startBuild_sourceAuthOverride :: Lens.Lens' StartBuild (Prelude.Maybe SourceAuth)
startBuild_sourceAuthOverride = Lens.lens (\StartBuild' {sourceAuthOverride} -> sourceAuthOverride) (\s@StartBuild' {} a -> s {sourceAuthOverride = a} :: StartBuild)

-- | Build output artifact settings that override, for this build only, the
-- latest ones already defined in the build project.
startBuild_artifactsOverride :: Lens.Lens' StartBuild (Prelude.Maybe ProjectArtifacts)
startBuild_artifactsOverride = Lens.lens (\StartBuild' {artifactsOverride} -> artifactsOverride) (\s@StartBuild' {} a -> s {artifactsOverride = a} :: StartBuild)

-- | The name of an image for this build that overrides the one specified in
-- the build project.
startBuild_imageOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_imageOverride = Lens.lens (\StartBuild' {imageOverride} -> imageOverride) (\s@StartBuild' {} a -> s {imageOverride = a} :: StartBuild)

-- | Enable this flag to override the insecure SSL setting that is specified
-- in the build project. The insecure SSL setting determines whether to
-- ignore SSL warnings while connecting to the project source code. This
-- override applies only if the build\'s source is GitHub Enterprise.
startBuild_insecureSslOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Bool)
startBuild_insecureSslOverride = Lens.lens (\StartBuild' {insecureSslOverride} -> insecureSslOverride) (\s@StartBuild' {} a -> s {insecureSslOverride = a} :: StartBuild)

-- | The number of minutes a build is allowed to be queued before it times
-- out.
startBuild_queuedTimeoutInMinutesOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Natural)
startBuild_queuedTimeoutInMinutesOverride = Lens.lens (\StartBuild' {queuedTimeoutInMinutesOverride} -> queuedTimeoutInMinutesOverride) (\s@StartBuild' {} a -> s {queuedTimeoutInMinutesOverride = a} :: StartBuild)

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
-- in the /CodeBuild User Guide/.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
startBuild_reportBuildStatusOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Bool)
startBuild_reportBuildStatusOverride = Lens.lens (\StartBuild' {reportBuildStatusOverride} -> reportBuildStatusOverride) (\s@StartBuild' {} a -> s {reportBuildStatusOverride = a} :: StartBuild)

-- | An array of @ProjectSource@ objects.
startBuild_secondarySourcesOverride :: Lens.Lens' StartBuild (Prelude.Maybe [ProjectSource])
startBuild_secondarySourcesOverride = Lens.lens (\StartBuild' {secondarySourcesOverride} -> secondarySourcesOverride) (\s@StartBuild' {} a -> s {secondarySourcesOverride = a} :: StartBuild) Prelude.. Lens.mapping Lens._Coerce

-- | The name of a service role for this build that overrides the one
-- specified in the build project.
startBuild_serviceRoleOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_serviceRoleOverride = Lens.lens (\StartBuild' {serviceRoleOverride} -> serviceRoleOverride) (\s@StartBuild' {} a -> s {serviceRoleOverride = a} :: StartBuild)

-- | An array of @ProjectSourceVersion@ objects that specify one or more
-- versions of the project\'s secondary sources to be used for this build
-- only.
startBuild_secondarySourcesVersionOverride :: Lens.Lens' StartBuild (Prelude.Maybe [ProjectSourceVersion])
startBuild_secondarySourcesVersionOverride = Lens.lens (\StartBuild' {secondarySourcesVersionOverride} -> secondarySourcesVersionOverride) (\s@StartBuild' {} a -> s {secondarySourcesVersionOverride = a} :: StartBuild) Prelude.. Lens.mapping Lens._Coerce

-- | The credentials for access to a private registry.
startBuild_registryCredentialOverride :: Lens.Lens' StartBuild (Prelude.Maybe RegistryCredential)
startBuild_registryCredentialOverride = Lens.lens (\StartBuild' {registryCredentialOverride} -> registryCredentialOverride) (\s@StartBuild' {} a -> s {registryCredentialOverride = a} :: StartBuild)

-- | Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
startBuild_buildStatusConfigOverride :: Lens.Lens' StartBuild (Prelude.Maybe BuildStatusConfig)
startBuild_buildStatusConfigOverride = Lens.lens (\StartBuild' {buildStatusConfigOverride} -> buildStatusConfigOverride) (\s@StartBuild' {} a -> s {buildStatusConfigOverride = a} :: StartBuild)

-- | The Key Management Service customer master key (CMK) that overrides the
-- one specified in the build project. The CMK key encrypts the build
-- output artifacts.
--
-- You can use a cross-account KMS key to encrypt the build output
-- artifacts if your service role has permission to that key.
--
-- You can specify either the Amazon Resource Name (ARN) of the CMK or, if
-- available, the CMK\'s alias (using the format @alias\/\<alias-name>@).
startBuild_encryptionKeyOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_encryptionKeyOverride = Lens.lens (\StartBuild' {encryptionKeyOverride} -> encryptionKeyOverride) (\s@StartBuild' {} a -> s {encryptionKeyOverride = a} :: StartBuild)

-- | Enable this flag to override privileged mode in the build project.
startBuild_privilegedModeOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Bool)
startBuild_privilegedModeOverride = Lens.lens (\StartBuild' {privilegedModeOverride} -> privilegedModeOverride) (\s@StartBuild' {} a -> s {privilegedModeOverride = a} :: StartBuild)

-- | Information about the Git submodules configuration for this build of an
-- CodeBuild build project.
startBuild_gitSubmodulesConfigOverride :: Lens.Lens' StartBuild (Prelude.Maybe GitSubmodulesConfig)
startBuild_gitSubmodulesConfigOverride = Lens.lens (\StartBuild' {gitSubmodulesConfigOverride} -> gitSubmodulesConfigOverride) (\s@StartBuild' {} a -> s {gitSubmodulesConfigOverride = a} :: StartBuild)

-- | A source input type, for this build, that overrides the source input
-- defined in the build project.
startBuild_sourceTypeOverride :: Lens.Lens' StartBuild (Prelude.Maybe SourceType)
startBuild_sourceTypeOverride = Lens.lens (\StartBuild' {sourceTypeOverride} -> sourceTypeOverride) (\s@StartBuild' {} a -> s {sourceTypeOverride = a} :: StartBuild)

-- | A container type for this build that overrides the one specified in the
-- build project.
startBuild_environmentTypeOverride :: Lens.Lens' StartBuild (Prelude.Maybe EnvironmentType)
startBuild_environmentTypeOverride = Lens.lens (\StartBuild' {environmentTypeOverride} -> environmentTypeOverride) (\s@StartBuild' {} a -> s {environmentTypeOverride = a} :: StartBuild)

-- | The name of a certificate for this build that overrides the one
-- specified in the build project.
startBuild_certificateOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Text)
startBuild_certificateOverride = Lens.lens (\StartBuild' {certificateOverride} -> certificateOverride) (\s@StartBuild' {} a -> s {certificateOverride = a} :: StartBuild)

-- | The name of a compute type for this build that overrides the one
-- specified in the build project.
startBuild_computeTypeOverride :: Lens.Lens' StartBuild (Prelude.Maybe ComputeType)
startBuild_computeTypeOverride = Lens.lens (\StartBuild' {computeTypeOverride} -> computeTypeOverride) (\s@StartBuild' {} a -> s {computeTypeOverride = a} :: StartBuild)

-- | The type of credentials CodeBuild uses to pull images in your build.
-- There are two valid values:
--
-- [CODEBUILD]
--     Specifies that CodeBuild uses its own credentials. This requires
--     that you modify your ECR repository policy to trust CodeBuild\'s
--     service principal.
--
-- [SERVICE_ROLE]
--     Specifies that CodeBuild uses your build project\'s service role.
--
-- When using a cross-account or private registry image, you must use
-- @SERVICE_ROLE@ credentials. When using an CodeBuild curated image, you
-- must use @CODEBUILD@ credentials.
startBuild_imagePullCredentialsTypeOverride :: Lens.Lens' StartBuild (Prelude.Maybe ImagePullCredentialsType)
startBuild_imagePullCredentialsTypeOverride = Lens.lens (\StartBuild' {imagePullCredentialsTypeOverride} -> imagePullCredentialsTypeOverride) (\s@StartBuild' {} a -> s {imagePullCredentialsTypeOverride = a} :: StartBuild)

-- | An array of @ProjectArtifacts@ objects.
startBuild_secondaryArtifactsOverride :: Lens.Lens' StartBuild (Prelude.Maybe [ProjectArtifacts])
startBuild_secondaryArtifactsOverride = Lens.lens (\StartBuild' {secondaryArtifactsOverride} -> secondaryArtifactsOverride) (\s@StartBuild' {} a -> s {secondaryArtifactsOverride = a} :: StartBuild) Prelude.. Lens.mapping Lens._Coerce

-- | The user-defined depth of history, with a minimum value of 0, that
-- overrides, for this build only, any previous depth of history defined in
-- the build project.
startBuild_gitCloneDepthOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Natural)
startBuild_gitCloneDepthOverride = Lens.lens (\StartBuild' {gitCloneDepthOverride} -> gitCloneDepthOverride) (\s@StartBuild' {} a -> s {gitCloneDepthOverride = a} :: StartBuild)

-- | A ProjectCache object specified for this build that overrides the one
-- defined in the build project.
startBuild_cacheOverride :: Lens.Lens' StartBuild (Prelude.Maybe ProjectCache)
startBuild_cacheOverride = Lens.lens (\StartBuild' {cacheOverride} -> cacheOverride) (\s@StartBuild' {} a -> s {cacheOverride = a} :: StartBuild)

-- | The number of build timeout minutes, from 5 to 480 (8 hours), that
-- overrides, for this build only, the latest setting already defined in
-- the build project.
startBuild_timeoutInMinutesOverride :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Natural)
startBuild_timeoutInMinutesOverride = Lens.lens (\StartBuild' {timeoutInMinutesOverride} -> timeoutInMinutesOverride) (\s@StartBuild' {} a -> s {timeoutInMinutesOverride = a} :: StartBuild)

-- | Specifies if session debugging is enabled for this build. For more
-- information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager>.
startBuild_debugSessionEnabled :: Lens.Lens' StartBuild (Prelude.Maybe Prelude.Bool)
startBuild_debugSessionEnabled = Lens.lens (\StartBuild' {debugSessionEnabled} -> debugSessionEnabled) (\s@StartBuild' {} a -> s {debugSessionEnabled = a} :: StartBuild)

-- | The name of the CodeBuild build project to start running a build.
startBuild_projectName :: Lens.Lens' StartBuild Prelude.Text
startBuild_projectName = Lens.lens (\StartBuild' {projectName} -> projectName) (\s@StartBuild' {} a -> s {projectName = a} :: StartBuild)

instance Core.AWSRequest StartBuild where
  type AWSResponse StartBuild = StartBuildResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartBuildResponse'
            Prelude.<$> (x Core..?> "build")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartBuild

instance Prelude.NFData StartBuild

instance Core.ToHeaders StartBuild where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.StartBuild" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartBuild where
  toJSON StartBuild' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sourceVersion" Core..=) Prelude.<$> sourceVersion,
            ("buildspecOverride" Core..=)
              Prelude.<$> buildspecOverride,
            ("sourceLocationOverride" Core..=)
              Prelude.<$> sourceLocationOverride,
            ("idempotencyToken" Core..=)
              Prelude.<$> idempotencyToken,
            ("environmentVariablesOverride" Core..=)
              Prelude.<$> environmentVariablesOverride,
            ("logsConfigOverride" Core..=)
              Prelude.<$> logsConfigOverride,
            ("sourceAuthOverride" Core..=)
              Prelude.<$> sourceAuthOverride,
            ("artifactsOverride" Core..=)
              Prelude.<$> artifactsOverride,
            ("imageOverride" Core..=) Prelude.<$> imageOverride,
            ("insecureSslOverride" Core..=)
              Prelude.<$> insecureSslOverride,
            ("queuedTimeoutInMinutesOverride" Core..=)
              Prelude.<$> queuedTimeoutInMinutesOverride,
            ("reportBuildStatusOverride" Core..=)
              Prelude.<$> reportBuildStatusOverride,
            ("secondarySourcesOverride" Core..=)
              Prelude.<$> secondarySourcesOverride,
            ("serviceRoleOverride" Core..=)
              Prelude.<$> serviceRoleOverride,
            ("secondarySourcesVersionOverride" Core..=)
              Prelude.<$> secondarySourcesVersionOverride,
            ("registryCredentialOverride" Core..=)
              Prelude.<$> registryCredentialOverride,
            ("buildStatusConfigOverride" Core..=)
              Prelude.<$> buildStatusConfigOverride,
            ("encryptionKeyOverride" Core..=)
              Prelude.<$> encryptionKeyOverride,
            ("privilegedModeOverride" Core..=)
              Prelude.<$> privilegedModeOverride,
            ("gitSubmodulesConfigOverride" Core..=)
              Prelude.<$> gitSubmodulesConfigOverride,
            ("sourceTypeOverride" Core..=)
              Prelude.<$> sourceTypeOverride,
            ("environmentTypeOverride" Core..=)
              Prelude.<$> environmentTypeOverride,
            ("certificateOverride" Core..=)
              Prelude.<$> certificateOverride,
            ("computeTypeOverride" Core..=)
              Prelude.<$> computeTypeOverride,
            ("imagePullCredentialsTypeOverride" Core..=)
              Prelude.<$> imagePullCredentialsTypeOverride,
            ("secondaryArtifactsOverride" Core..=)
              Prelude.<$> secondaryArtifactsOverride,
            ("gitCloneDepthOverride" Core..=)
              Prelude.<$> gitCloneDepthOverride,
            ("cacheOverride" Core..=) Prelude.<$> cacheOverride,
            ("timeoutInMinutesOverride" Core..=)
              Prelude.<$> timeoutInMinutesOverride,
            ("debugSessionEnabled" Core..=)
              Prelude.<$> debugSessionEnabled,
            Prelude.Just ("projectName" Core..= projectName)
          ]
      )

instance Core.ToPath StartBuild where
  toPath = Prelude.const "/"

instance Core.ToQuery StartBuild where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartBuildResponse' smart constructor.
data StartBuildResponse = StartBuildResponse'
  { -- | Information about the build to be run.
    build :: Prelude.Maybe Build,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartBuildResponse
newStartBuildResponse pHttpStatus_ =
  StartBuildResponse'
    { build = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the build to be run.
startBuildResponse_build :: Lens.Lens' StartBuildResponse (Prelude.Maybe Build)
startBuildResponse_build = Lens.lens (\StartBuildResponse' {build} -> build) (\s@StartBuildResponse' {} a -> s {build = a} :: StartBuildResponse)

-- | The response's http status code.
startBuildResponse_httpStatus :: Lens.Lens' StartBuildResponse Prelude.Int
startBuildResponse_httpStatus = Lens.lens (\StartBuildResponse' {httpStatus} -> httpStatus) (\s@StartBuildResponse' {} a -> s {httpStatus = a} :: StartBuildResponse)

instance Prelude.NFData StartBuildResponse
