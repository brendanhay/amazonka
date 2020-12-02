{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    startBuild,
    StartBuild,

    -- * Request Lenses
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

    -- * Destructuring the Response
    startBuildResponse,
    StartBuildResponse,

    -- * Response Lenses
    srsBuild,
    srsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startBuild' smart constructor.
data StartBuild = StartBuild'
  { _sbEncryptionKeyOverride ::
      !(Maybe Text),
    _sbSourceLocationOverride :: !(Maybe Text),
    _sbEnvironmentVariablesOverride :: !(Maybe [EnvironmentVariable]),
    _sbBuildStatusConfigOverride :: !(Maybe BuildStatusConfig),
    _sbIdempotencyToken :: !(Maybe Text),
    _sbDebugSessionEnabled :: !(Maybe Bool),
    _sbRegistryCredentialOverride :: !(Maybe RegistryCredential),
    _sbTimeoutInMinutesOverride :: !(Maybe Nat),
    _sbServiceRoleOverride :: !(Maybe Text),
    _sbCacheOverride :: !(Maybe ProjectCache),
    _sbQueuedTimeoutInMinutesOverride :: !(Maybe Nat),
    _sbSecondarySourcesOverride :: !(Maybe [ProjectSource]),
    _sbGitCloneDepthOverride :: !(Maybe Nat),
    _sbImagePullCredentialsTypeOverride ::
      !(Maybe ImagePullCredentialsType),
    _sbLogsConfigOverride :: !(Maybe LogsConfig),
    _sbSourceAuthOverride :: !(Maybe SourceAuth),
    _sbGitSubmodulesConfigOverride :: !(Maybe GitSubmodulesConfig),
    _sbEnvironmentTypeOverride :: !(Maybe EnvironmentType),
    _sbCertificateOverride :: !(Maybe Text),
    _sbComputeTypeOverride :: !(Maybe ComputeType),
    _sbPrivilegedModeOverride :: !(Maybe Bool),
    _sbSourceVersion :: !(Maybe Text),
    _sbBuildspecOverride :: !(Maybe Text),
    _sbSecondarySourcesVersionOverride ::
      !(Maybe [ProjectSourceVersion]),
    _sbReportBuildStatusOverride :: !(Maybe Bool),
    _sbInsecureSSLOverride :: !(Maybe Bool),
    _sbImageOverride :: !(Maybe Text),
    _sbSecondaryArtifactsOverride :: !(Maybe [ProjectArtifacts]),
    _sbArtifactsOverride :: !(Maybe ProjectArtifacts),
    _sbSourceTypeOverride :: !(Maybe SourceType),
    _sbProjectName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbEncryptionKeyOverride' - The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
--
-- * 'sbSourceLocationOverride' - A location that overrides, for this build, the source location for the one defined in the build project.
--
-- * 'sbEnvironmentVariablesOverride' - A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
--
-- * 'sbBuildStatusConfigOverride' - Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
--
-- * 'sbIdempotencyToken' - A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
--
-- * 'sbDebugSessionEnabled' - Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
--
-- * 'sbRegistryCredentialOverride' - The credentials for access to a private registry.
--
-- * 'sbTimeoutInMinutesOverride' - The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
--
-- * 'sbServiceRoleOverride' - The name of a service role for this build that overrides the one specified in the build project.
--
-- * 'sbCacheOverride' - A ProjectCache object specified for this build that overrides the one defined in the build project.
--
-- * 'sbQueuedTimeoutInMinutesOverride' - The number of minutes a build is allowed to be queued before it times out.
--
-- * 'sbSecondarySourcesOverride' - An array of @ProjectSource@ objects.
--
-- * 'sbGitCloneDepthOverride' - The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
--
-- * 'sbImagePullCredentialsTypeOverride' - The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:      * CODEBUILD    * Specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.     * SERVICE_ROLE    * Specifies that AWS CodeBuild uses your build project's service role.  When using a cross-account or private registry image, you must use @SERVICE_ROLE@ credentials. When using an AWS CodeBuild curated image, you must use @CODEBUILD@ credentials.
--
-- * 'sbLogsConfigOverride' - Log settings for this build that override the log settings defined in the build project.
--
-- * 'sbSourceAuthOverride' - An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
--
-- * 'sbGitSubmodulesConfigOverride' - Information about the Git submodules configuration for this build of an AWS CodeBuild build project.
--
-- * 'sbEnvironmentTypeOverride' - A container type for this build that overrides the one specified in the build project.
--
-- * 'sbCertificateOverride' - The name of a certificate for this build that overrides the one specified in the build project.
--
-- * 'sbComputeTypeOverride' - The name of a compute type for this build that overrides the one specified in the build project.
--
-- * 'sbPrivilegedModeOverride' - Enable this flag to override privileged mode in the build project.
--
-- * 'sbSourceVersion' - The version of the build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:     * AWS CodeCommit    * The commit ID, branch, or Git tag to use.     * GitHub    * The commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * Bitbucket    * The commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * Amazon Simple Storage Service (Amazon S3)    * The version ID of the object that represents the build input ZIP file to use. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
--
-- * 'sbBuildspecOverride' - A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project. If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- * 'sbSecondarySourcesVersionOverride' - An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only.
--
-- * 'sbReportBuildStatusOverride' - Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown.
--
-- * 'sbInsecureSSLOverride' - Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
--
-- * 'sbImageOverride' - The name of an image for this build that overrides the one specified in the build project.
--
-- * 'sbSecondaryArtifactsOverride' - An array of @ProjectArtifacts@ objects.
--
-- * 'sbArtifactsOverride' - Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
--
-- * 'sbSourceTypeOverride' - A source input type, for this build, that overrides the source input defined in the build project.
--
-- * 'sbProjectName' - The name of the AWS CodeBuild build project to start running a build.
startBuild ::
  -- | 'sbProjectName'
  Text ->
  StartBuild
startBuild pProjectName_ =
  StartBuild'
    { _sbEncryptionKeyOverride = Nothing,
      _sbSourceLocationOverride = Nothing,
      _sbEnvironmentVariablesOverride = Nothing,
      _sbBuildStatusConfigOverride = Nothing,
      _sbIdempotencyToken = Nothing,
      _sbDebugSessionEnabled = Nothing,
      _sbRegistryCredentialOverride = Nothing,
      _sbTimeoutInMinutesOverride = Nothing,
      _sbServiceRoleOverride = Nothing,
      _sbCacheOverride = Nothing,
      _sbQueuedTimeoutInMinutesOverride = Nothing,
      _sbSecondarySourcesOverride = Nothing,
      _sbGitCloneDepthOverride = Nothing,
      _sbImagePullCredentialsTypeOverride = Nothing,
      _sbLogsConfigOverride = Nothing,
      _sbSourceAuthOverride = Nothing,
      _sbGitSubmodulesConfigOverride = Nothing,
      _sbEnvironmentTypeOverride = Nothing,
      _sbCertificateOverride = Nothing,
      _sbComputeTypeOverride = Nothing,
      _sbPrivilegedModeOverride = Nothing,
      _sbSourceVersion = Nothing,
      _sbBuildspecOverride = Nothing,
      _sbSecondarySourcesVersionOverride = Nothing,
      _sbReportBuildStatusOverride = Nothing,
      _sbInsecureSSLOverride = Nothing,
      _sbImageOverride = Nothing,
      _sbSecondaryArtifactsOverride = Nothing,
      _sbArtifactsOverride = Nothing,
      _sbSourceTypeOverride = Nothing,
      _sbProjectName = pProjectName_
    }

-- | The AWS Key Management Service (AWS KMS) customer master key (CMK) that overrides the one specified in the build project. The CMK key encrypts the build output artifacts. You can specify either the Amazon Resource Name (ARN) of the CMK or, if available, the CMK's alias (using the format @alias/<alias-name>@ ).
sbEncryptionKeyOverride :: Lens' StartBuild (Maybe Text)
sbEncryptionKeyOverride = lens _sbEncryptionKeyOverride (\s a -> s {_sbEncryptionKeyOverride = a})

-- | A location that overrides, for this build, the source location for the one defined in the build project.
sbSourceLocationOverride :: Lens' StartBuild (Maybe Text)
sbSourceLocationOverride = lens _sbSourceLocationOverride (\s a -> s {_sbSourceLocationOverride = a})

-- | A set of environment variables that overrides, for this build only, the latest ones already defined in the build project.
sbEnvironmentVariablesOverride :: Lens' StartBuild [EnvironmentVariable]
sbEnvironmentVariablesOverride = lens _sbEnvironmentVariablesOverride (\s a -> s {_sbEnvironmentVariablesOverride = a}) . _Default . _Coerce

-- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
sbBuildStatusConfigOverride :: Lens' StartBuild (Maybe BuildStatusConfig)
sbBuildStatusConfigOverride = lens _sbBuildStatusConfigOverride (\s a -> s {_sbBuildStatusConfigOverride = a})

-- | A unique, case sensitive identifier you provide to ensure the idempotency of the StartBuild request. The token is included in the StartBuild request and is valid for 5 minutes. If you repeat the StartBuild request with the same token, but change a parameter, AWS CodeBuild returns a parameter mismatch error.
sbIdempotencyToken :: Lens' StartBuild (Maybe Text)
sbIdempotencyToken = lens _sbIdempotencyToken (\s a -> s {_sbIdempotencyToken = a})

-- | Specifies if session debugging is enabled for this build. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/session-manager.html Viewing a running build in Session Manager> .
sbDebugSessionEnabled :: Lens' StartBuild (Maybe Bool)
sbDebugSessionEnabled = lens _sbDebugSessionEnabled (\s a -> s {_sbDebugSessionEnabled = a})

-- | The credentials for access to a private registry.
sbRegistryCredentialOverride :: Lens' StartBuild (Maybe RegistryCredential)
sbRegistryCredentialOverride = lens _sbRegistryCredentialOverride (\s a -> s {_sbRegistryCredentialOverride = a})

-- | The number of build timeout minutes, from 5 to 480 (8 hours), that overrides, for this build only, the latest setting already defined in the build project.
sbTimeoutInMinutesOverride :: Lens' StartBuild (Maybe Natural)
sbTimeoutInMinutesOverride = lens _sbTimeoutInMinutesOverride (\s a -> s {_sbTimeoutInMinutesOverride = a}) . mapping _Nat

-- | The name of a service role for this build that overrides the one specified in the build project.
sbServiceRoleOverride :: Lens' StartBuild (Maybe Text)
sbServiceRoleOverride = lens _sbServiceRoleOverride (\s a -> s {_sbServiceRoleOverride = a})

-- | A ProjectCache object specified for this build that overrides the one defined in the build project.
sbCacheOverride :: Lens' StartBuild (Maybe ProjectCache)
sbCacheOverride = lens _sbCacheOverride (\s a -> s {_sbCacheOverride = a})

-- | The number of minutes a build is allowed to be queued before it times out.
sbQueuedTimeoutInMinutesOverride :: Lens' StartBuild (Maybe Natural)
sbQueuedTimeoutInMinutesOverride = lens _sbQueuedTimeoutInMinutesOverride (\s a -> s {_sbQueuedTimeoutInMinutesOverride = a}) . mapping _Nat

-- | An array of @ProjectSource@ objects.
sbSecondarySourcesOverride :: Lens' StartBuild [ProjectSource]
sbSecondarySourcesOverride = lens _sbSecondarySourcesOverride (\s a -> s {_sbSecondarySourcesOverride = a}) . _Default . _Coerce

-- | The user-defined depth of history, with a minimum value of 0, that overrides, for this build only, any previous depth of history defined in the build project.
sbGitCloneDepthOverride :: Lens' StartBuild (Maybe Natural)
sbGitCloneDepthOverride = lens _sbGitCloneDepthOverride (\s a -> s {_sbGitCloneDepthOverride = a}) . mapping _Nat

-- | The type of credentials AWS CodeBuild uses to pull images in your build. There are two valid values:      * CODEBUILD    * Specifies that AWS CodeBuild uses its own credentials. This requires that you modify your ECR repository policy to trust AWS CodeBuild's service principal.     * SERVICE_ROLE    * Specifies that AWS CodeBuild uses your build project's service role.  When using a cross-account or private registry image, you must use @SERVICE_ROLE@ credentials. When using an AWS CodeBuild curated image, you must use @CODEBUILD@ credentials.
sbImagePullCredentialsTypeOverride :: Lens' StartBuild (Maybe ImagePullCredentialsType)
sbImagePullCredentialsTypeOverride = lens _sbImagePullCredentialsTypeOverride (\s a -> s {_sbImagePullCredentialsTypeOverride = a})

-- | Log settings for this build that override the log settings defined in the build project.
sbLogsConfigOverride :: Lens' StartBuild (Maybe LogsConfig)
sbLogsConfigOverride = lens _sbLogsConfigOverride (\s a -> s {_sbLogsConfigOverride = a})

-- | An authorization type for this build that overrides the one defined in the build project. This override applies only if the build project's source is BitBucket or GitHub.
sbSourceAuthOverride :: Lens' StartBuild (Maybe SourceAuth)
sbSourceAuthOverride = lens _sbSourceAuthOverride (\s a -> s {_sbSourceAuthOverride = a})

-- | Information about the Git submodules configuration for this build of an AWS CodeBuild build project.
sbGitSubmodulesConfigOverride :: Lens' StartBuild (Maybe GitSubmodulesConfig)
sbGitSubmodulesConfigOverride = lens _sbGitSubmodulesConfigOverride (\s a -> s {_sbGitSubmodulesConfigOverride = a})

-- | A container type for this build that overrides the one specified in the build project.
sbEnvironmentTypeOverride :: Lens' StartBuild (Maybe EnvironmentType)
sbEnvironmentTypeOverride = lens _sbEnvironmentTypeOverride (\s a -> s {_sbEnvironmentTypeOverride = a})

-- | The name of a certificate for this build that overrides the one specified in the build project.
sbCertificateOverride :: Lens' StartBuild (Maybe Text)
sbCertificateOverride = lens _sbCertificateOverride (\s a -> s {_sbCertificateOverride = a})

-- | The name of a compute type for this build that overrides the one specified in the build project.
sbComputeTypeOverride :: Lens' StartBuild (Maybe ComputeType)
sbComputeTypeOverride = lens _sbComputeTypeOverride (\s a -> s {_sbComputeTypeOverride = a})

-- | Enable this flag to override privileged mode in the build project.
sbPrivilegedModeOverride :: Lens' StartBuild (Maybe Bool)
sbPrivilegedModeOverride = lens _sbPrivilegedModeOverride (\s a -> s {_sbPrivilegedModeOverride = a})

-- | The version of the build input to be built, for this build only. If not specified, the latest version is used. If specified, the contents depends on the source provider:     * AWS CodeCommit    * The commit ID, branch, or Git tag to use.     * GitHub    * The commit ID, pull request ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a pull request ID is specified, it must use the format @pr/pull-request-ID@ (for example @pr/25@ ). If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * Bitbucket    * The commit ID, branch name, or tag name that corresponds to the version of the source code you want to build. If a branch name is specified, the branch's HEAD commit ID is used. If not specified, the default branch's HEAD commit ID is used.     * Amazon Simple Storage Service (Amazon S3)    * The version ID of the object that represents the build input ZIP file to use. If @sourceVersion@ is specified at the project level, then this @sourceVersion@ (at the build level) takes precedence.  For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild> in the /AWS CodeBuild User Guide/ .
sbSourceVersion :: Lens' StartBuild (Maybe Text)
sbSourceVersion = lens _sbSourceVersion (\s a -> s {_sbSourceVersion = a})

-- | A buildspec file declaration that overrides, for this build only, the latest one already defined in the build project. If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
sbBuildspecOverride :: Lens' StartBuild (Maybe Text)
sbBuildspecOverride = lens _sbBuildspecOverride (\s a -> s {_sbBuildspecOverride = a})

-- | An array of @ProjectSourceVersion@ objects that specify one or more versions of the project's secondary sources to be used for this build only.
sbSecondarySourcesVersionOverride :: Lens' StartBuild [ProjectSourceVersion]
sbSecondarySourcesVersionOverride = lens _sbSecondarySourcesVersionOverride (\s a -> s {_sbSecondarySourcesVersionOverride = a}) . _Default . _Coerce

-- | Set to true to report to your source provider the status of a build's start and completion. If you use this option with a source provider other than GitHub, GitHub Enterprise, or Bitbucket, an invalidInputException is thrown.
sbReportBuildStatusOverride :: Lens' StartBuild (Maybe Bool)
sbReportBuildStatusOverride = lens _sbReportBuildStatusOverride (\s a -> s {_sbReportBuildStatusOverride = a})

-- | Enable this flag to override the insecure SSL setting that is specified in the build project. The insecure SSL setting determines whether to ignore SSL warnings while connecting to the project source code. This override applies only if the build's source is GitHub Enterprise.
sbInsecureSSLOverride :: Lens' StartBuild (Maybe Bool)
sbInsecureSSLOverride = lens _sbInsecureSSLOverride (\s a -> s {_sbInsecureSSLOverride = a})

-- | The name of an image for this build that overrides the one specified in the build project.
sbImageOverride :: Lens' StartBuild (Maybe Text)
sbImageOverride = lens _sbImageOverride (\s a -> s {_sbImageOverride = a})

-- | An array of @ProjectArtifacts@ objects.
sbSecondaryArtifactsOverride :: Lens' StartBuild [ProjectArtifacts]
sbSecondaryArtifactsOverride = lens _sbSecondaryArtifactsOverride (\s a -> s {_sbSecondaryArtifactsOverride = a}) . _Default . _Coerce

-- | Build output artifact settings that override, for this build only, the latest ones already defined in the build project.
sbArtifactsOverride :: Lens' StartBuild (Maybe ProjectArtifacts)
sbArtifactsOverride = lens _sbArtifactsOverride (\s a -> s {_sbArtifactsOverride = a})

-- | A source input type, for this build, that overrides the source input defined in the build project.
sbSourceTypeOverride :: Lens' StartBuild (Maybe SourceType)
sbSourceTypeOverride = lens _sbSourceTypeOverride (\s a -> s {_sbSourceTypeOverride = a})

-- | The name of the AWS CodeBuild build project to start running a build.
sbProjectName :: Lens' StartBuild Text
sbProjectName = lens _sbProjectName (\s a -> s {_sbProjectName = a})

instance AWSRequest StartBuild where
  type Rs StartBuild = StartBuildResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          StartBuildResponse' <$> (x .?> "build") <*> (pure (fromEnum s))
      )

instance Hashable StartBuild

instance NFData StartBuild

instance ToHeaders StartBuild where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("CodeBuild_20161006.StartBuild" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartBuild where
  toJSON StartBuild' {..} =
    object
      ( catMaybes
          [ ("encryptionKeyOverride" .=) <$> _sbEncryptionKeyOverride,
            ("sourceLocationOverride" .=) <$> _sbSourceLocationOverride,
            ("environmentVariablesOverride" .=)
              <$> _sbEnvironmentVariablesOverride,
            ("buildStatusConfigOverride" .=) <$> _sbBuildStatusConfigOverride,
            ("idempotencyToken" .=) <$> _sbIdempotencyToken,
            ("debugSessionEnabled" .=) <$> _sbDebugSessionEnabled,
            ("registryCredentialOverride" .=)
              <$> _sbRegistryCredentialOverride,
            ("timeoutInMinutesOverride" .=) <$> _sbTimeoutInMinutesOverride,
            ("serviceRoleOverride" .=) <$> _sbServiceRoleOverride,
            ("cacheOverride" .=) <$> _sbCacheOverride,
            ("queuedTimeoutInMinutesOverride" .=)
              <$> _sbQueuedTimeoutInMinutesOverride,
            ("secondarySourcesOverride" .=) <$> _sbSecondarySourcesOverride,
            ("gitCloneDepthOverride" .=) <$> _sbGitCloneDepthOverride,
            ("imagePullCredentialsTypeOverride" .=)
              <$> _sbImagePullCredentialsTypeOverride,
            ("logsConfigOverride" .=) <$> _sbLogsConfigOverride,
            ("sourceAuthOverride" .=) <$> _sbSourceAuthOverride,
            ("gitSubmodulesConfigOverride" .=)
              <$> _sbGitSubmodulesConfigOverride,
            ("environmentTypeOverride" .=) <$> _sbEnvironmentTypeOverride,
            ("certificateOverride" .=) <$> _sbCertificateOverride,
            ("computeTypeOverride" .=) <$> _sbComputeTypeOverride,
            ("privilegedModeOverride" .=) <$> _sbPrivilegedModeOverride,
            ("sourceVersion" .=) <$> _sbSourceVersion,
            ("buildspecOverride" .=) <$> _sbBuildspecOverride,
            ("secondarySourcesVersionOverride" .=)
              <$> _sbSecondarySourcesVersionOverride,
            ("reportBuildStatusOverride" .=) <$> _sbReportBuildStatusOverride,
            ("insecureSslOverride" .=) <$> _sbInsecureSSLOverride,
            ("imageOverride" .=) <$> _sbImageOverride,
            ("secondaryArtifactsOverride" .=)
              <$> _sbSecondaryArtifactsOverride,
            ("artifactsOverride" .=) <$> _sbArtifactsOverride,
            ("sourceTypeOverride" .=) <$> _sbSourceTypeOverride,
            Just ("projectName" .= _sbProjectName)
          ]
      )

instance ToPath StartBuild where
  toPath = const "/"

instance ToQuery StartBuild where
  toQuery = const mempty

-- | /See:/ 'startBuildResponse' smart constructor.
data StartBuildResponse = StartBuildResponse'
  { _srsBuild ::
      !(Maybe Build),
    _srsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsBuild' - Information about the build to be run.
--
-- * 'srsResponseStatus' - -- | The response status code.
startBuildResponse ::
  -- | 'srsResponseStatus'
  Int ->
  StartBuildResponse
startBuildResponse pResponseStatus_ =
  StartBuildResponse'
    { _srsBuild = Nothing,
      _srsResponseStatus = pResponseStatus_
    }

-- | Information about the build to be run.
srsBuild :: Lens' StartBuildResponse (Maybe Build)
srsBuild = lens _srsBuild (\s a -> s {_srsBuild = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' StartBuildResponse Int
srsResponseStatus = lens _srsResponseStatus (\s a -> s {_srsResponseStatus = a})

instance NFData StartBuildResponse
