{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectSource where

import Network.AWS.CodeBuild.Types.BuildStatusConfig
import Network.AWS.CodeBuild.Types.GitSubmodulesConfig
import Network.AWS.CodeBuild.Types.SourceAuth
import Network.AWS.CodeBuild.Types.SourceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the build input source code for the build project.
--
--
--
-- /See:/ 'projectSource' smart constructor.
data ProjectSource = ProjectSource'
  { _psReportBuildStatus ::
      !(Maybe Bool),
    _psInsecureSSL :: !(Maybe Bool),
    _psLocation :: !(Maybe Text),
    _psAuth :: !(Maybe SourceAuth),
    _psBuildspec :: !(Maybe Text),
    _psSourceIdentifier :: !(Maybe Text),
    _psGitCloneDepth :: !(Maybe Nat),
    _psGitSubmodulesConfig :: !(Maybe GitSubmodulesConfig),
    _psBuildStatusConfig :: !(Maybe BuildStatusConfig),
    _psType :: !SourceType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProjectSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psReportBuildStatus' - Set to true to report the status of a build's start and finish to your source provider. This option is valid only when your source provider is GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a different source provider, an invalidInputException is thrown.
--
-- * 'psInsecureSSL' - Enable this flag to ignore SSL warnings while connecting to the project source code.
--
-- * 'psLocation' - Information about the location of the source code to be built. Valid values include:     * For source code settings that are specified in the source action of a pipeline in AWS CodePipeline, @location@ should not be specified. If it is specified, AWS CodePipeline ignores it. This is because AWS CodePipeline uses the settings in a pipeline's source action instead of this value.     * For source code in an AWS CodeCommit repository, the HTTPS clone URL to the repository that contains the source code and the buildspec file (for example, @https://git-codecommit.<region-ID>.amazonaws.com/v1/repos/<repo-name>@ ).     * For source code in an Amazon Simple Storage Service (Amazon S3) input bucket, one of the following.      * The path to the ZIP file that contains the source code (for example, @<bucket-name>/<path>/<object-name>.zip@ ).      * The path to the folder that contains the source code (for example, @<bucket-name>/<path-to-source-code>/<folder>/@ ).      * For source code in a GitHub repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your GitHub account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with GitHub, on the GitHub __Authorize application__ page, for __Organization access__ , choose __Request access__ next to each repository you want to allow AWS CodeBuild to have access to, and then choose __Authorize application__ . (After you have connected to your GitHub account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .     * For source code in a Bitbucket repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your Bitbucket account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with Bitbucket, on the Bitbucket __Confirm access to your account__ page, choose __Grant access__ . (After you have connected to your Bitbucket account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
--
-- * 'psAuth' - Information about the authorization settings for AWS CodeBuild to access the source code to be built. This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
--
-- * 'psBuildspec' - The buildspec file declaration to use for the builds in this build project. If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- * 'psSourceIdentifier' - An identifier for this project source.
--
-- * 'psGitCloneDepth' - Information about the Git clone depth for the build project.
--
-- * 'psGitSubmodulesConfig' - Information about the Git submodules configuration for the build project.
--
-- * 'psBuildStatusConfig' - Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
--
-- * 'psType' - The type of repository that contains the source code to be built. Valid values include:     * @BITBUCKET@ : The source code is in a Bitbucket repository.     * @CODECOMMIT@ : The source code is in an AWS CodeCommit repository.     * @CODEPIPELINE@ : The source code settings are specified in the source action of a pipeline in AWS CodePipeline.     * @GITHUB@ : The source code is in a GitHub or GitHub Enterprise Cloud repository.     * @GITHUB_ENTERPRISE@ : The source code is in a GitHub Enterprise Server repository.     * @NO_SOURCE@ : The project does not have input source code.     * @S3@ : The source code is in an Amazon Simple Storage Service (Amazon S3) input bucket.
projectSource ::
  -- | 'psType'
  SourceType ->
  ProjectSource
projectSource pType_ =
  ProjectSource'
    { _psReportBuildStatus = Nothing,
      _psInsecureSSL = Nothing,
      _psLocation = Nothing,
      _psAuth = Nothing,
      _psBuildspec = Nothing,
      _psSourceIdentifier = Nothing,
      _psGitCloneDepth = Nothing,
      _psGitSubmodulesConfig = Nothing,
      _psBuildStatusConfig = Nothing,
      _psType = pType_
    }

-- | Set to true to report the status of a build's start and finish to your source provider. This option is valid only when your source provider is GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a different source provider, an invalidInputException is thrown.
psReportBuildStatus :: Lens' ProjectSource (Maybe Bool)
psReportBuildStatus = lens _psReportBuildStatus (\s a -> s {_psReportBuildStatus = a})

-- | Enable this flag to ignore SSL warnings while connecting to the project source code.
psInsecureSSL :: Lens' ProjectSource (Maybe Bool)
psInsecureSSL = lens _psInsecureSSL (\s a -> s {_psInsecureSSL = a})

-- | Information about the location of the source code to be built. Valid values include:     * For source code settings that are specified in the source action of a pipeline in AWS CodePipeline, @location@ should not be specified. If it is specified, AWS CodePipeline ignores it. This is because AWS CodePipeline uses the settings in a pipeline's source action instead of this value.     * For source code in an AWS CodeCommit repository, the HTTPS clone URL to the repository that contains the source code and the buildspec file (for example, @https://git-codecommit.<region-ID>.amazonaws.com/v1/repos/<repo-name>@ ).     * For source code in an Amazon Simple Storage Service (Amazon S3) input bucket, one of the following.      * The path to the ZIP file that contains the source code (for example, @<bucket-name>/<path>/<object-name>.zip@ ).      * The path to the folder that contains the source code (for example, @<bucket-name>/<path-to-source-code>/<folder>/@ ).      * For source code in a GitHub repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your GitHub account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with GitHub, on the GitHub __Authorize application__ page, for __Organization access__ , choose __Request access__ next to each repository you want to allow AWS CodeBuild to have access to, and then choose __Authorize application__ . (After you have connected to your GitHub account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .     * For source code in a Bitbucket repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your Bitbucket account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with Bitbucket, on the Bitbucket __Confirm access to your account__ page, choose __Grant access__ . (After you have connected to your Bitbucket account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
psLocation :: Lens' ProjectSource (Maybe Text)
psLocation = lens _psLocation (\s a -> s {_psLocation = a})

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built. This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
psAuth :: Lens' ProjectSource (Maybe SourceAuth)
psAuth = lens _psAuth (\s a -> s {_psAuth = a})

-- | The buildspec file declaration to use for the builds in this build project. If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
psBuildspec :: Lens' ProjectSource (Maybe Text)
psBuildspec = lens _psBuildspec (\s a -> s {_psBuildspec = a})

-- | An identifier for this project source.
psSourceIdentifier :: Lens' ProjectSource (Maybe Text)
psSourceIdentifier = lens _psSourceIdentifier (\s a -> s {_psSourceIdentifier = a})

-- | Information about the Git clone depth for the build project.
psGitCloneDepth :: Lens' ProjectSource (Maybe Natural)
psGitCloneDepth = lens _psGitCloneDepth (\s a -> s {_psGitCloneDepth = a}) . mapping _Nat

-- | Information about the Git submodules configuration for the build project.
psGitSubmodulesConfig :: Lens' ProjectSource (Maybe GitSubmodulesConfig)
psGitSubmodulesConfig = lens _psGitSubmodulesConfig (\s a -> s {_psGitSubmodulesConfig = a})

-- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
psBuildStatusConfig :: Lens' ProjectSource (Maybe BuildStatusConfig)
psBuildStatusConfig = lens _psBuildStatusConfig (\s a -> s {_psBuildStatusConfig = a})

-- | The type of repository that contains the source code to be built. Valid values include:     * @BITBUCKET@ : The source code is in a Bitbucket repository.     * @CODECOMMIT@ : The source code is in an AWS CodeCommit repository.     * @CODEPIPELINE@ : The source code settings are specified in the source action of a pipeline in AWS CodePipeline.     * @GITHUB@ : The source code is in a GitHub or GitHub Enterprise Cloud repository.     * @GITHUB_ENTERPRISE@ : The source code is in a GitHub Enterprise Server repository.     * @NO_SOURCE@ : The project does not have input source code.     * @S3@ : The source code is in an Amazon Simple Storage Service (Amazon S3) input bucket.
psType :: Lens' ProjectSource SourceType
psType = lens _psType (\s a -> s {_psType = a})

instance FromJSON ProjectSource where
  parseJSON =
    withObject
      "ProjectSource"
      ( \x ->
          ProjectSource'
            <$> (x .:? "reportBuildStatus")
            <*> (x .:? "insecureSsl")
            <*> (x .:? "location")
            <*> (x .:? "auth")
            <*> (x .:? "buildspec")
            <*> (x .:? "sourceIdentifier")
            <*> (x .:? "gitCloneDepth")
            <*> (x .:? "gitSubmodulesConfig")
            <*> (x .:? "buildStatusConfig")
            <*> (x .: "type")
      )

instance Hashable ProjectSource

instance NFData ProjectSource

instance ToJSON ProjectSource where
  toJSON ProjectSource' {..} =
    object
      ( catMaybes
          [ ("reportBuildStatus" .=) <$> _psReportBuildStatus,
            ("insecureSsl" .=) <$> _psInsecureSSL,
            ("location" .=) <$> _psLocation,
            ("auth" .=) <$> _psAuth,
            ("buildspec" .=) <$> _psBuildspec,
            ("sourceIdentifier" .=) <$> _psSourceIdentifier,
            ("gitCloneDepth" .=) <$> _psGitCloneDepth,
            ("gitSubmodulesConfig" .=) <$> _psGitSubmodulesConfig,
            ("buildStatusConfig" .=) <$> _psBuildStatusConfig,
            Just ("type" .= _psType)
          ]
      )
