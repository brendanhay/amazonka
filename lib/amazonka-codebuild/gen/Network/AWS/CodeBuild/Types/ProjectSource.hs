-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectSource
  ( ProjectSource (..),

    -- * Smart constructor
    mkProjectSource,

    -- * Lenses
    psReportBuildStatus,
    psInsecureSSL,
    psLocation,
    psAuth,
    psBuildspec,
    psSourceIdentifier,
    psGitCloneDepth,
    psGitSubmodulesConfig,
    psBuildStatusConfig,
    psType,
  )
where

import Network.AWS.CodeBuild.Types.BuildStatusConfig
import Network.AWS.CodeBuild.Types.GitSubmodulesConfig
import Network.AWS.CodeBuild.Types.SourceAuth
import Network.AWS.CodeBuild.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the build input source code for the build project.
--
-- /See:/ 'mkProjectSource' smart constructor.
data ProjectSource = ProjectSource'
  { reportBuildStatus ::
      Lude.Maybe Lude.Bool,
    insecureSSL :: Lude.Maybe Lude.Bool,
    location :: Lude.Maybe Lude.Text,
    auth :: Lude.Maybe SourceAuth,
    buildspec :: Lude.Maybe Lude.Text,
    sourceIdentifier :: Lude.Maybe Lude.Text,
    gitCloneDepth :: Lude.Maybe Lude.Natural,
    gitSubmodulesConfig :: Lude.Maybe GitSubmodulesConfig,
    buildStatusConfig :: Lude.Maybe BuildStatusConfig,
    type' :: SourceType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectSource' with the minimum fields required to make a request.
--
-- * 'auth' - Information about the authorization settings for AWS CodeBuild to access the source code to be built.
--
-- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
-- * 'buildStatusConfig' - Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
-- * 'buildspec' - The buildspec file declaration to use for the builds in this build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
-- * 'gitCloneDepth' - Information about the Git clone depth for the build project.
-- * 'gitSubmodulesConfig' - Information about the Git submodules configuration for the build project.
-- * 'insecureSSL' - Enable this flag to ignore SSL warnings while connecting to the project source code.
-- * 'location' - Information about the location of the source code to be built. Valid values include:
--
--
--     * For source code settings that are specified in the source action of a pipeline in AWS CodePipeline, @location@ should not be specified. If it is specified, AWS CodePipeline ignores it. This is because AWS CodePipeline uses the settings in a pipeline's source action instead of this value.
--
--
--     * For source code in an AWS CodeCommit repository, the HTTPS clone URL to the repository that contains the source code and the buildspec file (for example, @https://git-codecommit.<region-ID>.amazonaws.com/v1/repos/<repo-name>@ ).
--
--
--     * For source code in an Amazon Simple Storage Service (Amazon S3) input bucket, one of the following.
--
--     * The path to the ZIP file that contains the source code (for example, @<bucket-name>/<path>/<object-name>.zip@ ).
--
--
--     * The path to the folder that contains the source code (for example, @<bucket-name>/<path-to-source-code>/<folder>/@ ).
--
--
--
--
--     * For source code in a GitHub repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your GitHub account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with GitHub, on the GitHub __Authorize application__ page, for __Organization access__ , choose __Request access__ next to each repository you want to allow AWS CodeBuild to have access to, and then choose __Authorize application__ . (After you have connected to your GitHub account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
--
--
--     * For source code in a Bitbucket repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your Bitbucket account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with Bitbucket, on the Bitbucket __Confirm access to your account__ page, choose __Grant access__ . (After you have connected to your Bitbucket account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
--
--
-- * 'reportBuildStatus' - Set to true to report the status of a build's start and finish to your source provider. This option is valid only when your source provider is GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a different source provider, an invalidInputException is thrown.
-- * 'sourceIdentifier' - An identifier for this project source.
-- * 'type'' - The type of repository that contains the source code to be built. Valid values include:
--
--
--     * @BITBUCKET@ : The source code is in a Bitbucket repository.
--
--
--     * @CODECOMMIT@ : The source code is in an AWS CodeCommit repository.
--
--
--     * @CODEPIPELINE@ : The source code settings are specified in the source action of a pipeline in AWS CodePipeline.
--
--
--     * @GITHUB@ : The source code is in a GitHub or GitHub Enterprise Cloud repository.
--
--
--     * @GITHUB_ENTERPRISE@ : The source code is in a GitHub Enterprise Server repository.
--
--
--     * @NO_SOURCE@ : The project does not have input source code.
--
--
--     * @S3@ : The source code is in an Amazon Simple Storage Service (Amazon S3) input bucket.
mkProjectSource ::
  -- | 'type''
  SourceType ->
  ProjectSource
mkProjectSource pType_ =
  ProjectSource'
    { reportBuildStatus = Lude.Nothing,
      insecureSSL = Lude.Nothing,
      location = Lude.Nothing,
      auth = Lude.Nothing,
      buildspec = Lude.Nothing,
      sourceIdentifier = Lude.Nothing,
      gitCloneDepth = Lude.Nothing,
      gitSubmodulesConfig = Lude.Nothing,
      buildStatusConfig = Lude.Nothing,
      type' = pType_
    }

-- | Set to true to report the status of a build's start and finish to your source provider. This option is valid only when your source provider is GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a different source provider, an invalidInputException is thrown.
--
-- /Note:/ Consider using 'reportBuildStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psReportBuildStatus :: Lens.Lens' ProjectSource (Lude.Maybe Lude.Bool)
psReportBuildStatus = Lens.lens (reportBuildStatus :: ProjectSource -> Lude.Maybe Lude.Bool) (\s a -> s {reportBuildStatus = a} :: ProjectSource)
{-# DEPRECATED psReportBuildStatus "Use generic-lens or generic-optics with 'reportBuildStatus' instead." #-}

-- | Enable this flag to ignore SSL warnings while connecting to the project source code.
--
-- /Note:/ Consider using 'insecureSSL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psInsecureSSL :: Lens.Lens' ProjectSource (Lude.Maybe Lude.Bool)
psInsecureSSL = Lens.lens (insecureSSL :: ProjectSource -> Lude.Maybe Lude.Bool) (\s a -> s {insecureSSL = a} :: ProjectSource)
{-# DEPRECATED psInsecureSSL "Use generic-lens or generic-optics with 'insecureSSL' instead." #-}

-- | Information about the location of the source code to be built. Valid values include:
--
--
--     * For source code settings that are specified in the source action of a pipeline in AWS CodePipeline, @location@ should not be specified. If it is specified, AWS CodePipeline ignores it. This is because AWS CodePipeline uses the settings in a pipeline's source action instead of this value.
--
--
--     * For source code in an AWS CodeCommit repository, the HTTPS clone URL to the repository that contains the source code and the buildspec file (for example, @https://git-codecommit.<region-ID>.amazonaws.com/v1/repos/<repo-name>@ ).
--
--
--     * For source code in an Amazon Simple Storage Service (Amazon S3) input bucket, one of the following.
--
--     * The path to the ZIP file that contains the source code (for example, @<bucket-name>/<path>/<object-name>.zip@ ).
--
--
--     * The path to the folder that contains the source code (for example, @<bucket-name>/<path-to-source-code>/<folder>/@ ).
--
--
--
--
--     * For source code in a GitHub repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your GitHub account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with GitHub, on the GitHub __Authorize application__ page, for __Organization access__ , choose __Request access__ next to each repository you want to allow AWS CodeBuild to have access to, and then choose __Authorize application__ . (After you have connected to your GitHub account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
--
--
--     * For source code in a Bitbucket repository, the HTTPS clone URL to the repository that contains the source and the buildspec file. You must connect your AWS account to your Bitbucket account. Use the AWS CodeBuild console to start creating a build project. When you use the console to connect (or reconnect) with Bitbucket, on the Bitbucket __Confirm access to your account__ page, choose __Grant access__ . (After you have connected to your Bitbucket account, you do not need to finish creating the build project. You can leave the AWS CodeBuild console.) To instruct AWS CodeBuild to use this connection, in the @source@ object, set the @auth@ object's @type@ value to @OAUTH@ .
--
--
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psLocation :: Lens.Lens' ProjectSource (Lude.Maybe Lude.Text)
psLocation = Lens.lens (location :: ProjectSource -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: ProjectSource)
{-# DEPRECATED psLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built.
--
-- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAuth :: Lens.Lens' ProjectSource (Lude.Maybe SourceAuth)
psAuth = Lens.lens (auth :: ProjectSource -> Lude.Maybe SourceAuth) (\s a -> s {auth = a} :: ProjectSource)
{-# DEPRECATED psAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | The buildspec file declaration to use for the builds in this build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- /Note:/ Consider using 'buildspec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBuildspec :: Lens.Lens' ProjectSource (Lude.Maybe Lude.Text)
psBuildspec = Lens.lens (buildspec :: ProjectSource -> Lude.Maybe Lude.Text) (\s a -> s {buildspec = a} :: ProjectSource)
{-# DEPRECATED psBuildspec "Use generic-lens or generic-optics with 'buildspec' instead." #-}

-- | An identifier for this project source.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSourceIdentifier :: Lens.Lens' ProjectSource (Lude.Maybe Lude.Text)
psSourceIdentifier = Lens.lens (sourceIdentifier :: ProjectSource -> Lude.Maybe Lude.Text) (\s a -> s {sourceIdentifier = a} :: ProjectSource)
{-# DEPRECATED psSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

-- | Information about the Git clone depth for the build project.
--
-- /Note:/ Consider using 'gitCloneDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psGitCloneDepth :: Lens.Lens' ProjectSource (Lude.Maybe Lude.Natural)
psGitCloneDepth = Lens.lens (gitCloneDepth :: ProjectSource -> Lude.Maybe Lude.Natural) (\s a -> s {gitCloneDepth = a} :: ProjectSource)
{-# DEPRECATED psGitCloneDepth "Use generic-lens or generic-optics with 'gitCloneDepth' instead." #-}

-- | Information about the Git submodules configuration for the build project.
--
-- /Note:/ Consider using 'gitSubmodulesConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psGitSubmodulesConfig :: Lens.Lens' ProjectSource (Lude.Maybe GitSubmodulesConfig)
psGitSubmodulesConfig = Lens.lens (gitSubmodulesConfig :: ProjectSource -> Lude.Maybe GitSubmodulesConfig) (\s a -> s {gitSubmodulesConfig = a} :: ProjectSource)
{-# DEPRECATED psGitSubmodulesConfig "Use generic-lens or generic-optics with 'gitSubmodulesConfig' instead." #-}

-- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
--
-- /Note:/ Consider using 'buildStatusConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBuildStatusConfig :: Lens.Lens' ProjectSource (Lude.Maybe BuildStatusConfig)
psBuildStatusConfig = Lens.lens (buildStatusConfig :: ProjectSource -> Lude.Maybe BuildStatusConfig) (\s a -> s {buildStatusConfig = a} :: ProjectSource)
{-# DEPRECATED psBuildStatusConfig "Use generic-lens or generic-optics with 'buildStatusConfig' instead." #-}

-- | The type of repository that contains the source code to be built. Valid values include:
--
--
--     * @BITBUCKET@ : The source code is in a Bitbucket repository.
--
--
--     * @CODECOMMIT@ : The source code is in an AWS CodeCommit repository.
--
--
--     * @CODEPIPELINE@ : The source code settings are specified in the source action of a pipeline in AWS CodePipeline.
--
--
--     * @GITHUB@ : The source code is in a GitHub or GitHub Enterprise Cloud repository.
--
--
--     * @GITHUB_ENTERPRISE@ : The source code is in a GitHub Enterprise Server repository.
--
--
--     * @NO_SOURCE@ : The project does not have input source code.
--
--
--     * @S3@ : The source code is in an Amazon Simple Storage Service (Amazon S3) input bucket.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psType :: Lens.Lens' ProjectSource SourceType
psType = Lens.lens (type' :: ProjectSource -> SourceType) (\s a -> s {type' = a} :: ProjectSource)
{-# DEPRECATED psType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ProjectSource where
  parseJSON =
    Lude.withObject
      "ProjectSource"
      ( \x ->
          ProjectSource'
            Lude.<$> (x Lude..:? "reportBuildStatus")
            Lude.<*> (x Lude..:? "insecureSsl")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "auth")
            Lude.<*> (x Lude..:? "buildspec")
            Lude.<*> (x Lude..:? "sourceIdentifier")
            Lude.<*> (x Lude..:? "gitCloneDepth")
            Lude.<*> (x Lude..:? "gitSubmodulesConfig")
            Lude.<*> (x Lude..:? "buildStatusConfig")
            Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON ProjectSource where
  toJSON ProjectSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("reportBuildStatus" Lude..=) Lude.<$> reportBuildStatus,
            ("insecureSsl" Lude..=) Lude.<$> insecureSSL,
            ("location" Lude..=) Lude.<$> location,
            ("auth" Lude..=) Lude.<$> auth,
            ("buildspec" Lude..=) Lude.<$> buildspec,
            ("sourceIdentifier" Lude..=) Lude.<$> sourceIdentifier,
            ("gitCloneDepth" Lude..=) Lude.<$> gitCloneDepth,
            ("gitSubmodulesConfig" Lude..=) Lude.<$> gitSubmodulesConfig,
            ("buildStatusConfig" Lude..=) Lude.<$> buildStatusConfig,
            Lude.Just ("type" Lude..= type')
          ]
      )
