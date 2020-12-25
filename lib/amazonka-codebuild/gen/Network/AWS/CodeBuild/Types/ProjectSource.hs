{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    psType,
    psAuth,
    psBuildStatusConfig,
    psBuildspec,
    psGitCloneDepth,
    psGitSubmodulesConfig,
    psInsecureSsl,
    psLocation,
    psReportBuildStatus,
    psSourceIdentifier,
  )
where

import qualified Network.AWS.CodeBuild.Types.BuildStatusConfig as Types
import qualified Network.AWS.CodeBuild.Types.GitSubmodulesConfig as Types
import qualified Network.AWS.CodeBuild.Types.SourceAuth as Types
import qualified Network.AWS.CodeBuild.Types.SourceType as Types
import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the build input source code for the build project.
--
-- /See:/ 'mkProjectSource' smart constructor.
data ProjectSource = ProjectSource'
  { -- | The type of repository that contains the source code to be built. Valid values include:
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
    type' :: Types.SourceType,
    -- | Information about the authorization settings for AWS CodeBuild to access the source code to be built.
    --
    -- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
    auth :: Core.Maybe Types.SourceAuth,
    -- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
    buildStatusConfig :: Core.Maybe Types.BuildStatusConfig,
    -- | The buildspec file declaration to use for the builds in this build project.
    --
    -- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
    buildspec :: Core.Maybe Types.String,
    -- | Information about the Git clone depth for the build project.
    gitCloneDepth :: Core.Maybe Core.Natural,
    -- | Information about the Git submodules configuration for the build project.
    gitSubmodulesConfig :: Core.Maybe Types.GitSubmodulesConfig,
    -- | Enable this flag to ignore SSL warnings while connecting to the project source code.
    insecureSsl :: Core.Maybe Core.Bool,
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
    location :: Core.Maybe Types.String,
    -- | Set to true to report the status of a build's start and finish to your source provider. This option is valid only when your source provider is GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a different source provider, an invalidInputException is thrown.
    reportBuildStatus :: Core.Maybe Core.Bool,
    -- | An identifier for this project source.
    sourceIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectSource' value with any optional fields omitted.
mkProjectSource ::
  -- | 'type\''
  Types.SourceType ->
  ProjectSource
mkProjectSource type' =
  ProjectSource'
    { type',
      auth = Core.Nothing,
      buildStatusConfig = Core.Nothing,
      buildspec = Core.Nothing,
      gitCloneDepth = Core.Nothing,
      gitSubmodulesConfig = Core.Nothing,
      insecureSsl = Core.Nothing,
      location = Core.Nothing,
      reportBuildStatus = Core.Nothing,
      sourceIdentifier = Core.Nothing
    }

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
psType :: Lens.Lens' ProjectSource Types.SourceType
psType = Lens.field @"type'"
{-# DEPRECATED psType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Information about the authorization settings for AWS CodeBuild to access the source code to be built.
--
-- This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psAuth :: Lens.Lens' ProjectSource (Core.Maybe Types.SourceAuth)
psAuth = Lens.field @"auth"
{-# DEPRECATED psAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | Contains information that defines how the build project reports the build status to the source provider. This option is only used when the source provider is @GITHUB@ , @GITHUB_ENTERPRISE@ , or @BITBUCKET@ .
--
-- /Note:/ Consider using 'buildStatusConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBuildStatusConfig :: Lens.Lens' ProjectSource (Core.Maybe Types.BuildStatusConfig)
psBuildStatusConfig = Lens.field @"buildStatusConfig"
{-# DEPRECATED psBuildStatusConfig "Use generic-lens or generic-optics with 'buildStatusConfig' instead." #-}

-- | The buildspec file declaration to use for the builds in this build project.
--
-- If this value is set, it can be either an inline buildspec definition, the path to an alternate buildspec file relative to the value of the built-in @CODEBUILD_SRC_DIR@ environment variable, or the path to an S3 bucket. The bucket must be in the same AWS Region as the build project. Specify the buildspec file using its ARN (for example, @arn:aws:s3:::my-codebuild-sample2/buildspec.yml@ ). If this value is not provided or is set to an empty string, the source code must contain a buildspec file in its root directory. For more information, see <https://docs.aws.amazon.com/codebuild/latest/userguide/build-spec-ref.html#build-spec-ref-name-storage Buildspec File Name and Storage Location> .
--
-- /Note:/ Consider using 'buildspec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psBuildspec :: Lens.Lens' ProjectSource (Core.Maybe Types.String)
psBuildspec = Lens.field @"buildspec"
{-# DEPRECATED psBuildspec "Use generic-lens or generic-optics with 'buildspec' instead." #-}

-- | Information about the Git clone depth for the build project.
--
-- /Note:/ Consider using 'gitCloneDepth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psGitCloneDepth :: Lens.Lens' ProjectSource (Core.Maybe Core.Natural)
psGitCloneDepth = Lens.field @"gitCloneDepth"
{-# DEPRECATED psGitCloneDepth "Use generic-lens or generic-optics with 'gitCloneDepth' instead." #-}

-- | Information about the Git submodules configuration for the build project.
--
-- /Note:/ Consider using 'gitSubmodulesConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psGitSubmodulesConfig :: Lens.Lens' ProjectSource (Core.Maybe Types.GitSubmodulesConfig)
psGitSubmodulesConfig = Lens.field @"gitSubmodulesConfig"
{-# DEPRECATED psGitSubmodulesConfig "Use generic-lens or generic-optics with 'gitSubmodulesConfig' instead." #-}

-- | Enable this flag to ignore SSL warnings while connecting to the project source code.
--
-- /Note:/ Consider using 'insecureSsl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psInsecureSsl :: Lens.Lens' ProjectSource (Core.Maybe Core.Bool)
psInsecureSsl = Lens.field @"insecureSsl"
{-# DEPRECATED psInsecureSsl "Use generic-lens or generic-optics with 'insecureSsl' instead." #-}

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
psLocation :: Lens.Lens' ProjectSource (Core.Maybe Types.String)
psLocation = Lens.field @"location"
{-# DEPRECATED psLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Set to true to report the status of a build's start and finish to your source provider. This option is valid only when your source provider is GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a different source provider, an invalidInputException is thrown.
--
-- /Note:/ Consider using 'reportBuildStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psReportBuildStatus :: Lens.Lens' ProjectSource (Core.Maybe Core.Bool)
psReportBuildStatus = Lens.field @"reportBuildStatus"
{-# DEPRECATED psReportBuildStatus "Use generic-lens or generic-optics with 'reportBuildStatus' instead." #-}

-- | An identifier for this project source.
--
-- /Note:/ Consider using 'sourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psSourceIdentifier :: Lens.Lens' ProjectSource (Core.Maybe Types.String)
psSourceIdentifier = Lens.field @"sourceIdentifier"
{-# DEPRECATED psSourceIdentifier "Use generic-lens or generic-optics with 'sourceIdentifier' instead." #-}

instance Core.FromJSON ProjectSource where
  toJSON ProjectSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            ("auth" Core..=) Core.<$> auth,
            ("buildStatusConfig" Core..=) Core.<$> buildStatusConfig,
            ("buildspec" Core..=) Core.<$> buildspec,
            ("gitCloneDepth" Core..=) Core.<$> gitCloneDepth,
            ("gitSubmodulesConfig" Core..=) Core.<$> gitSubmodulesConfig,
            ("insecureSsl" Core..=) Core.<$> insecureSsl,
            ("location" Core..=) Core.<$> location,
            ("reportBuildStatus" Core..=) Core.<$> reportBuildStatus,
            ("sourceIdentifier" Core..=) Core.<$> sourceIdentifier
          ]
      )

instance Core.FromJSON ProjectSource where
  parseJSON =
    Core.withObject "ProjectSource" Core.$
      \x ->
        ProjectSource'
          Core.<$> (x Core..: "type")
          Core.<*> (x Core..:? "auth")
          Core.<*> (x Core..:? "buildStatusConfig")
          Core.<*> (x Core..:? "buildspec")
          Core.<*> (x Core..:? "gitCloneDepth")
          Core.<*> (x Core..:? "gitSubmodulesConfig")
          Core.<*> (x Core..:? "insecureSsl")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "reportBuildStatus")
          Core.<*> (x Core..:? "sourceIdentifier")
