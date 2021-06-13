{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ProjectSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectSource where

import Network.AWS.CodeBuild.Types.BuildStatusConfig
import Network.AWS.CodeBuild.Types.GitSubmodulesConfig
import Network.AWS.CodeBuild.Types.SourceAuth
import Network.AWS.CodeBuild.Types.SourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the build input source code for the build project.
--
-- /See:/ 'newProjectSource' smart constructor.
data ProjectSource = ProjectSource'
  { -- | Information about the Git clone depth for the build project.
    gitCloneDepth :: Prelude.Maybe Prelude.Natural,
    -- | Contains information that defines how the build project reports the
    -- build status to the source provider. This option is only used when the
    -- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
    buildStatusConfig :: Prelude.Maybe BuildStatusConfig,
    -- | Information about the authorization settings for AWS CodeBuild to access
    -- the source code to be built.
    --
    -- This information is for the AWS CodeBuild console\'s use only. Your code
    -- should not get or set this information directly.
    auth :: Prelude.Maybe SourceAuth,
    -- | Set to true to report the status of a build\'s start and finish to your
    -- source provider. This option is valid only when your source provider is
    -- GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a
    -- different source provider, an @invalidInputException@ is thrown.
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
    reportBuildStatus :: Prelude.Maybe Prelude.Bool,
    -- | Enable this flag to ignore SSL warnings while connecting to the project
    -- source code.
    insecureSsl :: Prelude.Maybe Prelude.Bool,
    -- | An identifier for this project source. The identifier can only contain
    -- alphanumeric characters and underscores, and must be less than 128
    -- characters in length.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The buildspec file declaration to use for the builds in this build
    -- project.
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
    buildspec :: Prelude.Maybe Prelude.Text,
    -- | Information about the location of the source code to be built. Valid
    -- values include:
    --
    -- -   For source code settings that are specified in the source action of
    --     a pipeline in AWS CodePipeline, @location@ should not be specified.
    --     If it is specified, AWS CodePipeline ignores it. This is because AWS
    --     CodePipeline uses the settings in a pipeline\'s source action
    --     instead of this value.
    --
    -- -   For source code in an AWS CodeCommit repository, the HTTPS clone URL
    --     to the repository that contains the source code and the buildspec
    --     file (for example,
    --     @https:\/\/git-codecommit.\<region-ID>.amazonaws.com\/v1\/repos\/\<repo-name>@).
    --
    -- -   For source code in an Amazon S3 input bucket, one of the following.
    --
    --     -   The path to the ZIP file that contains the source code (for
    --         example, @\<bucket-name>\/\<path>\/\<object-name>.zip@).
    --
    --     -   The path to the folder that contains the source code (for
    --         example, @\<bucket-name>\/\<path-to-source-code>\/\<folder>\/@).
    --
    -- -   For source code in a GitHub repository, the HTTPS clone URL to the
    --     repository that contains the source and the buildspec file. You must
    --     connect your AWS account to your GitHub account. Use the AWS
    --     CodeBuild console to start creating a build project. When you use
    --     the console to connect (or reconnect) with GitHub, on the GitHub
    --     __Authorize application__ page, for __Organization access__, choose
    --     __Request access__ next to each repository you want to allow AWS
    --     CodeBuild to have access to, and then choose __Authorize
    --     application__. (After you have connected to your GitHub account, you
    --     do not need to finish creating the build project. You can leave the
    --     AWS CodeBuild console.) To instruct AWS CodeBuild to use this
    --     connection, in the @source@ object, set the @auth@ object\'s @type@
    --     value to @OAUTH@.
    --
    -- -   For source code in a Bitbucket repository, the HTTPS clone URL to
    --     the repository that contains the source and the buildspec file. You
    --     must connect your AWS account to your Bitbucket account. Use the AWS
    --     CodeBuild console to start creating a build project. When you use
    --     the console to connect (or reconnect) with Bitbucket, on the
    --     Bitbucket __Confirm access to your account__ page, choose __Grant
    --     access__. (After you have connected to your Bitbucket account, you
    --     do not need to finish creating the build project. You can leave the
    --     AWS CodeBuild console.) To instruct AWS CodeBuild to use this
    --     connection, in the @source@ object, set the @auth@ object\'s @type@
    --     value to @OAUTH@.
    location :: Prelude.Maybe Prelude.Text,
    -- | Information about the Git submodules configuration for the build
    -- project.
    gitSubmodulesConfig :: Prelude.Maybe GitSubmodulesConfig,
    -- | The type of repository that contains the source code to be built. Valid
    -- values include:
    --
    -- -   @BITBUCKET@: The source code is in a Bitbucket repository.
    --
    -- -   @CODECOMMIT@: The source code is in an AWS CodeCommit repository.
    --
    -- -   @CODEPIPELINE@: The source code settings are specified in the source
    --     action of a pipeline in AWS CodePipeline.
    --
    -- -   @GITHUB@: The source code is in a GitHub or GitHub Enterprise Cloud
    --     repository.
    --
    -- -   @GITHUB_ENTERPRISE@: The source code is in a GitHub Enterprise
    --     Server repository.
    --
    -- -   @NO_SOURCE@: The project does not have input source code.
    --
    -- -   @S3@: The source code is in an Amazon S3 bucket.
    type' :: SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gitCloneDepth', 'projectSource_gitCloneDepth' - Information about the Git clone depth for the build project.
--
-- 'buildStatusConfig', 'projectSource_buildStatusConfig' - Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
--
-- 'auth', 'projectSource_auth' - Information about the authorization settings for AWS CodeBuild to access
-- the source code to be built.
--
-- This information is for the AWS CodeBuild console\'s use only. Your code
-- should not get or set this information directly.
--
-- 'reportBuildStatus', 'projectSource_reportBuildStatus' - Set to true to report the status of a build\'s start and finish to your
-- source provider. This option is valid only when your source provider is
-- GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a
-- different source provider, an @invalidInputException@ is thrown.
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
-- 'insecureSsl', 'projectSource_insecureSsl' - Enable this flag to ignore SSL warnings while connecting to the project
-- source code.
--
-- 'sourceIdentifier', 'projectSource_sourceIdentifier' - An identifier for this project source. The identifier can only contain
-- alphanumeric characters and underscores, and must be less than 128
-- characters in length.
--
-- 'buildspec', 'projectSource_buildspec' - The buildspec file declaration to use for the builds in this build
-- project.
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
-- 'location', 'projectSource_location' - Information about the location of the source code to be built. Valid
-- values include:
--
-- -   For source code settings that are specified in the source action of
--     a pipeline in AWS CodePipeline, @location@ should not be specified.
--     If it is specified, AWS CodePipeline ignores it. This is because AWS
--     CodePipeline uses the settings in a pipeline\'s source action
--     instead of this value.
--
-- -   For source code in an AWS CodeCommit repository, the HTTPS clone URL
--     to the repository that contains the source code and the buildspec
--     file (for example,
--     @https:\/\/git-codecommit.\<region-ID>.amazonaws.com\/v1\/repos\/\<repo-name>@).
--
-- -   For source code in an Amazon S3 input bucket, one of the following.
--
--     -   The path to the ZIP file that contains the source code (for
--         example, @\<bucket-name>\/\<path>\/\<object-name>.zip@).
--
--     -   The path to the folder that contains the source code (for
--         example, @\<bucket-name>\/\<path-to-source-code>\/\<folder>\/@).
--
-- -   For source code in a GitHub repository, the HTTPS clone URL to the
--     repository that contains the source and the buildspec file. You must
--     connect your AWS account to your GitHub account. Use the AWS
--     CodeBuild console to start creating a build project. When you use
--     the console to connect (or reconnect) with GitHub, on the GitHub
--     __Authorize application__ page, for __Organization access__, choose
--     __Request access__ next to each repository you want to allow AWS
--     CodeBuild to have access to, and then choose __Authorize
--     application__. (After you have connected to your GitHub account, you
--     do not need to finish creating the build project. You can leave the
--     AWS CodeBuild console.) To instruct AWS CodeBuild to use this
--     connection, in the @source@ object, set the @auth@ object\'s @type@
--     value to @OAUTH@.
--
-- -   For source code in a Bitbucket repository, the HTTPS clone URL to
--     the repository that contains the source and the buildspec file. You
--     must connect your AWS account to your Bitbucket account. Use the AWS
--     CodeBuild console to start creating a build project. When you use
--     the console to connect (or reconnect) with Bitbucket, on the
--     Bitbucket __Confirm access to your account__ page, choose __Grant
--     access__. (After you have connected to your Bitbucket account, you
--     do not need to finish creating the build project. You can leave the
--     AWS CodeBuild console.) To instruct AWS CodeBuild to use this
--     connection, in the @source@ object, set the @auth@ object\'s @type@
--     value to @OAUTH@.
--
-- 'gitSubmodulesConfig', 'projectSource_gitSubmodulesConfig' - Information about the Git submodules configuration for the build
-- project.
--
-- 'type'', 'projectSource_type' - The type of repository that contains the source code to be built. Valid
-- values include:
--
-- -   @BITBUCKET@: The source code is in a Bitbucket repository.
--
-- -   @CODECOMMIT@: The source code is in an AWS CodeCommit repository.
--
-- -   @CODEPIPELINE@: The source code settings are specified in the source
--     action of a pipeline in AWS CodePipeline.
--
-- -   @GITHUB@: The source code is in a GitHub or GitHub Enterprise Cloud
--     repository.
--
-- -   @GITHUB_ENTERPRISE@: The source code is in a GitHub Enterprise
--     Server repository.
--
-- -   @NO_SOURCE@: The project does not have input source code.
--
-- -   @S3@: The source code is in an Amazon S3 bucket.
newProjectSource ::
  -- | 'type''
  SourceType ->
  ProjectSource
newProjectSource pType_ =
  ProjectSource'
    { gitCloneDepth = Prelude.Nothing,
      buildStatusConfig = Prelude.Nothing,
      auth = Prelude.Nothing,
      reportBuildStatus = Prelude.Nothing,
      insecureSsl = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      buildspec = Prelude.Nothing,
      location = Prelude.Nothing,
      gitSubmodulesConfig = Prelude.Nothing,
      type' = pType_
    }

-- | Information about the Git clone depth for the build project.
projectSource_gitCloneDepth :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Natural)
projectSource_gitCloneDepth = Lens.lens (\ProjectSource' {gitCloneDepth} -> gitCloneDepth) (\s@ProjectSource' {} a -> s {gitCloneDepth = a} :: ProjectSource)

-- | Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
projectSource_buildStatusConfig :: Lens.Lens' ProjectSource (Prelude.Maybe BuildStatusConfig)
projectSource_buildStatusConfig = Lens.lens (\ProjectSource' {buildStatusConfig} -> buildStatusConfig) (\s@ProjectSource' {} a -> s {buildStatusConfig = a} :: ProjectSource)

-- | Information about the authorization settings for AWS CodeBuild to access
-- the source code to be built.
--
-- This information is for the AWS CodeBuild console\'s use only. Your code
-- should not get or set this information directly.
projectSource_auth :: Lens.Lens' ProjectSource (Prelude.Maybe SourceAuth)
projectSource_auth = Lens.lens (\ProjectSource' {auth} -> auth) (\s@ProjectSource' {} a -> s {auth = a} :: ProjectSource)

-- | Set to true to report the status of a build\'s start and finish to your
-- source provider. This option is valid only when your source provider is
-- GitHub, GitHub Enterprise, or Bitbucket. If this is set and you use a
-- different source provider, an @invalidInputException@ is thrown.
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
projectSource_reportBuildStatus :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Bool)
projectSource_reportBuildStatus = Lens.lens (\ProjectSource' {reportBuildStatus} -> reportBuildStatus) (\s@ProjectSource' {} a -> s {reportBuildStatus = a} :: ProjectSource)

-- | Enable this flag to ignore SSL warnings while connecting to the project
-- source code.
projectSource_insecureSsl :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Bool)
projectSource_insecureSsl = Lens.lens (\ProjectSource' {insecureSsl} -> insecureSsl) (\s@ProjectSource' {} a -> s {insecureSsl = a} :: ProjectSource)

-- | An identifier for this project source. The identifier can only contain
-- alphanumeric characters and underscores, and must be less than 128
-- characters in length.
projectSource_sourceIdentifier :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Text)
projectSource_sourceIdentifier = Lens.lens (\ProjectSource' {sourceIdentifier} -> sourceIdentifier) (\s@ProjectSource' {} a -> s {sourceIdentifier = a} :: ProjectSource)

-- | The buildspec file declaration to use for the builds in this build
-- project.
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
projectSource_buildspec :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Text)
projectSource_buildspec = Lens.lens (\ProjectSource' {buildspec} -> buildspec) (\s@ProjectSource' {} a -> s {buildspec = a} :: ProjectSource)

-- | Information about the location of the source code to be built. Valid
-- values include:
--
-- -   For source code settings that are specified in the source action of
--     a pipeline in AWS CodePipeline, @location@ should not be specified.
--     If it is specified, AWS CodePipeline ignores it. This is because AWS
--     CodePipeline uses the settings in a pipeline\'s source action
--     instead of this value.
--
-- -   For source code in an AWS CodeCommit repository, the HTTPS clone URL
--     to the repository that contains the source code and the buildspec
--     file (for example,
--     @https:\/\/git-codecommit.\<region-ID>.amazonaws.com\/v1\/repos\/\<repo-name>@).
--
-- -   For source code in an Amazon S3 input bucket, one of the following.
--
--     -   The path to the ZIP file that contains the source code (for
--         example, @\<bucket-name>\/\<path>\/\<object-name>.zip@).
--
--     -   The path to the folder that contains the source code (for
--         example, @\<bucket-name>\/\<path-to-source-code>\/\<folder>\/@).
--
-- -   For source code in a GitHub repository, the HTTPS clone URL to the
--     repository that contains the source and the buildspec file. You must
--     connect your AWS account to your GitHub account. Use the AWS
--     CodeBuild console to start creating a build project. When you use
--     the console to connect (or reconnect) with GitHub, on the GitHub
--     __Authorize application__ page, for __Organization access__, choose
--     __Request access__ next to each repository you want to allow AWS
--     CodeBuild to have access to, and then choose __Authorize
--     application__. (After you have connected to your GitHub account, you
--     do not need to finish creating the build project. You can leave the
--     AWS CodeBuild console.) To instruct AWS CodeBuild to use this
--     connection, in the @source@ object, set the @auth@ object\'s @type@
--     value to @OAUTH@.
--
-- -   For source code in a Bitbucket repository, the HTTPS clone URL to
--     the repository that contains the source and the buildspec file. You
--     must connect your AWS account to your Bitbucket account. Use the AWS
--     CodeBuild console to start creating a build project. When you use
--     the console to connect (or reconnect) with Bitbucket, on the
--     Bitbucket __Confirm access to your account__ page, choose __Grant
--     access__. (After you have connected to your Bitbucket account, you
--     do not need to finish creating the build project. You can leave the
--     AWS CodeBuild console.) To instruct AWS CodeBuild to use this
--     connection, in the @source@ object, set the @auth@ object\'s @type@
--     value to @OAUTH@.
projectSource_location :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Text)
projectSource_location = Lens.lens (\ProjectSource' {location} -> location) (\s@ProjectSource' {} a -> s {location = a} :: ProjectSource)

-- | Information about the Git submodules configuration for the build
-- project.
projectSource_gitSubmodulesConfig :: Lens.Lens' ProjectSource (Prelude.Maybe GitSubmodulesConfig)
projectSource_gitSubmodulesConfig = Lens.lens (\ProjectSource' {gitSubmodulesConfig} -> gitSubmodulesConfig) (\s@ProjectSource' {} a -> s {gitSubmodulesConfig = a} :: ProjectSource)

-- | The type of repository that contains the source code to be built. Valid
-- values include:
--
-- -   @BITBUCKET@: The source code is in a Bitbucket repository.
--
-- -   @CODECOMMIT@: The source code is in an AWS CodeCommit repository.
--
-- -   @CODEPIPELINE@: The source code settings are specified in the source
--     action of a pipeline in AWS CodePipeline.
--
-- -   @GITHUB@: The source code is in a GitHub or GitHub Enterprise Cloud
--     repository.
--
-- -   @GITHUB_ENTERPRISE@: The source code is in a GitHub Enterprise
--     Server repository.
--
-- -   @NO_SOURCE@: The project does not have input source code.
--
-- -   @S3@: The source code is in an Amazon S3 bucket.
projectSource_type :: Lens.Lens' ProjectSource SourceType
projectSource_type = Lens.lens (\ProjectSource' {type'} -> type') (\s@ProjectSource' {} a -> s {type' = a} :: ProjectSource)

instance Core.FromJSON ProjectSource where
  parseJSON =
    Core.withObject
      "ProjectSource"
      ( \x ->
          ProjectSource'
            Prelude.<$> (x Core..:? "gitCloneDepth")
            Prelude.<*> (x Core..:? "buildStatusConfig")
            Prelude.<*> (x Core..:? "auth")
            Prelude.<*> (x Core..:? "reportBuildStatus")
            Prelude.<*> (x Core..:? "insecureSsl")
            Prelude.<*> (x Core..:? "sourceIdentifier")
            Prelude.<*> (x Core..:? "buildspec")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "gitSubmodulesConfig")
            Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable ProjectSource

instance Prelude.NFData ProjectSource

instance Core.ToJSON ProjectSource where
  toJSON ProjectSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("gitCloneDepth" Core..=) Prelude.<$> gitCloneDepth,
            ("buildStatusConfig" Core..=)
              Prelude.<$> buildStatusConfig,
            ("auth" Core..=) Prelude.<$> auth,
            ("reportBuildStatus" Core..=)
              Prelude.<$> reportBuildStatus,
            ("insecureSsl" Core..=) Prelude.<$> insecureSsl,
            ("sourceIdentifier" Core..=)
              Prelude.<$> sourceIdentifier,
            ("buildspec" Core..=) Prelude.<$> buildspec,
            ("location" Core..=) Prelude.<$> location,
            ("gitSubmodulesConfig" Core..=)
              Prelude.<$> gitSubmodulesConfig,
            Prelude.Just ("type" Core..= type')
          ]
      )
