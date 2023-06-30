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
-- Module      : Amazonka.CodeBuild.Types.ProjectSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectSource where

import Amazonka.CodeBuild.Types.BuildStatusConfig
import Amazonka.CodeBuild.Types.GitSubmodulesConfig
import Amazonka.CodeBuild.Types.SourceAuth
import Amazonka.CodeBuild.Types.SourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the build input source code for the build project.
--
-- /See:/ 'newProjectSource' smart constructor.
data ProjectSource = ProjectSource'
  { -- | Information about the authorization settings for CodeBuild to access the
    -- source code to be built.
    --
    -- This information is for the CodeBuild console\'s use only. Your code
    -- should not get or set this information directly.
    auth :: Prelude.Maybe SourceAuth,
    -- | Contains information that defines how the build project reports the
    -- build status to the source provider. This option is only used when the
    -- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
    buildStatusConfig :: Prelude.Maybe BuildStatusConfig,
    -- | The buildspec file declaration to use for the builds in this build
    -- project.
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
    buildspec :: Prelude.Maybe Prelude.Text,
    -- | Information about the Git clone depth for the build project.
    gitCloneDepth :: Prelude.Maybe Prelude.Natural,
    -- | Information about the Git submodules configuration for the build
    -- project.
    gitSubmodulesConfig :: Prelude.Maybe GitSubmodulesConfig,
    -- | Enable this flag to ignore SSL warnings while connecting to the project
    -- source code.
    insecureSsl :: Prelude.Maybe Prelude.Bool,
    -- | Information about the location of the source code to be built. Valid
    -- values include:
    --
    -- -   For source code settings that are specified in the source action of
    --     a pipeline in CodePipeline, @location@ should not be specified. If
    --     it is specified, CodePipeline ignores it. This is because
    --     CodePipeline uses the settings in a pipeline\'s source action
    --     instead of this value.
    --
    -- -   For source code in an CodeCommit repository, the HTTPS clone URL to
    --     the repository that contains the source code and the buildspec file
    --     (for example,
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
    --     connect your Amazon Web Services account to your GitHub account. Use
    --     the CodeBuild console to start creating a build project. When you
    --     use the console to connect (or reconnect) with GitHub, on the GitHub
    --     __Authorize application__ page, for __Organization access__, choose
    --     __Request access__ next to each repository you want to allow
    --     CodeBuild to have access to, and then choose __Authorize
    --     application__. (After you have connected to your GitHub account, you
    --     do not need to finish creating the build project. You can leave the
    --     CodeBuild console.) To instruct CodeBuild to use this connection, in
    --     the @source@ object, set the @auth@ object\'s @type@ value to
    --     @OAUTH@.
    --
    -- -   For source code in a Bitbucket repository, the HTTPS clone URL to
    --     the repository that contains the source and the buildspec file. You
    --     must connect your Amazon Web Services account to your Bitbucket
    --     account. Use the CodeBuild console to start creating a build
    --     project. When you use the console to connect (or reconnect) with
    --     Bitbucket, on the Bitbucket __Confirm access to your account__ page,
    --     choose __Grant access__. (After you have connected to your Bitbucket
    --     account, you do not need to finish creating the build project. You
    --     can leave the CodeBuild console.) To instruct CodeBuild to use this
    --     connection, in the @source@ object, set the @auth@ object\'s @type@
    --     value to @OAUTH@.
    --
    -- If you specify @CODEPIPELINE@ for the @Type@ property, don\'t specify
    -- this property. For all of the other types, you must specify @Location@.
    location :: Prelude.Maybe Prelude.Text,
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
    -- in the /CodeBuild User Guide/.
    --
    -- The status of a build triggered by a webhook is always reported to your
    -- source provider.
    --
    -- If your project\'s builds are triggered by a webhook, you must push a
    -- new commit to the repo for a change to this property to take effect.
    reportBuildStatus :: Prelude.Maybe Prelude.Bool,
    -- | An identifier for this project source. The identifier can only contain
    -- alphanumeric characters and underscores, and must be less than 128
    -- characters in length.
    sourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The type of repository that contains the source code to be built. Valid
    -- values include:
    --
    -- -   @BITBUCKET@: The source code is in a Bitbucket repository.
    --
    -- -   @CODECOMMIT@: The source code is in an CodeCommit repository.
    --
    -- -   @CODEPIPELINE@: The source code settings are specified in the source
    --     action of a pipeline in CodePipeline.
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
-- 'auth', 'projectSource_auth' - Information about the authorization settings for CodeBuild to access the
-- source code to be built.
--
-- This information is for the CodeBuild console\'s use only. Your code
-- should not get or set this information directly.
--
-- 'buildStatusConfig', 'projectSource_buildStatusConfig' - Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
--
-- 'buildspec', 'projectSource_buildspec' - The buildspec file declaration to use for the builds in this build
-- project.
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
-- 'gitCloneDepth', 'projectSource_gitCloneDepth' - Information about the Git clone depth for the build project.
--
-- 'gitSubmodulesConfig', 'projectSource_gitSubmodulesConfig' - Information about the Git submodules configuration for the build
-- project.
--
-- 'insecureSsl', 'projectSource_insecureSsl' - Enable this flag to ignore SSL warnings while connecting to the project
-- source code.
--
-- 'location', 'projectSource_location' - Information about the location of the source code to be built. Valid
-- values include:
--
-- -   For source code settings that are specified in the source action of
--     a pipeline in CodePipeline, @location@ should not be specified. If
--     it is specified, CodePipeline ignores it. This is because
--     CodePipeline uses the settings in a pipeline\'s source action
--     instead of this value.
--
-- -   For source code in an CodeCommit repository, the HTTPS clone URL to
--     the repository that contains the source code and the buildspec file
--     (for example,
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
--     connect your Amazon Web Services account to your GitHub account. Use
--     the CodeBuild console to start creating a build project. When you
--     use the console to connect (or reconnect) with GitHub, on the GitHub
--     __Authorize application__ page, for __Organization access__, choose
--     __Request access__ next to each repository you want to allow
--     CodeBuild to have access to, and then choose __Authorize
--     application__. (After you have connected to your GitHub account, you
--     do not need to finish creating the build project. You can leave the
--     CodeBuild console.) To instruct CodeBuild to use this connection, in
--     the @source@ object, set the @auth@ object\'s @type@ value to
--     @OAUTH@.
--
-- -   For source code in a Bitbucket repository, the HTTPS clone URL to
--     the repository that contains the source and the buildspec file. You
--     must connect your Amazon Web Services account to your Bitbucket
--     account. Use the CodeBuild console to start creating a build
--     project. When you use the console to connect (or reconnect) with
--     Bitbucket, on the Bitbucket __Confirm access to your account__ page,
--     choose __Grant access__. (After you have connected to your Bitbucket
--     account, you do not need to finish creating the build project. You
--     can leave the CodeBuild console.) To instruct CodeBuild to use this
--     connection, in the @source@ object, set the @auth@ object\'s @type@
--     value to @OAUTH@.
--
-- If you specify @CODEPIPELINE@ for the @Type@ property, don\'t specify
-- this property. For all of the other types, you must specify @Location@.
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
-- in the /CodeBuild User Guide/.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
--
-- If your project\'s builds are triggered by a webhook, you must push a
-- new commit to the repo for a change to this property to take effect.
--
-- 'sourceIdentifier', 'projectSource_sourceIdentifier' - An identifier for this project source. The identifier can only contain
-- alphanumeric characters and underscores, and must be less than 128
-- characters in length.
--
-- 'type'', 'projectSource_type' - The type of repository that contains the source code to be built. Valid
-- values include:
--
-- -   @BITBUCKET@: The source code is in a Bitbucket repository.
--
-- -   @CODECOMMIT@: The source code is in an CodeCommit repository.
--
-- -   @CODEPIPELINE@: The source code settings are specified in the source
--     action of a pipeline in CodePipeline.
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
    { auth = Prelude.Nothing,
      buildStatusConfig = Prelude.Nothing,
      buildspec = Prelude.Nothing,
      gitCloneDepth = Prelude.Nothing,
      gitSubmodulesConfig = Prelude.Nothing,
      insecureSsl = Prelude.Nothing,
      location = Prelude.Nothing,
      reportBuildStatus = Prelude.Nothing,
      sourceIdentifier = Prelude.Nothing,
      type' = pType_
    }

-- | Information about the authorization settings for CodeBuild to access the
-- source code to be built.
--
-- This information is for the CodeBuild console\'s use only. Your code
-- should not get or set this information directly.
projectSource_auth :: Lens.Lens' ProjectSource (Prelude.Maybe SourceAuth)
projectSource_auth = Lens.lens (\ProjectSource' {auth} -> auth) (\s@ProjectSource' {} a -> s {auth = a} :: ProjectSource)

-- | Contains information that defines how the build project reports the
-- build status to the source provider. This option is only used when the
-- source provider is @GITHUB@, @GITHUB_ENTERPRISE@, or @BITBUCKET@.
projectSource_buildStatusConfig :: Lens.Lens' ProjectSource (Prelude.Maybe BuildStatusConfig)
projectSource_buildStatusConfig = Lens.lens (\ProjectSource' {buildStatusConfig} -> buildStatusConfig) (\s@ProjectSource' {} a -> s {buildStatusConfig = a} :: ProjectSource)

-- | The buildspec file declaration to use for the builds in this build
-- project.
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
projectSource_buildspec :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Text)
projectSource_buildspec = Lens.lens (\ProjectSource' {buildspec} -> buildspec) (\s@ProjectSource' {} a -> s {buildspec = a} :: ProjectSource)

-- | Information about the Git clone depth for the build project.
projectSource_gitCloneDepth :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Natural)
projectSource_gitCloneDepth = Lens.lens (\ProjectSource' {gitCloneDepth} -> gitCloneDepth) (\s@ProjectSource' {} a -> s {gitCloneDepth = a} :: ProjectSource)

-- | Information about the Git submodules configuration for the build
-- project.
projectSource_gitSubmodulesConfig :: Lens.Lens' ProjectSource (Prelude.Maybe GitSubmodulesConfig)
projectSource_gitSubmodulesConfig = Lens.lens (\ProjectSource' {gitSubmodulesConfig} -> gitSubmodulesConfig) (\s@ProjectSource' {} a -> s {gitSubmodulesConfig = a} :: ProjectSource)

-- | Enable this flag to ignore SSL warnings while connecting to the project
-- source code.
projectSource_insecureSsl :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Bool)
projectSource_insecureSsl = Lens.lens (\ProjectSource' {insecureSsl} -> insecureSsl) (\s@ProjectSource' {} a -> s {insecureSsl = a} :: ProjectSource)

-- | Information about the location of the source code to be built. Valid
-- values include:
--
-- -   For source code settings that are specified in the source action of
--     a pipeline in CodePipeline, @location@ should not be specified. If
--     it is specified, CodePipeline ignores it. This is because
--     CodePipeline uses the settings in a pipeline\'s source action
--     instead of this value.
--
-- -   For source code in an CodeCommit repository, the HTTPS clone URL to
--     the repository that contains the source code and the buildspec file
--     (for example,
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
--     connect your Amazon Web Services account to your GitHub account. Use
--     the CodeBuild console to start creating a build project. When you
--     use the console to connect (or reconnect) with GitHub, on the GitHub
--     __Authorize application__ page, for __Organization access__, choose
--     __Request access__ next to each repository you want to allow
--     CodeBuild to have access to, and then choose __Authorize
--     application__. (After you have connected to your GitHub account, you
--     do not need to finish creating the build project. You can leave the
--     CodeBuild console.) To instruct CodeBuild to use this connection, in
--     the @source@ object, set the @auth@ object\'s @type@ value to
--     @OAUTH@.
--
-- -   For source code in a Bitbucket repository, the HTTPS clone URL to
--     the repository that contains the source and the buildspec file. You
--     must connect your Amazon Web Services account to your Bitbucket
--     account. Use the CodeBuild console to start creating a build
--     project. When you use the console to connect (or reconnect) with
--     Bitbucket, on the Bitbucket __Confirm access to your account__ page,
--     choose __Grant access__. (After you have connected to your Bitbucket
--     account, you do not need to finish creating the build project. You
--     can leave the CodeBuild console.) To instruct CodeBuild to use this
--     connection, in the @source@ object, set the @auth@ object\'s @type@
--     value to @OAUTH@.
--
-- If you specify @CODEPIPELINE@ for the @Type@ property, don\'t specify
-- this property. For all of the other types, you must specify @Location@.
projectSource_location :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Text)
projectSource_location = Lens.lens (\ProjectSource' {location} -> location) (\s@ProjectSource' {} a -> s {location = a} :: ProjectSource)

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
-- in the /CodeBuild User Guide/.
--
-- The status of a build triggered by a webhook is always reported to your
-- source provider.
--
-- If your project\'s builds are triggered by a webhook, you must push a
-- new commit to the repo for a change to this property to take effect.
projectSource_reportBuildStatus :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Bool)
projectSource_reportBuildStatus = Lens.lens (\ProjectSource' {reportBuildStatus} -> reportBuildStatus) (\s@ProjectSource' {} a -> s {reportBuildStatus = a} :: ProjectSource)

-- | An identifier for this project source. The identifier can only contain
-- alphanumeric characters and underscores, and must be less than 128
-- characters in length.
projectSource_sourceIdentifier :: Lens.Lens' ProjectSource (Prelude.Maybe Prelude.Text)
projectSource_sourceIdentifier = Lens.lens (\ProjectSource' {sourceIdentifier} -> sourceIdentifier) (\s@ProjectSource' {} a -> s {sourceIdentifier = a} :: ProjectSource)

-- | The type of repository that contains the source code to be built. Valid
-- values include:
--
-- -   @BITBUCKET@: The source code is in a Bitbucket repository.
--
-- -   @CODECOMMIT@: The source code is in an CodeCommit repository.
--
-- -   @CODEPIPELINE@: The source code settings are specified in the source
--     action of a pipeline in CodePipeline.
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

instance Data.FromJSON ProjectSource where
  parseJSON =
    Data.withObject
      "ProjectSource"
      ( \x ->
          ProjectSource'
            Prelude.<$> (x Data..:? "auth")
            Prelude.<*> (x Data..:? "buildStatusConfig")
            Prelude.<*> (x Data..:? "buildspec")
            Prelude.<*> (x Data..:? "gitCloneDepth")
            Prelude.<*> (x Data..:? "gitSubmodulesConfig")
            Prelude.<*> (x Data..:? "insecureSsl")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "reportBuildStatus")
            Prelude.<*> (x Data..:? "sourceIdentifier")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable ProjectSource where
  hashWithSalt _salt ProjectSource' {..} =
    _salt
      `Prelude.hashWithSalt` auth
      `Prelude.hashWithSalt` buildStatusConfig
      `Prelude.hashWithSalt` buildspec
      `Prelude.hashWithSalt` gitCloneDepth
      `Prelude.hashWithSalt` gitSubmodulesConfig
      `Prelude.hashWithSalt` insecureSsl
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` reportBuildStatus
      `Prelude.hashWithSalt` sourceIdentifier
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ProjectSource where
  rnf ProjectSource' {..} =
    Prelude.rnf auth
      `Prelude.seq` Prelude.rnf buildStatusConfig
      `Prelude.seq` Prelude.rnf buildspec
      `Prelude.seq` Prelude.rnf gitCloneDepth
      `Prelude.seq` Prelude.rnf gitSubmodulesConfig
      `Prelude.seq` Prelude.rnf insecureSsl
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf reportBuildStatus
      `Prelude.seq` Prelude.rnf sourceIdentifier
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON ProjectSource where
  toJSON ProjectSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("auth" Data..=) Prelude.<$> auth,
            ("buildStatusConfig" Data..=)
              Prelude.<$> buildStatusConfig,
            ("buildspec" Data..=) Prelude.<$> buildspec,
            ("gitCloneDepth" Data..=) Prelude.<$> gitCloneDepth,
            ("gitSubmodulesConfig" Data..=)
              Prelude.<$> gitSubmodulesConfig,
            ("insecureSsl" Data..=) Prelude.<$> insecureSsl,
            ("location" Data..=) Prelude.<$> location,
            ("reportBuildStatus" Data..=)
              Prelude.<$> reportBuildStatus,
            ("sourceIdentifier" Data..=)
              Prelude.<$> sourceIdentifier,
            Prelude.Just ("type" Data..= type')
          ]
      )
