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
-- Module      : Amazonka.SecurityHub.Types.AwsCodeBuildProjectSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCodeBuildProjectSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the build input source code for this build project.
--
-- /See:/ 'newAwsCodeBuildProjectSource' smart constructor.
data AwsCodeBuildProjectSource = AwsCodeBuildProjectSource'
  { -- | Information about the Git clone depth for the build project.
    gitCloneDepth :: Prelude.Maybe Prelude.Int,
    -- | Whether to ignore SSL warnings while connecting to the project source
    -- code.
    insecureSsl :: Prelude.Maybe Prelude.Bool,
    -- | Information about the location of the source code to be built.
    --
    -- Valid values include:
    --
    -- -   For source code settings that are specified in the source action of
    --     a pipeline in CodePipeline, location should not be specified. If it
    --     is specified, CodePipeline ignores it. This is because CodePipeline
    --     uses the settings in a pipeline\'s source action instead of this
    --     value.
    --
    -- -   For source code in an CodeCommit repository, the HTTPS clone URL to
    --     the repository that contains the source code and the build spec file
    --     (for example,
    --     @https:\/\/git-codecommit.region-ID.amazonaws.com\/v1\/repos\/repo-name@
    --     ).
    --
    -- -   For source code in an S3 input bucket, one of the following.
    --
    --     -   The path to the ZIP file that contains the source code (for
    --         example, @bucket-name\/path\/to\/object-name.zip@).
    --
    --     -   The path to the folder that contains the source code (for
    --         example, @bucket-name\/path\/to\/source-code\/folder\/@).
    --
    -- -   For source code in a GitHub repository, the HTTPS clone URL to the
    --     repository that contains the source and the build spec file.
    --
    -- -   For source code in a Bitbucket repository, the HTTPS clone URL to
    --     the repository that contains the source and the build spec file.
    location :: Prelude.Maybe Prelude.Text,
    -- | The type of repository that contains the source code to be built. Valid
    -- values are:
    --
    -- -   @BITBUCKET@ - The source code is in a Bitbucket repository.
    --
    -- -   @CODECOMMIT@ - The source code is in an CodeCommit repository.
    --
    -- -   @CODEPIPELINE@ - The source code settings are specified in the
    --     source action of a pipeline in CodePipeline.
    --
    -- -   @GITHUB@ - The source code is in a GitHub repository.
    --
    -- -   @GITHUB_ENTERPRISE@ - The source code is in a GitHub Enterprise
    --     repository.
    --
    -- -   @NO_SOURCE@ - The project does not have input source code.
    --
    -- -   @S3@ - The source code is in an S3 input bucket.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCodeBuildProjectSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gitCloneDepth', 'awsCodeBuildProjectSource_gitCloneDepth' - Information about the Git clone depth for the build project.
--
-- 'insecureSsl', 'awsCodeBuildProjectSource_insecureSsl' - Whether to ignore SSL warnings while connecting to the project source
-- code.
--
-- 'location', 'awsCodeBuildProjectSource_location' - Information about the location of the source code to be built.
--
-- Valid values include:
--
-- -   For source code settings that are specified in the source action of
--     a pipeline in CodePipeline, location should not be specified. If it
--     is specified, CodePipeline ignores it. This is because CodePipeline
--     uses the settings in a pipeline\'s source action instead of this
--     value.
--
-- -   For source code in an CodeCommit repository, the HTTPS clone URL to
--     the repository that contains the source code and the build spec file
--     (for example,
--     @https:\/\/git-codecommit.region-ID.amazonaws.com\/v1\/repos\/repo-name@
--     ).
--
-- -   For source code in an S3 input bucket, one of the following.
--
--     -   The path to the ZIP file that contains the source code (for
--         example, @bucket-name\/path\/to\/object-name.zip@).
--
--     -   The path to the folder that contains the source code (for
--         example, @bucket-name\/path\/to\/source-code\/folder\/@).
--
-- -   For source code in a GitHub repository, the HTTPS clone URL to the
--     repository that contains the source and the build spec file.
--
-- -   For source code in a Bitbucket repository, the HTTPS clone URL to
--     the repository that contains the source and the build spec file.
--
-- 'type'', 'awsCodeBuildProjectSource_type' - The type of repository that contains the source code to be built. Valid
-- values are:
--
-- -   @BITBUCKET@ - The source code is in a Bitbucket repository.
--
-- -   @CODECOMMIT@ - The source code is in an CodeCommit repository.
--
-- -   @CODEPIPELINE@ - The source code settings are specified in the
--     source action of a pipeline in CodePipeline.
--
-- -   @GITHUB@ - The source code is in a GitHub repository.
--
-- -   @GITHUB_ENTERPRISE@ - The source code is in a GitHub Enterprise
--     repository.
--
-- -   @NO_SOURCE@ - The project does not have input source code.
--
-- -   @S3@ - The source code is in an S3 input bucket.
newAwsCodeBuildProjectSource ::
  AwsCodeBuildProjectSource
newAwsCodeBuildProjectSource =
  AwsCodeBuildProjectSource'
    { gitCloneDepth =
        Prelude.Nothing,
      insecureSsl = Prelude.Nothing,
      location = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Information about the Git clone depth for the build project.
awsCodeBuildProjectSource_gitCloneDepth :: Lens.Lens' AwsCodeBuildProjectSource (Prelude.Maybe Prelude.Int)
awsCodeBuildProjectSource_gitCloneDepth = Lens.lens (\AwsCodeBuildProjectSource' {gitCloneDepth} -> gitCloneDepth) (\s@AwsCodeBuildProjectSource' {} a -> s {gitCloneDepth = a} :: AwsCodeBuildProjectSource)

-- | Whether to ignore SSL warnings while connecting to the project source
-- code.
awsCodeBuildProjectSource_insecureSsl :: Lens.Lens' AwsCodeBuildProjectSource (Prelude.Maybe Prelude.Bool)
awsCodeBuildProjectSource_insecureSsl = Lens.lens (\AwsCodeBuildProjectSource' {insecureSsl} -> insecureSsl) (\s@AwsCodeBuildProjectSource' {} a -> s {insecureSsl = a} :: AwsCodeBuildProjectSource)

-- | Information about the location of the source code to be built.
--
-- Valid values include:
--
-- -   For source code settings that are specified in the source action of
--     a pipeline in CodePipeline, location should not be specified. If it
--     is specified, CodePipeline ignores it. This is because CodePipeline
--     uses the settings in a pipeline\'s source action instead of this
--     value.
--
-- -   For source code in an CodeCommit repository, the HTTPS clone URL to
--     the repository that contains the source code and the build spec file
--     (for example,
--     @https:\/\/git-codecommit.region-ID.amazonaws.com\/v1\/repos\/repo-name@
--     ).
--
-- -   For source code in an S3 input bucket, one of the following.
--
--     -   The path to the ZIP file that contains the source code (for
--         example, @bucket-name\/path\/to\/object-name.zip@).
--
--     -   The path to the folder that contains the source code (for
--         example, @bucket-name\/path\/to\/source-code\/folder\/@).
--
-- -   For source code in a GitHub repository, the HTTPS clone URL to the
--     repository that contains the source and the build spec file.
--
-- -   For source code in a Bitbucket repository, the HTTPS clone URL to
--     the repository that contains the source and the build spec file.
awsCodeBuildProjectSource_location :: Lens.Lens' AwsCodeBuildProjectSource (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectSource_location = Lens.lens (\AwsCodeBuildProjectSource' {location} -> location) (\s@AwsCodeBuildProjectSource' {} a -> s {location = a} :: AwsCodeBuildProjectSource)

-- | The type of repository that contains the source code to be built. Valid
-- values are:
--
-- -   @BITBUCKET@ - The source code is in a Bitbucket repository.
--
-- -   @CODECOMMIT@ - The source code is in an CodeCommit repository.
--
-- -   @CODEPIPELINE@ - The source code settings are specified in the
--     source action of a pipeline in CodePipeline.
--
-- -   @GITHUB@ - The source code is in a GitHub repository.
--
-- -   @GITHUB_ENTERPRISE@ - The source code is in a GitHub Enterprise
--     repository.
--
-- -   @NO_SOURCE@ - The project does not have input source code.
--
-- -   @S3@ - The source code is in an S3 input bucket.
awsCodeBuildProjectSource_type :: Lens.Lens' AwsCodeBuildProjectSource (Prelude.Maybe Prelude.Text)
awsCodeBuildProjectSource_type = Lens.lens (\AwsCodeBuildProjectSource' {type'} -> type') (\s@AwsCodeBuildProjectSource' {} a -> s {type' = a} :: AwsCodeBuildProjectSource)

instance Data.FromJSON AwsCodeBuildProjectSource where
  parseJSON =
    Data.withObject
      "AwsCodeBuildProjectSource"
      ( \x ->
          AwsCodeBuildProjectSource'
            Prelude.<$> (x Data..:? "GitCloneDepth")
            Prelude.<*> (x Data..:? "InsecureSsl")
            Prelude.<*> (x Data..:? "Location")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable AwsCodeBuildProjectSource where
  hashWithSalt _salt AwsCodeBuildProjectSource' {..} =
    _salt `Prelude.hashWithSalt` gitCloneDepth
      `Prelude.hashWithSalt` insecureSsl
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AwsCodeBuildProjectSource where
  rnf AwsCodeBuildProjectSource' {..} =
    Prelude.rnf gitCloneDepth
      `Prelude.seq` Prelude.rnf insecureSsl
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON AwsCodeBuildProjectSource where
  toJSON AwsCodeBuildProjectSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GitCloneDepth" Data..=) Prelude.<$> gitCloneDepth,
            ("InsecureSsl" Data..=) Prelude.<$> insecureSsl,
            ("Location" Data..=) Prelude.<$> location,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )
