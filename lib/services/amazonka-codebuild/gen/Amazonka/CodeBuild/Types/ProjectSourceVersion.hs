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
-- Module      : Amazonka.CodeBuild.Types.ProjectSourceVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectSourceVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A source identifier and its corresponding version.
--
-- /See:/ 'newProjectSourceVersion' smart constructor.
data ProjectSourceVersion = ProjectSourceVersion'
  { -- | An identifier for a source in the build project. The identifier can only
    -- contain alphanumeric characters and underscores, and must be less than
    -- 128 characters in length.
    sourceIdentifier :: Prelude.Text,
    -- | The source version for the corresponding source identifier. If
    -- specified, must be one of:
    --
    -- -   For CodeCommit: the commit ID, branch, or Git tag to use.
    --
    -- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
    --     that corresponds to the version of the source code you want to
    --     build. If a pull request ID is specified, it must use the format
    --     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
    --     specified, the branch\'s HEAD commit ID is used. If not specified,
    --     the default branch\'s HEAD commit ID is used.
    --
    -- -   For Bitbucket: the commit ID, branch name, or tag name that
    --     corresponds to the version of the source code you want to build. If
    --     a branch name is specified, the branch\'s HEAD commit ID is used. If
    --     not specified, the default branch\'s HEAD commit ID is used.
    --
    -- -   For Amazon S3: the version ID of the object that represents the
    --     build input ZIP file to use.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
    -- in the /CodeBuild User Guide/.
    sourceVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectSourceVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceIdentifier', 'projectSourceVersion_sourceIdentifier' - An identifier for a source in the build project. The identifier can only
-- contain alphanumeric characters and underscores, and must be less than
-- 128 characters in length.
--
-- 'sourceVersion', 'projectSourceVersion_sourceVersion' - The source version for the corresponding source identifier. If
-- specified, must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
newProjectSourceVersion ::
  -- | 'sourceIdentifier'
  Prelude.Text ->
  -- | 'sourceVersion'
  Prelude.Text ->
  ProjectSourceVersion
newProjectSourceVersion
  pSourceIdentifier_
  pSourceVersion_ =
    ProjectSourceVersion'
      { sourceIdentifier =
          pSourceIdentifier_,
        sourceVersion = pSourceVersion_
      }

-- | An identifier for a source in the build project. The identifier can only
-- contain alphanumeric characters and underscores, and must be less than
-- 128 characters in length.
projectSourceVersion_sourceIdentifier :: Lens.Lens' ProjectSourceVersion Prelude.Text
projectSourceVersion_sourceIdentifier = Lens.lens (\ProjectSourceVersion' {sourceIdentifier} -> sourceIdentifier) (\s@ProjectSourceVersion' {} a -> s {sourceIdentifier = a} :: ProjectSourceVersion)

-- | The source version for the corresponding source identifier. If
-- specified, must be one of:
--
-- -   For CodeCommit: the commit ID, branch, or Git tag to use.
--
-- -   For GitHub: the commit ID, pull request ID, branch name, or tag name
--     that corresponds to the version of the source code you want to
--     build. If a pull request ID is specified, it must use the format
--     @pr\/pull-request-ID@ (for example, @pr\/25@). If a branch name is
--     specified, the branch\'s HEAD commit ID is used. If not specified,
--     the default branch\'s HEAD commit ID is used.
--
-- -   For Bitbucket: the commit ID, branch name, or tag name that
--     corresponds to the version of the source code you want to build. If
--     a branch name is specified, the branch\'s HEAD commit ID is used. If
--     not specified, the default branch\'s HEAD commit ID is used.
--
-- -   For Amazon S3: the version ID of the object that represents the
--     build input ZIP file to use.
--
-- For more information, see
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/sample-source-version.html Source Version Sample with CodeBuild>
-- in the /CodeBuild User Guide/.
projectSourceVersion_sourceVersion :: Lens.Lens' ProjectSourceVersion Prelude.Text
projectSourceVersion_sourceVersion = Lens.lens (\ProjectSourceVersion' {sourceVersion} -> sourceVersion) (\s@ProjectSourceVersion' {} a -> s {sourceVersion = a} :: ProjectSourceVersion)

instance Data.FromJSON ProjectSourceVersion where
  parseJSON =
    Data.withObject
      "ProjectSourceVersion"
      ( \x ->
          ProjectSourceVersion'
            Prelude.<$> (x Data..: "sourceIdentifier")
            Prelude.<*> (x Data..: "sourceVersion")
      )

instance Prelude.Hashable ProjectSourceVersion where
  hashWithSalt _salt ProjectSourceVersion' {..} =
    _salt
      `Prelude.hashWithSalt` sourceIdentifier
      `Prelude.hashWithSalt` sourceVersion

instance Prelude.NFData ProjectSourceVersion where
  rnf ProjectSourceVersion' {..} =
    Prelude.rnf sourceIdentifier
      `Prelude.seq` Prelude.rnf sourceVersion

instance Data.ToJSON ProjectSourceVersion where
  toJSON ProjectSourceVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceIdentifier" Data..= sourceIdentifier),
            Prelude.Just
              ("sourceVersion" Data..= sourceVersion)
          ]
      )
