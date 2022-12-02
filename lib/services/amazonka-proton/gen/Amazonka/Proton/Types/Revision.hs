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
-- Module      : Amazonka.Proton.Types.Revision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.Revision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositoryProvider

-- | Revision detail data for a commit and push that activates a sync attempt
--
-- /See:/ 'newRevision' smart constructor.
data Revision = Revision'
  { -- | The repository branch.
    branch :: Prelude.Text,
    -- | The repository directory changed by a commit and push that activated the
    -- sync attempt.
    directory :: Prelude.Text,
    -- | The repository name.
    repositoryName :: Prelude.Text,
    -- | The repository provider.
    repositoryProvider :: RepositoryProvider,
    -- | The secure hash algorithm (SHA) hash for the revision.
    sha :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Revision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'revision_branch' - The repository branch.
--
-- 'directory', 'revision_directory' - The repository directory changed by a commit and push that activated the
-- sync attempt.
--
-- 'repositoryName', 'revision_repositoryName' - The repository name.
--
-- 'repositoryProvider', 'revision_repositoryProvider' - The repository provider.
--
-- 'sha', 'revision_sha' - The secure hash algorithm (SHA) hash for the revision.
newRevision ::
  -- | 'branch'
  Prelude.Text ->
  -- | 'directory'
  Prelude.Text ->
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'repositoryProvider'
  RepositoryProvider ->
  -- | 'sha'
  Prelude.Text ->
  Revision
newRevision
  pBranch_
  pDirectory_
  pRepositoryName_
  pRepositoryProvider_
  pSha_ =
    Revision'
      { branch = pBranch_,
        directory = pDirectory_,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        sha = pSha_
      }

-- | The repository branch.
revision_branch :: Lens.Lens' Revision Prelude.Text
revision_branch = Lens.lens (\Revision' {branch} -> branch) (\s@Revision' {} a -> s {branch = a} :: Revision)

-- | The repository directory changed by a commit and push that activated the
-- sync attempt.
revision_directory :: Lens.Lens' Revision Prelude.Text
revision_directory = Lens.lens (\Revision' {directory} -> directory) (\s@Revision' {} a -> s {directory = a} :: Revision)

-- | The repository name.
revision_repositoryName :: Lens.Lens' Revision Prelude.Text
revision_repositoryName = Lens.lens (\Revision' {repositoryName} -> repositoryName) (\s@Revision' {} a -> s {repositoryName = a} :: Revision)

-- | The repository provider.
revision_repositoryProvider :: Lens.Lens' Revision RepositoryProvider
revision_repositoryProvider = Lens.lens (\Revision' {repositoryProvider} -> repositoryProvider) (\s@Revision' {} a -> s {repositoryProvider = a} :: Revision)

-- | The secure hash algorithm (SHA) hash for the revision.
revision_sha :: Lens.Lens' Revision Prelude.Text
revision_sha = Lens.lens (\Revision' {sha} -> sha) (\s@Revision' {} a -> s {sha = a} :: Revision)

instance Data.FromJSON Revision where
  parseJSON =
    Data.withObject
      "Revision"
      ( \x ->
          Revision'
            Prelude.<$> (x Data..: "branch")
            Prelude.<*> (x Data..: "directory")
            Prelude.<*> (x Data..: "repositoryName")
            Prelude.<*> (x Data..: "repositoryProvider")
            Prelude.<*> (x Data..: "sha")
      )

instance Prelude.Hashable Revision where
  hashWithSalt _salt Revision' {..} =
    _salt `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` directory
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` sha

instance Prelude.NFData Revision where
  rnf Revision' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf directory
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf sha
