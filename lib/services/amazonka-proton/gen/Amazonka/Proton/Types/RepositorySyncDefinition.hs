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
-- Module      : Amazonka.Proton.Types.RepositorySyncDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositorySyncDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A repository sync definition.
--
-- /See:/ 'newRepositorySyncDefinition' smart constructor.
data RepositorySyncDefinition = RepositorySyncDefinition'
  { -- | The repository branch.
    branch :: Prelude.Text,
    -- | The directory in the repository.
    directory :: Prelude.Text,
    -- | The resource that is synced from.
    parent :: Prelude.Text,
    -- | The resource that is synced to.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositorySyncDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'repositorySyncDefinition_branch' - The repository branch.
--
-- 'directory', 'repositorySyncDefinition_directory' - The directory in the repository.
--
-- 'parent', 'repositorySyncDefinition_parent' - The resource that is synced from.
--
-- 'target', 'repositorySyncDefinition_target' - The resource that is synced to.
newRepositorySyncDefinition ::
  -- | 'branch'
  Prelude.Text ->
  -- | 'directory'
  Prelude.Text ->
  -- | 'parent'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  RepositorySyncDefinition
newRepositorySyncDefinition
  pBranch_
  pDirectory_
  pParent_
  pTarget_ =
    RepositorySyncDefinition'
      { branch = pBranch_,
        directory = pDirectory_,
        parent = pParent_,
        target = pTarget_
      }

-- | The repository branch.
repositorySyncDefinition_branch :: Lens.Lens' RepositorySyncDefinition Prelude.Text
repositorySyncDefinition_branch = Lens.lens (\RepositorySyncDefinition' {branch} -> branch) (\s@RepositorySyncDefinition' {} a -> s {branch = a} :: RepositorySyncDefinition)

-- | The directory in the repository.
repositorySyncDefinition_directory :: Lens.Lens' RepositorySyncDefinition Prelude.Text
repositorySyncDefinition_directory = Lens.lens (\RepositorySyncDefinition' {directory} -> directory) (\s@RepositorySyncDefinition' {} a -> s {directory = a} :: RepositorySyncDefinition)

-- | The resource that is synced from.
repositorySyncDefinition_parent :: Lens.Lens' RepositorySyncDefinition Prelude.Text
repositorySyncDefinition_parent = Lens.lens (\RepositorySyncDefinition' {parent} -> parent) (\s@RepositorySyncDefinition' {} a -> s {parent = a} :: RepositorySyncDefinition)

-- | The resource that is synced to.
repositorySyncDefinition_target :: Lens.Lens' RepositorySyncDefinition Prelude.Text
repositorySyncDefinition_target = Lens.lens (\RepositorySyncDefinition' {target} -> target) (\s@RepositorySyncDefinition' {} a -> s {target = a} :: RepositorySyncDefinition)

instance Data.FromJSON RepositorySyncDefinition where
  parseJSON =
    Data.withObject
      "RepositorySyncDefinition"
      ( \x ->
          RepositorySyncDefinition'
            Prelude.<$> (x Data..: "branch")
            Prelude.<*> (x Data..: "directory")
            Prelude.<*> (x Data..: "parent")
            Prelude.<*> (x Data..: "target")
      )

instance Prelude.Hashable RepositorySyncDefinition where
  hashWithSalt _salt RepositorySyncDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` directory
      `Prelude.hashWithSalt` parent
      `Prelude.hashWithSalt` target

instance Prelude.NFData RepositorySyncDefinition where
  rnf RepositorySyncDefinition' {..} =
    Prelude.rnf branch `Prelude.seq`
      Prelude.rnf directory `Prelude.seq`
        Prelude.rnf parent `Prelude.seq`
          Prelude.rnf target
