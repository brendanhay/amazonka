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
-- Module      : Amazonka.Proton.Types.RepositoryBranchInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositoryBranchInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositoryProvider

-- | Detail input data for a linked repository branch.
--
-- /See:/ 'newRepositoryBranchInput' smart constructor.
data RepositoryBranchInput = RepositoryBranchInput'
  { -- | The repository branch.
    branch :: Prelude.Text,
    -- | The repository name.
    name :: Prelude.Text,
    -- | The repository provider.
    provider :: RepositoryProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryBranchInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branch', 'repositoryBranchInput_branch' - The repository branch.
--
-- 'name', 'repositoryBranchInput_name' - The repository name.
--
-- 'provider', 'repositoryBranchInput_provider' - The repository provider.
newRepositoryBranchInput ::
  -- | 'branch'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'provider'
  RepositoryProvider ->
  RepositoryBranchInput
newRepositoryBranchInput pBranch_ pName_ pProvider_ =
  RepositoryBranchInput'
    { branch = pBranch_,
      name = pName_,
      provider = pProvider_
    }

-- | The repository branch.
repositoryBranchInput_branch :: Lens.Lens' RepositoryBranchInput Prelude.Text
repositoryBranchInput_branch = Lens.lens (\RepositoryBranchInput' {branch} -> branch) (\s@RepositoryBranchInput' {} a -> s {branch = a} :: RepositoryBranchInput)

-- | The repository name.
repositoryBranchInput_name :: Lens.Lens' RepositoryBranchInput Prelude.Text
repositoryBranchInput_name = Lens.lens (\RepositoryBranchInput' {name} -> name) (\s@RepositoryBranchInput' {} a -> s {name = a} :: RepositoryBranchInput)

-- | The repository provider.
repositoryBranchInput_provider :: Lens.Lens' RepositoryBranchInput RepositoryProvider
repositoryBranchInput_provider = Lens.lens (\RepositoryBranchInput' {provider} -> provider) (\s@RepositoryBranchInput' {} a -> s {provider = a} :: RepositoryBranchInput)

instance Prelude.Hashable RepositoryBranchInput where
  hashWithSalt _salt RepositoryBranchInput' {..} =
    _salt
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provider

instance Prelude.NFData RepositoryBranchInput where
  rnf RepositoryBranchInput' {..} =
    Prelude.rnf branch
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf provider

instance Data.ToJSON RepositoryBranchInput where
  toJSON RepositoryBranchInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("branch" Data..= branch),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("provider" Data..= provider)
          ]
      )
