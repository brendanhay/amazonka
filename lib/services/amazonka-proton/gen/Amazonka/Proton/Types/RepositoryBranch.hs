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
-- Module      : Amazonka.Proton.Types.RepositoryBranch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositoryBranch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositoryProvider

-- | Detail data for a linked repository branch.
--
-- /See:/ 'newRepositoryBranch' smart constructor.
data RepositoryBranch = RepositoryBranch'
  { -- | The Amazon Resource Name (ARN) of the linked repository.
    arn :: Prelude.Text,
    -- | The repository branch.
    branch :: Prelude.Text,
    -- | The repository name.
    name :: Prelude.Text,
    -- | The repository provider.
    provider :: RepositoryProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryBranch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'repositoryBranch_arn' - The Amazon Resource Name (ARN) of the linked repository.
--
-- 'branch', 'repositoryBranch_branch' - The repository branch.
--
-- 'name', 'repositoryBranch_name' - The repository name.
--
-- 'provider', 'repositoryBranch_provider' - The repository provider.
newRepositoryBranch ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'branch'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'provider'
  RepositoryProvider ->
  RepositoryBranch
newRepositoryBranch pArn_ pBranch_ pName_ pProvider_ =
  RepositoryBranch'
    { arn = pArn_,
      branch = pBranch_,
      name = pName_,
      provider = pProvider_
    }

-- | The Amazon Resource Name (ARN) of the linked repository.
repositoryBranch_arn :: Lens.Lens' RepositoryBranch Prelude.Text
repositoryBranch_arn = Lens.lens (\RepositoryBranch' {arn} -> arn) (\s@RepositoryBranch' {} a -> s {arn = a} :: RepositoryBranch)

-- | The repository branch.
repositoryBranch_branch :: Lens.Lens' RepositoryBranch Prelude.Text
repositoryBranch_branch = Lens.lens (\RepositoryBranch' {branch} -> branch) (\s@RepositoryBranch' {} a -> s {branch = a} :: RepositoryBranch)

-- | The repository name.
repositoryBranch_name :: Lens.Lens' RepositoryBranch Prelude.Text
repositoryBranch_name = Lens.lens (\RepositoryBranch' {name} -> name) (\s@RepositoryBranch' {} a -> s {name = a} :: RepositoryBranch)

-- | The repository provider.
repositoryBranch_provider :: Lens.Lens' RepositoryBranch RepositoryProvider
repositoryBranch_provider = Lens.lens (\RepositoryBranch' {provider} -> provider) (\s@RepositoryBranch' {} a -> s {provider = a} :: RepositoryBranch)

instance Core.FromJSON RepositoryBranch where
  parseJSON =
    Core.withObject
      "RepositoryBranch"
      ( \x ->
          RepositoryBranch'
            Prelude.<$> (x Core..: "arn")
            Prelude.<*> (x Core..: "branch")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "provider")
      )

instance Prelude.Hashable RepositoryBranch where
  hashWithSalt _salt RepositoryBranch' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provider

instance Prelude.NFData RepositoryBranch where
  rnf RepositoryBranch' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf branch
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf provider
