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
-- Module      : Amazonka.Proton.Types.RepositorySummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.RepositorySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositoryProvider

-- | Summary data of a linked repository—a repository that has been
-- registered with Proton.
--
-- /See:/ 'newRepositorySummary' smart constructor.
data RepositorySummary = RepositorySummary'
  { -- | The Amazon Resource Name (ARN) of the linked repository.
    arn :: Prelude.Text,
    -- | The repository name.
    name :: Prelude.Text,
    -- | The repository provider.
    provider :: RepositoryProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositorySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'repositorySummary_arn' - The Amazon Resource Name (ARN) of the linked repository.
--
-- 'name', 'repositorySummary_name' - The repository name.
--
-- 'provider', 'repositorySummary_provider' - The repository provider.
newRepositorySummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'provider'
  RepositoryProvider ->
  RepositorySummary
newRepositorySummary pArn_ pName_ pProvider_ =
  RepositorySummary'
    { arn = pArn_,
      name = pName_,
      provider = pProvider_
    }

-- | The Amazon Resource Name (ARN) of the linked repository.
repositorySummary_arn :: Lens.Lens' RepositorySummary Prelude.Text
repositorySummary_arn = Lens.lens (\RepositorySummary' {arn} -> arn) (\s@RepositorySummary' {} a -> s {arn = a} :: RepositorySummary)

-- | The repository name.
repositorySummary_name :: Lens.Lens' RepositorySummary Prelude.Text
repositorySummary_name = Lens.lens (\RepositorySummary' {name} -> name) (\s@RepositorySummary' {} a -> s {name = a} :: RepositorySummary)

-- | The repository provider.
repositorySummary_provider :: Lens.Lens' RepositorySummary RepositoryProvider
repositorySummary_provider = Lens.lens (\RepositorySummary' {provider} -> provider) (\s@RepositorySummary' {} a -> s {provider = a} :: RepositorySummary)

instance Data.FromJSON RepositorySummary where
  parseJSON =
    Data.withObject
      "RepositorySummary"
      ( \x ->
          RepositorySummary'
            Prelude.<$> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "provider")
      )

instance Prelude.Hashable RepositorySummary where
  hashWithSalt _salt RepositorySummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provider

instance Prelude.NFData RepositorySummary where
  rnf RepositorySummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf provider
