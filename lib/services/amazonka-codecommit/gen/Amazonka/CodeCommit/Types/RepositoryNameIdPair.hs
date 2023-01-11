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
-- Module      : Amazonka.CodeCommit.Types.RepositoryNameIdPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.RepositoryNameIdPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a repository name and ID.
--
-- /See:/ 'newRepositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
  { -- | The ID associated with the repository.
    repositoryId :: Prelude.Maybe Prelude.Text,
    -- | The name associated with the repository.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryNameIdPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryId', 'repositoryNameIdPair_repositoryId' - The ID associated with the repository.
--
-- 'repositoryName', 'repositoryNameIdPair_repositoryName' - The name associated with the repository.
newRepositoryNameIdPair ::
  RepositoryNameIdPair
newRepositoryNameIdPair =
  RepositoryNameIdPair'
    { repositoryId =
        Prelude.Nothing,
      repositoryName = Prelude.Nothing
    }

-- | The ID associated with the repository.
repositoryNameIdPair_repositoryId :: Lens.Lens' RepositoryNameIdPair (Prelude.Maybe Prelude.Text)
repositoryNameIdPair_repositoryId = Lens.lens (\RepositoryNameIdPair' {repositoryId} -> repositoryId) (\s@RepositoryNameIdPair' {} a -> s {repositoryId = a} :: RepositoryNameIdPair)

-- | The name associated with the repository.
repositoryNameIdPair_repositoryName :: Lens.Lens' RepositoryNameIdPair (Prelude.Maybe Prelude.Text)
repositoryNameIdPair_repositoryName = Lens.lens (\RepositoryNameIdPair' {repositoryName} -> repositoryName) (\s@RepositoryNameIdPair' {} a -> s {repositoryName = a} :: RepositoryNameIdPair)

instance Data.FromJSON RepositoryNameIdPair where
  parseJSON =
    Data.withObject
      "RepositoryNameIdPair"
      ( \x ->
          RepositoryNameIdPair'
            Prelude.<$> (x Data..:? "repositoryId")
            Prelude.<*> (x Data..:? "repositoryName")
      )

instance Prelude.Hashable RepositoryNameIdPair where
  hashWithSalt _salt RepositoryNameIdPair' {..} =
    _salt `Prelude.hashWithSalt` repositoryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData RepositoryNameIdPair where
  rnf RepositoryNameIdPair' {..} =
    Prelude.rnf repositoryId
      `Prelude.seq` Prelude.rnf repositoryName
