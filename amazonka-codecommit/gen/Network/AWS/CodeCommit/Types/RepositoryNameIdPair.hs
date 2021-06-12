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
-- Module      : Network.AWS.CodeCommit.Types.RepositoryNameIdPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryNameIdPair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a repository name and ID.
--
-- /See:/ 'newRepositoryNameIdPair' smart constructor.
data RepositoryNameIdPair = RepositoryNameIdPair'
  { -- | The name associated with the repository.
    repositoryName :: Core.Maybe Core.Text,
    -- | The ID associated with the repository.
    repositoryId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RepositoryNameIdPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'repositoryNameIdPair_repositoryName' - The name associated with the repository.
--
-- 'repositoryId', 'repositoryNameIdPair_repositoryId' - The ID associated with the repository.
newRepositoryNameIdPair ::
  RepositoryNameIdPair
newRepositoryNameIdPair =
  RepositoryNameIdPair'
    { repositoryName =
        Core.Nothing,
      repositoryId = Core.Nothing
    }

-- | The name associated with the repository.
repositoryNameIdPair_repositoryName :: Lens.Lens' RepositoryNameIdPair (Core.Maybe Core.Text)
repositoryNameIdPair_repositoryName = Lens.lens (\RepositoryNameIdPair' {repositoryName} -> repositoryName) (\s@RepositoryNameIdPair' {} a -> s {repositoryName = a} :: RepositoryNameIdPair)

-- | The ID associated with the repository.
repositoryNameIdPair_repositoryId :: Lens.Lens' RepositoryNameIdPair (Core.Maybe Core.Text)
repositoryNameIdPair_repositoryId = Lens.lens (\RepositoryNameIdPair' {repositoryId} -> repositoryId) (\s@RepositoryNameIdPair' {} a -> s {repositoryId = a} :: RepositoryNameIdPair)

instance Core.FromJSON RepositoryNameIdPair where
  parseJSON =
    Core.withObject
      "RepositoryNameIdPair"
      ( \x ->
          RepositoryNameIdPair'
            Core.<$> (x Core..:? "repositoryName")
            Core.<*> (x Core..:? "repositoryId")
      )

instance Core.Hashable RepositoryNameIdPair

instance Core.NFData RepositoryNameIdPair
