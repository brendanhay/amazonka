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
-- Module      : Amazonka.CodeCommit.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a target for a pull request.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | The branch of the repository where the pull request changes are merged.
    -- Also known as the destination branch.
    destinationReference :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the pull request.
    repositoryName :: Prelude.Text,
    -- | The branch of the repository that contains the changes for the pull
    -- request. Also known as the source branch.
    sourceReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationReference', 'target_destinationReference' - The branch of the repository where the pull request changes are merged.
-- Also known as the destination branch.
--
-- 'repositoryName', 'target_repositoryName' - The name of the repository that contains the pull request.
--
-- 'sourceReference', 'target_sourceReference' - The branch of the repository that contains the changes for the pull
-- request. Also known as the source branch.
newTarget ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'sourceReference'
  Prelude.Text ->
  Target
newTarget pRepositoryName_ pSourceReference_ =
  Target'
    { destinationReference = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      sourceReference = pSourceReference_
    }

-- | The branch of the repository where the pull request changes are merged.
-- Also known as the destination branch.
target_destinationReference :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_destinationReference = Lens.lens (\Target' {destinationReference} -> destinationReference) (\s@Target' {} a -> s {destinationReference = a} :: Target)

-- | The name of the repository that contains the pull request.
target_repositoryName :: Lens.Lens' Target Prelude.Text
target_repositoryName = Lens.lens (\Target' {repositoryName} -> repositoryName) (\s@Target' {} a -> s {repositoryName = a} :: Target)

-- | The branch of the repository that contains the changes for the pull
-- request. Also known as the source branch.
target_sourceReference :: Lens.Lens' Target Prelude.Text
target_sourceReference = Lens.lens (\Target' {sourceReference} -> sourceReference) (\s@Target' {} a -> s {sourceReference = a} :: Target)

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt
      `Prelude.hashWithSalt` destinationReference
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` sourceReference

instance Prelude.NFData Target where
  rnf Target' {..} =
    Prelude.rnf destinationReference
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf sourceReference

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("destinationReference" Data..=)
              Prelude.<$> destinationReference,
            Prelude.Just
              ("repositoryName" Data..= repositoryName),
            Prelude.Just
              ("sourceReference" Data..= sourceReference)
          ]
      )
