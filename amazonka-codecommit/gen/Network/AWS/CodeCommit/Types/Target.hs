{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.Types.Target
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Target where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable Target

instance Prelude.NFData Target

instance Prelude.ToJSON Target where
  toJSON Target' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("destinationReference" Prelude..=)
              Prelude.<$> destinationReference,
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName),
            Prelude.Just
              ("sourceReference" Prelude..= sourceReference)
          ]
      )
