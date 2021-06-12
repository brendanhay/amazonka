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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a target for a pull request.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | The branch of the repository where the pull request changes are merged.
    -- Also known as the destination branch.
    destinationReference :: Core.Maybe Core.Text,
    -- | The name of the repository that contains the pull request.
    repositoryName :: Core.Text,
    -- | The branch of the repository that contains the changes for the pull
    -- request. Also known as the source branch.
    sourceReference :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'sourceReference'
  Core.Text ->
  Target
newTarget pRepositoryName_ pSourceReference_ =
  Target'
    { destinationReference = Core.Nothing,
      repositoryName = pRepositoryName_,
      sourceReference = pSourceReference_
    }

-- | The branch of the repository where the pull request changes are merged.
-- Also known as the destination branch.
target_destinationReference :: Lens.Lens' Target (Core.Maybe Core.Text)
target_destinationReference = Lens.lens (\Target' {destinationReference} -> destinationReference) (\s@Target' {} a -> s {destinationReference = a} :: Target)

-- | The name of the repository that contains the pull request.
target_repositoryName :: Lens.Lens' Target Core.Text
target_repositoryName = Lens.lens (\Target' {repositoryName} -> repositoryName) (\s@Target' {} a -> s {repositoryName = a} :: Target)

-- | The branch of the repository that contains the changes for the pull
-- request. Also known as the source branch.
target_sourceReference :: Lens.Lens' Target Core.Text
target_sourceReference = Lens.lens (\Target' {sourceReference} -> sourceReference) (\s@Target' {} a -> s {sourceReference = a} :: Target)

instance Core.Hashable Target

instance Core.NFData Target

instance Core.ToJSON Target where
  toJSON Target' {..} =
    Core.object
      ( Core.catMaybes
          [ ("destinationReference" Core..=)
              Core.<$> destinationReference,
            Core.Just ("repositoryName" Core..= repositoryName),
            Core.Just
              ("sourceReference" Core..= sourceReference)
          ]
      )
