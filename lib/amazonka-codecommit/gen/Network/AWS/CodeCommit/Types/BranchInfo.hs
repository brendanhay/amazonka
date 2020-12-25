{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.BranchInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.BranchInfo
  ( BranchInfo (..),

    -- * Smart constructor
    mkBranchInfo,

    -- * Lenses
    biBranchName,
    biCommitId,
  )
where

import qualified Network.AWS.CodeCommit.Types.BranchName as Types
import qualified Network.AWS.CodeCommit.Types.CommitId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a branch.
--
-- /See:/ 'mkBranchInfo' smart constructor.
data BranchInfo = BranchInfo'
  { -- | The name of the branch.
    branchName :: Core.Maybe Types.BranchName,
    -- | The ID of the last commit made to the branch.
    commitId :: Core.Maybe Types.CommitId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BranchInfo' value with any optional fields omitted.
mkBranchInfo ::
  BranchInfo
mkBranchInfo =
  BranchInfo' {branchName = Core.Nothing, commitId = Core.Nothing}

-- | The name of the branch.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biBranchName :: Lens.Lens' BranchInfo (Core.Maybe Types.BranchName)
biBranchName = Lens.field @"branchName"
{-# DEPRECATED biBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

-- | The ID of the last commit made to the branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biCommitId :: Lens.Lens' BranchInfo (Core.Maybe Types.CommitId)
biCommitId = Lens.field @"commitId"
{-# DEPRECATED biCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

instance Core.FromJSON BranchInfo where
  parseJSON =
    Core.withObject "BranchInfo" Core.$
      \x ->
        BranchInfo'
          Core.<$> (x Core..:? "branchName") Core.<*> (x Core..:? "commitId")
