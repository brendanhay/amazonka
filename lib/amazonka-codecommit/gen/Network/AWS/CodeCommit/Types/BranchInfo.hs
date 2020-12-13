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
    biCommitId,
    biBranchName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a branch.
--
-- /See:/ 'mkBranchInfo' smart constructor.
data BranchInfo = BranchInfo'
  { -- | The ID of the last commit made to the branch.
    commitId :: Lude.Maybe Lude.Text,
    -- | The name of the branch.
    branchName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BranchInfo' with the minimum fields required to make a request.
--
-- * 'commitId' - The ID of the last commit made to the branch.
-- * 'branchName' - The name of the branch.
mkBranchInfo ::
  BranchInfo
mkBranchInfo =
  BranchInfo' {commitId = Lude.Nothing, branchName = Lude.Nothing}

-- | The ID of the last commit made to the branch.
--
-- /Note:/ Consider using 'commitId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biCommitId :: Lens.Lens' BranchInfo (Lude.Maybe Lude.Text)
biCommitId = Lens.lens (commitId :: BranchInfo -> Lude.Maybe Lude.Text) (\s a -> s {commitId = a} :: BranchInfo)
{-# DEPRECATED biCommitId "Use generic-lens or generic-optics with 'commitId' instead." #-}

-- | The name of the branch.
--
-- /Note:/ Consider using 'branchName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biBranchName :: Lens.Lens' BranchInfo (Lude.Maybe Lude.Text)
biBranchName = Lens.lens (branchName :: BranchInfo -> Lude.Maybe Lude.Text) (\s a -> s {branchName = a} :: BranchInfo)
{-# DEPRECATED biBranchName "Use generic-lens or generic-optics with 'branchName' instead." #-}

instance Lude.FromJSON BranchInfo where
  parseJSON =
    Lude.withObject
      "BranchInfo"
      ( \x ->
          BranchInfo'
            Lude.<$> (x Lude..:? "commitId") Lude.<*> (x Lude..:? "branchName")
      )
