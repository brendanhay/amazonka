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
-- Module      : Network.AWS.CodeCommit.Types.PullRequestTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestTarget where

import Network.AWS.CodeCommit.Types.MergeMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about a pull request target.
--
-- /See:/ 'newPullRequestTarget' smart constructor.
data PullRequestTarget = PullRequestTarget'
  { -- | The branch of the repository where the pull request changes are merged.
    -- Also known as the destination branch.
    destinationReference :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the tip of the source branch used to create the
    -- pull request. If the pull request branch is updated by a push while the
    -- pull request is open, the commit ID changes to reflect the new tip of
    -- the branch.
    sourceCommit :: Prelude.Maybe Prelude.Text,
    -- | Returns metadata about the state of the merge, including whether the
    -- merge has been made.
    mergeMetadata :: Prelude.Maybe MergeMetadata,
    -- | The name of the repository that contains the pull request source and
    -- destination branches.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The branch of the repository that contains the changes for the pull
    -- request. Also known as the source branch.
    sourceReference :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID that is the tip of the destination branch. This is
    -- the commit where the pull request was or will be merged.
    destinationCommit :: Prelude.Maybe Prelude.Text,
    -- | The commit ID of the most recent commit that the source branch and the
    -- destination branch have in common.
    mergeBase :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PullRequestTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationReference', 'pullRequestTarget_destinationReference' - The branch of the repository where the pull request changes are merged.
-- Also known as the destination branch.
--
-- 'sourceCommit', 'pullRequestTarget_sourceCommit' - The full commit ID of the tip of the source branch used to create the
-- pull request. If the pull request branch is updated by a push while the
-- pull request is open, the commit ID changes to reflect the new tip of
-- the branch.
--
-- 'mergeMetadata', 'pullRequestTarget_mergeMetadata' - Returns metadata about the state of the merge, including whether the
-- merge has been made.
--
-- 'repositoryName', 'pullRequestTarget_repositoryName' - The name of the repository that contains the pull request source and
-- destination branches.
--
-- 'sourceReference', 'pullRequestTarget_sourceReference' - The branch of the repository that contains the changes for the pull
-- request. Also known as the source branch.
--
-- 'destinationCommit', 'pullRequestTarget_destinationCommit' - The full commit ID that is the tip of the destination branch. This is
-- the commit where the pull request was or will be merged.
--
-- 'mergeBase', 'pullRequestTarget_mergeBase' - The commit ID of the most recent commit that the source branch and the
-- destination branch have in common.
newPullRequestTarget ::
  PullRequestTarget
newPullRequestTarget =
  PullRequestTarget'
    { destinationReference =
        Prelude.Nothing,
      sourceCommit = Prelude.Nothing,
      mergeMetadata = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      sourceReference = Prelude.Nothing,
      destinationCommit = Prelude.Nothing,
      mergeBase = Prelude.Nothing
    }

-- | The branch of the repository where the pull request changes are merged.
-- Also known as the destination branch.
pullRequestTarget_destinationReference :: Lens.Lens' PullRequestTarget (Prelude.Maybe Prelude.Text)
pullRequestTarget_destinationReference = Lens.lens (\PullRequestTarget' {destinationReference} -> destinationReference) (\s@PullRequestTarget' {} a -> s {destinationReference = a} :: PullRequestTarget)

-- | The full commit ID of the tip of the source branch used to create the
-- pull request. If the pull request branch is updated by a push while the
-- pull request is open, the commit ID changes to reflect the new tip of
-- the branch.
pullRequestTarget_sourceCommit :: Lens.Lens' PullRequestTarget (Prelude.Maybe Prelude.Text)
pullRequestTarget_sourceCommit = Lens.lens (\PullRequestTarget' {sourceCommit} -> sourceCommit) (\s@PullRequestTarget' {} a -> s {sourceCommit = a} :: PullRequestTarget)

-- | Returns metadata about the state of the merge, including whether the
-- merge has been made.
pullRequestTarget_mergeMetadata :: Lens.Lens' PullRequestTarget (Prelude.Maybe MergeMetadata)
pullRequestTarget_mergeMetadata = Lens.lens (\PullRequestTarget' {mergeMetadata} -> mergeMetadata) (\s@PullRequestTarget' {} a -> s {mergeMetadata = a} :: PullRequestTarget)

-- | The name of the repository that contains the pull request source and
-- destination branches.
pullRequestTarget_repositoryName :: Lens.Lens' PullRequestTarget (Prelude.Maybe Prelude.Text)
pullRequestTarget_repositoryName = Lens.lens (\PullRequestTarget' {repositoryName} -> repositoryName) (\s@PullRequestTarget' {} a -> s {repositoryName = a} :: PullRequestTarget)

-- | The branch of the repository that contains the changes for the pull
-- request. Also known as the source branch.
pullRequestTarget_sourceReference :: Lens.Lens' PullRequestTarget (Prelude.Maybe Prelude.Text)
pullRequestTarget_sourceReference = Lens.lens (\PullRequestTarget' {sourceReference} -> sourceReference) (\s@PullRequestTarget' {} a -> s {sourceReference = a} :: PullRequestTarget)

-- | The full commit ID that is the tip of the destination branch. This is
-- the commit where the pull request was or will be merged.
pullRequestTarget_destinationCommit :: Lens.Lens' PullRequestTarget (Prelude.Maybe Prelude.Text)
pullRequestTarget_destinationCommit = Lens.lens (\PullRequestTarget' {destinationCommit} -> destinationCommit) (\s@PullRequestTarget' {} a -> s {destinationCommit = a} :: PullRequestTarget)

-- | The commit ID of the most recent commit that the source branch and the
-- destination branch have in common.
pullRequestTarget_mergeBase :: Lens.Lens' PullRequestTarget (Prelude.Maybe Prelude.Text)
pullRequestTarget_mergeBase = Lens.lens (\PullRequestTarget' {mergeBase} -> mergeBase) (\s@PullRequestTarget' {} a -> s {mergeBase = a} :: PullRequestTarget)

instance Prelude.FromJSON PullRequestTarget where
  parseJSON =
    Prelude.withObject
      "PullRequestTarget"
      ( \x ->
          PullRequestTarget'
            Prelude.<$> (x Prelude..:? "destinationReference")
            Prelude.<*> (x Prelude..:? "sourceCommit")
            Prelude.<*> (x Prelude..:? "mergeMetadata")
            Prelude.<*> (x Prelude..:? "repositoryName")
            Prelude.<*> (x Prelude..:? "sourceReference")
            Prelude.<*> (x Prelude..:? "destinationCommit")
            Prelude.<*> (x Prelude..:? "mergeBase")
      )

instance Prelude.Hashable PullRequestTarget

instance Prelude.NFData PullRequestTarget
