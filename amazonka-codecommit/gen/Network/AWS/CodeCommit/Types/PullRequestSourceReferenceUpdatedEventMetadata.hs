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
-- Module      : Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an update to the source branch of a pull request.
--
-- /See:/ 'newPullRequestSourceReferenceUpdatedEventMetadata' smart constructor.
data PullRequestSourceReferenceUpdatedEventMetadata = PullRequestSourceReferenceUpdatedEventMetadata'
  { -- | The name of the repository where the pull request was updated.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit in the destination branch that was the
    -- tip of the branch at the time the pull request was updated.
    beforeCommitId :: Prelude.Maybe Prelude.Text,
    -- | The full commit ID of the commit in the source branch that was the tip
    -- of the branch at the time the pull request was updated.
    afterCommitId :: Prelude.Maybe Prelude.Text,
    -- | The commit ID of the most recent commit that the source branch and the
    -- destination branch have in common.
    mergeBase :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PullRequestSourceReferenceUpdatedEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryName', 'pullRequestSourceReferenceUpdatedEventMetadata_repositoryName' - The name of the repository where the pull request was updated.
--
-- 'beforeCommitId', 'pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId' - The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was updated.
--
-- 'afterCommitId', 'pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId' - The full commit ID of the commit in the source branch that was the tip
-- of the branch at the time the pull request was updated.
--
-- 'mergeBase', 'pullRequestSourceReferenceUpdatedEventMetadata_mergeBase' - The commit ID of the most recent commit that the source branch and the
-- destination branch have in common.
newPullRequestSourceReferenceUpdatedEventMetadata ::
  PullRequestSourceReferenceUpdatedEventMetadata
newPullRequestSourceReferenceUpdatedEventMetadata =
  PullRequestSourceReferenceUpdatedEventMetadata'
    { repositoryName =
        Prelude.Nothing,
      beforeCommitId =
        Prelude.Nothing,
      afterCommitId =
        Prelude.Nothing,
      mergeBase = Prelude.Nothing
    }

-- | The name of the repository where the pull request was updated.
pullRequestSourceReferenceUpdatedEventMetadata_repositoryName :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestSourceReferenceUpdatedEventMetadata_repositoryName = Lens.lens (\PullRequestSourceReferenceUpdatedEventMetadata' {repositoryName} -> repositoryName) (\s@PullRequestSourceReferenceUpdatedEventMetadata' {} a -> s {repositoryName = a} :: PullRequestSourceReferenceUpdatedEventMetadata)

-- | The full commit ID of the commit in the destination branch that was the
-- tip of the branch at the time the pull request was updated.
pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId = Lens.lens (\PullRequestSourceReferenceUpdatedEventMetadata' {beforeCommitId} -> beforeCommitId) (\s@PullRequestSourceReferenceUpdatedEventMetadata' {} a -> s {beforeCommitId = a} :: PullRequestSourceReferenceUpdatedEventMetadata)

-- | The full commit ID of the commit in the source branch that was the tip
-- of the branch at the time the pull request was updated.
pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId = Lens.lens (\PullRequestSourceReferenceUpdatedEventMetadata' {afterCommitId} -> afterCommitId) (\s@PullRequestSourceReferenceUpdatedEventMetadata' {} a -> s {afterCommitId = a} :: PullRequestSourceReferenceUpdatedEventMetadata)

-- | The commit ID of the most recent commit that the source branch and the
-- destination branch have in common.
pullRequestSourceReferenceUpdatedEventMetadata_mergeBase :: Lens.Lens' PullRequestSourceReferenceUpdatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestSourceReferenceUpdatedEventMetadata_mergeBase = Lens.lens (\PullRequestSourceReferenceUpdatedEventMetadata' {mergeBase} -> mergeBase) (\s@PullRequestSourceReferenceUpdatedEventMetadata' {} a -> s {mergeBase = a} :: PullRequestSourceReferenceUpdatedEventMetadata)

instance
  Prelude.FromJSON
    PullRequestSourceReferenceUpdatedEventMetadata
  where
  parseJSON =
    Prelude.withObject
      "PullRequestSourceReferenceUpdatedEventMetadata"
      ( \x ->
          PullRequestSourceReferenceUpdatedEventMetadata'
            Prelude.<$> (x Prelude..:? "repositoryName")
              Prelude.<*> (x Prelude..:? "beforeCommitId")
              Prelude.<*> (x Prelude..:? "afterCommitId")
              Prelude.<*> (x Prelude..:? "mergeBase")
      )

instance
  Prelude.Hashable
    PullRequestSourceReferenceUpdatedEventMetadata

instance
  Prelude.NFData
    PullRequestSourceReferenceUpdatedEventMetadata
