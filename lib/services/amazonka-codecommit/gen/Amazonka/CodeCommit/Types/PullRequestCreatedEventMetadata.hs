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
-- Module      : Amazonka.CodeCommit.Types.PullRequestCreatedEventMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.PullRequestCreatedEventMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata about the pull request that is used when comparing the pull
-- request source with its destination.
--
-- /See:/ 'newPullRequestCreatedEventMetadata' smart constructor.
data PullRequestCreatedEventMetadata = PullRequestCreatedEventMetadata'
  { -- | The commit ID of the tip of the branch specified as the destination
    -- branch when the pull request was created.
    destinationCommitId :: Prelude.Maybe Prelude.Text,
    -- | The commit ID of the most recent commit that the source branch and the
    -- destination branch have in common.
    mergeBase :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The commit ID on the source branch used when the pull request was
    -- created.
    sourceCommitId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PullRequestCreatedEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCommitId', 'pullRequestCreatedEventMetadata_destinationCommitId' - The commit ID of the tip of the branch specified as the destination
-- branch when the pull request was created.
--
-- 'mergeBase', 'pullRequestCreatedEventMetadata_mergeBase' - The commit ID of the most recent commit that the source branch and the
-- destination branch have in common.
--
-- 'repositoryName', 'pullRequestCreatedEventMetadata_repositoryName' - The name of the repository where the pull request was created.
--
-- 'sourceCommitId', 'pullRequestCreatedEventMetadata_sourceCommitId' - The commit ID on the source branch used when the pull request was
-- created.
newPullRequestCreatedEventMetadata ::
  PullRequestCreatedEventMetadata
newPullRequestCreatedEventMetadata =
  PullRequestCreatedEventMetadata'
    { destinationCommitId =
        Prelude.Nothing,
      mergeBase = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      sourceCommitId = Prelude.Nothing
    }

-- | The commit ID of the tip of the branch specified as the destination
-- branch when the pull request was created.
pullRequestCreatedEventMetadata_destinationCommitId :: Lens.Lens' PullRequestCreatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestCreatedEventMetadata_destinationCommitId = Lens.lens (\PullRequestCreatedEventMetadata' {destinationCommitId} -> destinationCommitId) (\s@PullRequestCreatedEventMetadata' {} a -> s {destinationCommitId = a} :: PullRequestCreatedEventMetadata)

-- | The commit ID of the most recent commit that the source branch and the
-- destination branch have in common.
pullRequestCreatedEventMetadata_mergeBase :: Lens.Lens' PullRequestCreatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestCreatedEventMetadata_mergeBase = Lens.lens (\PullRequestCreatedEventMetadata' {mergeBase} -> mergeBase) (\s@PullRequestCreatedEventMetadata' {} a -> s {mergeBase = a} :: PullRequestCreatedEventMetadata)

-- | The name of the repository where the pull request was created.
pullRequestCreatedEventMetadata_repositoryName :: Lens.Lens' PullRequestCreatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestCreatedEventMetadata_repositoryName = Lens.lens (\PullRequestCreatedEventMetadata' {repositoryName} -> repositoryName) (\s@PullRequestCreatedEventMetadata' {} a -> s {repositoryName = a} :: PullRequestCreatedEventMetadata)

-- | The commit ID on the source branch used when the pull request was
-- created.
pullRequestCreatedEventMetadata_sourceCommitId :: Lens.Lens' PullRequestCreatedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestCreatedEventMetadata_sourceCommitId = Lens.lens (\PullRequestCreatedEventMetadata' {sourceCommitId} -> sourceCommitId) (\s@PullRequestCreatedEventMetadata' {} a -> s {sourceCommitId = a} :: PullRequestCreatedEventMetadata)

instance
  Data.FromJSON
    PullRequestCreatedEventMetadata
  where
  parseJSON =
    Data.withObject
      "PullRequestCreatedEventMetadata"
      ( \x ->
          PullRequestCreatedEventMetadata'
            Prelude.<$> (x Data..:? "destinationCommitId")
            Prelude.<*> (x Data..:? "mergeBase")
            Prelude.<*> (x Data..:? "repositoryName")
            Prelude.<*> (x Data..:? "sourceCommitId")
      )

instance
  Prelude.Hashable
    PullRequestCreatedEventMetadata
  where
  hashWithSalt
    _salt
    PullRequestCreatedEventMetadata' {..} =
      _salt
        `Prelude.hashWithSalt` destinationCommitId
        `Prelude.hashWithSalt` mergeBase
        `Prelude.hashWithSalt` repositoryName
        `Prelude.hashWithSalt` sourceCommitId

instance
  Prelude.NFData
    PullRequestCreatedEventMetadata
  where
  rnf PullRequestCreatedEventMetadata' {..} =
    Prelude.rnf destinationCommitId `Prelude.seq`
      Prelude.rnf mergeBase `Prelude.seq`
        Prelude.rnf repositoryName `Prelude.seq`
          Prelude.rnf sourceCommitId
