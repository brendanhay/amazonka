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
-- Module      : Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata where

import Network.AWS.CodeCommit.Types.MergeMetadata
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about the change in the merge state for a pull
-- request event.
--
-- /See:/ 'newPullRequestMergedStateChangedEventMetadata' smart constructor.
data PullRequestMergedStateChangedEventMetadata = PullRequestMergedStateChangedEventMetadata'
  { -- | The name of the branch that the pull request is merged into.
    destinationReference :: Prelude.Maybe Prelude.Text,
    -- | Information about the merge state change event.
    mergeMetadata :: Prelude.Maybe MergeMetadata,
    -- | The name of the repository where the pull request was created.
    repositoryName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PullRequestMergedStateChangedEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationReference', 'pullRequestMergedStateChangedEventMetadata_destinationReference' - The name of the branch that the pull request is merged into.
--
-- 'mergeMetadata', 'pullRequestMergedStateChangedEventMetadata_mergeMetadata' - Information about the merge state change event.
--
-- 'repositoryName', 'pullRequestMergedStateChangedEventMetadata_repositoryName' - The name of the repository where the pull request was created.
newPullRequestMergedStateChangedEventMetadata ::
  PullRequestMergedStateChangedEventMetadata
newPullRequestMergedStateChangedEventMetadata =
  PullRequestMergedStateChangedEventMetadata'
    { destinationReference =
        Prelude.Nothing,
      mergeMetadata = Prelude.Nothing,
      repositoryName =
        Prelude.Nothing
    }

-- | The name of the branch that the pull request is merged into.
pullRequestMergedStateChangedEventMetadata_destinationReference :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestMergedStateChangedEventMetadata_destinationReference = Lens.lens (\PullRequestMergedStateChangedEventMetadata' {destinationReference} -> destinationReference) (\s@PullRequestMergedStateChangedEventMetadata' {} a -> s {destinationReference = a} :: PullRequestMergedStateChangedEventMetadata)

-- | Information about the merge state change event.
pullRequestMergedStateChangedEventMetadata_mergeMetadata :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Prelude.Maybe MergeMetadata)
pullRequestMergedStateChangedEventMetadata_mergeMetadata = Lens.lens (\PullRequestMergedStateChangedEventMetadata' {mergeMetadata} -> mergeMetadata) (\s@PullRequestMergedStateChangedEventMetadata' {} a -> s {mergeMetadata = a} :: PullRequestMergedStateChangedEventMetadata)

-- | The name of the repository where the pull request was created.
pullRequestMergedStateChangedEventMetadata_repositoryName :: Lens.Lens' PullRequestMergedStateChangedEventMetadata (Prelude.Maybe Prelude.Text)
pullRequestMergedStateChangedEventMetadata_repositoryName = Lens.lens (\PullRequestMergedStateChangedEventMetadata' {repositoryName} -> repositoryName) (\s@PullRequestMergedStateChangedEventMetadata' {} a -> s {repositoryName = a} :: PullRequestMergedStateChangedEventMetadata)

instance
  Prelude.FromJSON
    PullRequestMergedStateChangedEventMetadata
  where
  parseJSON =
    Prelude.withObject
      "PullRequestMergedStateChangedEventMetadata"
      ( \x ->
          PullRequestMergedStateChangedEventMetadata'
            Prelude.<$> (x Prelude..:? "destinationReference")
              Prelude.<*> (x Prelude..:? "mergeMetadata")
              Prelude.<*> (x Prelude..:? "repositoryName")
      )

instance
  Prelude.Hashable
    PullRequestMergedStateChangedEventMetadata

instance
  Prelude.NFData
    PullRequestMergedStateChangedEventMetadata
