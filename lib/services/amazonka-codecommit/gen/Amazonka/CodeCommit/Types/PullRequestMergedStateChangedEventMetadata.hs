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
-- Module      : Amazonka.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata where

import Amazonka.CodeCommit.Types.MergeMetadata
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Data.FromJSON
    PullRequestMergedStateChangedEventMetadata
  where
  parseJSON =
    Data.withObject
      "PullRequestMergedStateChangedEventMetadata"
      ( \x ->
          PullRequestMergedStateChangedEventMetadata'
            Prelude.<$> (x Data..:? "destinationReference")
              Prelude.<*> (x Data..:? "mergeMetadata")
              Prelude.<*> (x Data..:? "repositoryName")
      )

instance
  Prelude.Hashable
    PullRequestMergedStateChangedEventMetadata
  where
  hashWithSalt
    _salt
    PullRequestMergedStateChangedEventMetadata' {..} =
      _salt `Prelude.hashWithSalt` destinationReference
        `Prelude.hashWithSalt` mergeMetadata
        `Prelude.hashWithSalt` repositoryName

instance
  Prelude.NFData
    PullRequestMergedStateChangedEventMetadata
  where
  rnf PullRequestMergedStateChangedEventMetadata' {..} =
    Prelude.rnf destinationReference
      `Prelude.seq` Prelude.rnf mergeMetadata
      `Prelude.seq` Prelude.rnf repositoryName
