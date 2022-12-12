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
-- Module      : Amazonka.CodeGuruReviewer.Types.CommitDiffSourceCodeType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.CommitDiffSourceCodeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A type of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies the commit diff for a pull request on an associated
-- repository. The @SourceCommit@ and @DestinationCommit@ fields are
-- required to do a pull request code review.
--
-- /See:/ 'newCommitDiffSourceCodeType' smart constructor.
data CommitDiffSourceCodeType = CommitDiffSourceCodeType'
  { -- | The SHA of the destination commit used to generate a commit diff. This
    -- field is required for a pull request code review.
    destinationCommit :: Prelude.Maybe Prelude.Text,
    -- | The SHA of the merge base of a commit.
    mergeBaseCommit :: Prelude.Maybe Prelude.Text,
    -- | The SHA of the source commit used to generate a commit diff. This field
    -- is required for a pull request code review.
    sourceCommit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommitDiffSourceCodeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCommit', 'commitDiffSourceCodeType_destinationCommit' - The SHA of the destination commit used to generate a commit diff. This
-- field is required for a pull request code review.
--
-- 'mergeBaseCommit', 'commitDiffSourceCodeType_mergeBaseCommit' - The SHA of the merge base of a commit.
--
-- 'sourceCommit', 'commitDiffSourceCodeType_sourceCommit' - The SHA of the source commit used to generate a commit diff. This field
-- is required for a pull request code review.
newCommitDiffSourceCodeType ::
  CommitDiffSourceCodeType
newCommitDiffSourceCodeType =
  CommitDiffSourceCodeType'
    { destinationCommit =
        Prelude.Nothing,
      mergeBaseCommit = Prelude.Nothing,
      sourceCommit = Prelude.Nothing
    }

-- | The SHA of the destination commit used to generate a commit diff. This
-- field is required for a pull request code review.
commitDiffSourceCodeType_destinationCommit :: Lens.Lens' CommitDiffSourceCodeType (Prelude.Maybe Prelude.Text)
commitDiffSourceCodeType_destinationCommit = Lens.lens (\CommitDiffSourceCodeType' {destinationCommit} -> destinationCommit) (\s@CommitDiffSourceCodeType' {} a -> s {destinationCommit = a} :: CommitDiffSourceCodeType)

-- | The SHA of the merge base of a commit.
commitDiffSourceCodeType_mergeBaseCommit :: Lens.Lens' CommitDiffSourceCodeType (Prelude.Maybe Prelude.Text)
commitDiffSourceCodeType_mergeBaseCommit = Lens.lens (\CommitDiffSourceCodeType' {mergeBaseCommit} -> mergeBaseCommit) (\s@CommitDiffSourceCodeType' {} a -> s {mergeBaseCommit = a} :: CommitDiffSourceCodeType)

-- | The SHA of the source commit used to generate a commit diff. This field
-- is required for a pull request code review.
commitDiffSourceCodeType_sourceCommit :: Lens.Lens' CommitDiffSourceCodeType (Prelude.Maybe Prelude.Text)
commitDiffSourceCodeType_sourceCommit = Lens.lens (\CommitDiffSourceCodeType' {sourceCommit} -> sourceCommit) (\s@CommitDiffSourceCodeType' {} a -> s {sourceCommit = a} :: CommitDiffSourceCodeType)

instance Data.FromJSON CommitDiffSourceCodeType where
  parseJSON =
    Data.withObject
      "CommitDiffSourceCodeType"
      ( \x ->
          CommitDiffSourceCodeType'
            Prelude.<$> (x Data..:? "DestinationCommit")
            Prelude.<*> (x Data..:? "MergeBaseCommit")
            Prelude.<*> (x Data..:? "SourceCommit")
      )

instance Prelude.Hashable CommitDiffSourceCodeType where
  hashWithSalt _salt CommitDiffSourceCodeType' {..} =
    _salt `Prelude.hashWithSalt` destinationCommit
      `Prelude.hashWithSalt` mergeBaseCommit
      `Prelude.hashWithSalt` sourceCommit

instance Prelude.NFData CommitDiffSourceCodeType where
  rnf CommitDiffSourceCodeType' {..} =
    Prelude.rnf destinationCommit
      `Prelude.seq` Prelude.rnf mergeBaseCommit
      `Prelude.seq` Prelude.rnf sourceCommit

instance Data.ToJSON CommitDiffSourceCodeType where
  toJSON CommitDiffSourceCodeType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationCommit" Data..=)
              Prelude.<$> destinationCommit,
            ("MergeBaseCommit" Data..=)
              Prelude.<$> mergeBaseCommit,
            ("SourceCommit" Data..=) Prelude.<$> sourceCommit
          ]
      )
