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
-- Module      : Amazonka.CodeCommit.Types.MergeMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.MergeMetadata where

import Amazonka.CodeCommit.Types.MergeOptionTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a merge or potential merge between a source
-- reference and a destination reference in a pull request.
--
-- /See:/ 'newMergeMetadata' smart constructor.
data MergeMetadata = MergeMetadata'
  { -- | A Boolean value indicating whether the merge has been made.
    isMerged :: Prelude.Maybe Prelude.Bool,
    -- | The commit ID for the merge commit, if any.
    mergeCommitId :: Prelude.Maybe Prelude.Text,
    -- | The merge strategy used in the merge.
    mergeOption :: Prelude.Maybe MergeOptionTypeEnum,
    -- | The Amazon Resource Name (ARN) of the user who merged the branches.
    mergedBy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isMerged', 'mergeMetadata_isMerged' - A Boolean value indicating whether the merge has been made.
--
-- 'mergeCommitId', 'mergeMetadata_mergeCommitId' - The commit ID for the merge commit, if any.
--
-- 'mergeOption', 'mergeMetadata_mergeOption' - The merge strategy used in the merge.
--
-- 'mergedBy', 'mergeMetadata_mergedBy' - The Amazon Resource Name (ARN) of the user who merged the branches.
newMergeMetadata ::
  MergeMetadata
newMergeMetadata =
  MergeMetadata'
    { isMerged = Prelude.Nothing,
      mergeCommitId = Prelude.Nothing,
      mergeOption = Prelude.Nothing,
      mergedBy = Prelude.Nothing
    }

-- | A Boolean value indicating whether the merge has been made.
mergeMetadata_isMerged :: Lens.Lens' MergeMetadata (Prelude.Maybe Prelude.Bool)
mergeMetadata_isMerged = Lens.lens (\MergeMetadata' {isMerged} -> isMerged) (\s@MergeMetadata' {} a -> s {isMerged = a} :: MergeMetadata)

-- | The commit ID for the merge commit, if any.
mergeMetadata_mergeCommitId :: Lens.Lens' MergeMetadata (Prelude.Maybe Prelude.Text)
mergeMetadata_mergeCommitId = Lens.lens (\MergeMetadata' {mergeCommitId} -> mergeCommitId) (\s@MergeMetadata' {} a -> s {mergeCommitId = a} :: MergeMetadata)

-- | The merge strategy used in the merge.
mergeMetadata_mergeOption :: Lens.Lens' MergeMetadata (Prelude.Maybe MergeOptionTypeEnum)
mergeMetadata_mergeOption = Lens.lens (\MergeMetadata' {mergeOption} -> mergeOption) (\s@MergeMetadata' {} a -> s {mergeOption = a} :: MergeMetadata)

-- | The Amazon Resource Name (ARN) of the user who merged the branches.
mergeMetadata_mergedBy :: Lens.Lens' MergeMetadata (Prelude.Maybe Prelude.Text)
mergeMetadata_mergedBy = Lens.lens (\MergeMetadata' {mergedBy} -> mergedBy) (\s@MergeMetadata' {} a -> s {mergedBy = a} :: MergeMetadata)

instance Data.FromJSON MergeMetadata where
  parseJSON =
    Data.withObject
      "MergeMetadata"
      ( \x ->
          MergeMetadata'
            Prelude.<$> (x Data..:? "isMerged")
            Prelude.<*> (x Data..:? "mergeCommitId")
            Prelude.<*> (x Data..:? "mergeOption")
            Prelude.<*> (x Data..:? "mergedBy")
      )

instance Prelude.Hashable MergeMetadata where
  hashWithSalt _salt MergeMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` isMerged
      `Prelude.hashWithSalt` mergeCommitId
      `Prelude.hashWithSalt` mergeOption
      `Prelude.hashWithSalt` mergedBy

instance Prelude.NFData MergeMetadata where
  rnf MergeMetadata' {..} =
    Prelude.rnf isMerged
      `Prelude.seq` Prelude.rnf mergeCommitId
      `Prelude.seq` Prelude.rnf mergeOption
      `Prelude.seq` Prelude.rnf mergedBy
