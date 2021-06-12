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
-- Module      : Network.AWS.CodeCommit.Types.MergeMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeMetadata where

import Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about a merge or potential merge between a source
-- reference and a destination reference in a pull request.
--
-- /See:/ 'newMergeMetadata' smart constructor.
data MergeMetadata = MergeMetadata'
  { -- | The Amazon Resource Name (ARN) of the user who merged the branches.
    mergedBy :: Core.Maybe Core.Text,
    -- | The commit ID for the merge commit, if any.
    mergeCommitId :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether the merge has been made.
    isMerged :: Core.Maybe Core.Bool,
    -- | The merge strategy used in the merge.
    mergeOption :: Core.Maybe MergeOptionTypeEnum
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MergeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mergedBy', 'mergeMetadata_mergedBy' - The Amazon Resource Name (ARN) of the user who merged the branches.
--
-- 'mergeCommitId', 'mergeMetadata_mergeCommitId' - The commit ID for the merge commit, if any.
--
-- 'isMerged', 'mergeMetadata_isMerged' - A Boolean value indicating whether the merge has been made.
--
-- 'mergeOption', 'mergeMetadata_mergeOption' - The merge strategy used in the merge.
newMergeMetadata ::
  MergeMetadata
newMergeMetadata =
  MergeMetadata'
    { mergedBy = Core.Nothing,
      mergeCommitId = Core.Nothing,
      isMerged = Core.Nothing,
      mergeOption = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user who merged the branches.
mergeMetadata_mergedBy :: Lens.Lens' MergeMetadata (Core.Maybe Core.Text)
mergeMetadata_mergedBy = Lens.lens (\MergeMetadata' {mergedBy} -> mergedBy) (\s@MergeMetadata' {} a -> s {mergedBy = a} :: MergeMetadata)

-- | The commit ID for the merge commit, if any.
mergeMetadata_mergeCommitId :: Lens.Lens' MergeMetadata (Core.Maybe Core.Text)
mergeMetadata_mergeCommitId = Lens.lens (\MergeMetadata' {mergeCommitId} -> mergeCommitId) (\s@MergeMetadata' {} a -> s {mergeCommitId = a} :: MergeMetadata)

-- | A Boolean value indicating whether the merge has been made.
mergeMetadata_isMerged :: Lens.Lens' MergeMetadata (Core.Maybe Core.Bool)
mergeMetadata_isMerged = Lens.lens (\MergeMetadata' {isMerged} -> isMerged) (\s@MergeMetadata' {} a -> s {isMerged = a} :: MergeMetadata)

-- | The merge strategy used in the merge.
mergeMetadata_mergeOption :: Lens.Lens' MergeMetadata (Core.Maybe MergeOptionTypeEnum)
mergeMetadata_mergeOption = Lens.lens (\MergeMetadata' {mergeOption} -> mergeOption) (\s@MergeMetadata' {} a -> s {mergeOption = a} :: MergeMetadata)

instance Core.FromJSON MergeMetadata where
  parseJSON =
    Core.withObject
      "MergeMetadata"
      ( \x ->
          MergeMetadata'
            Core.<$> (x Core..:? "mergedBy")
            Core.<*> (x Core..:? "mergeCommitId")
            Core.<*> (x Core..:? "isMerged")
            Core.<*> (x Core..:? "mergeOption")
      )

instance Core.Hashable MergeMetadata

instance Core.NFData MergeMetadata
