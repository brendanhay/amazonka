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
-- Module      : Amazonka.EBS.Types.ChangedBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EBS.Types.ChangedBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A block of data in an Amazon Elastic Block Store snapshot that is
-- different from another snapshot of the same volume\/snapshot lineage.
--
-- /See:/ 'newChangedBlock' smart constructor.
data ChangedBlock = ChangedBlock'
  { -- | The block token for the block index of the @SecondSnapshotId@ specified
    -- in the @ListChangedBlocks@ operation.
    secondBlockToken :: Prelude.Maybe Prelude.Text,
    -- | The block token for the block index of the @FirstSnapshotId@ specified
    -- in the @ListChangedBlocks@ operation. This value is absent if the first
    -- snapshot does not have the changed block that is on the second snapshot.
    firstBlockToken :: Prelude.Maybe Prelude.Text,
    -- | The block index.
    blockIndex :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangedBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'secondBlockToken', 'changedBlock_secondBlockToken' - The block token for the block index of the @SecondSnapshotId@ specified
-- in the @ListChangedBlocks@ operation.
--
-- 'firstBlockToken', 'changedBlock_firstBlockToken' - The block token for the block index of the @FirstSnapshotId@ specified
-- in the @ListChangedBlocks@ operation. This value is absent if the first
-- snapshot does not have the changed block that is on the second snapshot.
--
-- 'blockIndex', 'changedBlock_blockIndex' - The block index.
newChangedBlock ::
  ChangedBlock
newChangedBlock =
  ChangedBlock'
    { secondBlockToken = Prelude.Nothing,
      firstBlockToken = Prelude.Nothing,
      blockIndex = Prelude.Nothing
    }

-- | The block token for the block index of the @SecondSnapshotId@ specified
-- in the @ListChangedBlocks@ operation.
changedBlock_secondBlockToken :: Lens.Lens' ChangedBlock (Prelude.Maybe Prelude.Text)
changedBlock_secondBlockToken = Lens.lens (\ChangedBlock' {secondBlockToken} -> secondBlockToken) (\s@ChangedBlock' {} a -> s {secondBlockToken = a} :: ChangedBlock)

-- | The block token for the block index of the @FirstSnapshotId@ specified
-- in the @ListChangedBlocks@ operation. This value is absent if the first
-- snapshot does not have the changed block that is on the second snapshot.
changedBlock_firstBlockToken :: Lens.Lens' ChangedBlock (Prelude.Maybe Prelude.Text)
changedBlock_firstBlockToken = Lens.lens (\ChangedBlock' {firstBlockToken} -> firstBlockToken) (\s@ChangedBlock' {} a -> s {firstBlockToken = a} :: ChangedBlock)

-- | The block index.
changedBlock_blockIndex :: Lens.Lens' ChangedBlock (Prelude.Maybe Prelude.Natural)
changedBlock_blockIndex = Lens.lens (\ChangedBlock' {blockIndex} -> blockIndex) (\s@ChangedBlock' {} a -> s {blockIndex = a} :: ChangedBlock)

instance Data.FromJSON ChangedBlock where
  parseJSON =
    Data.withObject
      "ChangedBlock"
      ( \x ->
          ChangedBlock'
            Prelude.<$> (x Data..:? "SecondBlockToken")
            Prelude.<*> (x Data..:? "FirstBlockToken")
            Prelude.<*> (x Data..:? "BlockIndex")
      )

instance Prelude.Hashable ChangedBlock where
  hashWithSalt _salt ChangedBlock' {..} =
    _salt `Prelude.hashWithSalt` secondBlockToken
      `Prelude.hashWithSalt` firstBlockToken
      `Prelude.hashWithSalt` blockIndex

instance Prelude.NFData ChangedBlock where
  rnf ChangedBlock' {..} =
    Prelude.rnf secondBlockToken
      `Prelude.seq` Prelude.rnf firstBlockToken
      `Prelude.seq` Prelude.rnf blockIndex
