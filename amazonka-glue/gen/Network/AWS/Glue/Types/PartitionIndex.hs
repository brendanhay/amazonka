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
-- Module      : Network.AWS.Glue.Types.PartitionIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndex where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure for a partition index.
--
-- /See:/ 'newPartitionIndex' smart constructor.
data PartitionIndex = PartitionIndex'
  { -- | The keys for the partition index.
    keys :: Core.NonEmpty Core.Text,
    -- | The name of the partition index.
    indexName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PartitionIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keys', 'partitionIndex_keys' - The keys for the partition index.
--
-- 'indexName', 'partitionIndex_indexName' - The name of the partition index.
newPartitionIndex ::
  -- | 'keys'
  Core.NonEmpty Core.Text ->
  -- | 'indexName'
  Core.Text ->
  PartitionIndex
newPartitionIndex pKeys_ pIndexName_ =
  PartitionIndex'
    { keys = Lens._Coerce Lens.# pKeys_,
      indexName = pIndexName_
    }

-- | The keys for the partition index.
partitionIndex_keys :: Lens.Lens' PartitionIndex (Core.NonEmpty Core.Text)
partitionIndex_keys = Lens.lens (\PartitionIndex' {keys} -> keys) (\s@PartitionIndex' {} a -> s {keys = a} :: PartitionIndex) Core.. Lens._Coerce

-- | The name of the partition index.
partitionIndex_indexName :: Lens.Lens' PartitionIndex Core.Text
partitionIndex_indexName = Lens.lens (\PartitionIndex' {indexName} -> indexName) (\s@PartitionIndex' {} a -> s {indexName = a} :: PartitionIndex)

instance Core.Hashable PartitionIndex

instance Core.NFData PartitionIndex

instance Core.ToJSON PartitionIndex where
  toJSON PartitionIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Keys" Core..= keys),
            Core.Just ("IndexName" Core..= indexName)
          ]
      )
