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
-- Module      : Amazonka.Glue.Types.BatchUpdatePartitionRequestEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BatchUpdatePartitionRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.PartitionInput
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the values and structure used to update a
-- partition.
--
-- /See:/ 'newBatchUpdatePartitionRequestEntry' smart constructor.
data BatchUpdatePartitionRequestEntry = BatchUpdatePartitionRequestEntry'
  { -- | A list of values defining the partitions.
    partitionValueList :: [Prelude.Text],
    -- | The structure used to update a partition.
    partitionInput :: PartitionInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdatePartitionRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionValueList', 'batchUpdatePartitionRequestEntry_partitionValueList' - A list of values defining the partitions.
--
-- 'partitionInput', 'batchUpdatePartitionRequestEntry_partitionInput' - The structure used to update a partition.
newBatchUpdatePartitionRequestEntry ::
  -- | 'partitionInput'
  PartitionInput ->
  BatchUpdatePartitionRequestEntry
newBatchUpdatePartitionRequestEntry pPartitionInput_ =
  BatchUpdatePartitionRequestEntry'
    { partitionValueList =
        Prelude.mempty,
      partitionInput = pPartitionInput_
    }

-- | A list of values defining the partitions.
batchUpdatePartitionRequestEntry_partitionValueList :: Lens.Lens' BatchUpdatePartitionRequestEntry [Prelude.Text]
batchUpdatePartitionRequestEntry_partitionValueList = Lens.lens (\BatchUpdatePartitionRequestEntry' {partitionValueList} -> partitionValueList) (\s@BatchUpdatePartitionRequestEntry' {} a -> s {partitionValueList = a} :: BatchUpdatePartitionRequestEntry) Prelude.. Lens.coerced

-- | The structure used to update a partition.
batchUpdatePartitionRequestEntry_partitionInput :: Lens.Lens' BatchUpdatePartitionRequestEntry PartitionInput
batchUpdatePartitionRequestEntry_partitionInput = Lens.lens (\BatchUpdatePartitionRequestEntry' {partitionInput} -> partitionInput) (\s@BatchUpdatePartitionRequestEntry' {} a -> s {partitionInput = a} :: BatchUpdatePartitionRequestEntry)

instance
  Prelude.Hashable
    BatchUpdatePartitionRequestEntry
  where
  hashWithSalt
    _salt
    BatchUpdatePartitionRequestEntry' {..} =
      _salt `Prelude.hashWithSalt` partitionValueList
        `Prelude.hashWithSalt` partitionInput

instance
  Prelude.NFData
    BatchUpdatePartitionRequestEntry
  where
  rnf BatchUpdatePartitionRequestEntry' {..} =
    Prelude.rnf partitionValueList
      `Prelude.seq` Prelude.rnf partitionInput

instance Data.ToJSON BatchUpdatePartitionRequestEntry where
  toJSON BatchUpdatePartitionRequestEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PartitionValueList" Data..= partitionValueList),
            Prelude.Just
              ("PartitionInput" Data..= partitionInput)
          ]
      )
