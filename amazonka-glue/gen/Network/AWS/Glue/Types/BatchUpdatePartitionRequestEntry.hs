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
-- Module      : Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchUpdatePartitionRequestEntry where

import Network.AWS.Glue.Types.PartitionInput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchUpdatePartitionRequestEntry_partitionValueList = Lens.lens (\BatchUpdatePartitionRequestEntry' {partitionValueList} -> partitionValueList) (\s@BatchUpdatePartitionRequestEntry' {} a -> s {partitionValueList = a} :: BatchUpdatePartitionRequestEntry) Prelude.. Prelude._Coerce

-- | The structure used to update a partition.
batchUpdatePartitionRequestEntry_partitionInput :: Lens.Lens' BatchUpdatePartitionRequestEntry PartitionInput
batchUpdatePartitionRequestEntry_partitionInput = Lens.lens (\BatchUpdatePartitionRequestEntry' {partitionInput} -> partitionInput) (\s@BatchUpdatePartitionRequestEntry' {} a -> s {partitionInput = a} :: BatchUpdatePartitionRequestEntry)

instance
  Prelude.Hashable
    BatchUpdatePartitionRequestEntry

instance
  Prelude.NFData
    BatchUpdatePartitionRequestEntry

instance
  Prelude.ToJSON
    BatchUpdatePartitionRequestEntry
  where
  toJSON BatchUpdatePartitionRequestEntry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PartitionValueList" Prelude..= partitionValueList),
            Prelude.Just
              ("PartitionInput" Prelude..= partitionInput)
          ]
      )
