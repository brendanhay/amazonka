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
-- Module      : Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BatchUpdatePartitionFailureEntry where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a batch update partition error.
--
-- /See:/ 'newBatchUpdatePartitionFailureEntry' smart constructor.
data BatchUpdatePartitionFailureEntry = BatchUpdatePartitionFailureEntry'
  { -- | The details about the batch update partition error.
    errorDetail :: Prelude.Maybe ErrorDetail,
    -- | A list of values defining the partitions.
    partitionValueList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdatePartitionFailureEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDetail', 'batchUpdatePartitionFailureEntry_errorDetail' - The details about the batch update partition error.
--
-- 'partitionValueList', 'batchUpdatePartitionFailureEntry_partitionValueList' - A list of values defining the partitions.
newBatchUpdatePartitionFailureEntry ::
  BatchUpdatePartitionFailureEntry
newBatchUpdatePartitionFailureEntry =
  BatchUpdatePartitionFailureEntry'
    { errorDetail =
        Prelude.Nothing,
      partitionValueList = Prelude.Nothing
    }

-- | The details about the batch update partition error.
batchUpdatePartitionFailureEntry_errorDetail :: Lens.Lens' BatchUpdatePartitionFailureEntry (Prelude.Maybe ErrorDetail)
batchUpdatePartitionFailureEntry_errorDetail = Lens.lens (\BatchUpdatePartitionFailureEntry' {errorDetail} -> errorDetail) (\s@BatchUpdatePartitionFailureEntry' {} a -> s {errorDetail = a} :: BatchUpdatePartitionFailureEntry)

-- | A list of values defining the partitions.
batchUpdatePartitionFailureEntry_partitionValueList :: Lens.Lens' BatchUpdatePartitionFailureEntry (Prelude.Maybe [Prelude.Text])
batchUpdatePartitionFailureEntry_partitionValueList = Lens.lens (\BatchUpdatePartitionFailureEntry' {partitionValueList} -> partitionValueList) (\s@BatchUpdatePartitionFailureEntry' {} a -> s {partitionValueList = a} :: BatchUpdatePartitionFailureEntry) Prelude.. Lens.mapping Prelude._Coerce

instance
  Prelude.FromJSON
    BatchUpdatePartitionFailureEntry
  where
  parseJSON =
    Prelude.withObject
      "BatchUpdatePartitionFailureEntry"
      ( \x ->
          BatchUpdatePartitionFailureEntry'
            Prelude.<$> (x Prelude..:? "ErrorDetail")
            Prelude.<*> ( x Prelude..:? "PartitionValueList"
                            Prelude..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    BatchUpdatePartitionFailureEntry

instance
  Prelude.NFData
    BatchUpdatePartitionFailureEntry
