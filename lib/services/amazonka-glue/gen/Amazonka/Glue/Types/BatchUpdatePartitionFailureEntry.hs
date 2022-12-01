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
-- Module      : Amazonka.Glue.Types.BatchUpdatePartitionFailureEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BatchUpdatePartitionFailureEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.ErrorDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a batch update partition error.
--
-- /See:/ 'newBatchUpdatePartitionFailureEntry' smart constructor.
data BatchUpdatePartitionFailureEntry = BatchUpdatePartitionFailureEntry'
  { -- | A list of values defining the partitions.
    partitionValueList :: Prelude.Maybe [Prelude.Text],
    -- | The details about the batch update partition error.
    errorDetail :: Prelude.Maybe ErrorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdatePartitionFailureEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionValueList', 'batchUpdatePartitionFailureEntry_partitionValueList' - A list of values defining the partitions.
--
-- 'errorDetail', 'batchUpdatePartitionFailureEntry_errorDetail' - The details about the batch update partition error.
newBatchUpdatePartitionFailureEntry ::
  BatchUpdatePartitionFailureEntry
newBatchUpdatePartitionFailureEntry =
  BatchUpdatePartitionFailureEntry'
    { partitionValueList =
        Prelude.Nothing,
      errorDetail = Prelude.Nothing
    }

-- | A list of values defining the partitions.
batchUpdatePartitionFailureEntry_partitionValueList :: Lens.Lens' BatchUpdatePartitionFailureEntry (Prelude.Maybe [Prelude.Text])
batchUpdatePartitionFailureEntry_partitionValueList = Lens.lens (\BatchUpdatePartitionFailureEntry' {partitionValueList} -> partitionValueList) (\s@BatchUpdatePartitionFailureEntry' {} a -> s {partitionValueList = a} :: BatchUpdatePartitionFailureEntry) Prelude.. Lens.mapping Lens.coerced

-- | The details about the batch update partition error.
batchUpdatePartitionFailureEntry_errorDetail :: Lens.Lens' BatchUpdatePartitionFailureEntry (Prelude.Maybe ErrorDetail)
batchUpdatePartitionFailureEntry_errorDetail = Lens.lens (\BatchUpdatePartitionFailureEntry' {errorDetail} -> errorDetail) (\s@BatchUpdatePartitionFailureEntry' {} a -> s {errorDetail = a} :: BatchUpdatePartitionFailureEntry)

instance
  Core.FromJSON
    BatchUpdatePartitionFailureEntry
  where
  parseJSON =
    Core.withObject
      "BatchUpdatePartitionFailureEntry"
      ( \x ->
          BatchUpdatePartitionFailureEntry'
            Prelude.<$> ( x Core..:? "PartitionValueList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ErrorDetail")
      )

instance
  Prelude.Hashable
    BatchUpdatePartitionFailureEntry
  where
  hashWithSalt
    _salt
    BatchUpdatePartitionFailureEntry' {..} =
      _salt `Prelude.hashWithSalt` partitionValueList
        `Prelude.hashWithSalt` errorDetail

instance
  Prelude.NFData
    BatchUpdatePartitionFailureEntry
  where
  rnf BatchUpdatePartitionFailureEntry' {..} =
    Prelude.rnf partitionValueList
      `Prelude.seq` Prelude.rnf errorDetail
