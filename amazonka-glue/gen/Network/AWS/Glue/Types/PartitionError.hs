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
-- Module      : Network.AWS.Glue.Types.PartitionError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionError where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens

-- | Contains information about a partition error.
--
-- /See:/ 'newPartitionError' smart constructor.
data PartitionError = PartitionError'
  { -- | The details about the partition error.
    errorDetail :: Core.Maybe ErrorDetail,
    -- | The values that define the partition.
    partitionValues :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PartitionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDetail', 'partitionError_errorDetail' - The details about the partition error.
--
-- 'partitionValues', 'partitionError_partitionValues' - The values that define the partition.
newPartitionError ::
  PartitionError
newPartitionError =
  PartitionError'
    { errorDetail = Core.Nothing,
      partitionValues = Core.Nothing
    }

-- | The details about the partition error.
partitionError_errorDetail :: Lens.Lens' PartitionError (Core.Maybe ErrorDetail)
partitionError_errorDetail = Lens.lens (\PartitionError' {errorDetail} -> errorDetail) (\s@PartitionError' {} a -> s {errorDetail = a} :: PartitionError)

-- | The values that define the partition.
partitionError_partitionValues :: Lens.Lens' PartitionError (Core.Maybe [Core.Text])
partitionError_partitionValues = Lens.lens (\PartitionError' {partitionValues} -> partitionValues) (\s@PartitionError' {} a -> s {partitionValues = a} :: PartitionError) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON PartitionError where
  parseJSON =
    Core.withObject
      "PartitionError"
      ( \x ->
          PartitionError'
            Core.<$> (x Core..:? "ErrorDetail")
            Core.<*> (x Core..:? "PartitionValues" Core..!= Core.mempty)
      )

instance Core.Hashable PartitionError

instance Core.NFData PartitionError
