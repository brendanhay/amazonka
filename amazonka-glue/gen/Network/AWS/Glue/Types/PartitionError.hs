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
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a partition error.
--
-- /See:/ 'newPartitionError' smart constructor.
data PartitionError = PartitionError'
  { -- | The details about the partition error.
    errorDetail :: Prelude.Maybe ErrorDetail,
    -- | The values that define the partition.
    partitionValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { errorDetail = Prelude.Nothing,
      partitionValues = Prelude.Nothing
    }

-- | The details about the partition error.
partitionError_errorDetail :: Lens.Lens' PartitionError (Prelude.Maybe ErrorDetail)
partitionError_errorDetail = Lens.lens (\PartitionError' {errorDetail} -> errorDetail) (\s@PartitionError' {} a -> s {errorDetail = a} :: PartitionError)

-- | The values that define the partition.
partitionError_partitionValues :: Lens.Lens' PartitionError (Prelude.Maybe [Prelude.Text])
partitionError_partitionValues = Lens.lens (\PartitionError' {partitionValues} -> partitionValues) (\s@PartitionError' {} a -> s {partitionValues = a} :: PartitionError) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON PartitionError where
  parseJSON =
    Core.withObject
      "PartitionError"
      ( \x ->
          PartitionError'
            Prelude.<$> (x Core..:? "ErrorDetail")
            Prelude.<*> ( x Core..:? "PartitionValues"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PartitionError

instance Prelude.NFData PartitionError
