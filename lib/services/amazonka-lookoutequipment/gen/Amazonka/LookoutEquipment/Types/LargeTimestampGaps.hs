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
-- Module      : Amazonka.LookoutEquipment.Types.LargeTimestampGaps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.LargeTimestampGaps where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.StatisticalIssueStatus
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information on large gaps between consecutive
-- timestamps in data.
--
-- /See:/ 'newLargeTimestampGaps' smart constructor.
data LargeTimestampGaps = LargeTimestampGaps'
  { -- | Indicates the size of the largest timestamp gap, in days.
    maxTimestampGapInDays :: Prelude.Maybe Prelude.Int,
    -- | Indicates the number of large timestamp gaps, if there are any.
    numberOfLargeTimestampGaps :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether there is a potential data issue related to large gaps
    -- in timestamps.
    status :: StatisticalIssueStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LargeTimestampGaps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxTimestampGapInDays', 'largeTimestampGaps_maxTimestampGapInDays' - Indicates the size of the largest timestamp gap, in days.
--
-- 'numberOfLargeTimestampGaps', 'largeTimestampGaps_numberOfLargeTimestampGaps' - Indicates the number of large timestamp gaps, if there are any.
--
-- 'status', 'largeTimestampGaps_status' - Indicates whether there is a potential data issue related to large gaps
-- in timestamps.
newLargeTimestampGaps ::
  -- | 'status'
  StatisticalIssueStatus ->
  LargeTimestampGaps
newLargeTimestampGaps pStatus_ =
  LargeTimestampGaps'
    { maxTimestampGapInDays =
        Prelude.Nothing,
      numberOfLargeTimestampGaps = Prelude.Nothing,
      status = pStatus_
    }

-- | Indicates the size of the largest timestamp gap, in days.
largeTimestampGaps_maxTimestampGapInDays :: Lens.Lens' LargeTimestampGaps (Prelude.Maybe Prelude.Int)
largeTimestampGaps_maxTimestampGapInDays = Lens.lens (\LargeTimestampGaps' {maxTimestampGapInDays} -> maxTimestampGapInDays) (\s@LargeTimestampGaps' {} a -> s {maxTimestampGapInDays = a} :: LargeTimestampGaps)

-- | Indicates the number of large timestamp gaps, if there are any.
largeTimestampGaps_numberOfLargeTimestampGaps :: Lens.Lens' LargeTimestampGaps (Prelude.Maybe Prelude.Int)
largeTimestampGaps_numberOfLargeTimestampGaps = Lens.lens (\LargeTimestampGaps' {numberOfLargeTimestampGaps} -> numberOfLargeTimestampGaps) (\s@LargeTimestampGaps' {} a -> s {numberOfLargeTimestampGaps = a} :: LargeTimestampGaps)

-- | Indicates whether there is a potential data issue related to large gaps
-- in timestamps.
largeTimestampGaps_status :: Lens.Lens' LargeTimestampGaps StatisticalIssueStatus
largeTimestampGaps_status = Lens.lens (\LargeTimestampGaps' {status} -> status) (\s@LargeTimestampGaps' {} a -> s {status = a} :: LargeTimestampGaps)

instance Data.FromJSON LargeTimestampGaps where
  parseJSON =
    Data.withObject
      "LargeTimestampGaps"
      ( \x ->
          LargeTimestampGaps'
            Prelude.<$> (x Data..:? "MaxTimestampGapInDays")
            Prelude.<*> (x Data..:? "NumberOfLargeTimestampGaps")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable LargeTimestampGaps where
  hashWithSalt _salt LargeTimestampGaps' {..} =
    _salt
      `Prelude.hashWithSalt` maxTimestampGapInDays
      `Prelude.hashWithSalt` numberOfLargeTimestampGaps
      `Prelude.hashWithSalt` status

instance Prelude.NFData LargeTimestampGaps where
  rnf LargeTimestampGaps' {..} =
    Prelude.rnf maxTimestampGapInDays
      `Prelude.seq` Prelude.rnf numberOfLargeTimestampGaps
      `Prelude.seq` Prelude.rnf status
