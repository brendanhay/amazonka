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
-- Module      : Amazonka.DevOpsGuru.Types.CostEstimationTimeRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CostEstimationTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The time range of a cost estimation.
--
-- /See:/ 'newCostEstimationTimeRange' smart constructor.
data CostEstimationTimeRange = CostEstimationTimeRange'
  { -- | The end time of the cost estimation.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The start time of the cost estimation.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostEstimationTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'costEstimationTimeRange_endTime' - The end time of the cost estimation.
--
-- 'startTime', 'costEstimationTimeRange_startTime' - The start time of the cost estimation.
newCostEstimationTimeRange ::
  CostEstimationTimeRange
newCostEstimationTimeRange =
  CostEstimationTimeRange'
    { endTime = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The end time of the cost estimation.
costEstimationTimeRange_endTime :: Lens.Lens' CostEstimationTimeRange (Prelude.Maybe Prelude.UTCTime)
costEstimationTimeRange_endTime = Lens.lens (\CostEstimationTimeRange' {endTime} -> endTime) (\s@CostEstimationTimeRange' {} a -> s {endTime = a} :: CostEstimationTimeRange) Prelude.. Lens.mapping Data._Time

-- | The start time of the cost estimation.
costEstimationTimeRange_startTime :: Lens.Lens' CostEstimationTimeRange (Prelude.Maybe Prelude.UTCTime)
costEstimationTimeRange_startTime = Lens.lens (\CostEstimationTimeRange' {startTime} -> startTime) (\s@CostEstimationTimeRange' {} a -> s {startTime = a} :: CostEstimationTimeRange) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON CostEstimationTimeRange where
  parseJSON =
    Data.withObject
      "CostEstimationTimeRange"
      ( \x ->
          CostEstimationTimeRange'
            Prelude.<$> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "StartTime")
      )

instance Prelude.Hashable CostEstimationTimeRange where
  hashWithSalt _salt CostEstimationTimeRange' {..} =
    _salt `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData CostEstimationTimeRange where
  rnf CostEstimationTimeRange' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
