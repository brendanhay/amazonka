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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.CostEstimationTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The time range of a cost estimation.
--
-- /See:/ 'newCostEstimationTimeRange' smart constructor.
data CostEstimationTimeRange = CostEstimationTimeRange'
  { -- | The start time of the cost estimation.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The end time of the cost estimation.
    endTime :: Prelude.Maybe Core.POSIX
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
-- 'startTime', 'costEstimationTimeRange_startTime' - The start time of the cost estimation.
--
-- 'endTime', 'costEstimationTimeRange_endTime' - The end time of the cost estimation.
newCostEstimationTimeRange ::
  CostEstimationTimeRange
newCostEstimationTimeRange =
  CostEstimationTimeRange'
    { startTime =
        Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The start time of the cost estimation.
costEstimationTimeRange_startTime :: Lens.Lens' CostEstimationTimeRange (Prelude.Maybe Prelude.UTCTime)
costEstimationTimeRange_startTime = Lens.lens (\CostEstimationTimeRange' {startTime} -> startTime) (\s@CostEstimationTimeRange' {} a -> s {startTime = a} :: CostEstimationTimeRange) Prelude.. Lens.mapping Core._Time

-- | The end time of the cost estimation.
costEstimationTimeRange_endTime :: Lens.Lens' CostEstimationTimeRange (Prelude.Maybe Prelude.UTCTime)
costEstimationTimeRange_endTime = Lens.lens (\CostEstimationTimeRange' {endTime} -> endTime) (\s@CostEstimationTimeRange' {} a -> s {endTime = a} :: CostEstimationTimeRange) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CostEstimationTimeRange where
  parseJSON =
    Core.withObject
      "CostEstimationTimeRange"
      ( \x ->
          CostEstimationTimeRange'
            Prelude.<$> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "EndTime")
      )

instance Prelude.Hashable CostEstimationTimeRange where
  hashWithSalt _salt CostEstimationTimeRange' {..} =
    _salt `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData CostEstimationTimeRange where
  rnf CostEstimationTimeRange' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime
