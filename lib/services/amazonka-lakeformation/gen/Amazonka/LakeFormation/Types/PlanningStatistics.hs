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
-- Module      : Amazonka.LakeFormation.Types.PlanningStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.PlanningStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Statistics related to the processing of a query statement.
--
-- /See:/ 'newPlanningStatistics' smart constructor.
data PlanningStatistics = PlanningStatistics'
  { -- | An estimate of the data that was scanned in bytes.
    estimatedDataToScanBytes :: Prelude.Maybe Prelude.Integer,
    -- | The time that it took to process the request.
    planningTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The time the request was in queue to be processed.
    queueTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The number of work units generated.
    workUnitsGeneratedCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PlanningStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedDataToScanBytes', 'planningStatistics_estimatedDataToScanBytes' - An estimate of the data that was scanned in bytes.
--
-- 'planningTimeMillis', 'planningStatistics_planningTimeMillis' - The time that it took to process the request.
--
-- 'queueTimeMillis', 'planningStatistics_queueTimeMillis' - The time the request was in queue to be processed.
--
-- 'workUnitsGeneratedCount', 'planningStatistics_workUnitsGeneratedCount' - The number of work units generated.
newPlanningStatistics ::
  PlanningStatistics
newPlanningStatistics =
  PlanningStatistics'
    { estimatedDataToScanBytes =
        Prelude.Nothing,
      planningTimeMillis = Prelude.Nothing,
      queueTimeMillis = Prelude.Nothing,
      workUnitsGeneratedCount = Prelude.Nothing
    }

-- | An estimate of the data that was scanned in bytes.
planningStatistics_estimatedDataToScanBytes :: Lens.Lens' PlanningStatistics (Prelude.Maybe Prelude.Integer)
planningStatistics_estimatedDataToScanBytes = Lens.lens (\PlanningStatistics' {estimatedDataToScanBytes} -> estimatedDataToScanBytes) (\s@PlanningStatistics' {} a -> s {estimatedDataToScanBytes = a} :: PlanningStatistics)

-- | The time that it took to process the request.
planningStatistics_planningTimeMillis :: Lens.Lens' PlanningStatistics (Prelude.Maybe Prelude.Integer)
planningStatistics_planningTimeMillis = Lens.lens (\PlanningStatistics' {planningTimeMillis} -> planningTimeMillis) (\s@PlanningStatistics' {} a -> s {planningTimeMillis = a} :: PlanningStatistics)

-- | The time the request was in queue to be processed.
planningStatistics_queueTimeMillis :: Lens.Lens' PlanningStatistics (Prelude.Maybe Prelude.Integer)
planningStatistics_queueTimeMillis = Lens.lens (\PlanningStatistics' {queueTimeMillis} -> queueTimeMillis) (\s@PlanningStatistics' {} a -> s {queueTimeMillis = a} :: PlanningStatistics)

-- | The number of work units generated.
planningStatistics_workUnitsGeneratedCount :: Lens.Lens' PlanningStatistics (Prelude.Maybe Prelude.Integer)
planningStatistics_workUnitsGeneratedCount = Lens.lens (\PlanningStatistics' {workUnitsGeneratedCount} -> workUnitsGeneratedCount) (\s@PlanningStatistics' {} a -> s {workUnitsGeneratedCount = a} :: PlanningStatistics)

instance Data.FromJSON PlanningStatistics where
  parseJSON =
    Data.withObject
      "PlanningStatistics"
      ( \x ->
          PlanningStatistics'
            Prelude.<$> (x Data..:? "EstimatedDataToScanBytes")
            Prelude.<*> (x Data..:? "PlanningTimeMillis")
            Prelude.<*> (x Data..:? "QueueTimeMillis")
            Prelude.<*> (x Data..:? "WorkUnitsGeneratedCount")
      )

instance Prelude.Hashable PlanningStatistics where
  hashWithSalt _salt PlanningStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` estimatedDataToScanBytes
      `Prelude.hashWithSalt` planningTimeMillis
      `Prelude.hashWithSalt` queueTimeMillis
      `Prelude.hashWithSalt` workUnitsGeneratedCount

instance Prelude.NFData PlanningStatistics where
  rnf PlanningStatistics' {..} =
    Prelude.rnf estimatedDataToScanBytes
      `Prelude.seq` Prelude.rnf planningTimeMillis
      `Prelude.seq` Prelude.rnf queueTimeMillis
      `Prelude.seq` Prelude.rnf workUnitsGeneratedCount
