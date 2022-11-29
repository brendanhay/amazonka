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
-- Module      : Amazonka.DevOpsGuru.Types.AnomalousLogGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalousLogGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DevOpsGuru.Types.LogAnomalyShowcase
import qualified Amazonka.Prelude as Prelude

-- | An Amazon CloudWatch log group that contains log anomalies and is used
-- to generate an insight.
--
-- /See:/ 'newAnomalousLogGroup' smart constructor.
data AnomalousLogGroup = AnomalousLogGroup'
  { -- | The time the anomalous log events stopped.
    impactEndTime :: Prelude.Maybe Core.POSIX,
    -- | The number of log lines that were scanned for anomalous log events.
    numberOfLogLinesScanned :: Prelude.Maybe Prelude.Int,
    -- | The time the anomalous log events began. The impact start time indicates
    -- the time of the first log anomaly event that occurs.
    impactStartTime :: Prelude.Maybe Core.POSIX,
    -- | The log anomalies in the log group. Each log anomaly displayed
    -- represents a cluster of similar anomalous log events.
    logAnomalyShowcases :: Prelude.Maybe [LogAnomalyShowcase],
    -- | The name of the CloudWatch log group.
    logGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalousLogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'impactEndTime', 'anomalousLogGroup_impactEndTime' - The time the anomalous log events stopped.
--
-- 'numberOfLogLinesScanned', 'anomalousLogGroup_numberOfLogLinesScanned' - The number of log lines that were scanned for anomalous log events.
--
-- 'impactStartTime', 'anomalousLogGroup_impactStartTime' - The time the anomalous log events began. The impact start time indicates
-- the time of the first log anomaly event that occurs.
--
-- 'logAnomalyShowcases', 'anomalousLogGroup_logAnomalyShowcases' - The log anomalies in the log group. Each log anomaly displayed
-- represents a cluster of similar anomalous log events.
--
-- 'logGroupName', 'anomalousLogGroup_logGroupName' - The name of the CloudWatch log group.
newAnomalousLogGroup ::
  AnomalousLogGroup
newAnomalousLogGroup =
  AnomalousLogGroup'
    { impactEndTime = Prelude.Nothing,
      numberOfLogLinesScanned = Prelude.Nothing,
      impactStartTime = Prelude.Nothing,
      logAnomalyShowcases = Prelude.Nothing,
      logGroupName = Prelude.Nothing
    }

-- | The time the anomalous log events stopped.
anomalousLogGroup_impactEndTime :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.UTCTime)
anomalousLogGroup_impactEndTime = Lens.lens (\AnomalousLogGroup' {impactEndTime} -> impactEndTime) (\s@AnomalousLogGroup' {} a -> s {impactEndTime = a} :: AnomalousLogGroup) Prelude.. Lens.mapping Core._Time

-- | The number of log lines that were scanned for anomalous log events.
anomalousLogGroup_numberOfLogLinesScanned :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.Int)
anomalousLogGroup_numberOfLogLinesScanned = Lens.lens (\AnomalousLogGroup' {numberOfLogLinesScanned} -> numberOfLogLinesScanned) (\s@AnomalousLogGroup' {} a -> s {numberOfLogLinesScanned = a} :: AnomalousLogGroup)

-- | The time the anomalous log events began. The impact start time indicates
-- the time of the first log anomaly event that occurs.
anomalousLogGroup_impactStartTime :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.UTCTime)
anomalousLogGroup_impactStartTime = Lens.lens (\AnomalousLogGroup' {impactStartTime} -> impactStartTime) (\s@AnomalousLogGroup' {} a -> s {impactStartTime = a} :: AnomalousLogGroup) Prelude.. Lens.mapping Core._Time

-- | The log anomalies in the log group. Each log anomaly displayed
-- represents a cluster of similar anomalous log events.
anomalousLogGroup_logAnomalyShowcases :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe [LogAnomalyShowcase])
anomalousLogGroup_logAnomalyShowcases = Lens.lens (\AnomalousLogGroup' {logAnomalyShowcases} -> logAnomalyShowcases) (\s@AnomalousLogGroup' {} a -> s {logAnomalyShowcases = a} :: AnomalousLogGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the CloudWatch log group.
anomalousLogGroup_logGroupName :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.Text)
anomalousLogGroup_logGroupName = Lens.lens (\AnomalousLogGroup' {logGroupName} -> logGroupName) (\s@AnomalousLogGroup' {} a -> s {logGroupName = a} :: AnomalousLogGroup)

instance Core.FromJSON AnomalousLogGroup where
  parseJSON =
    Core.withObject
      "AnomalousLogGroup"
      ( \x ->
          AnomalousLogGroup'
            Prelude.<$> (x Core..:? "ImpactEndTime")
            Prelude.<*> (x Core..:? "NumberOfLogLinesScanned")
            Prelude.<*> (x Core..:? "ImpactStartTime")
            Prelude.<*> ( x Core..:? "LogAnomalyShowcases"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LogGroupName")
      )

instance Prelude.Hashable AnomalousLogGroup where
  hashWithSalt _salt AnomalousLogGroup' {..} =
    _salt `Prelude.hashWithSalt` impactEndTime
      `Prelude.hashWithSalt` numberOfLogLinesScanned
      `Prelude.hashWithSalt` impactStartTime
      `Prelude.hashWithSalt` logAnomalyShowcases
      `Prelude.hashWithSalt` logGroupName

instance Prelude.NFData AnomalousLogGroup where
  rnf AnomalousLogGroup' {..} =
    Prelude.rnf impactEndTime
      `Prelude.seq` Prelude.rnf numberOfLogLinesScanned
      `Prelude.seq` Prelude.rnf impactStartTime
      `Prelude.seq` Prelude.rnf logAnomalyShowcases
      `Prelude.seq` Prelude.rnf logGroupName
