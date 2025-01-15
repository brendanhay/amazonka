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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalousLogGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.LogAnomalyShowcase
import qualified Amazonka.Prelude as Prelude

-- | An Amazon CloudWatch log group that contains log anomalies and is used
-- to generate an insight.
--
-- /See:/ 'newAnomalousLogGroup' smart constructor.
data AnomalousLogGroup = AnomalousLogGroup'
  { -- | The time the anomalous log events stopped.
    impactEndTime :: Prelude.Maybe Data.POSIX,
    -- | The time the anomalous log events began. The impact start time indicates
    -- the time of the first log anomaly event that occurs.
    impactStartTime :: Prelude.Maybe Data.POSIX,
    -- | The log anomalies in the log group. Each log anomaly displayed
    -- represents a cluster of similar anomalous log events.
    logAnomalyShowcases :: Prelude.Maybe [LogAnomalyShowcase],
    -- | The name of the CloudWatch log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The number of log lines that were scanned for anomalous log events.
    numberOfLogLinesScanned :: Prelude.Maybe Prelude.Int
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
-- 'impactStartTime', 'anomalousLogGroup_impactStartTime' - The time the anomalous log events began. The impact start time indicates
-- the time of the first log anomaly event that occurs.
--
-- 'logAnomalyShowcases', 'anomalousLogGroup_logAnomalyShowcases' - The log anomalies in the log group. Each log anomaly displayed
-- represents a cluster of similar anomalous log events.
--
-- 'logGroupName', 'anomalousLogGroup_logGroupName' - The name of the CloudWatch log group.
--
-- 'numberOfLogLinesScanned', 'anomalousLogGroup_numberOfLogLinesScanned' - The number of log lines that were scanned for anomalous log events.
newAnomalousLogGroup ::
  AnomalousLogGroup
newAnomalousLogGroup =
  AnomalousLogGroup'
    { impactEndTime = Prelude.Nothing,
      impactStartTime = Prelude.Nothing,
      logAnomalyShowcases = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      numberOfLogLinesScanned = Prelude.Nothing
    }

-- | The time the anomalous log events stopped.
anomalousLogGroup_impactEndTime :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.UTCTime)
anomalousLogGroup_impactEndTime = Lens.lens (\AnomalousLogGroup' {impactEndTime} -> impactEndTime) (\s@AnomalousLogGroup' {} a -> s {impactEndTime = a} :: AnomalousLogGroup) Prelude.. Lens.mapping Data._Time

-- | The time the anomalous log events began. The impact start time indicates
-- the time of the first log anomaly event that occurs.
anomalousLogGroup_impactStartTime :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.UTCTime)
anomalousLogGroup_impactStartTime = Lens.lens (\AnomalousLogGroup' {impactStartTime} -> impactStartTime) (\s@AnomalousLogGroup' {} a -> s {impactStartTime = a} :: AnomalousLogGroup) Prelude.. Lens.mapping Data._Time

-- | The log anomalies in the log group. Each log anomaly displayed
-- represents a cluster of similar anomalous log events.
anomalousLogGroup_logAnomalyShowcases :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe [LogAnomalyShowcase])
anomalousLogGroup_logAnomalyShowcases = Lens.lens (\AnomalousLogGroup' {logAnomalyShowcases} -> logAnomalyShowcases) (\s@AnomalousLogGroup' {} a -> s {logAnomalyShowcases = a} :: AnomalousLogGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the CloudWatch log group.
anomalousLogGroup_logGroupName :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.Text)
anomalousLogGroup_logGroupName = Lens.lens (\AnomalousLogGroup' {logGroupName} -> logGroupName) (\s@AnomalousLogGroup' {} a -> s {logGroupName = a} :: AnomalousLogGroup)

-- | The number of log lines that were scanned for anomalous log events.
anomalousLogGroup_numberOfLogLinesScanned :: Lens.Lens' AnomalousLogGroup (Prelude.Maybe Prelude.Int)
anomalousLogGroup_numberOfLogLinesScanned = Lens.lens (\AnomalousLogGroup' {numberOfLogLinesScanned} -> numberOfLogLinesScanned) (\s@AnomalousLogGroup' {} a -> s {numberOfLogLinesScanned = a} :: AnomalousLogGroup)

instance Data.FromJSON AnomalousLogGroup where
  parseJSON =
    Data.withObject
      "AnomalousLogGroup"
      ( \x ->
          AnomalousLogGroup'
            Prelude.<$> (x Data..:? "ImpactEndTime")
            Prelude.<*> (x Data..:? "ImpactStartTime")
            Prelude.<*> ( x
                            Data..:? "LogAnomalyShowcases"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LogGroupName")
            Prelude.<*> (x Data..:? "NumberOfLogLinesScanned")
      )

instance Prelude.Hashable AnomalousLogGroup where
  hashWithSalt _salt AnomalousLogGroup' {..} =
    _salt
      `Prelude.hashWithSalt` impactEndTime
      `Prelude.hashWithSalt` impactStartTime
      `Prelude.hashWithSalt` logAnomalyShowcases
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` numberOfLogLinesScanned

instance Prelude.NFData AnomalousLogGroup where
  rnf AnomalousLogGroup' {..} =
    Prelude.rnf impactEndTime `Prelude.seq`
      Prelude.rnf impactStartTime `Prelude.seq`
        Prelude.rnf logAnomalyShowcases `Prelude.seq`
          Prelude.rnf logGroupName `Prelude.seq`
            Prelude.rnf numberOfLogLinesScanned
