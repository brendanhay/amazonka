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
-- Module      : Amazonka.SageMaker.Types.MonitoringAlertSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringAlertSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringAlertActions
import Amazonka.SageMaker.Types.MonitoringAlertStatus

-- | Provides summary information about a monitor alert.
--
-- /See:/ 'newMonitoringAlertSummary' smart constructor.
data MonitoringAlertSummary = MonitoringAlertSummary'
  { -- | The name of a monitoring alert.
    monitoringAlertName :: Prelude.Text,
    -- | A timestamp that indicates when a monitor alert was created.
    creationTime :: Data.POSIX,
    -- | A timestamp that indicates when a monitor alert was last updated.
    lastModifiedTime :: Data.POSIX,
    -- | The current status of an alert.
    alertStatus :: MonitoringAlertStatus,
    -- | Within @EvaluationPeriod@, how many execution failures will raise an
    -- alert.
    datapointsToAlert :: Prelude.Natural,
    -- | The number of most recent monitoring executions to consider when
    -- evaluating alert status.
    evaluationPeriod :: Prelude.Natural,
    -- | A list of alert actions taken in response to an alert going into
    -- @InAlert@ status.
    actions :: MonitoringAlertActions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringAlertSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringAlertName', 'monitoringAlertSummary_monitoringAlertName' - The name of a monitoring alert.
--
-- 'creationTime', 'monitoringAlertSummary_creationTime' - A timestamp that indicates when a monitor alert was created.
--
-- 'lastModifiedTime', 'monitoringAlertSummary_lastModifiedTime' - A timestamp that indicates when a monitor alert was last updated.
--
-- 'alertStatus', 'monitoringAlertSummary_alertStatus' - The current status of an alert.
--
-- 'datapointsToAlert', 'monitoringAlertSummary_datapointsToAlert' - Within @EvaluationPeriod@, how many execution failures will raise an
-- alert.
--
-- 'evaluationPeriod', 'monitoringAlertSummary_evaluationPeriod' - The number of most recent monitoring executions to consider when
-- evaluating alert status.
--
-- 'actions', 'monitoringAlertSummary_actions' - A list of alert actions taken in response to an alert going into
-- @InAlert@ status.
newMonitoringAlertSummary ::
  -- | 'monitoringAlertName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  -- | 'alertStatus'
  MonitoringAlertStatus ->
  -- | 'datapointsToAlert'
  Prelude.Natural ->
  -- | 'evaluationPeriod'
  Prelude.Natural ->
  -- | 'actions'
  MonitoringAlertActions ->
  MonitoringAlertSummary
newMonitoringAlertSummary
  pMonitoringAlertName_
  pCreationTime_
  pLastModifiedTime_
  pAlertStatus_
  pDatapointsToAlert_
  pEvaluationPeriod_
  pActions_ =
    MonitoringAlertSummary'
      { monitoringAlertName =
          pMonitoringAlertName_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_,
        alertStatus = pAlertStatus_,
        datapointsToAlert = pDatapointsToAlert_,
        evaluationPeriod = pEvaluationPeriod_,
        actions = pActions_
      }

-- | The name of a monitoring alert.
monitoringAlertSummary_monitoringAlertName :: Lens.Lens' MonitoringAlertSummary Prelude.Text
monitoringAlertSummary_monitoringAlertName = Lens.lens (\MonitoringAlertSummary' {monitoringAlertName} -> monitoringAlertName) (\s@MonitoringAlertSummary' {} a -> s {monitoringAlertName = a} :: MonitoringAlertSummary)

-- | A timestamp that indicates when a monitor alert was created.
monitoringAlertSummary_creationTime :: Lens.Lens' MonitoringAlertSummary Prelude.UTCTime
monitoringAlertSummary_creationTime = Lens.lens (\MonitoringAlertSummary' {creationTime} -> creationTime) (\s@MonitoringAlertSummary' {} a -> s {creationTime = a} :: MonitoringAlertSummary) Prelude.. Data._Time

-- | A timestamp that indicates when a monitor alert was last updated.
monitoringAlertSummary_lastModifiedTime :: Lens.Lens' MonitoringAlertSummary Prelude.UTCTime
monitoringAlertSummary_lastModifiedTime = Lens.lens (\MonitoringAlertSummary' {lastModifiedTime} -> lastModifiedTime) (\s@MonitoringAlertSummary' {} a -> s {lastModifiedTime = a} :: MonitoringAlertSummary) Prelude.. Data._Time

-- | The current status of an alert.
monitoringAlertSummary_alertStatus :: Lens.Lens' MonitoringAlertSummary MonitoringAlertStatus
monitoringAlertSummary_alertStatus = Lens.lens (\MonitoringAlertSummary' {alertStatus} -> alertStatus) (\s@MonitoringAlertSummary' {} a -> s {alertStatus = a} :: MonitoringAlertSummary)

-- | Within @EvaluationPeriod@, how many execution failures will raise an
-- alert.
monitoringAlertSummary_datapointsToAlert :: Lens.Lens' MonitoringAlertSummary Prelude.Natural
monitoringAlertSummary_datapointsToAlert = Lens.lens (\MonitoringAlertSummary' {datapointsToAlert} -> datapointsToAlert) (\s@MonitoringAlertSummary' {} a -> s {datapointsToAlert = a} :: MonitoringAlertSummary)

-- | The number of most recent monitoring executions to consider when
-- evaluating alert status.
monitoringAlertSummary_evaluationPeriod :: Lens.Lens' MonitoringAlertSummary Prelude.Natural
monitoringAlertSummary_evaluationPeriod = Lens.lens (\MonitoringAlertSummary' {evaluationPeriod} -> evaluationPeriod) (\s@MonitoringAlertSummary' {} a -> s {evaluationPeriod = a} :: MonitoringAlertSummary)

-- | A list of alert actions taken in response to an alert going into
-- @InAlert@ status.
monitoringAlertSummary_actions :: Lens.Lens' MonitoringAlertSummary MonitoringAlertActions
monitoringAlertSummary_actions = Lens.lens (\MonitoringAlertSummary' {actions} -> actions) (\s@MonitoringAlertSummary' {} a -> s {actions = a} :: MonitoringAlertSummary)

instance Data.FromJSON MonitoringAlertSummary where
  parseJSON =
    Data.withObject
      "MonitoringAlertSummary"
      ( \x ->
          MonitoringAlertSummary'
            Prelude.<$> (x Data..: "MonitoringAlertName")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "AlertStatus")
            Prelude.<*> (x Data..: "DatapointsToAlert")
            Prelude.<*> (x Data..: "EvaluationPeriod")
            Prelude.<*> (x Data..: "Actions")
      )

instance Prelude.Hashable MonitoringAlertSummary where
  hashWithSalt _salt MonitoringAlertSummary' {..} =
    _salt
      `Prelude.hashWithSalt` monitoringAlertName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` alertStatus
      `Prelude.hashWithSalt` datapointsToAlert
      `Prelude.hashWithSalt` evaluationPeriod
      `Prelude.hashWithSalt` actions

instance Prelude.NFData MonitoringAlertSummary where
  rnf MonitoringAlertSummary' {..} =
    Prelude.rnf monitoringAlertName `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf lastModifiedTime `Prelude.seq`
          Prelude.rnf alertStatus `Prelude.seq`
            Prelude.rnf datapointsToAlert `Prelude.seq`
              Prelude.rnf evaluationPeriod `Prelude.seq`
                Prelude.rnf actions
