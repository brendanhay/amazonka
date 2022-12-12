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
-- Module      : Amazonka.SageMaker.Types.MonitoringAlertHistorySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MonitoringAlertHistorySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MonitoringAlertStatus

-- | Provides summary information of an alert\'s history.
--
-- /See:/ 'newMonitoringAlertHistorySummary' smart constructor.
data MonitoringAlertHistorySummary = MonitoringAlertHistorySummary'
  { -- | The name of a monitoring schedule.
    monitoringScheduleName :: Prelude.Text,
    -- | The name of a monitoring alert.
    monitoringAlertName :: Prelude.Text,
    -- | A timestamp that indicates when the first alert transition occurred in
    -- an alert history. An alert transition can be from status @InAlert@ to
    -- @OK@, or from @OK@ to @InAlert@.
    creationTime :: Data.POSIX,
    -- | The current alert status of an alert.
    alertStatus :: MonitoringAlertStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringAlertHistorySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringScheduleName', 'monitoringAlertHistorySummary_monitoringScheduleName' - The name of a monitoring schedule.
--
-- 'monitoringAlertName', 'monitoringAlertHistorySummary_monitoringAlertName' - The name of a monitoring alert.
--
-- 'creationTime', 'monitoringAlertHistorySummary_creationTime' - A timestamp that indicates when the first alert transition occurred in
-- an alert history. An alert transition can be from status @InAlert@ to
-- @OK@, or from @OK@ to @InAlert@.
--
-- 'alertStatus', 'monitoringAlertHistorySummary_alertStatus' - The current alert status of an alert.
newMonitoringAlertHistorySummary ::
  -- | 'monitoringScheduleName'
  Prelude.Text ->
  -- | 'monitoringAlertName'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'alertStatus'
  MonitoringAlertStatus ->
  MonitoringAlertHistorySummary
newMonitoringAlertHistorySummary
  pMonitoringScheduleName_
  pMonitoringAlertName_
  pCreationTime_
  pAlertStatus_ =
    MonitoringAlertHistorySummary'
      { monitoringScheduleName =
          pMonitoringScheduleName_,
        monitoringAlertName = pMonitoringAlertName_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        alertStatus = pAlertStatus_
      }

-- | The name of a monitoring schedule.
monitoringAlertHistorySummary_monitoringScheduleName :: Lens.Lens' MonitoringAlertHistorySummary Prelude.Text
monitoringAlertHistorySummary_monitoringScheduleName = Lens.lens (\MonitoringAlertHistorySummary' {monitoringScheduleName} -> monitoringScheduleName) (\s@MonitoringAlertHistorySummary' {} a -> s {monitoringScheduleName = a} :: MonitoringAlertHistorySummary)

-- | The name of a monitoring alert.
monitoringAlertHistorySummary_monitoringAlertName :: Lens.Lens' MonitoringAlertHistorySummary Prelude.Text
monitoringAlertHistorySummary_monitoringAlertName = Lens.lens (\MonitoringAlertHistorySummary' {monitoringAlertName} -> monitoringAlertName) (\s@MonitoringAlertHistorySummary' {} a -> s {monitoringAlertName = a} :: MonitoringAlertHistorySummary)

-- | A timestamp that indicates when the first alert transition occurred in
-- an alert history. An alert transition can be from status @InAlert@ to
-- @OK@, or from @OK@ to @InAlert@.
monitoringAlertHistorySummary_creationTime :: Lens.Lens' MonitoringAlertHistorySummary Prelude.UTCTime
monitoringAlertHistorySummary_creationTime = Lens.lens (\MonitoringAlertHistorySummary' {creationTime} -> creationTime) (\s@MonitoringAlertHistorySummary' {} a -> s {creationTime = a} :: MonitoringAlertHistorySummary) Prelude.. Data._Time

-- | The current alert status of an alert.
monitoringAlertHistorySummary_alertStatus :: Lens.Lens' MonitoringAlertHistorySummary MonitoringAlertStatus
monitoringAlertHistorySummary_alertStatus = Lens.lens (\MonitoringAlertHistorySummary' {alertStatus} -> alertStatus) (\s@MonitoringAlertHistorySummary' {} a -> s {alertStatus = a} :: MonitoringAlertHistorySummary)

instance Data.FromJSON MonitoringAlertHistorySummary where
  parseJSON =
    Data.withObject
      "MonitoringAlertHistorySummary"
      ( \x ->
          MonitoringAlertHistorySummary'
            Prelude.<$> (x Data..: "MonitoringScheduleName")
            Prelude.<*> (x Data..: "MonitoringAlertName")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "AlertStatus")
      )

instance
  Prelude.Hashable
    MonitoringAlertHistorySummary
  where
  hashWithSalt _salt MonitoringAlertHistorySummary' {..} =
    _salt `Prelude.hashWithSalt` monitoringScheduleName
      `Prelude.hashWithSalt` monitoringAlertName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` alertStatus

instance Prelude.NFData MonitoringAlertHistorySummary where
  rnf MonitoringAlertHistorySummary' {..} =
    Prelude.rnf monitoringScheduleName
      `Prelude.seq` Prelude.rnf monitoringAlertName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf alertStatus
