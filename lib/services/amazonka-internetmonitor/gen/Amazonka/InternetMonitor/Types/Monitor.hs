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
-- Module      : Amazonka.InternetMonitor.Types.Monitor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.Monitor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.MonitorConfigState
import Amazonka.InternetMonitor.Types.MonitorProcessingStatusCode
import qualified Amazonka.Prelude as Prelude

-- | The description of and information about a monitor in Amazon CloudWatch
-- Internet Monitor.
--
-- /See:/ 'newMonitor' smart constructor.
data Monitor = Monitor'
  { -- | The health of data processing for the monitor.
    processingStatus :: Prelude.Maybe MonitorProcessingStatusCode,
    -- | The name of the monitor.
    monitorName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the monitor.
    monitorArn :: Prelude.Text,
    -- | The status of a monitor.
    status :: MonitorConfigState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Monitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingStatus', 'monitor_processingStatus' - The health of data processing for the monitor.
--
-- 'monitorName', 'monitor_monitorName' - The name of the monitor.
--
-- 'monitorArn', 'monitor_monitorArn' - The Amazon Resource Name (ARN) of the monitor.
--
-- 'status', 'monitor_status' - The status of a monitor.
newMonitor ::
  -- | 'monitorName'
  Prelude.Text ->
  -- | 'monitorArn'
  Prelude.Text ->
  -- | 'status'
  MonitorConfigState ->
  Monitor
newMonitor pMonitorName_ pMonitorArn_ pStatus_ =
  Monitor'
    { processingStatus = Prelude.Nothing,
      monitorName = pMonitorName_,
      monitorArn = pMonitorArn_,
      status = pStatus_
    }

-- | The health of data processing for the monitor.
monitor_processingStatus :: Lens.Lens' Monitor (Prelude.Maybe MonitorProcessingStatusCode)
monitor_processingStatus = Lens.lens (\Monitor' {processingStatus} -> processingStatus) (\s@Monitor' {} a -> s {processingStatus = a} :: Monitor)

-- | The name of the monitor.
monitor_monitorName :: Lens.Lens' Monitor Prelude.Text
monitor_monitorName = Lens.lens (\Monitor' {monitorName} -> monitorName) (\s@Monitor' {} a -> s {monitorName = a} :: Monitor)

-- | The Amazon Resource Name (ARN) of the monitor.
monitor_monitorArn :: Lens.Lens' Monitor Prelude.Text
monitor_monitorArn = Lens.lens (\Monitor' {monitorArn} -> monitorArn) (\s@Monitor' {} a -> s {monitorArn = a} :: Monitor)

-- | The status of a monitor.
monitor_status :: Lens.Lens' Monitor MonitorConfigState
monitor_status = Lens.lens (\Monitor' {status} -> status) (\s@Monitor' {} a -> s {status = a} :: Monitor)

instance Data.FromJSON Monitor where
  parseJSON =
    Data.withObject
      "Monitor"
      ( \x ->
          Monitor'
            Prelude.<$> (x Data..:? "ProcessingStatus")
            Prelude.<*> (x Data..: "MonitorName")
            Prelude.<*> (x Data..: "MonitorArn")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable Monitor where
  hashWithSalt _salt Monitor' {..} =
    _salt
      `Prelude.hashWithSalt` processingStatus
      `Prelude.hashWithSalt` monitorName
      `Prelude.hashWithSalt` monitorArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData Monitor where
  rnf Monitor' {..} =
    Prelude.rnf processingStatus
      `Prelude.seq` Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf status
