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
-- Module      : Amazonka.Forecast.Types.MonitorSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.MonitorSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the monitor properties used in the ListMonitors
-- operation. To get a complete set of properties, call the DescribeMonitor
-- operation, and provide the listed @MonitorArn@.
--
-- /See:/ 'newMonitorSummary' smart constructor.
data MonitorSummary = MonitorSummary'
  { -- | The last time the monitor resource was modified. The timestamp depends
    -- on the status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @STOPPED@ - When the resource stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the monitor creation finished or
    --     failed.
    lastModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the monitor resource.
    monitorArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the monitor. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @ACTIVE_STOPPING@, @ACTIVE_STOPPED@
    --
    -- -   @UPDATE_IN_PROGRESS@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the monitor resource.
    monitorName :: Prelude.Maybe Prelude.Text,
    -- | When the monitor resource was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the predictor being monitored.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitorSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'monitorSummary_lastModificationTime' - The last time the monitor resource was modified. The timestamp depends
-- on the status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @STOPPED@ - When the resource stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the monitor creation finished or
--     failed.
--
-- 'monitorArn', 'monitorSummary_monitorArn' - The Amazon Resource Name (ARN) of the monitor resource.
--
-- 'status', 'monitorSummary_status' - The status of the monitor. States include:
--
-- -   @ACTIVE@
--
-- -   @ACTIVE_STOPPING@, @ACTIVE_STOPPED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- 'monitorName', 'monitorSummary_monitorName' - The name of the monitor resource.
--
-- 'creationTime', 'monitorSummary_creationTime' - When the monitor resource was created.
--
-- 'resourceArn', 'monitorSummary_resourceArn' - The Amazon Resource Name (ARN) of the predictor being monitored.
newMonitorSummary ::
  MonitorSummary
newMonitorSummary =
  MonitorSummary'
    { lastModificationTime =
        Prelude.Nothing,
      monitorArn = Prelude.Nothing,
      status = Prelude.Nothing,
      monitorName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The last time the monitor resource was modified. The timestamp depends
-- on the status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @STOPPED@ - When the resource stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the monitor creation finished or
--     failed.
monitorSummary_lastModificationTime :: Lens.Lens' MonitorSummary (Prelude.Maybe Prelude.UTCTime)
monitorSummary_lastModificationTime = Lens.lens (\MonitorSummary' {lastModificationTime} -> lastModificationTime) (\s@MonitorSummary' {} a -> s {lastModificationTime = a} :: MonitorSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the monitor resource.
monitorSummary_monitorArn :: Lens.Lens' MonitorSummary (Prelude.Maybe Prelude.Text)
monitorSummary_monitorArn = Lens.lens (\MonitorSummary' {monitorArn} -> monitorArn) (\s@MonitorSummary' {} a -> s {monitorArn = a} :: MonitorSummary)

-- | The status of the monitor. States include:
--
-- -   @ACTIVE@
--
-- -   @ACTIVE_STOPPING@, @ACTIVE_STOPPED@
--
-- -   @UPDATE_IN_PROGRESS@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
monitorSummary_status :: Lens.Lens' MonitorSummary (Prelude.Maybe Prelude.Text)
monitorSummary_status = Lens.lens (\MonitorSummary' {status} -> status) (\s@MonitorSummary' {} a -> s {status = a} :: MonitorSummary)

-- | The name of the monitor resource.
monitorSummary_monitorName :: Lens.Lens' MonitorSummary (Prelude.Maybe Prelude.Text)
monitorSummary_monitorName = Lens.lens (\MonitorSummary' {monitorName} -> monitorName) (\s@MonitorSummary' {} a -> s {monitorName = a} :: MonitorSummary)

-- | When the monitor resource was created.
monitorSummary_creationTime :: Lens.Lens' MonitorSummary (Prelude.Maybe Prelude.UTCTime)
monitorSummary_creationTime = Lens.lens (\MonitorSummary' {creationTime} -> creationTime) (\s@MonitorSummary' {} a -> s {creationTime = a} :: MonitorSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the predictor being monitored.
monitorSummary_resourceArn :: Lens.Lens' MonitorSummary (Prelude.Maybe Prelude.Text)
monitorSummary_resourceArn = Lens.lens (\MonitorSummary' {resourceArn} -> resourceArn) (\s@MonitorSummary' {} a -> s {resourceArn = a} :: MonitorSummary)

instance Core.FromJSON MonitorSummary where
  parseJSON =
    Core.withObject
      "MonitorSummary"
      ( \x ->
          MonitorSummary'
            Prelude.<$> (x Core..:? "LastModificationTime")
            Prelude.<*> (x Core..:? "MonitorArn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "MonitorName")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ResourceArn")
      )

instance Prelude.Hashable MonitorSummary where
  hashWithSalt _salt MonitorSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` monitorArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` monitorName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData MonitorSummary where
  rnf MonitorSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf monitorArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf monitorName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf resourceArn
