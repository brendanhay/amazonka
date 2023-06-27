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
-- Module      : Amazonka.IoTWireless.Types.WirelessDeviceImportTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessDeviceImportTask where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.ImportTaskStatus
import Amazonka.IoTWireless.Types.SidewalkGetStartImportInfo
import qualified Amazonka.Prelude as Prelude

-- | Information about an import task for wireless devices.
--
-- /See:/ 'newWirelessDeviceImportTask' smart constructor.
data WirelessDeviceImportTask = WirelessDeviceImportTask'
  { -- | The ARN (Amazon Resource Name) of the wireless device import task.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the import task was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the Sidewalk destination that that describes the IoT rule to
    -- route messages from the device in the import task that will be onboarded
    -- to AWS IoT Wireless
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | The summary information of count of wireless devices in an import task
    -- that failed to onboarded to the import task.
    failedImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The ID of the wireless device import task.
    id :: Prelude.Maybe Prelude.Text,
    -- | The summary information of count of wireless devices that are waiting
    -- for the control log to be added to an import task.
    initializedImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The summary information of count of wireless devices in an import task
    -- that have been onboarded to the import task.
    onboardedImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The summary information of count of wireless devices in an import task
    -- that are waiting in the queue to be onboarded.
    pendingImportedDeviceCount :: Prelude.Maybe Prelude.Integer,
    -- | The Sidewalk-related information of the wireless device import task.
    sidewalk :: Prelude.Maybe SidewalkGetStartImportInfo,
    -- | The status information of the wireless device import task.
    status :: Prelude.Maybe ImportTaskStatus,
    -- | The reason that provides additional information about the import task
    -- status.
    statusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessDeviceImportTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'wirelessDeviceImportTask_arn' - The ARN (Amazon Resource Name) of the wireless device import task.
--
-- 'creationTime', 'wirelessDeviceImportTask_creationTime' - The time at which the import task was created.
--
-- 'destinationName', 'wirelessDeviceImportTask_destinationName' - The name of the Sidewalk destination that that describes the IoT rule to
-- route messages from the device in the import task that will be onboarded
-- to AWS IoT Wireless
--
-- 'failedImportedDeviceCount', 'wirelessDeviceImportTask_failedImportedDeviceCount' - The summary information of count of wireless devices in an import task
-- that failed to onboarded to the import task.
--
-- 'id', 'wirelessDeviceImportTask_id' - The ID of the wireless device import task.
--
-- 'initializedImportedDeviceCount', 'wirelessDeviceImportTask_initializedImportedDeviceCount' - The summary information of count of wireless devices that are waiting
-- for the control log to be added to an import task.
--
-- 'onboardedImportedDeviceCount', 'wirelessDeviceImportTask_onboardedImportedDeviceCount' - The summary information of count of wireless devices in an import task
-- that have been onboarded to the import task.
--
-- 'pendingImportedDeviceCount', 'wirelessDeviceImportTask_pendingImportedDeviceCount' - The summary information of count of wireless devices in an import task
-- that are waiting in the queue to be onboarded.
--
-- 'sidewalk', 'wirelessDeviceImportTask_sidewalk' - The Sidewalk-related information of the wireless device import task.
--
-- 'status', 'wirelessDeviceImportTask_status' - The status information of the wireless device import task.
--
-- 'statusReason', 'wirelessDeviceImportTask_statusReason' - The reason that provides additional information about the import task
-- status.
newWirelessDeviceImportTask ::
  WirelessDeviceImportTask
newWirelessDeviceImportTask =
  WirelessDeviceImportTask'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      failedImportedDeviceCount = Prelude.Nothing,
      id = Prelude.Nothing,
      initializedImportedDeviceCount = Prelude.Nothing,
      onboardedImportedDeviceCount = Prelude.Nothing,
      pendingImportedDeviceCount = Prelude.Nothing,
      sidewalk = Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the wireless device import task.
wirelessDeviceImportTask_arn :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
wirelessDeviceImportTask_arn = Lens.lens (\WirelessDeviceImportTask' {arn} -> arn) (\s@WirelessDeviceImportTask' {} a -> s {arn = a} :: WirelessDeviceImportTask)

-- | The time at which the import task was created.
wirelessDeviceImportTask_creationTime :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.UTCTime)
wirelessDeviceImportTask_creationTime = Lens.lens (\WirelessDeviceImportTask' {creationTime} -> creationTime) (\s@WirelessDeviceImportTask' {} a -> s {creationTime = a} :: WirelessDeviceImportTask) Prelude.. Lens.mapping Data._Time

-- | The name of the Sidewalk destination that that describes the IoT rule to
-- route messages from the device in the import task that will be onboarded
-- to AWS IoT Wireless
wirelessDeviceImportTask_destinationName :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
wirelessDeviceImportTask_destinationName = Lens.lens (\WirelessDeviceImportTask' {destinationName} -> destinationName) (\s@WirelessDeviceImportTask' {} a -> s {destinationName = a} :: WirelessDeviceImportTask)

-- | The summary information of count of wireless devices in an import task
-- that failed to onboarded to the import task.
wirelessDeviceImportTask_failedImportedDeviceCount :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Integer)
wirelessDeviceImportTask_failedImportedDeviceCount = Lens.lens (\WirelessDeviceImportTask' {failedImportedDeviceCount} -> failedImportedDeviceCount) (\s@WirelessDeviceImportTask' {} a -> s {failedImportedDeviceCount = a} :: WirelessDeviceImportTask)

-- | The ID of the wireless device import task.
wirelessDeviceImportTask_id :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
wirelessDeviceImportTask_id = Lens.lens (\WirelessDeviceImportTask' {id} -> id) (\s@WirelessDeviceImportTask' {} a -> s {id = a} :: WirelessDeviceImportTask)

-- | The summary information of count of wireless devices that are waiting
-- for the control log to be added to an import task.
wirelessDeviceImportTask_initializedImportedDeviceCount :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Integer)
wirelessDeviceImportTask_initializedImportedDeviceCount = Lens.lens (\WirelessDeviceImportTask' {initializedImportedDeviceCount} -> initializedImportedDeviceCount) (\s@WirelessDeviceImportTask' {} a -> s {initializedImportedDeviceCount = a} :: WirelessDeviceImportTask)

-- | The summary information of count of wireless devices in an import task
-- that have been onboarded to the import task.
wirelessDeviceImportTask_onboardedImportedDeviceCount :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Integer)
wirelessDeviceImportTask_onboardedImportedDeviceCount = Lens.lens (\WirelessDeviceImportTask' {onboardedImportedDeviceCount} -> onboardedImportedDeviceCount) (\s@WirelessDeviceImportTask' {} a -> s {onboardedImportedDeviceCount = a} :: WirelessDeviceImportTask)

-- | The summary information of count of wireless devices in an import task
-- that are waiting in the queue to be onboarded.
wirelessDeviceImportTask_pendingImportedDeviceCount :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Integer)
wirelessDeviceImportTask_pendingImportedDeviceCount = Lens.lens (\WirelessDeviceImportTask' {pendingImportedDeviceCount} -> pendingImportedDeviceCount) (\s@WirelessDeviceImportTask' {} a -> s {pendingImportedDeviceCount = a} :: WirelessDeviceImportTask)

-- | The Sidewalk-related information of the wireless device import task.
wirelessDeviceImportTask_sidewalk :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe SidewalkGetStartImportInfo)
wirelessDeviceImportTask_sidewalk = Lens.lens (\WirelessDeviceImportTask' {sidewalk} -> sidewalk) (\s@WirelessDeviceImportTask' {} a -> s {sidewalk = a} :: WirelessDeviceImportTask)

-- | The status information of the wireless device import task.
wirelessDeviceImportTask_status :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe ImportTaskStatus)
wirelessDeviceImportTask_status = Lens.lens (\WirelessDeviceImportTask' {status} -> status) (\s@WirelessDeviceImportTask' {} a -> s {status = a} :: WirelessDeviceImportTask)

-- | The reason that provides additional information about the import task
-- status.
wirelessDeviceImportTask_statusReason :: Lens.Lens' WirelessDeviceImportTask (Prelude.Maybe Prelude.Text)
wirelessDeviceImportTask_statusReason = Lens.lens (\WirelessDeviceImportTask' {statusReason} -> statusReason) (\s@WirelessDeviceImportTask' {} a -> s {statusReason = a} :: WirelessDeviceImportTask)

instance Data.FromJSON WirelessDeviceImportTask where
  parseJSON =
    Data.withObject
      "WirelessDeviceImportTask"
      ( \x ->
          WirelessDeviceImportTask'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "DestinationName")
            Prelude.<*> (x Data..:? "FailedImportedDeviceCount")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "InitializedImportedDeviceCount")
            Prelude.<*> (x Data..:? "OnboardedImportedDeviceCount")
            Prelude.<*> (x Data..:? "PendingImportedDeviceCount")
            Prelude.<*> (x Data..:? "Sidewalk")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusReason")
      )

instance Prelude.Hashable WirelessDeviceImportTask where
  hashWithSalt _salt WirelessDeviceImportTask' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` failedImportedDeviceCount
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` initializedImportedDeviceCount
      `Prelude.hashWithSalt` onboardedImportedDeviceCount
      `Prelude.hashWithSalt` pendingImportedDeviceCount
      `Prelude.hashWithSalt` sidewalk
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusReason

instance Prelude.NFData WirelessDeviceImportTask where
  rnf WirelessDeviceImportTask' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf failedImportedDeviceCount
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf initializedImportedDeviceCount
      `Prelude.seq` Prelude.rnf onboardedImportedDeviceCount
      `Prelude.seq` Prelude.rnf pendingImportedDeviceCount
      `Prelude.seq` Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
