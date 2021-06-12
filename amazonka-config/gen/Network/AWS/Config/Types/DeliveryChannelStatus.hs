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
-- Module      : Network.AWS.Config.Types.DeliveryChannelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.DeliveryChannelStatus where

import Network.AWS.Config.Types.ConfigExportDeliveryInfo
import Network.AWS.Config.Types.ConfigStreamDeliveryInfo
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The status of a specified delivery channel.
--
-- Valid values: @Success@ | @Failure@
--
-- /See:/ 'newDeliveryChannelStatus' smart constructor.
data DeliveryChannelStatus = DeliveryChannelStatus'
  { -- | A list containing the status of the delivery of the snapshot to the
    -- specified Amazon S3 bucket.
    configSnapshotDeliveryInfo :: Core.Maybe ConfigExportDeliveryInfo,
    -- | A list containing the status of the delivery of the configuration stream
    -- notification to the specified Amazon SNS topic.
    configStreamDeliveryInfo :: Core.Maybe ConfigStreamDeliveryInfo,
    -- | The name of the delivery channel.
    name :: Core.Maybe Core.Text,
    -- | A list that contains the status of the delivery of the configuration
    -- history to the specified Amazon S3 bucket.
    configHistoryDeliveryInfo :: Core.Maybe ConfigExportDeliveryInfo
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeliveryChannelStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configSnapshotDeliveryInfo', 'deliveryChannelStatus_configSnapshotDeliveryInfo' - A list containing the status of the delivery of the snapshot to the
-- specified Amazon S3 bucket.
--
-- 'configStreamDeliveryInfo', 'deliveryChannelStatus_configStreamDeliveryInfo' - A list containing the status of the delivery of the configuration stream
-- notification to the specified Amazon SNS topic.
--
-- 'name', 'deliveryChannelStatus_name' - The name of the delivery channel.
--
-- 'configHistoryDeliveryInfo', 'deliveryChannelStatus_configHistoryDeliveryInfo' - A list that contains the status of the delivery of the configuration
-- history to the specified Amazon S3 bucket.
newDeliveryChannelStatus ::
  DeliveryChannelStatus
newDeliveryChannelStatus =
  DeliveryChannelStatus'
    { configSnapshotDeliveryInfo =
        Core.Nothing,
      configStreamDeliveryInfo = Core.Nothing,
      name = Core.Nothing,
      configHistoryDeliveryInfo = Core.Nothing
    }

-- | A list containing the status of the delivery of the snapshot to the
-- specified Amazon S3 bucket.
deliveryChannelStatus_configSnapshotDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Core.Maybe ConfigExportDeliveryInfo)
deliveryChannelStatus_configSnapshotDeliveryInfo = Lens.lens (\DeliveryChannelStatus' {configSnapshotDeliveryInfo} -> configSnapshotDeliveryInfo) (\s@DeliveryChannelStatus' {} a -> s {configSnapshotDeliveryInfo = a} :: DeliveryChannelStatus)

-- | A list containing the status of the delivery of the configuration stream
-- notification to the specified Amazon SNS topic.
deliveryChannelStatus_configStreamDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Core.Maybe ConfigStreamDeliveryInfo)
deliveryChannelStatus_configStreamDeliveryInfo = Lens.lens (\DeliveryChannelStatus' {configStreamDeliveryInfo} -> configStreamDeliveryInfo) (\s@DeliveryChannelStatus' {} a -> s {configStreamDeliveryInfo = a} :: DeliveryChannelStatus)

-- | The name of the delivery channel.
deliveryChannelStatus_name :: Lens.Lens' DeliveryChannelStatus (Core.Maybe Core.Text)
deliveryChannelStatus_name = Lens.lens (\DeliveryChannelStatus' {name} -> name) (\s@DeliveryChannelStatus' {} a -> s {name = a} :: DeliveryChannelStatus)

-- | A list that contains the status of the delivery of the configuration
-- history to the specified Amazon S3 bucket.
deliveryChannelStatus_configHistoryDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Core.Maybe ConfigExportDeliveryInfo)
deliveryChannelStatus_configHistoryDeliveryInfo = Lens.lens (\DeliveryChannelStatus' {configHistoryDeliveryInfo} -> configHistoryDeliveryInfo) (\s@DeliveryChannelStatus' {} a -> s {configHistoryDeliveryInfo = a} :: DeliveryChannelStatus)

instance Core.FromJSON DeliveryChannelStatus where
  parseJSON =
    Core.withObject
      "DeliveryChannelStatus"
      ( \x ->
          DeliveryChannelStatus'
            Core.<$> (x Core..:? "configSnapshotDeliveryInfo")
            Core.<*> (x Core..:? "configStreamDeliveryInfo")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "configHistoryDeliveryInfo")
      )

instance Core.Hashable DeliveryChannelStatus

instance Core.NFData DeliveryChannelStatus
