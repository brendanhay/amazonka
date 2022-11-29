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
-- Module      : Amazonka.Config.Types.DeliveryChannelStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.DeliveryChannelStatus where

import Amazonka.Config.Types.ConfigExportDeliveryInfo
import Amazonka.Config.Types.ConfigStreamDeliveryInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status of a specified delivery channel.
--
-- Valid values: @Success@ | @Failure@
--
-- /See:/ 'newDeliveryChannelStatus' smart constructor.
data DeliveryChannelStatus = DeliveryChannelStatus'
  { -- | The name of the delivery channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list that contains the status of the delivery of the configuration
    -- history to the specified Amazon S3 bucket.
    configHistoryDeliveryInfo :: Prelude.Maybe ConfigExportDeliveryInfo,
    -- | A list containing the status of the delivery of the snapshot to the
    -- specified Amazon S3 bucket.
    configSnapshotDeliveryInfo :: Prelude.Maybe ConfigExportDeliveryInfo,
    -- | A list containing the status of the delivery of the configuration stream
    -- notification to the specified Amazon SNS topic.
    configStreamDeliveryInfo :: Prelude.Maybe ConfigStreamDeliveryInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeliveryChannelStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deliveryChannelStatus_name' - The name of the delivery channel.
--
-- 'configHistoryDeliveryInfo', 'deliveryChannelStatus_configHistoryDeliveryInfo' - A list that contains the status of the delivery of the configuration
-- history to the specified Amazon S3 bucket.
--
-- 'configSnapshotDeliveryInfo', 'deliveryChannelStatus_configSnapshotDeliveryInfo' - A list containing the status of the delivery of the snapshot to the
-- specified Amazon S3 bucket.
--
-- 'configStreamDeliveryInfo', 'deliveryChannelStatus_configStreamDeliveryInfo' - A list containing the status of the delivery of the configuration stream
-- notification to the specified Amazon SNS topic.
newDeliveryChannelStatus ::
  DeliveryChannelStatus
newDeliveryChannelStatus =
  DeliveryChannelStatus'
    { name = Prelude.Nothing,
      configHistoryDeliveryInfo = Prelude.Nothing,
      configSnapshotDeliveryInfo = Prelude.Nothing,
      configStreamDeliveryInfo = Prelude.Nothing
    }

-- | The name of the delivery channel.
deliveryChannelStatus_name :: Lens.Lens' DeliveryChannelStatus (Prelude.Maybe Prelude.Text)
deliveryChannelStatus_name = Lens.lens (\DeliveryChannelStatus' {name} -> name) (\s@DeliveryChannelStatus' {} a -> s {name = a} :: DeliveryChannelStatus)

-- | A list that contains the status of the delivery of the configuration
-- history to the specified Amazon S3 bucket.
deliveryChannelStatus_configHistoryDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Prelude.Maybe ConfigExportDeliveryInfo)
deliveryChannelStatus_configHistoryDeliveryInfo = Lens.lens (\DeliveryChannelStatus' {configHistoryDeliveryInfo} -> configHistoryDeliveryInfo) (\s@DeliveryChannelStatus' {} a -> s {configHistoryDeliveryInfo = a} :: DeliveryChannelStatus)

-- | A list containing the status of the delivery of the snapshot to the
-- specified Amazon S3 bucket.
deliveryChannelStatus_configSnapshotDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Prelude.Maybe ConfigExportDeliveryInfo)
deliveryChannelStatus_configSnapshotDeliveryInfo = Lens.lens (\DeliveryChannelStatus' {configSnapshotDeliveryInfo} -> configSnapshotDeliveryInfo) (\s@DeliveryChannelStatus' {} a -> s {configSnapshotDeliveryInfo = a} :: DeliveryChannelStatus)

-- | A list containing the status of the delivery of the configuration stream
-- notification to the specified Amazon SNS topic.
deliveryChannelStatus_configStreamDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Prelude.Maybe ConfigStreamDeliveryInfo)
deliveryChannelStatus_configStreamDeliveryInfo = Lens.lens (\DeliveryChannelStatus' {configStreamDeliveryInfo} -> configStreamDeliveryInfo) (\s@DeliveryChannelStatus' {} a -> s {configStreamDeliveryInfo = a} :: DeliveryChannelStatus)

instance Core.FromJSON DeliveryChannelStatus where
  parseJSON =
    Core.withObject
      "DeliveryChannelStatus"
      ( \x ->
          DeliveryChannelStatus'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "configHistoryDeliveryInfo")
            Prelude.<*> (x Core..:? "configSnapshotDeliveryInfo")
            Prelude.<*> (x Core..:? "configStreamDeliveryInfo")
      )

instance Prelude.Hashable DeliveryChannelStatus where
  hashWithSalt _salt DeliveryChannelStatus' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` configHistoryDeliveryInfo
      `Prelude.hashWithSalt` configSnapshotDeliveryInfo
      `Prelude.hashWithSalt` configStreamDeliveryInfo

instance Prelude.NFData DeliveryChannelStatus where
  rnf DeliveryChannelStatus' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf configHistoryDeliveryInfo
      `Prelude.seq` Prelude.rnf configSnapshotDeliveryInfo
      `Prelude.seq` Prelude.rnf configStreamDeliveryInfo
