{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.DeliveryChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.DeliveryChannelStatus
  ( DeliveryChannelStatus (..),

    -- * Smart constructor
    mkDeliveryChannelStatus,

    -- * Lenses
    dcsConfigSnapshotDeliveryInfo,
    dcsConfigStreamDeliveryInfo,
    dcsConfigHistoryDeliveryInfo,
    dcsName,
  )
where

import Network.AWS.Config.Types.ConfigExportDeliveryInfo
import Network.AWS.Config.Types.ConfigStreamDeliveryInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of a specified delivery channel.
--
-- Valid values: @Success@ | @Failure@
--
-- /See:/ 'mkDeliveryChannelStatus' smart constructor.
data DeliveryChannelStatus = DeliveryChannelStatus'
  { -- | A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
    configSnapshotDeliveryInfo :: Lude.Maybe ConfigExportDeliveryInfo,
    -- | A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
    configStreamDeliveryInfo :: Lude.Maybe ConfigStreamDeliveryInfo,
    -- | A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
    configHistoryDeliveryInfo :: Lude.Maybe ConfigExportDeliveryInfo,
    -- | The name of the delivery channel.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeliveryChannelStatus' with the minimum fields required to make a request.
--
-- * 'configSnapshotDeliveryInfo' - A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
-- * 'configStreamDeliveryInfo' - A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
-- * 'configHistoryDeliveryInfo' - A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
-- * 'name' - The name of the delivery channel.
mkDeliveryChannelStatus ::
  DeliveryChannelStatus
mkDeliveryChannelStatus =
  DeliveryChannelStatus'
    { configSnapshotDeliveryInfo = Lude.Nothing,
      configStreamDeliveryInfo = Lude.Nothing,
      configHistoryDeliveryInfo = Lude.Nothing,
      name = Lude.Nothing
    }

-- | A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
--
-- /Note:/ Consider using 'configSnapshotDeliveryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigSnapshotDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Lude.Maybe ConfigExportDeliveryInfo)
dcsConfigSnapshotDeliveryInfo = Lens.lens (configSnapshotDeliveryInfo :: DeliveryChannelStatus -> Lude.Maybe ConfigExportDeliveryInfo) (\s a -> s {configSnapshotDeliveryInfo = a} :: DeliveryChannelStatus)
{-# DEPRECATED dcsConfigSnapshotDeliveryInfo "Use generic-lens or generic-optics with 'configSnapshotDeliveryInfo' instead." #-}

-- | A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
--
-- /Note:/ Consider using 'configStreamDeliveryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigStreamDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Lude.Maybe ConfigStreamDeliveryInfo)
dcsConfigStreamDeliveryInfo = Lens.lens (configStreamDeliveryInfo :: DeliveryChannelStatus -> Lude.Maybe ConfigStreamDeliveryInfo) (\s a -> s {configStreamDeliveryInfo = a} :: DeliveryChannelStatus)
{-# DEPRECATED dcsConfigStreamDeliveryInfo "Use generic-lens or generic-optics with 'configStreamDeliveryInfo' instead." #-}

-- | A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
--
-- /Note:/ Consider using 'configHistoryDeliveryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigHistoryDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Lude.Maybe ConfigExportDeliveryInfo)
dcsConfigHistoryDeliveryInfo = Lens.lens (configHistoryDeliveryInfo :: DeliveryChannelStatus -> Lude.Maybe ConfigExportDeliveryInfo) (\s a -> s {configHistoryDeliveryInfo = a} :: DeliveryChannelStatus)
{-# DEPRECATED dcsConfigHistoryDeliveryInfo "Use generic-lens or generic-optics with 'configHistoryDeliveryInfo' instead." #-}

-- | The name of the delivery channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsName :: Lens.Lens' DeliveryChannelStatus (Lude.Maybe Lude.Text)
dcsName = Lens.lens (name :: DeliveryChannelStatus -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeliveryChannelStatus)
{-# DEPRECATED dcsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON DeliveryChannelStatus where
  parseJSON =
    Lude.withObject
      "DeliveryChannelStatus"
      ( \x ->
          DeliveryChannelStatus'
            Lude.<$> (x Lude..:? "configSnapshotDeliveryInfo")
            Lude.<*> (x Lude..:? "configStreamDeliveryInfo")
            Lude.<*> (x Lude..:? "configHistoryDeliveryInfo")
            Lude.<*> (x Lude..:? "name")
      )
