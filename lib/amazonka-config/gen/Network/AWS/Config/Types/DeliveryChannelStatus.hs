{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.DeliveryChannelStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.DeliveryChannelStatus
  ( DeliveryChannelStatus (..)
  -- * Smart constructor
  , mkDeliveryChannelStatus
  -- * Lenses
  , dcsConfigHistoryDeliveryInfo
  , dcsConfigSnapshotDeliveryInfo
  , dcsConfigStreamDeliveryInfo
  , dcsName
  ) where

import qualified Network.AWS.Config.Types.ConfigExportDeliveryInfo as Types
import qualified Network.AWS.Config.Types.ConfigStreamDeliveryInfo as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of a specified delivery channel.
--
-- Valid values: @Success@ | @Failure@ 
--
-- /See:/ 'mkDeliveryChannelStatus' smart constructor.
data DeliveryChannelStatus = DeliveryChannelStatus'
  { configHistoryDeliveryInfo :: Core.Maybe Types.ConfigExportDeliveryInfo
    -- ^ A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
  , configSnapshotDeliveryInfo :: Core.Maybe Types.ConfigExportDeliveryInfo
    -- ^ A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
  , configStreamDeliveryInfo :: Core.Maybe Types.ConfigStreamDeliveryInfo
    -- ^ A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the delivery channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeliveryChannelStatus' value with any optional fields omitted.
mkDeliveryChannelStatus
    :: DeliveryChannelStatus
mkDeliveryChannelStatus
  = DeliveryChannelStatus'{configHistoryDeliveryInfo = Core.Nothing,
                           configSnapshotDeliveryInfo = Core.Nothing,
                           configStreamDeliveryInfo = Core.Nothing, name = Core.Nothing}

-- | A list that contains the status of the delivery of the configuration history to the specified Amazon S3 bucket.
--
-- /Note:/ Consider using 'configHistoryDeliveryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigHistoryDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Core.Maybe Types.ConfigExportDeliveryInfo)
dcsConfigHistoryDeliveryInfo = Lens.field @"configHistoryDeliveryInfo"
{-# INLINEABLE dcsConfigHistoryDeliveryInfo #-}
{-# DEPRECATED configHistoryDeliveryInfo "Use generic-lens or generic-optics with 'configHistoryDeliveryInfo' instead"  #-}

-- | A list containing the status of the delivery of the snapshot to the specified Amazon S3 bucket.
--
-- /Note:/ Consider using 'configSnapshotDeliveryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigSnapshotDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Core.Maybe Types.ConfigExportDeliveryInfo)
dcsConfigSnapshotDeliveryInfo = Lens.field @"configSnapshotDeliveryInfo"
{-# INLINEABLE dcsConfigSnapshotDeliveryInfo #-}
{-# DEPRECATED configSnapshotDeliveryInfo "Use generic-lens or generic-optics with 'configSnapshotDeliveryInfo' instead"  #-}

-- | A list containing the status of the delivery of the configuration stream notification to the specified Amazon SNS topic.
--
-- /Note:/ Consider using 'configStreamDeliveryInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsConfigStreamDeliveryInfo :: Lens.Lens' DeliveryChannelStatus (Core.Maybe Types.ConfigStreamDeliveryInfo)
dcsConfigStreamDeliveryInfo = Lens.field @"configStreamDeliveryInfo"
{-# INLINEABLE dcsConfigStreamDeliveryInfo #-}
{-# DEPRECATED configStreamDeliveryInfo "Use generic-lens or generic-optics with 'configStreamDeliveryInfo' instead"  #-}

-- | The name of the delivery channel.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsName :: Lens.Lens' DeliveryChannelStatus (Core.Maybe Core.Text)
dcsName = Lens.field @"name"
{-# INLINEABLE dcsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON DeliveryChannelStatus where
        parseJSON
          = Core.withObject "DeliveryChannelStatus" Core.$
              \ x ->
                DeliveryChannelStatus' Core.<$>
                  (x Core..:? "configHistoryDeliveryInfo") Core.<*>
                    x Core..:? "configSnapshotDeliveryInfo"
                    Core.<*> x Core..:? "configStreamDeliveryInfo"
                    Core.<*> x Core..:? "name"
