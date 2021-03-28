{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.DeviceiSCSIAttributes
  ( DeviceiSCSIAttributes (..)
  -- * Smart constructor
  , mkDeviceiSCSIAttributes
  -- * Lenses
  , dscsiaChapEnabled
  , dscsiaNetworkInterfaceId
  , dscsiaNetworkInterfacePort
  , dscsiaTargetARN
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.NetworkInterfaceId as Types
import qualified Network.AWS.StorageGateway.Types.TargetARN as Types

-- | Lists iSCSI information about a VTL device.
--
-- /See:/ 'mkDeviceiSCSIAttributes' smart constructor.
data DeviceiSCSIAttributes = DeviceiSCSIAttributes'
  { chapEnabled :: Core.Maybe Core.Bool
    -- ^ Indicates whether mutual CHAP is enabled for the iSCSI target.
  , networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId
    -- ^ The network interface identifier of the VTL device.
  , networkInterfacePort :: Core.Maybe Core.Int
    -- ^ The port used to communicate with iSCSI VTL device targets.
  , targetARN :: Core.Maybe Types.TargetARN
    -- ^ Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceiSCSIAttributes' value with any optional fields omitted.
mkDeviceiSCSIAttributes
    :: DeviceiSCSIAttributes
mkDeviceiSCSIAttributes
  = DeviceiSCSIAttributes'{chapEnabled = Core.Nothing,
                           networkInterfaceId = Core.Nothing,
                           networkInterfacePort = Core.Nothing, targetARN = Core.Nothing}

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- /Note:/ Consider using 'chapEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaChapEnabled :: Lens.Lens' DeviceiSCSIAttributes (Core.Maybe Core.Bool)
dscsiaChapEnabled = Lens.field @"chapEnabled"
{-# INLINEABLE dscsiaChapEnabled #-}
{-# DEPRECATED chapEnabled "Use generic-lens or generic-optics with 'chapEnabled' instead"  #-}

-- | The network interface identifier of the VTL device.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaNetworkInterfaceId :: Lens.Lens' DeviceiSCSIAttributes (Core.Maybe Types.NetworkInterfaceId)
dscsiaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE dscsiaNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The port used to communicate with iSCSI VTL device targets.
--
-- /Note:/ Consider using 'networkInterfacePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaNetworkInterfacePort :: Lens.Lens' DeviceiSCSIAttributes (Core.Maybe Core.Int)
dscsiaNetworkInterfacePort = Lens.field @"networkInterfacePort"
{-# INLINEABLE dscsiaNetworkInterfacePort #-}
{-# DEPRECATED networkInterfacePort "Use generic-lens or generic-optics with 'networkInterfacePort' instead"  #-}

-- | Specifies the unique Amazon Resource Name (ARN) that encodes the iSCSI qualified name(iqn) of a tape drive or media changer target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dscsiaTargetARN :: Lens.Lens' DeviceiSCSIAttributes (Core.Maybe Types.TargetARN)
dscsiaTargetARN = Lens.field @"targetARN"
{-# INLINEABLE dscsiaTargetARN #-}
{-# DEPRECATED targetARN "Use generic-lens or generic-optics with 'targetARN' instead"  #-}

instance Core.FromJSON DeviceiSCSIAttributes where
        parseJSON
          = Core.withObject "DeviceiSCSIAttributes" Core.$
              \ x ->
                DeviceiSCSIAttributes' Core.<$>
                  (x Core..:? "ChapEnabled") Core.<*> x Core..:? "NetworkInterfaceId"
                    Core.<*> x Core..:? "NetworkInterfacePort"
                    Core.<*> x Core..:? "TargetARN"
