{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.VolumeiSCSIAttributes
  ( VolumeiSCSIAttributes (..),

    -- * Smart constructor
    mkVolumeiSCSIAttributes,

    -- * Lenses
    vscsiaChapEnabled,
    vscsiaLunNumber,
    vscsiaNetworkInterfaceId,
    vscsiaNetworkInterfacePort,
    vscsiaTargetARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.NetworkInterfaceId as Types
import qualified Network.AWS.StorageGateway.Types.TargetARN as Types

-- | Lists iSCSI information about a volume.
--
-- /See:/ 'mkVolumeiSCSIAttributes' smart constructor.
data VolumeiSCSIAttributes = VolumeiSCSIAttributes'
  { -- | Indicates whether mutual CHAP is enabled for the iSCSI target.
    chapEnabled :: Core.Maybe Core.Bool,
    -- | The logical disk number.
    lunNumber :: Core.Maybe Core.Natural,
    -- | The network interface identifier.
    networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId,
    -- | The port used to communicate with iSCSI targets.
    networkInterfacePort :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the volume target.
    targetARN :: Core.Maybe Types.TargetARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VolumeiSCSIAttributes' value with any optional fields omitted.
mkVolumeiSCSIAttributes ::
  VolumeiSCSIAttributes
mkVolumeiSCSIAttributes =
  VolumeiSCSIAttributes'
    { chapEnabled = Core.Nothing,
      lunNumber = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      networkInterfacePort = Core.Nothing,
      targetARN = Core.Nothing
    }

-- | Indicates whether mutual CHAP is enabled for the iSCSI target.
--
-- /Note:/ Consider using 'chapEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaChapEnabled :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Bool)
vscsiaChapEnabled = Lens.field @"chapEnabled"
{-# DEPRECATED vscsiaChapEnabled "Use generic-lens or generic-optics with 'chapEnabled' instead." #-}

-- | The logical disk number.
--
-- /Note:/ Consider using 'lunNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaLunNumber :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Natural)
vscsiaLunNumber = Lens.field @"lunNumber"
{-# DEPRECATED vscsiaLunNumber "Use generic-lens or generic-optics with 'lunNumber' instead." #-}

-- | The network interface identifier.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaNetworkInterfaceId :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Types.NetworkInterfaceId)
vscsiaNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED vscsiaNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The port used to communicate with iSCSI targets.
--
-- /Note:/ Consider using 'networkInterfacePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaNetworkInterfacePort :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Core.Int)
vscsiaNetworkInterfacePort = Lens.field @"networkInterfacePort"
{-# DEPRECATED vscsiaNetworkInterfacePort "Use generic-lens or generic-optics with 'networkInterfacePort' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume target.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vscsiaTargetARN :: Lens.Lens' VolumeiSCSIAttributes (Core.Maybe Types.TargetARN)
vscsiaTargetARN = Lens.field @"targetARN"
{-# DEPRECATED vscsiaTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

instance Core.FromJSON VolumeiSCSIAttributes where
  parseJSON =
    Core.withObject "VolumeiSCSIAttributes" Core.$
      \x ->
        VolumeiSCSIAttributes'
          Core.<$> (x Core..:? "ChapEnabled")
          Core.<*> (x Core..:? "LunNumber")
          Core.<*> (x Core..:? "NetworkInterfaceId")
          Core.<*> (x Core..:? "NetworkInterfacePort")
          Core.<*> (x Core..:? "TargetARN")
