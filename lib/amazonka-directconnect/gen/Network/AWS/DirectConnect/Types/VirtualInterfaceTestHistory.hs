{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
  ( VirtualInterfaceTestHistory (..)
  -- * Smart constructor
  , mkVirtualInterfaceTestHistory
  -- * Lenses
  , vithBgpPeers
  , vithEndTime
  , vithOwnerAccount
  , vithStartTime
  , vithStatus
  , vithTestDurationInMinutes
  , vithTestId
  , vithVirtualInterfaceId
  ) where

import qualified Network.AWS.DirectConnect.Types.BGPPeerId as Types
import qualified Network.AWS.DirectConnect.Types.FailureTestHistoryStatus as Types
import qualified Network.AWS.DirectConnect.Types.OwnerAccount as Types
import qualified Network.AWS.DirectConnect.Types.TestId as Types
import qualified Network.AWS.DirectConnect.Types.VirtualInterfaceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the virtual interface failover test.
--
-- /See:/ 'mkVirtualInterfaceTestHistory' smart constructor.
data VirtualInterfaceTestHistory = VirtualInterfaceTestHistory'
  { bgpPeers :: Core.Maybe [Types.BGPPeerId]
    -- ^ The BGP peers that were put in the DOWN state as part of the virtual interface failover test.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the virtual interface moves out of the DOWN state.
  , ownerAccount :: Core.Maybe Types.OwnerAccount
    -- ^ The owner ID of the tested virtual interface.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the virtual interface moves to the DOWN state.
  , status :: Core.Maybe Types.FailureTestHistoryStatus
    -- ^ The status of the virtual interface failover test.
  , testDurationInMinutes :: Core.Maybe Core.Int
    -- ^ The time that the virtual interface failover test ran in minutes.
  , testId :: Core.Maybe Types.TestId
    -- ^ The ID of the virtual interface failover test.
  , virtualInterfaceId :: Core.Maybe Types.VirtualInterfaceId
    -- ^ The ID of the tested virtual interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'VirtualInterfaceTestHistory' value with any optional fields omitted.
mkVirtualInterfaceTestHistory
    :: VirtualInterfaceTestHistory
mkVirtualInterfaceTestHistory
  = VirtualInterfaceTestHistory'{bgpPeers = Core.Nothing,
                                 endTime = Core.Nothing, ownerAccount = Core.Nothing,
                                 startTime = Core.Nothing, status = Core.Nothing,
                                 testDurationInMinutes = Core.Nothing, testId = Core.Nothing,
                                 virtualInterfaceId = Core.Nothing}

-- | The BGP peers that were put in the DOWN state as part of the virtual interface failover test.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithBgpPeers :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe [Types.BGPPeerId])
vithBgpPeers = Lens.field @"bgpPeers"
{-# INLINEABLE vithBgpPeers #-}
{-# DEPRECATED bgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead"  #-}

-- | The time that the virtual interface moves out of the DOWN state.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithEndTime :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.NominalDiffTime)
vithEndTime = Lens.field @"endTime"
{-# INLINEABLE vithEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The owner ID of the tested virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithOwnerAccount :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Types.OwnerAccount)
vithOwnerAccount = Lens.field @"ownerAccount"
{-# INLINEABLE vithOwnerAccount #-}
{-# DEPRECATED ownerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead"  #-}

-- | The time that the virtual interface moves to the DOWN state.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithStartTime :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.NominalDiffTime)
vithStartTime = Lens.field @"startTime"
{-# INLINEABLE vithStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the virtual interface failover test.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithStatus :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Types.FailureTestHistoryStatus)
vithStatus = Lens.field @"status"
{-# INLINEABLE vithStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The time that the virtual interface failover test ran in minutes.
--
-- /Note:/ Consider using 'testDurationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithTestDurationInMinutes :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.Int)
vithTestDurationInMinutes = Lens.field @"testDurationInMinutes"
{-# INLINEABLE vithTestDurationInMinutes #-}
{-# DEPRECATED testDurationInMinutes "Use generic-lens or generic-optics with 'testDurationInMinutes' instead"  #-}

-- | The ID of the virtual interface failover test.
--
-- /Note:/ Consider using 'testId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithTestId :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Types.TestId)
vithTestId = Lens.field @"testId"
{-# INLINEABLE vithTestId #-}
{-# DEPRECATED testId "Use generic-lens or generic-optics with 'testId' instead"  #-}

-- | The ID of the tested virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithVirtualInterfaceId :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Types.VirtualInterfaceId)
vithVirtualInterfaceId = Lens.field @"virtualInterfaceId"
{-# INLINEABLE vithVirtualInterfaceId #-}
{-# DEPRECATED virtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead"  #-}

instance Core.FromJSON VirtualInterfaceTestHistory where
        parseJSON
          = Core.withObject "VirtualInterfaceTestHistory" Core.$
              \ x ->
                VirtualInterfaceTestHistory' Core.<$>
                  (x Core..:? "bgpPeers") Core.<*> x Core..:? "endTime" Core.<*>
                    x Core..:? "ownerAccount"
                    Core.<*> x Core..:? "startTime"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "testDurationInMinutes"
                    Core.<*> x Core..:? "testId"
                    Core.<*> x Core..:? "virtualInterfaceId"
