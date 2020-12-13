{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
  ( VirtualInterfaceTestHistory (..),

    -- * Smart constructor
    mkVirtualInterfaceTestHistory,

    -- * Lenses
    vithBgpPeers,
    vithStatus,
    vithTestDurationInMinutes,
    vithStartTime,
    vithTestId,
    vithEndTime,
    vithOwnerAccount,
    vithVirtualInterfaceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the virtual interface failover test.
--
-- /See:/ 'mkVirtualInterfaceTestHistory' smart constructor.
data VirtualInterfaceTestHistory = VirtualInterfaceTestHistory'
  { -- | The BGP peers that were put in the DOWN state as part of the virtual interface failover test.
    bgpPeers :: Lude.Maybe [Lude.Text],
    -- | The status of the virtual interface failover test.
    status :: Lude.Maybe Lude.Text,
    -- | The time that the virtual interface failover test ran in minutes.
    testDurationInMinutes :: Lude.Maybe Lude.Int,
    -- | The time that the virtual interface moves to the DOWN state.
    startTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the virtual interface failover test.
    testId :: Lude.Maybe Lude.Text,
    -- | The time that the virtual interface moves out of the DOWN state.
    endTime :: Lude.Maybe Lude.Timestamp,
    -- | The owner ID of the tested virtual interface.
    ownerAccount :: Lude.Maybe Lude.Text,
    -- | The ID of the tested virtual interface.
    virtualInterfaceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VirtualInterfaceTestHistory' with the minimum fields required to make a request.
--
-- * 'bgpPeers' - The BGP peers that were put in the DOWN state as part of the virtual interface failover test.
-- * 'status' - The status of the virtual interface failover test.
-- * 'testDurationInMinutes' - The time that the virtual interface failover test ran in minutes.
-- * 'startTime' - The time that the virtual interface moves to the DOWN state.
-- * 'testId' - The ID of the virtual interface failover test.
-- * 'endTime' - The time that the virtual interface moves out of the DOWN state.
-- * 'ownerAccount' - The owner ID of the tested virtual interface.
-- * 'virtualInterfaceId' - The ID of the tested virtual interface.
mkVirtualInterfaceTestHistory ::
  VirtualInterfaceTestHistory
mkVirtualInterfaceTestHistory =
  VirtualInterfaceTestHistory'
    { bgpPeers = Lude.Nothing,
      status = Lude.Nothing,
      testDurationInMinutes = Lude.Nothing,
      startTime = Lude.Nothing,
      testId = Lude.Nothing,
      endTime = Lude.Nothing,
      ownerAccount = Lude.Nothing,
      virtualInterfaceId = Lude.Nothing
    }

-- | The BGP peers that were put in the DOWN state as part of the virtual interface failover test.
--
-- /Note:/ Consider using 'bgpPeers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithBgpPeers :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe [Lude.Text])
vithBgpPeers = Lens.lens (bgpPeers :: VirtualInterfaceTestHistory -> Lude.Maybe [Lude.Text]) (\s a -> s {bgpPeers = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithBgpPeers "Use generic-lens or generic-optics with 'bgpPeers' instead." #-}

-- | The status of the virtual interface failover test.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithStatus :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
vithStatus = Lens.lens (status :: VirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The time that the virtual interface failover test ran in minutes.
--
-- /Note:/ Consider using 'testDurationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithTestDurationInMinutes :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe Lude.Int)
vithTestDurationInMinutes = Lens.lens (testDurationInMinutes :: VirtualInterfaceTestHistory -> Lude.Maybe Lude.Int) (\s a -> s {testDurationInMinutes = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithTestDurationInMinutes "Use generic-lens or generic-optics with 'testDurationInMinutes' instead." #-}

-- | The time that the virtual interface moves to the DOWN state.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithStartTime :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe Lude.Timestamp)
vithStartTime = Lens.lens (startTime :: VirtualInterfaceTestHistory -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The ID of the virtual interface failover test.
--
-- /Note:/ Consider using 'testId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithTestId :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
vithTestId = Lens.lens (testId :: VirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {testId = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithTestId "Use generic-lens or generic-optics with 'testId' instead." #-}

-- | The time that the virtual interface moves out of the DOWN state.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithEndTime :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe Lude.Timestamp)
vithEndTime = Lens.lens (endTime :: VirtualInterfaceTestHistory -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The owner ID of the tested virtual interface.
--
-- /Note:/ Consider using 'ownerAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithOwnerAccount :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
vithOwnerAccount = Lens.lens (ownerAccount :: VirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccount = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithOwnerAccount "Use generic-lens or generic-optics with 'ownerAccount' instead." #-}

-- | The ID of the tested virtual interface.
--
-- /Note:/ Consider using 'virtualInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vithVirtualInterfaceId :: Lens.Lens' VirtualInterfaceTestHistory (Lude.Maybe Lude.Text)
vithVirtualInterfaceId = Lens.lens (virtualInterfaceId :: VirtualInterfaceTestHistory -> Lude.Maybe Lude.Text) (\s a -> s {virtualInterfaceId = a} :: VirtualInterfaceTestHistory)
{-# DEPRECATED vithVirtualInterfaceId "Use generic-lens or generic-optics with 'virtualInterfaceId' instead." #-}

instance Lude.FromJSON VirtualInterfaceTestHistory where
  parseJSON =
    Lude.withObject
      "VirtualInterfaceTestHistory"
      ( \x ->
          VirtualInterfaceTestHistory'
            Lude.<$> (x Lude..:? "bgpPeers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "testDurationInMinutes")
            Lude.<*> (x Lude..:? "startTime")
            Lude.<*> (x Lude..:? "testId")
            Lude.<*> (x Lude..:? "endTime")
            Lude.<*> (x Lude..:? "ownerAccount")
            Lude.<*> (x Lude..:? "virtualInterfaceId")
      )
