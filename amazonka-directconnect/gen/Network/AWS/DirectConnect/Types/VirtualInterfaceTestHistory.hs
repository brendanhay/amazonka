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
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterfaceTestHistory where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the virtual interface failover test.
--
-- /See:/ 'newVirtualInterfaceTestHistory' smart constructor.
data VirtualInterfaceTestHistory = VirtualInterfaceTestHistory'
  { -- | The BGP peers that were put in the DOWN state as part of the virtual
    -- interface failover test.
    bgpPeers :: Core.Maybe [Core.Text],
    -- | The status of the virtual interface failover test.
    status :: Core.Maybe Core.Text,
    -- | The ID of the virtual interface failover test.
    testId :: Core.Maybe Core.Text,
    -- | The time that the virtual interface moves to the DOWN state.
    startTime :: Core.Maybe Core.POSIX,
    -- | The time that the virtual interface moves out of the DOWN state.
    endTime :: Core.Maybe Core.POSIX,
    -- | The ID of the tested virtual interface.
    virtualInterfaceId :: Core.Maybe Core.Text,
    -- | The owner ID of the tested virtual interface.
    ownerAccount :: Core.Maybe Core.Text,
    -- | The time that the virtual interface failover test ran in minutes.
    testDurationInMinutes :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VirtualInterfaceTestHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bgpPeers', 'virtualInterfaceTestHistory_bgpPeers' - The BGP peers that were put in the DOWN state as part of the virtual
-- interface failover test.
--
-- 'status', 'virtualInterfaceTestHistory_status' - The status of the virtual interface failover test.
--
-- 'testId', 'virtualInterfaceTestHistory_testId' - The ID of the virtual interface failover test.
--
-- 'startTime', 'virtualInterfaceTestHistory_startTime' - The time that the virtual interface moves to the DOWN state.
--
-- 'endTime', 'virtualInterfaceTestHistory_endTime' - The time that the virtual interface moves out of the DOWN state.
--
-- 'virtualInterfaceId', 'virtualInterfaceTestHistory_virtualInterfaceId' - The ID of the tested virtual interface.
--
-- 'ownerAccount', 'virtualInterfaceTestHistory_ownerAccount' - The owner ID of the tested virtual interface.
--
-- 'testDurationInMinutes', 'virtualInterfaceTestHistory_testDurationInMinutes' - The time that the virtual interface failover test ran in minutes.
newVirtualInterfaceTestHistory ::
  VirtualInterfaceTestHistory
newVirtualInterfaceTestHistory =
  VirtualInterfaceTestHistory'
    { bgpPeers =
        Core.Nothing,
      status = Core.Nothing,
      testId = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      virtualInterfaceId = Core.Nothing,
      ownerAccount = Core.Nothing,
      testDurationInMinutes = Core.Nothing
    }

-- | The BGP peers that were put in the DOWN state as part of the virtual
-- interface failover test.
virtualInterfaceTestHistory_bgpPeers :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe [Core.Text])
virtualInterfaceTestHistory_bgpPeers = Lens.lens (\VirtualInterfaceTestHistory' {bgpPeers} -> bgpPeers) (\s@VirtualInterfaceTestHistory' {} a -> s {bgpPeers = a} :: VirtualInterfaceTestHistory) Core.. Lens.mapping Lens._Coerce

-- | The status of the virtual interface failover test.
virtualInterfaceTestHistory_status :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.Text)
virtualInterfaceTestHistory_status = Lens.lens (\VirtualInterfaceTestHistory' {status} -> status) (\s@VirtualInterfaceTestHistory' {} a -> s {status = a} :: VirtualInterfaceTestHistory)

-- | The ID of the virtual interface failover test.
virtualInterfaceTestHistory_testId :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.Text)
virtualInterfaceTestHistory_testId = Lens.lens (\VirtualInterfaceTestHistory' {testId} -> testId) (\s@VirtualInterfaceTestHistory' {} a -> s {testId = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface moves to the DOWN state.
virtualInterfaceTestHistory_startTime :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.UTCTime)
virtualInterfaceTestHistory_startTime = Lens.lens (\VirtualInterfaceTestHistory' {startTime} -> startTime) (\s@VirtualInterfaceTestHistory' {} a -> s {startTime = a} :: VirtualInterfaceTestHistory) Core.. Lens.mapping Core._Time

-- | The time that the virtual interface moves out of the DOWN state.
virtualInterfaceTestHistory_endTime :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.UTCTime)
virtualInterfaceTestHistory_endTime = Lens.lens (\VirtualInterfaceTestHistory' {endTime} -> endTime) (\s@VirtualInterfaceTestHistory' {} a -> s {endTime = a} :: VirtualInterfaceTestHistory) Core.. Lens.mapping Core._Time

-- | The ID of the tested virtual interface.
virtualInterfaceTestHistory_virtualInterfaceId :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.Text)
virtualInterfaceTestHistory_virtualInterfaceId = Lens.lens (\VirtualInterfaceTestHistory' {virtualInterfaceId} -> virtualInterfaceId) (\s@VirtualInterfaceTestHistory' {} a -> s {virtualInterfaceId = a} :: VirtualInterfaceTestHistory)

-- | The owner ID of the tested virtual interface.
virtualInterfaceTestHistory_ownerAccount :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.Text)
virtualInterfaceTestHistory_ownerAccount = Lens.lens (\VirtualInterfaceTestHistory' {ownerAccount} -> ownerAccount) (\s@VirtualInterfaceTestHistory' {} a -> s {ownerAccount = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface failover test ran in minutes.
virtualInterfaceTestHistory_testDurationInMinutes :: Lens.Lens' VirtualInterfaceTestHistory (Core.Maybe Core.Int)
virtualInterfaceTestHistory_testDurationInMinutes = Lens.lens (\VirtualInterfaceTestHistory' {testDurationInMinutes} -> testDurationInMinutes) (\s@VirtualInterfaceTestHistory' {} a -> s {testDurationInMinutes = a} :: VirtualInterfaceTestHistory)

instance Core.FromJSON VirtualInterfaceTestHistory where
  parseJSON =
    Core.withObject
      "VirtualInterfaceTestHistory"
      ( \x ->
          VirtualInterfaceTestHistory'
            Core.<$> (x Core..:? "bgpPeers" Core..!= Core.mempty)
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "testId")
            Core.<*> (x Core..:? "startTime")
            Core.<*> (x Core..:? "endTime")
            Core.<*> (x Core..:? "virtualInterfaceId")
            Core.<*> (x Core..:? "ownerAccount")
            Core.<*> (x Core..:? "testDurationInMinutes")
      )

instance Core.Hashable VirtualInterfaceTestHistory

instance Core.NFData VirtualInterfaceTestHistory
