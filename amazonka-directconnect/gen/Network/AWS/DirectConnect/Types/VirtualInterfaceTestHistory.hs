{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the virtual interface failover test.
--
-- /See:/ 'newVirtualInterfaceTestHistory' smart constructor.
data VirtualInterfaceTestHistory = VirtualInterfaceTestHistory'
  { -- | The BGP peers that were put in the DOWN state as part of the virtual
    -- interface failover test.
    bgpPeers :: Prelude.Maybe [Prelude.Text],
    -- | The status of the virtual interface failover test.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface failover test.
    testId :: Prelude.Maybe Prelude.Text,
    -- | The time that the virtual interface moves to the DOWN state.
    startTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time that the virtual interface moves out of the DOWN state.
    endTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the tested virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The owner ID of the tested virtual interface.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The time that the virtual interface failover test ran in minutes.
    testDurationInMinutes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      status = Prelude.Nothing,
      testId = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      testDurationInMinutes = Prelude.Nothing
    }

-- | The BGP peers that were put in the DOWN state as part of the virtual
-- interface failover test.
virtualInterfaceTestHistory_bgpPeers :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe [Prelude.Text])
virtualInterfaceTestHistory_bgpPeers = Lens.lens (\VirtualInterfaceTestHistory' {bgpPeers} -> bgpPeers) (\s@VirtualInterfaceTestHistory' {} a -> s {bgpPeers = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Prelude._Coerce

-- | The status of the virtual interface failover test.
virtualInterfaceTestHistory_status :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_status = Lens.lens (\VirtualInterfaceTestHistory' {status} -> status) (\s@VirtualInterfaceTestHistory' {} a -> s {status = a} :: VirtualInterfaceTestHistory)

-- | The ID of the virtual interface failover test.
virtualInterfaceTestHistory_testId :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_testId = Lens.lens (\VirtualInterfaceTestHistory' {testId} -> testId) (\s@VirtualInterfaceTestHistory' {} a -> s {testId = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface moves to the DOWN state.
virtualInterfaceTestHistory_startTime :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.UTCTime)
virtualInterfaceTestHistory_startTime = Lens.lens (\VirtualInterfaceTestHistory' {startTime} -> startTime) (\s@VirtualInterfaceTestHistory' {} a -> s {startTime = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Prelude._Time

-- | The time that the virtual interface moves out of the DOWN state.
virtualInterfaceTestHistory_endTime :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.UTCTime)
virtualInterfaceTestHistory_endTime = Lens.lens (\VirtualInterfaceTestHistory' {endTime} -> endTime) (\s@VirtualInterfaceTestHistory' {} a -> s {endTime = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the tested virtual interface.
virtualInterfaceTestHistory_virtualInterfaceId :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_virtualInterfaceId = Lens.lens (\VirtualInterfaceTestHistory' {virtualInterfaceId} -> virtualInterfaceId) (\s@VirtualInterfaceTestHistory' {} a -> s {virtualInterfaceId = a} :: VirtualInterfaceTestHistory)

-- | The owner ID of the tested virtual interface.
virtualInterfaceTestHistory_ownerAccount :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_ownerAccount = Lens.lens (\VirtualInterfaceTestHistory' {ownerAccount} -> ownerAccount) (\s@VirtualInterfaceTestHistory' {} a -> s {ownerAccount = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface failover test ran in minutes.
virtualInterfaceTestHistory_testDurationInMinutes :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Int)
virtualInterfaceTestHistory_testDurationInMinutes = Lens.lens (\VirtualInterfaceTestHistory' {testDurationInMinutes} -> testDurationInMinutes) (\s@VirtualInterfaceTestHistory' {} a -> s {testDurationInMinutes = a} :: VirtualInterfaceTestHistory)

instance Prelude.FromJSON VirtualInterfaceTestHistory where
  parseJSON =
    Prelude.withObject
      "VirtualInterfaceTestHistory"
      ( \x ->
          VirtualInterfaceTestHistory'
            Prelude.<$> (x Prelude..:? "bgpPeers" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "testId")
            Prelude.<*> (x Prelude..:? "startTime")
            Prelude.<*> (x Prelude..:? "endTime")
            Prelude.<*> (x Prelude..:? "virtualInterfaceId")
            Prelude.<*> (x Prelude..:? "ownerAccount")
            Prelude.<*> (x Prelude..:? "testDurationInMinutes")
      )

instance Prelude.Hashable VirtualInterfaceTestHistory

instance Prelude.NFData VirtualInterfaceTestHistory
