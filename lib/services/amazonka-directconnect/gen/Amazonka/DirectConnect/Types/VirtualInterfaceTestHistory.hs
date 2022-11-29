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
-- Module      : Amazonka.DirectConnect.Types.VirtualInterfaceTestHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.VirtualInterfaceTestHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the virtual interface failover test.
--
-- /See:/ 'newVirtualInterfaceTestHistory' smart constructor.
data VirtualInterfaceTestHistory = VirtualInterfaceTestHistory'
  { -- | The time that the virtual interface failover test ran in minutes.
    testDurationInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The status of the virtual interface failover test.
    status :: Prelude.Maybe Prelude.Text,
    -- | The time that the virtual interface moves out of the DOWN state.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the tested virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The BGP peers that were put in the DOWN state as part of the virtual
    -- interface failover test.
    bgpPeers :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the virtual interface failover test.
    testId :: Prelude.Maybe Prelude.Text,
    -- | The owner ID of the tested virtual interface.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The time that the virtual interface moves to the DOWN state.
    startTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualInterfaceTestHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testDurationInMinutes', 'virtualInterfaceTestHistory_testDurationInMinutes' - The time that the virtual interface failover test ran in minutes.
--
-- 'status', 'virtualInterfaceTestHistory_status' - The status of the virtual interface failover test.
--
-- 'endTime', 'virtualInterfaceTestHistory_endTime' - The time that the virtual interface moves out of the DOWN state.
--
-- 'virtualInterfaceId', 'virtualInterfaceTestHistory_virtualInterfaceId' - The ID of the tested virtual interface.
--
-- 'bgpPeers', 'virtualInterfaceTestHistory_bgpPeers' - The BGP peers that were put in the DOWN state as part of the virtual
-- interface failover test.
--
-- 'testId', 'virtualInterfaceTestHistory_testId' - The ID of the virtual interface failover test.
--
-- 'ownerAccount', 'virtualInterfaceTestHistory_ownerAccount' - The owner ID of the tested virtual interface.
--
-- 'startTime', 'virtualInterfaceTestHistory_startTime' - The time that the virtual interface moves to the DOWN state.
newVirtualInterfaceTestHistory ::
  VirtualInterfaceTestHistory
newVirtualInterfaceTestHistory =
  VirtualInterfaceTestHistory'
    { testDurationInMinutes =
        Prelude.Nothing,
      status = Prelude.Nothing,
      endTime = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing,
      bgpPeers = Prelude.Nothing,
      testId = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The time that the virtual interface failover test ran in minutes.
virtualInterfaceTestHistory_testDurationInMinutes :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Int)
virtualInterfaceTestHistory_testDurationInMinutes = Lens.lens (\VirtualInterfaceTestHistory' {testDurationInMinutes} -> testDurationInMinutes) (\s@VirtualInterfaceTestHistory' {} a -> s {testDurationInMinutes = a} :: VirtualInterfaceTestHistory)

-- | The status of the virtual interface failover test.
virtualInterfaceTestHistory_status :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_status = Lens.lens (\VirtualInterfaceTestHistory' {status} -> status) (\s@VirtualInterfaceTestHistory' {} a -> s {status = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface moves out of the DOWN state.
virtualInterfaceTestHistory_endTime :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.UTCTime)
virtualInterfaceTestHistory_endTime = Lens.lens (\VirtualInterfaceTestHistory' {endTime} -> endTime) (\s@VirtualInterfaceTestHistory' {} a -> s {endTime = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Core._Time

-- | The ID of the tested virtual interface.
virtualInterfaceTestHistory_virtualInterfaceId :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_virtualInterfaceId = Lens.lens (\VirtualInterfaceTestHistory' {virtualInterfaceId} -> virtualInterfaceId) (\s@VirtualInterfaceTestHistory' {} a -> s {virtualInterfaceId = a} :: VirtualInterfaceTestHistory)

-- | The BGP peers that were put in the DOWN state as part of the virtual
-- interface failover test.
virtualInterfaceTestHistory_bgpPeers :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe [Prelude.Text])
virtualInterfaceTestHistory_bgpPeers = Lens.lens (\VirtualInterfaceTestHistory' {bgpPeers} -> bgpPeers) (\s@VirtualInterfaceTestHistory' {} a -> s {bgpPeers = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the virtual interface failover test.
virtualInterfaceTestHistory_testId :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_testId = Lens.lens (\VirtualInterfaceTestHistory' {testId} -> testId) (\s@VirtualInterfaceTestHistory' {} a -> s {testId = a} :: VirtualInterfaceTestHistory)

-- | The owner ID of the tested virtual interface.
virtualInterfaceTestHistory_ownerAccount :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_ownerAccount = Lens.lens (\VirtualInterfaceTestHistory' {ownerAccount} -> ownerAccount) (\s@VirtualInterfaceTestHistory' {} a -> s {ownerAccount = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface moves to the DOWN state.
virtualInterfaceTestHistory_startTime :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.UTCTime)
virtualInterfaceTestHistory_startTime = Lens.lens (\VirtualInterfaceTestHistory' {startTime} -> startTime) (\s@VirtualInterfaceTestHistory' {} a -> s {startTime = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON VirtualInterfaceTestHistory where
  parseJSON =
    Core.withObject
      "VirtualInterfaceTestHistory"
      ( \x ->
          VirtualInterfaceTestHistory'
            Prelude.<$> (x Core..:? "testDurationInMinutes")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "endTime")
            Prelude.<*> (x Core..:? "virtualInterfaceId")
            Prelude.<*> (x Core..:? "bgpPeers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "testId")
            Prelude.<*> (x Core..:? "ownerAccount")
            Prelude.<*> (x Core..:? "startTime")
      )

instance Prelude.Hashable VirtualInterfaceTestHistory where
  hashWithSalt _salt VirtualInterfaceTestHistory' {..} =
    _salt `Prelude.hashWithSalt` testDurationInMinutes
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` virtualInterfaceId
      `Prelude.hashWithSalt` bgpPeers
      `Prelude.hashWithSalt` testId
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData VirtualInterfaceTestHistory where
  rnf VirtualInterfaceTestHistory' {..} =
    Prelude.rnf testDurationInMinutes
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf virtualInterfaceId
      `Prelude.seq` Prelude.rnf bgpPeers
      `Prelude.seq` Prelude.rnf testId
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf startTime
