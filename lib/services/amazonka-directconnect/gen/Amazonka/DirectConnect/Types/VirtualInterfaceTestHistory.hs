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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.VirtualInterfaceTestHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the virtual interface failover test.
--
-- /See:/ 'newVirtualInterfaceTestHistory' smart constructor.
data VirtualInterfaceTestHistory = VirtualInterfaceTestHistory'
  { -- | The BGP peers that were put in the DOWN state as part of the virtual
    -- interface failover test.
    bgpPeers :: Prelude.Maybe [Prelude.Text],
    -- | The time that the virtual interface moves out of the DOWN state.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The owner ID of the tested virtual interface.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The time that the virtual interface moves to the DOWN state.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the virtual interface failover test.
    status :: Prelude.Maybe Prelude.Text,
    -- | The time that the virtual interface failover test ran in minutes.
    testDurationInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The ID of the virtual interface failover test.
    testId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the tested virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text
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
-- 'bgpPeers', 'virtualInterfaceTestHistory_bgpPeers' - The BGP peers that were put in the DOWN state as part of the virtual
-- interface failover test.
--
-- 'endTime', 'virtualInterfaceTestHistory_endTime' - The time that the virtual interface moves out of the DOWN state.
--
-- 'ownerAccount', 'virtualInterfaceTestHistory_ownerAccount' - The owner ID of the tested virtual interface.
--
-- 'startTime', 'virtualInterfaceTestHistory_startTime' - The time that the virtual interface moves to the DOWN state.
--
-- 'status', 'virtualInterfaceTestHistory_status' - The status of the virtual interface failover test.
--
-- 'testDurationInMinutes', 'virtualInterfaceTestHistory_testDurationInMinutes' - The time that the virtual interface failover test ran in minutes.
--
-- 'testId', 'virtualInterfaceTestHistory_testId' - The ID of the virtual interface failover test.
--
-- 'virtualInterfaceId', 'virtualInterfaceTestHistory_virtualInterfaceId' - The ID of the tested virtual interface.
newVirtualInterfaceTestHistory ::
  VirtualInterfaceTestHistory
newVirtualInterfaceTestHistory =
  VirtualInterfaceTestHistory'
    { bgpPeers =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      testDurationInMinutes = Prelude.Nothing,
      testId = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing
    }

-- | The BGP peers that were put in the DOWN state as part of the virtual
-- interface failover test.
virtualInterfaceTestHistory_bgpPeers :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe [Prelude.Text])
virtualInterfaceTestHistory_bgpPeers = Lens.lens (\VirtualInterfaceTestHistory' {bgpPeers} -> bgpPeers) (\s@VirtualInterfaceTestHistory' {} a -> s {bgpPeers = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Lens.coerced

-- | The time that the virtual interface moves out of the DOWN state.
virtualInterfaceTestHistory_endTime :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.UTCTime)
virtualInterfaceTestHistory_endTime = Lens.lens (\VirtualInterfaceTestHistory' {endTime} -> endTime) (\s@VirtualInterfaceTestHistory' {} a -> s {endTime = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Data._Time

-- | The owner ID of the tested virtual interface.
virtualInterfaceTestHistory_ownerAccount :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_ownerAccount = Lens.lens (\VirtualInterfaceTestHistory' {ownerAccount} -> ownerAccount) (\s@VirtualInterfaceTestHistory' {} a -> s {ownerAccount = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface moves to the DOWN state.
virtualInterfaceTestHistory_startTime :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.UTCTime)
virtualInterfaceTestHistory_startTime = Lens.lens (\VirtualInterfaceTestHistory' {startTime} -> startTime) (\s@VirtualInterfaceTestHistory' {} a -> s {startTime = a} :: VirtualInterfaceTestHistory) Prelude.. Lens.mapping Data._Time

-- | The status of the virtual interface failover test.
virtualInterfaceTestHistory_status :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_status = Lens.lens (\VirtualInterfaceTestHistory' {status} -> status) (\s@VirtualInterfaceTestHistory' {} a -> s {status = a} :: VirtualInterfaceTestHistory)

-- | The time that the virtual interface failover test ran in minutes.
virtualInterfaceTestHistory_testDurationInMinutes :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Int)
virtualInterfaceTestHistory_testDurationInMinutes = Lens.lens (\VirtualInterfaceTestHistory' {testDurationInMinutes} -> testDurationInMinutes) (\s@VirtualInterfaceTestHistory' {} a -> s {testDurationInMinutes = a} :: VirtualInterfaceTestHistory)

-- | The ID of the virtual interface failover test.
virtualInterfaceTestHistory_testId :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_testId = Lens.lens (\VirtualInterfaceTestHistory' {testId} -> testId) (\s@VirtualInterfaceTestHistory' {} a -> s {testId = a} :: VirtualInterfaceTestHistory)

-- | The ID of the tested virtual interface.
virtualInterfaceTestHistory_virtualInterfaceId :: Lens.Lens' VirtualInterfaceTestHistory (Prelude.Maybe Prelude.Text)
virtualInterfaceTestHistory_virtualInterfaceId = Lens.lens (\VirtualInterfaceTestHistory' {virtualInterfaceId} -> virtualInterfaceId) (\s@VirtualInterfaceTestHistory' {} a -> s {virtualInterfaceId = a} :: VirtualInterfaceTestHistory)

instance Data.FromJSON VirtualInterfaceTestHistory where
  parseJSON =
    Data.withObject
      "VirtualInterfaceTestHistory"
      ( \x ->
          VirtualInterfaceTestHistory'
            Prelude.<$> (x Data..:? "bgpPeers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "ownerAccount")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "testDurationInMinutes")
            Prelude.<*> (x Data..:? "testId")
            Prelude.<*> (x Data..:? "virtualInterfaceId")
      )

instance Prelude.Hashable VirtualInterfaceTestHistory where
  hashWithSalt _salt VirtualInterfaceTestHistory' {..} =
    _salt
      `Prelude.hashWithSalt` bgpPeers
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` testDurationInMinutes
      `Prelude.hashWithSalt` testId
      `Prelude.hashWithSalt` virtualInterfaceId

instance Prelude.NFData VirtualInterfaceTestHistory where
  rnf VirtualInterfaceTestHistory' {..} =
    Prelude.rnf bgpPeers `Prelude.seq`
      Prelude.rnf endTime `Prelude.seq`
        Prelude.rnf ownerAccount `Prelude.seq`
          Prelude.rnf startTime `Prelude.seq`
            Prelude.rnf status `Prelude.seq`
              Prelude.rnf testDurationInMinutes `Prelude.seq`
                Prelude.rnf testId `Prelude.seq`
                  Prelude.rnf virtualInterfaceId
