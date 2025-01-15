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
-- Module      : Amazonka.Panorama.Types.NetworkStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NetworkStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.EthernetStatus
import Amazonka.Panorama.Types.NtpStatus
import qualified Amazonka.Prelude as Prelude

-- | The network status of a device.
--
-- /See:/ 'newNetworkStatus' smart constructor.
data NetworkStatus = NetworkStatus'
  { -- | The status of Ethernet port 0.
    ethernet0Status :: Prelude.Maybe EthernetStatus,
    -- | The status of Ethernet port 1.
    ethernet1Status :: Prelude.Maybe EthernetStatus,
    -- | When the network status changed.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Details about a network time protocol (NTP) server connection.
    ntpStatus :: Prelude.Maybe NtpStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ethernet0Status', 'networkStatus_ethernet0Status' - The status of Ethernet port 0.
--
-- 'ethernet1Status', 'networkStatus_ethernet1Status' - The status of Ethernet port 1.
--
-- 'lastUpdatedTime', 'networkStatus_lastUpdatedTime' - When the network status changed.
--
-- 'ntpStatus', 'networkStatus_ntpStatus' - Details about a network time protocol (NTP) server connection.
newNetworkStatus ::
  NetworkStatus
newNetworkStatus =
  NetworkStatus'
    { ethernet0Status = Prelude.Nothing,
      ethernet1Status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      ntpStatus = Prelude.Nothing
    }

-- | The status of Ethernet port 0.
networkStatus_ethernet0Status :: Lens.Lens' NetworkStatus (Prelude.Maybe EthernetStatus)
networkStatus_ethernet0Status = Lens.lens (\NetworkStatus' {ethernet0Status} -> ethernet0Status) (\s@NetworkStatus' {} a -> s {ethernet0Status = a} :: NetworkStatus)

-- | The status of Ethernet port 1.
networkStatus_ethernet1Status :: Lens.Lens' NetworkStatus (Prelude.Maybe EthernetStatus)
networkStatus_ethernet1Status = Lens.lens (\NetworkStatus' {ethernet1Status} -> ethernet1Status) (\s@NetworkStatus' {} a -> s {ethernet1Status = a} :: NetworkStatus)

-- | When the network status changed.
networkStatus_lastUpdatedTime :: Lens.Lens' NetworkStatus (Prelude.Maybe Prelude.UTCTime)
networkStatus_lastUpdatedTime = Lens.lens (\NetworkStatus' {lastUpdatedTime} -> lastUpdatedTime) (\s@NetworkStatus' {} a -> s {lastUpdatedTime = a} :: NetworkStatus) Prelude.. Lens.mapping Data._Time

-- | Details about a network time protocol (NTP) server connection.
networkStatus_ntpStatus :: Lens.Lens' NetworkStatus (Prelude.Maybe NtpStatus)
networkStatus_ntpStatus = Lens.lens (\NetworkStatus' {ntpStatus} -> ntpStatus) (\s@NetworkStatus' {} a -> s {ntpStatus = a} :: NetworkStatus)

instance Data.FromJSON NetworkStatus where
  parseJSON =
    Data.withObject
      "NetworkStatus"
      ( \x ->
          NetworkStatus'
            Prelude.<$> (x Data..:? "Ethernet0Status")
            Prelude.<*> (x Data..:? "Ethernet1Status")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "NtpStatus")
      )

instance Prelude.Hashable NetworkStatus where
  hashWithSalt _salt NetworkStatus' {..} =
    _salt
      `Prelude.hashWithSalt` ethernet0Status
      `Prelude.hashWithSalt` ethernet1Status
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` ntpStatus

instance Prelude.NFData NetworkStatus where
  rnf NetworkStatus' {..} =
    Prelude.rnf ethernet0Status `Prelude.seq`
      Prelude.rnf ethernet1Status `Prelude.seq`
        Prelude.rnf lastUpdatedTime `Prelude.seq`
          Prelude.rnf ntpStatus
