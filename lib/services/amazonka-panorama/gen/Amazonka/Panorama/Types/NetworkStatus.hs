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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NetworkStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Panorama.Types.EthernetStatus
import qualified Amazonka.Prelude as Prelude

-- | The network status of a device.
--
-- /See:/ 'newNetworkStatus' smart constructor.
data NetworkStatus = NetworkStatus'
  { -- | The status of Ethernet port 1.
    ethernet1Status :: Prelude.Maybe EthernetStatus,
    -- | The status of Ethernet port 0.
    ethernet0Status :: Prelude.Maybe EthernetStatus
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
-- 'ethernet1Status', 'networkStatus_ethernet1Status' - The status of Ethernet port 1.
--
-- 'ethernet0Status', 'networkStatus_ethernet0Status' - The status of Ethernet port 0.
newNetworkStatus ::
  NetworkStatus
newNetworkStatus =
  NetworkStatus'
    { ethernet1Status = Prelude.Nothing,
      ethernet0Status = Prelude.Nothing
    }

-- | The status of Ethernet port 1.
networkStatus_ethernet1Status :: Lens.Lens' NetworkStatus (Prelude.Maybe EthernetStatus)
networkStatus_ethernet1Status = Lens.lens (\NetworkStatus' {ethernet1Status} -> ethernet1Status) (\s@NetworkStatus' {} a -> s {ethernet1Status = a} :: NetworkStatus)

-- | The status of Ethernet port 0.
networkStatus_ethernet0Status :: Lens.Lens' NetworkStatus (Prelude.Maybe EthernetStatus)
networkStatus_ethernet0Status = Lens.lens (\NetworkStatus' {ethernet0Status} -> ethernet0Status) (\s@NetworkStatus' {} a -> s {ethernet0Status = a} :: NetworkStatus)

instance Core.FromJSON NetworkStatus where
  parseJSON =
    Core.withObject
      "NetworkStatus"
      ( \x ->
          NetworkStatus'
            Prelude.<$> (x Core..:? "Ethernet1Status")
            Prelude.<*> (x Core..:? "Ethernet0Status")
      )

instance Prelude.Hashable NetworkStatus where
  hashWithSalt _salt NetworkStatus' {..} =
    _salt `Prelude.hashWithSalt` ethernet1Status
      `Prelude.hashWithSalt` ethernet0Status

instance Prelude.NFData NetworkStatus where
  rnf NetworkStatus' {..} =
    Prelude.rnf ethernet1Status
      `Prelude.seq` Prelude.rnf ethernet0Status
