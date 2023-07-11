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
-- Module      : Amazonka.Panorama.Types.NetworkPayload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NetworkPayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.EthernetPayload
import Amazonka.Panorama.Types.NtpPayload
import qualified Amazonka.Prelude as Prelude

-- | The network configuration for a device.
--
-- /See:/ 'newNetworkPayload' smart constructor.
data NetworkPayload = NetworkPayload'
  { -- | Settings for Ethernet port 0.
    ethernet0 :: Prelude.Maybe EthernetPayload,
    -- | Settings for Ethernet port 1.
    ethernet1 :: Prelude.Maybe EthernetPayload,
    -- | Network time protocol (NTP) server settings.
    ntp :: Prelude.Maybe NtpPayload
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ethernet0', 'networkPayload_ethernet0' - Settings for Ethernet port 0.
--
-- 'ethernet1', 'networkPayload_ethernet1' - Settings for Ethernet port 1.
--
-- 'ntp', 'networkPayload_ntp' - Network time protocol (NTP) server settings.
newNetworkPayload ::
  NetworkPayload
newNetworkPayload =
  NetworkPayload'
    { ethernet0 = Prelude.Nothing,
      ethernet1 = Prelude.Nothing,
      ntp = Prelude.Nothing
    }

-- | Settings for Ethernet port 0.
networkPayload_ethernet0 :: Lens.Lens' NetworkPayload (Prelude.Maybe EthernetPayload)
networkPayload_ethernet0 = Lens.lens (\NetworkPayload' {ethernet0} -> ethernet0) (\s@NetworkPayload' {} a -> s {ethernet0 = a} :: NetworkPayload)

-- | Settings for Ethernet port 1.
networkPayload_ethernet1 :: Lens.Lens' NetworkPayload (Prelude.Maybe EthernetPayload)
networkPayload_ethernet1 = Lens.lens (\NetworkPayload' {ethernet1} -> ethernet1) (\s@NetworkPayload' {} a -> s {ethernet1 = a} :: NetworkPayload)

-- | Network time protocol (NTP) server settings.
networkPayload_ntp :: Lens.Lens' NetworkPayload (Prelude.Maybe NtpPayload)
networkPayload_ntp = Lens.lens (\NetworkPayload' {ntp} -> ntp) (\s@NetworkPayload' {} a -> s {ntp = a} :: NetworkPayload)

instance Data.FromJSON NetworkPayload where
  parseJSON =
    Data.withObject
      "NetworkPayload"
      ( \x ->
          NetworkPayload'
            Prelude.<$> (x Data..:? "Ethernet0")
            Prelude.<*> (x Data..:? "Ethernet1")
            Prelude.<*> (x Data..:? "Ntp")
      )

instance Prelude.Hashable NetworkPayload where
  hashWithSalt _salt NetworkPayload' {..} =
    _salt
      `Prelude.hashWithSalt` ethernet0
      `Prelude.hashWithSalt` ethernet1
      `Prelude.hashWithSalt` ntp

instance Prelude.NFData NetworkPayload where
  rnf NetworkPayload' {..} =
    Prelude.rnf ethernet0
      `Prelude.seq` Prelude.rnf ethernet1
      `Prelude.seq` Prelude.rnf ntp

instance Data.ToJSON NetworkPayload where
  toJSON NetworkPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Ethernet0" Data..=) Prelude.<$> ethernet0,
            ("Ethernet1" Data..=) Prelude.<$> ethernet1,
            ("Ntp" Data..=) Prelude.<$> ntp
          ]
      )
