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
-- Module      : Amazonka.MediaLive.Types.InputDeviceNetworkSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDeviceNetworkSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.InputDeviceIpScheme
import qualified Amazonka.Prelude as Prelude

-- | The network settings for the input device.
--
-- /See:/ 'newInputDeviceNetworkSettings' smart constructor.
data InputDeviceNetworkSettings = InputDeviceNetworkSettings'
  { -- | The network gateway IP address.
    gateway :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the input device has been configured (outside of
    -- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
    -- address.
    ipScheme :: Prelude.Maybe InputDeviceIpScheme,
    -- | The DNS addresses of the input device.
    dnsAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The subnet mask of the input device.
    subnetMask :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the input device.
    ipAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDeviceNetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gateway', 'inputDeviceNetworkSettings_gateway' - The network gateway IP address.
--
-- 'ipScheme', 'inputDeviceNetworkSettings_ipScheme' - Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
--
-- 'dnsAddresses', 'inputDeviceNetworkSettings_dnsAddresses' - The DNS addresses of the input device.
--
-- 'subnetMask', 'inputDeviceNetworkSettings_subnetMask' - The subnet mask of the input device.
--
-- 'ipAddress', 'inputDeviceNetworkSettings_ipAddress' - The IP address of the input device.
newInputDeviceNetworkSettings ::
  InputDeviceNetworkSettings
newInputDeviceNetworkSettings =
  InputDeviceNetworkSettings'
    { gateway =
        Prelude.Nothing,
      ipScheme = Prelude.Nothing,
      dnsAddresses = Prelude.Nothing,
      subnetMask = Prelude.Nothing,
      ipAddress = Prelude.Nothing
    }

-- | The network gateway IP address.
inputDeviceNetworkSettings_gateway :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_gateway = Lens.lens (\InputDeviceNetworkSettings' {gateway} -> gateway) (\s@InputDeviceNetworkSettings' {} a -> s {gateway = a} :: InputDeviceNetworkSettings)

-- | Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
inputDeviceNetworkSettings_ipScheme :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe InputDeviceIpScheme)
inputDeviceNetworkSettings_ipScheme = Lens.lens (\InputDeviceNetworkSettings' {ipScheme} -> ipScheme) (\s@InputDeviceNetworkSettings' {} a -> s {ipScheme = a} :: InputDeviceNetworkSettings)

-- | The DNS addresses of the input device.
inputDeviceNetworkSettings_dnsAddresses :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe [Prelude.Text])
inputDeviceNetworkSettings_dnsAddresses = Lens.lens (\InputDeviceNetworkSettings' {dnsAddresses} -> dnsAddresses) (\s@InputDeviceNetworkSettings' {} a -> s {dnsAddresses = a} :: InputDeviceNetworkSettings) Prelude.. Lens.mapping Lens.coerced

-- | The subnet mask of the input device.
inputDeviceNetworkSettings_subnetMask :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_subnetMask = Lens.lens (\InputDeviceNetworkSettings' {subnetMask} -> subnetMask) (\s@InputDeviceNetworkSettings' {} a -> s {subnetMask = a} :: InputDeviceNetworkSettings)

-- | The IP address of the input device.
inputDeviceNetworkSettings_ipAddress :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_ipAddress = Lens.lens (\InputDeviceNetworkSettings' {ipAddress} -> ipAddress) (\s@InputDeviceNetworkSettings' {} a -> s {ipAddress = a} :: InputDeviceNetworkSettings)

instance Core.FromJSON InputDeviceNetworkSettings where
  parseJSON =
    Core.withObject
      "InputDeviceNetworkSettings"
      ( \x ->
          InputDeviceNetworkSettings'
            Prelude.<$> (x Core..:? "gateway")
            Prelude.<*> (x Core..:? "ipScheme")
            Prelude.<*> (x Core..:? "dnsAddresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subnetMask")
            Prelude.<*> (x Core..:? "ipAddress")
      )

instance Prelude.Hashable InputDeviceNetworkSettings where
  hashWithSalt _salt InputDeviceNetworkSettings' {..} =
    _salt `Prelude.hashWithSalt` gateway
      `Prelude.hashWithSalt` ipScheme
      `Prelude.hashWithSalt` dnsAddresses
      `Prelude.hashWithSalt` subnetMask
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData InputDeviceNetworkSettings where
  rnf InputDeviceNetworkSettings' {..} =
    Prelude.rnf gateway
      `Prelude.seq` Prelude.rnf ipScheme
      `Prelude.seq` Prelude.rnf dnsAddresses
      `Prelude.seq` Prelude.rnf subnetMask
      `Prelude.seq` Prelude.rnf ipAddress
