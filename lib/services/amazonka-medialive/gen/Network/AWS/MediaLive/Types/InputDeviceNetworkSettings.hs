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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceNetworkSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceIpScheme
import qualified Network.AWS.Prelude as Prelude

-- | The network settings for the input device.
--
-- /See:/ 'newInputDeviceNetworkSettings' smart constructor.
data InputDeviceNetworkSettings = InputDeviceNetworkSettings'
  { -- | The IP address of the input device.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The network gateway IP address.
    gateway :: Prelude.Maybe Prelude.Text,
    -- | The DNS addresses of the input device.
    dnsAddresses :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether the input device has been configured (outside of
    -- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
    -- address.
    ipScheme :: Prelude.Maybe InputDeviceIpScheme,
    -- | The subnet mask of the input device.
    subnetMask :: Prelude.Maybe Prelude.Text
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
-- 'ipAddress', 'inputDeviceNetworkSettings_ipAddress' - The IP address of the input device.
--
-- 'gateway', 'inputDeviceNetworkSettings_gateway' - The network gateway IP address.
--
-- 'dnsAddresses', 'inputDeviceNetworkSettings_dnsAddresses' - The DNS addresses of the input device.
--
-- 'ipScheme', 'inputDeviceNetworkSettings_ipScheme' - Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
--
-- 'subnetMask', 'inputDeviceNetworkSettings_subnetMask' - The subnet mask of the input device.
newInputDeviceNetworkSettings ::
  InputDeviceNetworkSettings
newInputDeviceNetworkSettings =
  InputDeviceNetworkSettings'
    { ipAddress =
        Prelude.Nothing,
      gateway = Prelude.Nothing,
      dnsAddresses = Prelude.Nothing,
      ipScheme = Prelude.Nothing,
      subnetMask = Prelude.Nothing
    }

-- | The IP address of the input device.
inputDeviceNetworkSettings_ipAddress :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_ipAddress = Lens.lens (\InputDeviceNetworkSettings' {ipAddress} -> ipAddress) (\s@InputDeviceNetworkSettings' {} a -> s {ipAddress = a} :: InputDeviceNetworkSettings)

-- | The network gateway IP address.
inputDeviceNetworkSettings_gateway :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_gateway = Lens.lens (\InputDeviceNetworkSettings' {gateway} -> gateway) (\s@InputDeviceNetworkSettings' {} a -> s {gateway = a} :: InputDeviceNetworkSettings)

-- | The DNS addresses of the input device.
inputDeviceNetworkSettings_dnsAddresses :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe [Prelude.Text])
inputDeviceNetworkSettings_dnsAddresses = Lens.lens (\InputDeviceNetworkSettings' {dnsAddresses} -> dnsAddresses) (\s@InputDeviceNetworkSettings' {} a -> s {dnsAddresses = a} :: InputDeviceNetworkSettings) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
inputDeviceNetworkSettings_ipScheme :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe InputDeviceIpScheme)
inputDeviceNetworkSettings_ipScheme = Lens.lens (\InputDeviceNetworkSettings' {ipScheme} -> ipScheme) (\s@InputDeviceNetworkSettings' {} a -> s {ipScheme = a} :: InputDeviceNetworkSettings)

-- | The subnet mask of the input device.
inputDeviceNetworkSettings_subnetMask :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_subnetMask = Lens.lens (\InputDeviceNetworkSettings' {subnetMask} -> subnetMask) (\s@InputDeviceNetworkSettings' {} a -> s {subnetMask = a} :: InputDeviceNetworkSettings)

instance Core.FromJSON InputDeviceNetworkSettings where
  parseJSON =
    Core.withObject
      "InputDeviceNetworkSettings"
      ( \x ->
          InputDeviceNetworkSettings'
            Prelude.<$> (x Core..:? "ipAddress")
            Prelude.<*> (x Core..:? "gateway")
            Prelude.<*> (x Core..:? "dnsAddresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ipScheme")
            Prelude.<*> (x Core..:? "subnetMask")
      )

instance Prelude.Hashable InputDeviceNetworkSettings

instance Prelude.NFData InputDeviceNetworkSettings
