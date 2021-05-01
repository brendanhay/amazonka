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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceNetworkSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceNetworkSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceIpScheme
import qualified Network.AWS.Prelude as Prelude

-- | The network settings for the input device.
--
-- /See:/ 'newInputDeviceNetworkSettings' smart constructor.
data InputDeviceNetworkSettings = InputDeviceNetworkSettings'
  { -- | The DNS addresses of the input device.
    dnsAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The IP address of the input device.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The subnet mask of the input device.
    subnetMask :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the input device has been configured (outside of
    -- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
    -- address.
    ipScheme :: Prelude.Maybe InputDeviceIpScheme,
    -- | The network gateway IP address.
    gateway :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputDeviceNetworkSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsAddresses', 'inputDeviceNetworkSettings_dnsAddresses' - The DNS addresses of the input device.
--
-- 'ipAddress', 'inputDeviceNetworkSettings_ipAddress' - The IP address of the input device.
--
-- 'subnetMask', 'inputDeviceNetworkSettings_subnetMask' - The subnet mask of the input device.
--
-- 'ipScheme', 'inputDeviceNetworkSettings_ipScheme' - Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
--
-- 'gateway', 'inputDeviceNetworkSettings_gateway' - The network gateway IP address.
newInputDeviceNetworkSettings ::
  InputDeviceNetworkSettings
newInputDeviceNetworkSettings =
  InputDeviceNetworkSettings'
    { dnsAddresses =
        Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      subnetMask = Prelude.Nothing,
      ipScheme = Prelude.Nothing,
      gateway = Prelude.Nothing
    }

-- | The DNS addresses of the input device.
inputDeviceNetworkSettings_dnsAddresses :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe [Prelude.Text])
inputDeviceNetworkSettings_dnsAddresses = Lens.lens (\InputDeviceNetworkSettings' {dnsAddresses} -> dnsAddresses) (\s@InputDeviceNetworkSettings' {} a -> s {dnsAddresses = a} :: InputDeviceNetworkSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | The IP address of the input device.
inputDeviceNetworkSettings_ipAddress :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_ipAddress = Lens.lens (\InputDeviceNetworkSettings' {ipAddress} -> ipAddress) (\s@InputDeviceNetworkSettings' {} a -> s {ipAddress = a} :: InputDeviceNetworkSettings)

-- | The subnet mask of the input device.
inputDeviceNetworkSettings_subnetMask :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_subnetMask = Lens.lens (\InputDeviceNetworkSettings' {subnetMask} -> subnetMask) (\s@InputDeviceNetworkSettings' {} a -> s {subnetMask = a} :: InputDeviceNetworkSettings)

-- | Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
inputDeviceNetworkSettings_ipScheme :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe InputDeviceIpScheme)
inputDeviceNetworkSettings_ipScheme = Lens.lens (\InputDeviceNetworkSettings' {ipScheme} -> ipScheme) (\s@InputDeviceNetworkSettings' {} a -> s {ipScheme = a} :: InputDeviceNetworkSettings)

-- | The network gateway IP address.
inputDeviceNetworkSettings_gateway :: Lens.Lens' InputDeviceNetworkSettings (Prelude.Maybe Prelude.Text)
inputDeviceNetworkSettings_gateway = Lens.lens (\InputDeviceNetworkSettings' {gateway} -> gateway) (\s@InputDeviceNetworkSettings' {} a -> s {gateway = a} :: InputDeviceNetworkSettings)

instance Prelude.FromJSON InputDeviceNetworkSettings where
  parseJSON =
    Prelude.withObject
      "InputDeviceNetworkSettings"
      ( \x ->
          InputDeviceNetworkSettings'
            Prelude.<$> ( x Prelude..:? "dnsAddresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "ipAddress")
            Prelude.<*> (x Prelude..:? "subnetMask")
            Prelude.<*> (x Prelude..:? "ipScheme")
            Prelude.<*> (x Prelude..:? "gateway")
      )

instance Prelude.Hashable InputDeviceNetworkSettings

instance Prelude.NFData InputDeviceNetworkSettings
