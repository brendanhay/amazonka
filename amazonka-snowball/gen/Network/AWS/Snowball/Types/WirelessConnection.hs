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
-- Module      : Network.AWS.Snowball.Types.WirelessConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.WirelessConnection where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configures the wireless connection on an AWS Snowcone device.
--
-- /See:/ 'newWirelessConnection' smart constructor.
data WirelessConnection = WirelessConnection'
  { -- | Enables the Wi-Fi adapter on an AWS Snowcone device.
    isWifiEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WirelessConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isWifiEnabled', 'wirelessConnection_isWifiEnabled' - Enables the Wi-Fi adapter on an AWS Snowcone device.
newWirelessConnection ::
  WirelessConnection
newWirelessConnection =
  WirelessConnection'
    { isWifiEnabled =
        Prelude.Nothing
    }

-- | Enables the Wi-Fi adapter on an AWS Snowcone device.
wirelessConnection_isWifiEnabled :: Lens.Lens' WirelessConnection (Prelude.Maybe Prelude.Bool)
wirelessConnection_isWifiEnabled = Lens.lens (\WirelessConnection' {isWifiEnabled} -> isWifiEnabled) (\s@WirelessConnection' {} a -> s {isWifiEnabled = a} :: WirelessConnection)

instance Prelude.FromJSON WirelessConnection where
  parseJSON =
    Prelude.withObject
      "WirelessConnection"
      ( \x ->
          WirelessConnection'
            Prelude.<$> (x Prelude..:? "IsWifiEnabled")
      )

instance Prelude.Hashable WirelessConnection

instance Prelude.NFData WirelessConnection

instance Prelude.ToJSON WirelessConnection where
  toJSON WirelessConnection' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IsWifiEnabled" Prelude..=)
              Prelude.<$> isWifiEnabled
          ]
      )
