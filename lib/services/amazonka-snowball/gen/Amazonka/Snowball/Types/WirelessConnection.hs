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
-- Module      : Amazonka.Snowball.Types.WirelessConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.WirelessConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configures the wireless connection on an Snowcone device.
--
-- /See:/ 'newWirelessConnection' smart constructor.
data WirelessConnection = WirelessConnection'
  { -- | Enables the Wi-Fi adapter on an Snowcone device.
    isWifiEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isWifiEnabled', 'wirelessConnection_isWifiEnabled' - Enables the Wi-Fi adapter on an Snowcone device.
newWirelessConnection ::
  WirelessConnection
newWirelessConnection =
  WirelessConnection'
    { isWifiEnabled =
        Prelude.Nothing
    }

-- | Enables the Wi-Fi adapter on an Snowcone device.
wirelessConnection_isWifiEnabled :: Lens.Lens' WirelessConnection (Prelude.Maybe Prelude.Bool)
wirelessConnection_isWifiEnabled = Lens.lens (\WirelessConnection' {isWifiEnabled} -> isWifiEnabled) (\s@WirelessConnection' {} a -> s {isWifiEnabled = a} :: WirelessConnection)

instance Data.FromJSON WirelessConnection where
  parseJSON =
    Data.withObject
      "WirelessConnection"
      ( \x ->
          WirelessConnection'
            Prelude.<$> (x Data..:? "IsWifiEnabled")
      )

instance Prelude.Hashable WirelessConnection where
  hashWithSalt _salt WirelessConnection' {..} =
    _salt `Prelude.hashWithSalt` isWifiEnabled

instance Prelude.NFData WirelessConnection where
  rnf WirelessConnection' {..} =
    Prelude.rnf isWifiEnabled

instance Data.ToJSON WirelessConnection where
  toJSON WirelessConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsWifiEnabled" Data..=)
              Prelude.<$> isWifiEnabled
          ]
      )
