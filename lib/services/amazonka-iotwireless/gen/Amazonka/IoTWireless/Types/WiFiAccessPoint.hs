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
-- Module      : Amazonka.IoTWireless.Types.WiFiAccessPoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WiFiAccessPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Wi-Fi access point.
--
-- /See:/ 'newWiFiAccessPoint' smart constructor.
data WiFiAccessPoint = WiFiAccessPoint'
  { -- | Wi-Fi MAC Address.
    macAddress :: Prelude.Text,
    -- | Recived signal strength of the WLAN measurement data.
    rss :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WiFiAccessPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'macAddress', 'wiFiAccessPoint_macAddress' - Wi-Fi MAC Address.
--
-- 'rss', 'wiFiAccessPoint_rss' - Recived signal strength of the WLAN measurement data.
newWiFiAccessPoint ::
  -- | 'macAddress'
  Prelude.Text ->
  -- | 'rss'
  Prelude.Int ->
  WiFiAccessPoint
newWiFiAccessPoint pMacAddress_ pRss_ =
  WiFiAccessPoint'
    { macAddress = pMacAddress_,
      rss = pRss_
    }

-- | Wi-Fi MAC Address.
wiFiAccessPoint_macAddress :: Lens.Lens' WiFiAccessPoint Prelude.Text
wiFiAccessPoint_macAddress = Lens.lens (\WiFiAccessPoint' {macAddress} -> macAddress) (\s@WiFiAccessPoint' {} a -> s {macAddress = a} :: WiFiAccessPoint)

-- | Recived signal strength of the WLAN measurement data.
wiFiAccessPoint_rss :: Lens.Lens' WiFiAccessPoint Prelude.Int
wiFiAccessPoint_rss = Lens.lens (\WiFiAccessPoint' {rss} -> rss) (\s@WiFiAccessPoint' {} a -> s {rss = a} :: WiFiAccessPoint)

instance Prelude.Hashable WiFiAccessPoint where
  hashWithSalt _salt WiFiAccessPoint' {..} =
    _salt
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` rss

instance Prelude.NFData WiFiAccessPoint where
  rnf WiFiAccessPoint' {..} =
    Prelude.rnf macAddress `Prelude.seq`
      Prelude.rnf rss

instance Data.ToJSON WiFiAccessPoint where
  toJSON WiFiAccessPoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MacAddress" Data..= macAddress),
            Prelude.Just ("Rss" Data..= rss)
          ]
      )
