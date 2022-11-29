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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANSendDataToDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANSendDataToDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.ParticipatingGateways
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN router info.
--
-- /See:/ 'newLoRaWANSendDataToDevice' smart constructor.
data LoRaWANSendDataToDevice = LoRaWANSendDataToDevice'
  { fPort :: Prelude.Maybe Prelude.Natural,
    -- | Choose the gateways that you want to use for the downlink data traffic
    -- when the wireless device is running in class B or class C mode.
    participatingGateways :: Prelude.Maybe ParticipatingGateways
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANSendDataToDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fPort', 'loRaWANSendDataToDevice_fPort' - Undocumented member.
--
-- 'participatingGateways', 'loRaWANSendDataToDevice_participatingGateways' - Choose the gateways that you want to use for the downlink data traffic
-- when the wireless device is running in class B or class C mode.
newLoRaWANSendDataToDevice ::
  LoRaWANSendDataToDevice
newLoRaWANSendDataToDevice =
  LoRaWANSendDataToDevice'
    { fPort = Prelude.Nothing,
      participatingGateways = Prelude.Nothing
    }

-- | Undocumented member.
loRaWANSendDataToDevice_fPort :: Lens.Lens' LoRaWANSendDataToDevice (Prelude.Maybe Prelude.Natural)
loRaWANSendDataToDevice_fPort = Lens.lens (\LoRaWANSendDataToDevice' {fPort} -> fPort) (\s@LoRaWANSendDataToDevice' {} a -> s {fPort = a} :: LoRaWANSendDataToDevice)

-- | Choose the gateways that you want to use for the downlink data traffic
-- when the wireless device is running in class B or class C mode.
loRaWANSendDataToDevice_participatingGateways :: Lens.Lens' LoRaWANSendDataToDevice (Prelude.Maybe ParticipatingGateways)
loRaWANSendDataToDevice_participatingGateways = Lens.lens (\LoRaWANSendDataToDevice' {participatingGateways} -> participatingGateways) (\s@LoRaWANSendDataToDevice' {} a -> s {participatingGateways = a} :: LoRaWANSendDataToDevice)

instance Core.FromJSON LoRaWANSendDataToDevice where
  parseJSON =
    Core.withObject
      "LoRaWANSendDataToDevice"
      ( \x ->
          LoRaWANSendDataToDevice'
            Prelude.<$> (x Core..:? "FPort")
            Prelude.<*> (x Core..:? "ParticipatingGateways")
      )

instance Prelude.Hashable LoRaWANSendDataToDevice where
  hashWithSalt _salt LoRaWANSendDataToDevice' {..} =
    _salt `Prelude.hashWithSalt` fPort
      `Prelude.hashWithSalt` participatingGateways

instance Prelude.NFData LoRaWANSendDataToDevice where
  rnf LoRaWANSendDataToDevice' {..} =
    Prelude.rnf fPort
      `Prelude.seq` Prelude.rnf participatingGateways

instance Core.ToJSON LoRaWANSendDataToDevice where
  toJSON LoRaWANSendDataToDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FPort" Core..=) Prelude.<$> fPort,
            ("ParticipatingGateways" Core..=)
              Prelude.<$> participatingGateways
          ]
      )
