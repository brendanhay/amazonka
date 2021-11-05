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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANSendDataToDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN router info.
--
-- /See:/ 'newLoRaWANSendDataToDevice' smart constructor.
data LoRaWANSendDataToDevice = LoRaWANSendDataToDevice'
  { -- | The Fport value.
    fPort :: Prelude.Maybe Prelude.Natural
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
-- 'fPort', 'loRaWANSendDataToDevice_fPort' - The Fport value.
newLoRaWANSendDataToDevice ::
  LoRaWANSendDataToDevice
newLoRaWANSendDataToDevice =
  LoRaWANSendDataToDevice' {fPort = Prelude.Nothing}

-- | The Fport value.
loRaWANSendDataToDevice_fPort :: Lens.Lens' LoRaWANSendDataToDevice (Prelude.Maybe Prelude.Natural)
loRaWANSendDataToDevice_fPort = Lens.lens (\LoRaWANSendDataToDevice' {fPort} -> fPort) (\s@LoRaWANSendDataToDevice' {} a -> s {fPort = a} :: LoRaWANSendDataToDevice)

instance Prelude.Hashable LoRaWANSendDataToDevice

instance Prelude.NFData LoRaWANSendDataToDevice

instance Core.ToJSON LoRaWANSendDataToDevice where
  toJSON LoRaWANSendDataToDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [("FPort" Core..=) Prelude.<$> fPort]
      )
