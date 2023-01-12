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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANListDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANListDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN object for list functions.
--
-- /See:/ 'newLoRaWANListDevice' smart constructor.
data LoRaWANListDevice = LoRaWANListDevice'
  { -- | The DevEUI value.
    devEui :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANListDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devEui', 'loRaWANListDevice_devEui' - The DevEUI value.
newLoRaWANListDevice ::
  LoRaWANListDevice
newLoRaWANListDevice =
  LoRaWANListDevice' {devEui = Prelude.Nothing}

-- | The DevEUI value.
loRaWANListDevice_devEui :: Lens.Lens' LoRaWANListDevice (Prelude.Maybe Prelude.Text)
loRaWANListDevice_devEui = Lens.lens (\LoRaWANListDevice' {devEui} -> devEui) (\s@LoRaWANListDevice' {} a -> s {devEui = a} :: LoRaWANListDevice)

instance Data.FromJSON LoRaWANListDevice where
  parseJSON =
    Data.withObject
      "LoRaWANListDevice"
      ( \x ->
          LoRaWANListDevice' Prelude.<$> (x Data..:? "DevEui")
      )

instance Prelude.Hashable LoRaWANListDevice where
  hashWithSalt _salt LoRaWANListDevice' {..} =
    _salt `Prelude.hashWithSalt` devEui

instance Prelude.NFData LoRaWANListDevice where
  rnf LoRaWANListDevice' {..} = Prelude.rnf devEui
