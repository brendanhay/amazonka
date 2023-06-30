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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskCreate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.LoRaWANGatewayVersion
import qualified Amazonka.Prelude as Prelude

-- | LoRaWANUpdateGatewayTaskCreate object.
--
-- /See:/ 'newLoRaWANUpdateGatewayTaskCreate' smart constructor.
data LoRaWANUpdateGatewayTaskCreate = LoRaWANUpdateGatewayTaskCreate'
  { -- | The version of the gateways that should receive the update.
    currentVersion :: Prelude.Maybe LoRaWANGatewayVersion,
    -- | The CRC of the signature private key to check.
    sigKeyCrc :: Prelude.Maybe Prelude.Natural,
    -- | The signature used to verify the update firmware.
    updateSignature :: Prelude.Maybe Prelude.Text,
    -- | The firmware version to update the gateway to.
    updateVersion :: Prelude.Maybe LoRaWANGatewayVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANUpdateGatewayTaskCreate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'loRaWANUpdateGatewayTaskCreate_currentVersion' - The version of the gateways that should receive the update.
--
-- 'sigKeyCrc', 'loRaWANUpdateGatewayTaskCreate_sigKeyCrc' - The CRC of the signature private key to check.
--
-- 'updateSignature', 'loRaWANUpdateGatewayTaskCreate_updateSignature' - The signature used to verify the update firmware.
--
-- 'updateVersion', 'loRaWANUpdateGatewayTaskCreate_updateVersion' - The firmware version to update the gateway to.
newLoRaWANUpdateGatewayTaskCreate ::
  LoRaWANUpdateGatewayTaskCreate
newLoRaWANUpdateGatewayTaskCreate =
  LoRaWANUpdateGatewayTaskCreate'
    { currentVersion =
        Prelude.Nothing,
      sigKeyCrc = Prelude.Nothing,
      updateSignature = Prelude.Nothing,
      updateVersion = Prelude.Nothing
    }

-- | The version of the gateways that should receive the update.
loRaWANUpdateGatewayTaskCreate_currentVersion :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe LoRaWANGatewayVersion)
loRaWANUpdateGatewayTaskCreate_currentVersion = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {currentVersion} -> currentVersion) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {currentVersion = a} :: LoRaWANUpdateGatewayTaskCreate)

-- | The CRC of the signature private key to check.
loRaWANUpdateGatewayTaskCreate_sigKeyCrc :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe Prelude.Natural)
loRaWANUpdateGatewayTaskCreate_sigKeyCrc = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {sigKeyCrc} -> sigKeyCrc) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {sigKeyCrc = a} :: LoRaWANUpdateGatewayTaskCreate)

-- | The signature used to verify the update firmware.
loRaWANUpdateGatewayTaskCreate_updateSignature :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe Prelude.Text)
loRaWANUpdateGatewayTaskCreate_updateSignature = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {updateSignature} -> updateSignature) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {updateSignature = a} :: LoRaWANUpdateGatewayTaskCreate)

-- | The firmware version to update the gateway to.
loRaWANUpdateGatewayTaskCreate_updateVersion :: Lens.Lens' LoRaWANUpdateGatewayTaskCreate (Prelude.Maybe LoRaWANGatewayVersion)
loRaWANUpdateGatewayTaskCreate_updateVersion = Lens.lens (\LoRaWANUpdateGatewayTaskCreate' {updateVersion} -> updateVersion) (\s@LoRaWANUpdateGatewayTaskCreate' {} a -> s {updateVersion = a} :: LoRaWANUpdateGatewayTaskCreate)

instance Data.FromJSON LoRaWANUpdateGatewayTaskCreate where
  parseJSON =
    Data.withObject
      "LoRaWANUpdateGatewayTaskCreate"
      ( \x ->
          LoRaWANUpdateGatewayTaskCreate'
            Prelude.<$> (x Data..:? "CurrentVersion")
            Prelude.<*> (x Data..:? "SigKeyCrc")
            Prelude.<*> (x Data..:? "UpdateSignature")
            Prelude.<*> (x Data..:? "UpdateVersion")
      )

instance
  Prelude.Hashable
    LoRaWANUpdateGatewayTaskCreate
  where
  hashWithSalt
    _salt
    LoRaWANUpdateGatewayTaskCreate' {..} =
      _salt
        `Prelude.hashWithSalt` currentVersion
        `Prelude.hashWithSalt` sigKeyCrc
        `Prelude.hashWithSalt` updateSignature
        `Prelude.hashWithSalt` updateVersion

instance
  Prelude.NFData
    LoRaWANUpdateGatewayTaskCreate
  where
  rnf LoRaWANUpdateGatewayTaskCreate' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf sigKeyCrc
      `Prelude.seq` Prelude.rnf updateSignature
      `Prelude.seq` Prelude.rnf updateVersion

instance Data.ToJSON LoRaWANUpdateGatewayTaskCreate where
  toJSON LoRaWANUpdateGatewayTaskCreate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CurrentVersion" Data..=)
              Prelude.<$> currentVersion,
            ("SigKeyCrc" Data..=) Prelude.<$> sigKeyCrc,
            ("UpdateSignature" Data..=)
              Prelude.<$> updateSignature,
            ("UpdateVersion" Data..=) Prelude.<$> updateVersion
          ]
      )
